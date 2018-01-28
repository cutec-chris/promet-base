{*******************************************************************************
  Copyright (C) Christian Ulrich info@cu-tec.de

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or commercial alternative
  contact us for more information

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
Created 15.04.2016
*******************************************************************************}
unit uappserverhttp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, synautil, uAppServer, blcksock, uhttputil, uBaseApplication;

type
  THTTPHandlerProc = function(Sender : TAppNetworkThrd;Method,URL : string;Headers : TStringList;Input,Output : TMemoryStream;StatusText : string) : Integer;

  { THTTPSession }

  THTTPSession = class
  private
    FSocket: TAppNetworkThrd;
    procedure SetSocket(AValue: TAppNetworkThrd);
  public
    Url : string;
    Code : Integer;
    Command : string;
    Protocol : string;
    NewHeaders : string;
    Close : Boolean;
    Result : TFileStream;
    headers: TStringList;
    InputData: TMemoryStream;
    OutputData: TMemoryStream;
    property Socket : TAppNetworkThrd read FSocket write SetSocket;

    constructor Create;
    destructor Destroy; override;
    procedure ProcessHTTPRequest;
  end;

procedure RegisterHTTPHandler(aHandler : THTTPHandlerProc);
function GetContentType(Extension : string) : string;

implementation

var
  HTTPHandlers : array of THTTPHandlerProc;

function HandleHTTPCommand(Sender : TAppNetworkThrd;FCommand : string) : Boolean;
var
  aCmd: String;
  i: Integer;
  aSock: THTTPSession = nil;
  uri: String;
  n: Integer;
  aReqTime: TDateTime;
  ResultStatusText , tmp: string;
  ProtocolVersion: Extended;
  Close : Boolean = False;
begin
  Result := False;
  if pos(' ',FCommand)>0 then
    aCmd := copy(FCommand,0,pos(' ',FCommand)-1)
  else aCmd := FCommand;
  Fetch(FCommand,' ');
  case Uppercase(aCmd) of
  'GET','HEAD','POST','PUT','DELETE','OPTIONS','REPORT','PROPFIND','PROPPATCH','COPY','MOVE','LOCK','UNLOCK','MKCOL'://HTTP Request
    begin
      for i := 0 to Sender.Objects.Count-1 do
        if TObject(Sender.Objects[i]) is THTTPSession then
          aSock := THTTPSession(Sender.Objects[i]);
      if not Assigned(aSock) then
        begin
          aSock := THTTPSession.Create;
          aSock.Socket := Sender;
          Sender.Objects.Add(aSock);
        end;
      uri := fetch(FCommand, ' ');
      if uri = '' then
        Exit;
      aSock.Url:=uri;
      aSock.protocol := fetch(FCommand, ' ');
      if aSock.protocol = '' then
        aSock.protocol := 'HTTP/1.1'; //direct command handler ??
      aSock.Command := aCmd;
      //aSock.FSocket.Synchronize(aSock.FSocket,@aSock.ProcessHTTPRequest);
      with BaseApplication as IBaseApplication do
        Debug('Processing HTTP Request in Thread');
      aSock.ProcessHTTPRequest;
      //ignore folder icons since they are not included in template
      if (pos('/favicon.',aSock.Url)=0)
      and (pos('/folder.',aSock.Url)=0)
      then
        if (aSock.Code<>200) and (aSock.Code<>301) and (aSock.Code<>304) then
          begin
            with BaseApplication as IBaseApplication do
              Debug('Processing HTTP Handlers');
            aReqTime := Now();
            for i := 0 to Length(HTTPHandlers)-1 do
              begin
                aSock.Code := HTTPHandlers[i](Sender,aCmd, aSock.Url, aSock.Headers, aSock.InputData, aSock.OutputData, ResultStatusText);
                if aSock.Code<>404 then
                  begin
                    writeln(aCmd+' '+aSock.Url+'=>'+IntToStr(aSock.Code)+' in '+IntToStr(round((Now()-aReqTime)*MSecsPerDay))+' ms');
                    break;
                  end;
              end;
          end;
      if aSock.Code=404 then
        writeln(aCmd+' '+aSock.Url+'=>'+IntToStr(aSock.Code)+' in '+IntToStr(round((Now()-aReqTime)*MSecsPerDay))+' ms');
      tmp := aSock.protocol;
      ProtocolVersion := StrToFloatDef(StringReplace(copy(aSock.Protocol,pos('/',aSock.Protocol)+1,length(aSock.Protocol)),'.',DecimalSeparator,[]),0.9);
      tmp := copy(tmp,0,pos('/',tmp)-1);
      with BaseApplication as IBaseApplication do
        Debug('Sending Headers');
      if ResultStatusText = '' then
        begin
          case aSock.Code of
          100:ResultStatusText := 'Continue';
          101:ResultStatusText := 'Switching Protocols';
          200:ResultStatusText := 'OK';
          201:ResultStatusText := 'Created';
          202:ResultStatusText := 'Accepted';
          203:ResultStatusText := 'Non-Authoritative Information';
          204:ResultStatusText := 'No Content';
          205:ResultStatusText := 'Reset Content';
          206:ResultStatusText := 'Partial Content';
          300:ResultStatusText := 'Multiple Choices';
          301:ResultStatusText := 'Moved Permanently';
          302:ResultStatusText := 'Found';
          303:ResultStatusText := 'See Other';
          304:ResultStatusText := 'Not Modified';
          305:ResultStatusText := 'Use Proxy';
          307:ResultStatusText := 'Temporary Redirect';
          400:ResultStatusText := 'Bad Request';
          401:ResultStatusText := 'Unauthorized';
          402:ResultStatusText := 'Payment Required';
          403:ResultStatusText := 'Forbidden';
          404:ResultStatusText := 'Not Found';
          405:ResultStatusText := 'Method Not Allowed';
          406:ResultStatusText := 'Not Acceptable';
          407:ResultStatusText := 'Proxy Authentication Required';
          408:ResultStatusText := 'Request Time-out';
          409:ResultStatusText := 'Conflict';
          410:ResultStatusText := 'Gone';
          411:ResultStatusText := 'Length Required';
          412:ResultStatusText := 'Precondition Failed';
          413:ResultStatusText := 'Request Entity Too Large';
          414:ResultStatusText := 'Request-URI Too Large';
          415:ResultStatusText := 'Unsupported Media Type';
          416:ResultStatusText := 'Requested range not satisfiable';
          417:ResultStatusText := 'Expectation Failed';
          500:ResultStatusText := 'Internal Server Error';
          501:ResultStatusText := 'Not Implemented';
          502:ResultStatusText := 'Bad Gateway';
          503:ResultStatusText := 'Service Unavailable';
          504:ResultStatusText := 'Gateway Time-out';
          505:ResultStatusText := 'HTTP Version not supported';
          else
            ResultStatusText := 'WTF';
          end;
        end;
      TAppNetworkThrd(Sender).sock.SendString(tmp+'/1.1 ' + IntTostr(aSock.Code) + ' '+ ResultStatusText + CRLF);
      if aSock.protocol <> '' then
      begin
        //aSock.Close:=True;//TODO:find keep-alive bug and remove this
        aSock.headers.Add('Date: ' + Rfc822DateTime(LocalTimeToGMT(now)));
        aSock.headers.Add('Server: Avamm Internal Network');
        if aSock.Code<>304 then
          aSock.headers.Add('Content-length: ' + IntTostr(aSock.OutputData.Size));
        if (ProtocolVersion < 1.1) or aSock.close then
          aSock.headers.Add('Connection: close')
        else
          aSock.headers.Add('Connection: keep-alive');
        tmp := '';
        for n := 0 to aSock.headers.count - 1 do
          if aSock.headers[n]<>'' then
            tmp := tmp+aSock.headers[n] + CRLF;
        TAppNetworkThrd(Sender).sock.sendstring(tmp+CRLF);
        if TAppNetworkThrd(Sender).sock.lasterror <> 0 then
          begin
            writeln(IntToStr(TAppNetworkThrd(Sender).Id)+':Sock.LastError is '+TAppNetworkThrd(Sender).sock.LastErrorDesc);
            Exit;
          end;
      end
      else
        with BaseApplication as IBaseApplication do
          Error('No protocoll ?! ...');
      with BaseApplication as IBaseApplication do
        Debug('Sending Body');
      TAppNetworkThrd(Sender).Sock.SendBuffer(aSock.OutputData.Memory, aSock.OutputData.Size);
      if TAppNetworkThrd(Sender).sock.lasterror <> 0 then
        begin
          with BaseApplication as IBaseApplication do
            Debug(IntToStr(TAppNetworkThrd(Sender).Id)+':Sock.LastError is '+TAppNetworkThrd(Sender).sock.LastErrorDesc);
          Exit;
        end;
      TAppNetworkThrd(Sender).Close:=aSock.Close;
      with BaseApplication as IBaseApplication do
        Debug('done.');
      Result:=True;
    end;
  end;
end;

function GetContentType(Extension : string) : string;
begin
  case Extension of
  '.html','.htm':Result := 'text/html';
  '.txt':Result := 'text/plain';
  '.css':Result := 'text/css';
  '.js':Result := 'application/javascript';
  '.png','.jpg','.jpeg','.tga','.gif','.bmp':Result := 'image/'+copy(Extension,2,length(Extension));
  else Result := 'application/'+copy(Extension,2,length(Extension));
  end;
end;

procedure RegisterHTTPHandler(aHandler: THTTPHandlerProc);
begin
  Setlength(HTTPHandlers,length(HTTPHandlers)+1);
  HTTPHandlers[Length(HTTPHandlers)-1] := aHandler;
end;

{ THTTPSession }

procedure THTTPSession.SetSocket(AValue: TAppNetworkThrd);
begin
  if FSocket=AValue then Exit;
  FSocket:=AValue;
end;

constructor THTTPSession.Create;
begin
  Headers := TStringList.Create;
  InputData := TMemoryStream.Create;
  OutputData := TMemoryStream.Create;
  Close:=False;
end;

destructor THTTPSession.Destroy;
begin
  InputData.Free;
  OutputData.Free;
  Headers.Free;
  inherited Destroy;
end;

procedure THTTPSession.ProcessHTTPRequest;
var
  size: Integer = 0;
  s: String;
  x: Integer;
  aPath: String;
  uriparam, tmp: String;
  i: Integer;
  ModifiedSince : TDateTime;
  sl: TStringList;
  FileLastModified: TDateTime;
const
  HeaderTimeout = 100;
  Timeout = 12000;
label
  retry;
begin
  try
    OutputData.Clear;
    ModifiedSince:=MinDateTime;
    size := -1;
    //read request headers
    if protocol <> '' then
    begin
      if pos('HTTP/', protocol) <> 1 then
        Exit;
      repeat
        s := Socket.sock.RecvString(HeaderTimeout);
        if Socket.sock.lasterror <> 0 then
          Exit;
        if s <> '' then
          Headers.add(s);
        if Pos('CONTENT-LENGTH:', Uppercase(s)) = 1 then
          Size := StrToIntDef(SeparateRight(s, ' '), -1);
        if Pos('IF-MODIFIED-SINCE:', Uppercase(s)) = 1 then
          ModifiedSince := synautil.DecodeRfcDateTime(SeparateRight(s, ' '));
        if Pos('CONNECTION:CLOSE', Uppercase(StringReplace(s,' ','',[rfReplaceAll]))) = 1 then
          Close := True;
      until s = '';
    end;
    Code:=500;
    //recv document...
    if size >= 0 then
    begin
      InputData.SetSize(Size);
      x := Socket.Sock.RecvBufferEx(InputData.Memory, Size, Timeout);
      InputData.SetSize(x);
      if Socket.sock.lasterror <> 0 then
        Exit;
    end;
retry:
    while copy(url,length(url),1)='/' do
      url := copy(url,0,length(url)-1);
    if ((Uppercase(Command)='GET') or (Uppercase(Command)='HEAD') or (Uppercase(Command)='OPTIONS'))  then
      begin
        aPath := ExtractFileDir(ParamStr(0))+DirectorySeparator+'web'+Stringreplace(url,'/',DirectorySeparator,[rfReplaceAll]);
        if pos('?',aPath)>0 then
          aPath := copy(aPath,0,pos('?',aPath)-1);
        if (not (FileExists(aPath) and (not DirectoryExists(aPath))))
        and (not (DirectoryExists(aPath) and (FileExists(aPath+DirectorySeparator+'index.html'))))  then
          begin
            aPath := ExtractFileDir(ParamStr(0))+DirectorySeparator+'web2'+Stringreplace(url,'/',DirectorySeparator,[rfReplaceAll]);
            if pos('?',aPath)>0 then
              aPath := copy(aPath,0,pos('?',aPath)-1);
          end;
        if ((Uppercase(Command)='GET') or (Uppercase(Command)='HEAD')) and (DirectoryExists(aPath)) and (FileExists(aPath+DirectorySeparator+'index.html')) then
          begin
            aPath := (aPath)+DirectorySeparator+'index.html';
            headers.Clear;
            if pos('?',url)>0 then
              begin
                uriparam := copy(url,pos('?',url),length(url));
                url := copy(url,0,pos('?',url)-1);
              end;
            if copy(url,length(url),1)<>'/' then
              url := url+'/';
            //headers.Add('Location: '+url+'index.html');
            Result := TFileStream.Create(aPath,fmOpenRead or fmShareDenyNone);
            sl := TStringList.Create;
            sl.LoadFromStream(result);
            Result.Position:=0;
            tmp := '';
            if (pos('<meta http-equiv="refresh"',lowercase(sl.Text))>0) and BaseApplication.HasOption('replace-refresh') then
              begin
                tmp := copy(sl.Text,pos('<meta http-equiv="refresh"',lowercase(sl.Text))+26,length(sl.Text));
                tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
                tmp := copy(tmp,0,pos('"',tmp)-1);
              end;
            sl.Free;
            if tmp <> '' then
              begin
                url := tmp;
                aPath:='';
                goto retry;
              end
            else
              begin
                OutputData.CopyFrom(Result,0);
                OutputData.Position:=0;
                Result.Free;
                with BaseApplication as IBaseApplication do
                  Info('HTTP: using '+url+'index.html');
                Code := 200;
              end;
          end
        else if FileExists(aPath) and (pos('/.',aPath)=0) then
          begin
            with BaseApplication as IBaseApplication do
              Info('HTTP:'+Command+' '+url+' ('+aPath+')');
            Headers.Clear;
            if Uppercase(Command)='OPTIONS' then
              begin
                headers.Add('Allow: GET,HEAD,OPTIONS');
              end
            else if Uppercase(Command)='GET' then
              begin
                if (FileAge(aPath,FileLastModified)) and
                   (FileLastModified < ModifiedSince)
                then
                  begin
                    Code := 304;//not modified
                  end
                else
                  begin
                    try
                      Result := TFileStream.Create(aPath,fmOpenRead or fmShareDenyNone);
                      OutputData.CopyFrom(Result,0);
                      OutputData.Position:=0;
                      Result.Free;
                      if Code=500 then
                        Code:=200;
                    except
                    end;
                  end;
              end
            else if Uppercase(Command)='HEAD' then
              Code:=200;
            headers.Add('Content-Type:'+GetContentType(ExtractFileExt(aPath)));
            if Code = 200 then
              begin
                if FileAge(aPath,FileLastModified) and (not BaseApplication.HasOption('nocache') and (not BaseApplication.HasOption('debug'))) then
                  begin
                    headers.Add('Last-Modified: '+Rfc822DateTime(FileLastModified));
                    headers.Add('ETag: '+Rfc822DateTime(FileLastModified));
                    headers.Add('Expires: '+Rfc822DateTime(Now()+(Now()-FileLastModified)));
                    headers.Add('Cache-Control: public');
                  end
                else
                  begin
                    headers.Add('Cache-Control: no-cache');
                    headers.Add('Expires: '+Rfc822DateTime(Now()));
                  end;
              end;
          end
        else
//          with BaseApplication as IBaseApplication do
//            Info('HTTP:'+aCmd+' '+uri+' not found')
          ;
      end;
  except
    on e : Exception do
      begin
        Code := 400;
        sl := TStringList.Create;
        sl.Add(e.Message);
        sl.SaveToStream(OutputData);
        sl.Free;
      end;
  end;
end;

initialization
  uAppServer.RegisterCommandHandler(@HandleHTTPCommand);
end.

