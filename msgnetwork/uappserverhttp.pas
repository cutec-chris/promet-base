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
  Classes, SysUtils, synautil, uAppServer, blcksock;

type
  THTTPHandlerProc = function(Sender : TAppNetworkThrd;Method,URL : string;Headers : TStringList;Input,Output : TMemoryStream) : Integer;

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

function HandleHTTPCommand(Sender : TAppNetworkThrd;FCommand : string) : string;
var
  aCmd: String;
  i: Integer;
  aSock: THTTPSession = nil;
  uri: String;
  n: Integer;
  aReqTime: TDateTime;
begin
  Result := '';
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
      aSock.Command := aCmd;
      aSock.ProcessHTTPRequest;
      if (aSock.Code<>200) and (aSock.Code<>301) then
        begin
          aReqTime := Now();
          for i := 0 to Length(HTTPHandlers)-1 do
            begin
              aSock.Code := HTTPHandlers[i](Sender,aCmd, aSock.Url, aSock.Headers, aSock.InputData, aSock.OutputData);
              if aSock.Code<>500 then
                begin
                  writeln(aCmd+' '+aSock.Url+'=>'+IntToStr(aSock.Code)+' in '+IntToStr(round((Now()-aReqTime)*MSecsPerDay))+' ms');
                  break;
                end;
            end;
        end;
      TAppNetworkThrd(Sender).sock.SendString('HTTP/1.1 ' + IntTostr(aSock.Code) + CRLF);
      if aSock.protocol <> '' then
      begin
        aSock.headers.Add('Date: ' + Rfc822DateTime(now));
        aSock.headers.Add('Server: Avamm Internal Network');
        aSock.headers.Add('Content-length: ' + IntTostr(aSock.OutputData.Size));
        for n := 0 to aSock.headers.count - 1 do
          if aSock.headers[n]<>'' then
            TAppNetworkThrd(Sender).sock.sendstring(aSock.headers[n] + CRLF);
        TAppNetworkThrd(Sender).sock.sendstring(CRLF);
      end;
      if TAppNetworkThrd(Sender).sock.lasterror <> 0 then
        Exit;
      TAppNetworkThrd(Sender).Sock.SendBuffer(aSock.OutputData.Memory, aSock.OutputData.Size);
      Result:=' ';
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
  size: Integer;
  s: String;
  x: Integer;
  aPath: String;
  uriparam: String;
  i: Integer;
const
  Timeout = 12000;
begin
  OutputData.Clear;
  size := -1;
  //read request headers
  if protocol <> '' then
  begin
    if pos('HTTP/', protocol) <> 1 then
      Exit;
    repeat
      s := Socket.sock.RecvString(Timeout);
      if Socket.sock.lasterror <> 0 then
        Exit;
      if s <> '' then
        Headers.add(s);
      if Pos('CONTENT-LENGTH:', Uppercase(s)) = 1 then
        Size := StrToIntDef(SeparateRight(s, ' '), -1);
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
  if ((Uppercase(Command)='GET') or (Uppercase(Command)='HEAD') or (Uppercase(Command)='OPTIONS'))  then
    begin
      aPath := ExtractFileDir(ParamStr(0))+DirectorySeparator+'web'+Stringreplace(url,'/',DirectorySeparator,[rfReplaceAll]);
      if pos('?',aPath)>0 then
        aPath := copy(aPath,0,pos('?',aPath)-1);
      if (not FileExists(aPath))
      and (not (DirectoryExists(aPath) and (FileExists(aPath+'index.html'))))  then
        begin
          aPath := ExtractFileDir(ParamStr(0))+DirectorySeparator+'web2'+Stringreplace(url,'/',DirectorySeparator,[rfReplaceAll]);
          if pos('?',aPath)>0 then
            aPath := copy(aPath,0,pos('?',aPath)-1);
        end;
      if ((Uppercase(Command)='GET') or (Uppercase(Command)='HEAD')) and ((not FileExists(aPath)) or DirectoryExists(aPath)) and (FileExists(aPath+'index.html')) then
        begin
          //aPath := aPath+'index.html';
          Code := 301;
          headers.Clear;
          if pos('?',url)>0 then
            begin
              uriparam := copy(url,pos('?',url),length(url));
              url := copy(url,0,pos('?',url)-1);
            end;
          if copy(url,length(url),1)<>'/' then
            url := url+'/';
          headers.Add('Location: '+url+'index.html');
          writeln('HTTP: redirecting to '+url+'index.html');
        end
      else if FileExists(aPath) and (copy(ExtractFileName(aPath),0,1)<>'.') then
        begin
          writeln('HTTP:'+Command+' '+url+' ('+aPath+')');
          Headers.Clear;
          if Uppercase(Command)='OPTIONS' then
            begin
              headers.Add('Allow: GET,HEAD,OPTIONS');
            end
          else if Uppercase(Command)='GET' then
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
            end
          else if Uppercase(Command)='HEAD' then
            Code:=200;
          headers.Add('Content-Type:'+GetContentType(ExtractFileExt(aPath)));
          headers.Add('Last-Modified: '+Rfc822DateTime(FileDateToDateTime(FileAge(aPath))));
          headers.Add('ETag: '+Rfc822DateTime(FileDateToDateTime(FileAge(aPath))));
        end
      //else writeln('HTTP:'+aCmd+' '+uri+' not found')
        ;
      headers.Add('Connection: keep-alive');
    end;
end;

initialization
  uAppServer.RegisterCommandHandler(@HandleHTTPCommand);
end.

