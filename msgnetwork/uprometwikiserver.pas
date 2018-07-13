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
Created 01.06.2016
*******************************************************************************}
unit uprometwikiserver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uappserverhttp, uWiki, syncobjs,uAppServer,uDocuments,
  Utils, IniFiles,wikitohtml,synautil,blcksock,
  uBaseDBInterface;

implementation

uses uData;

type

  { TWikiSession }

  TWikiSession = class
  private
    FData: Pointer;
    FSocket: TAppNetworkThrd;
    procedure CreateWikiList;
    procedure DestroyWikiList;
    procedure SetSocket(AValue: TAppNetworkThrd);
  public
    Url : string;
    Code : Integer;
    NewHeaders : TStringList;
    Result : TStream;
    WikiList: TWikiList;
    Config : TIniFile;
    Document : TDocument;
    Template : TStringList;
    property Socket : TAppNetworkThrd read FSocket write SetSocket;
    property Data : Pointer read FData write FData;

    constructor Create;
    destructor Destroy; override;
    procedure ProcessWikiRequest;
    procedure CreateDataModule;
    procedure OpenConfig;
  end;

function HandleWikiRequest(Sender : TAppNetworkThrd;Method, URL: string;SID : string;Parameters,Headers : TStringList;Input,Output : TMemoryStream;ResultStatusText : string): Integer;
var
  lOut: TStringList;
  aSock: TWikiSession = nil;
  sl: TStringList;
  aParams: String;
  Path: String;
  ms: TMemoryStream;
  aNumber: integer;
  tmp: String;

  procedure FindSocket;
  var
    i: Integer;
  begin
    for i := 0 to Sender.Objects.Count-1 do
      if TObject(Sender.Objects[i]) is TWikiSession then
        aSock := TWikiSession(Sender.Objects[i]);
    if not Assigned(aSock) then
      begin
        aSock := TWikiSession.Create;
        aSock.Socket := Sender;
        Sender.Objects.Add(aSock);
      end;
  end;
begin
  Result := 404;
  ResultStatusText := '';
  {
  http://www.odata.org/
  /wiki/folder1/page2
  }
  if (pos('icon(',lowercase(url))>0) or (pos('historyicon(',lowercase(url))>0) then
    begin
      FindSocket;
      Path := Url;
      Path := copy(Path,rpos('/',Path)+1,length(Path));
      if copy(uppercase(Path),0,5)='ICON(' then
        begin
          ms := TMemoryStream.Create;
          if TryStrToInt(copy(Path,6,length(Path)-6),aNumber) and FileExists(AppendPathDelim('icons')+IntToStr(aNumber)+'.png') then
            begin
              ms.LoadFromFile(AppendPathDelim('icons')+IntToStr(aNumber)+'.png');
              ms.Position:=0;
              aSock.Result := ms;
            end;
          aSock.NewHeaders.Add('Content-Type: image/png');
          aSock.NewHeaders.Add('Last-Modified: '+Rfc822DateTime(100));
          aSock.NewHeaders.Add('ETag: '+Rfc822DateTime(100));
          aSock.Code := 200;
        end
      else if (copy(uppercase(Path),0,12)='HISTORYICON(') then
        begin
          ms := TMemoryStream.Create;
          tmp := copy(Path,13,length(Path)-13);
          if TryStrToInt(tmp,aNumber) and FileExists(AppendPathDelim('histicons')+IntToStr(aNumber)+'.png') then
            begin
              ms.LoadFromFile(AppendPathDelim('icons')+IntToStr(aNumber)+'.png');
              ms.Position:=0;
              aSock.Result := ms;
            end;
          aSock.NewHeaders.Add('Content-Type: image/png');
          aSock.NewHeaders.Add('Last-Modified: '+Rfc822DateTime(100));
          aSock.NewHeaders.Add('ETag: '+Rfc822DateTime(100));
          aSock.Code := 200;
        end
    end
  else if (copy(lowercase(url),0,6)='/wiki/') then
    begin
      try
        FindSocket;
        if pos('?',url)>0 then
          begin
            aParams := copy(url,pos('?',url)+1,length(url));
            url := copy(url,0,pos('?',url)-1);
          end;
        aSock.Url := copy(url,7,length(url));
        Sender.InternalSynchronize(Sender,@aSock.ProcessWikiRequest);
        Result := aSock.Code;
        if Assigned(aSock.Result) then
          Output.CopyFrom(aSock.Result,0);
        aSock.Result.Free;
        aSock.Result := nil;
        Headers.Clear;
        if aSock.NewHeaders.Count>0 then
          Headers.AddStrings(aSock.NewHeaders);
      except
        on e : Exception do
          begin
            Result := 500;
            sl := TStringList.Create;
            sl.Add(e.Message);
            sl.SaveToStream(Output);
            sl.Free;
          end;
      end;
    end;
end;

procedure TWikiSession.CreateWikiList;
begin
  if not Assigned(Data) then
    CreateDataModule;
  WikiList := TWikiList.CreateEx(nil,TBaseDBModule(Data));
  Document := TDocument.CreateEx(nil,TBaseDBModule(Data));
end;

procedure TWikiSession.DestroyWikiList;
begin
  if Assigned(WikiList) then
    begin
      Document.Free;
      WikiList.Free;
    end;
end;

procedure TWikiSession.SetSocket(AValue: TAppNetworkThrd);
begin
  if FSocket=AValue then Exit;
  FSocket:=AValue;
  FSocket.InternalSynchronize(FSocket,@CreateWikiList);
end;

constructor TWikiSession.Create;
begin
  Code := 500;
  Data := nil;
  Template := TStringList.Create;
  NewHeaders:=TStringList.Create;
end;

destructor TWikiSession.Destroy;
begin
  FSocket.InternalSynchronize(FSocket,@DestroyWikiList);
  if Assigned(Config) then Config.Free;
  Template.Free;
  NewHeaders.Free;
  inherited Destroy;
end;

procedure TWikiSession.ProcessWikiRequest;
var
  Path, tmp: String;
  sl: TStringList;
  aNumber: integer;
  ms: TMemoryStream;
begin
  try
    Code:=404;
    OpenConfig;
    NewHeaders.Clear;
    if ((Url = '')
    or (Url = '/'))
    and Assigned(Config)
    then
      begin
        Code := 301;
        NewHeaders.Add('Location: '+Config.ReadString('wiki','index','index'));
        exit;
      end;
    if WikiList.FindWikiPage(Url) then
      begin
        sl := TStringList.Create;
        if pos('%CONTENT%',Template.Text)>0 then
          begin
            sl.Text := StringReplace(Template.Text,'%CONTENT%',WikiList.PageAsHtml(True),[]);
            sl.Text := StringReplace(sl.Text,'%TIME%',DateTimeToStr(Now()),[]);

            sl.Text := StringReplace(sl.Text,'%TITLE%',WikiList.FieldByName('CAPTION').AsString,[]);
            try
              tmp := WikiList.GenerateKeywords;
              sl.Text := StringReplace(sl.Text,'%KEYWORDS%',tmp,[]);
            except
            end;
            tmp := WikiList.GenerateDescription;
            sl.Text := StringReplace(sl.Text,'%DESCRIPTION%',tmp,[]);
          end
        else
          sl.Text := WikiList.PageAsHtml;
        result := TMemoryStream.Create;
        sl.SaveToStream(result);
        sl.Free;
        NewHeaders.Add('Access-Control-Allow-Headers: Authorization,X-Requested-With');
        NewHeaders.Add('Access-Control-Allow-Methods: GET, OPTIONS');
        NewHeaders.Add('Access-Control-Allow-Origin: *');
        NewHeaders.Add('Content-Type: text/html');
        NewHeaders.Add('Last-Modified: '+Rfc822DateTime(WikiList.TimeStamp.AsDateTime));
        NewHeaders.Add('ETag: '+Rfc822DateTime(WikiList.TimeStamp.AsDateTime));
        Code := 200;
      end
    else
      begin
        Path := Url;
        Path := copy(Path,rpos('/',Path)+1,length(Path));
        if Assigned(Document) then
          begin
            Document.Filter(TBaseDBModule(Data).QuoteField('TYPE')+'=''W'' and '+TBaseDBModule(Data).QuoteField('NAME')+'='+TBaseDBModule(Data).QuoteValue(copy(ExtractFileName(Path),0,rpos('.',ExtractFileName(Path))-1)),1);
            if Document.DataSet.RecordCount > 0 then
              begin
                ms := TMemoryStream.Create;
                Document.CheckoutToStream(ms);
                ms.Position:=0;
                Result := ms;
                NewHeaders.Add('Content-Type: '+GetContentType('.'+Document.FieldByName('EXTENSION').AsString));
                NewHeaders.Add('Last-Modified: '+Rfc822DateTime(Document.TimeStamp.AsDateTime));
                NewHeaders.Add('ETag: '+Rfc822DateTime(Document.TimeStamp.AsDateTime));
                if Document.TimeStamp.AsDateTime = Document.FieldByName('DATE').AsDateTime then
                  NewHeaders.Add('Cache-Control: max-age=31536000');
                Code := 200;
              end;
          end;
      end;
  except
    on e : Exception do
      begin
        Code := 400;
        sl := TStringList.Create;
        sl.Add(e.Message);
        ms := TMemoryStream.Create;
        sl.SaveToStream(ms);
        ms.Position:=0;
        Result := ms;
        sl.Free;
      end;
  end;
end;

procedure TWikiSession.CreateDataModule;
var
  aType: TBaseDBModuleClass;
begin
  EnterCriticalsection(GlobalLock);
  if Assigned(Data) then exit;
  {
  aType := TBaseDBModuleClass(uData.Data.ClassType);
  Data := aType.Create(aSocket);
  with TBaseDBModule(aSocket.Data) do
    begin
      SetProperties(uData.Data.Properties);
    end;
  }
  Data := uData.Data;
  //TODO:select rigth User
  {
  if not TBaseDBModule(Data).Users.Locate('SQL_ID',aSocket.User,[]) then
    begin
      TBaseDBModule(Data).Users.Filter('',0);
      if not TBaseDBModule(aSocket.Data).Users.Locate('SQL_ID',aSocket.User,[]) then exit;
    end;
  TBaseDBModule(Data).RefreshUsersFilter;
  }
  TBaseDBModule(Data).RegisterLinkHandlers;
  LeaveCriticalsection(GlobalLock);
end;

procedure TWikiSession.OpenConfig;
var
  aPath: String;
begin
  if not Assigned(Config) then
    begin
      aPath := ExtractFileDir(ParamStr(0))+DirectorySeparator+'web'+DirectorySeparator;
      if FileExists(aPath+'wiki-config.ini') then
        begin
          Config := TIniFile.Create(aPath+'wiki-config.ini');
          if FileExists(aPath+'wiki-template.html') then
            Template.LoadFromFile(aPath+'wiki-template.html');
        end;
      if not Assigned(Config) then
        begin
          aPath := ExtractFileDir(ParamStr(0))+DirectorySeparator+'web2'+DirectorySeparator;
          if FileExists(aPath+'wiki-config.ini') then
            begin
              Config := TIniFile.Create(aPath+'wiki-config.ini');
              if FileExists(aPath+'wiki-template.html') then
                Template.LoadFromFile(aPath+'wiki-template.html');
            end;
        end;
    end;
end;

initialization
  uappserverhttp.RegisterHTTPHandler(@HandleWikiRequest);
finalization
end.

