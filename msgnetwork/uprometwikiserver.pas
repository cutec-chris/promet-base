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
  Utils,IniFiles;

implementation

uses uData;

type

  { TWikiSession }

  TWikiSession = class
  private
    FSocket: TAppNetworkThrd;
    procedure CreateWikiList;
    procedure DestroyWikiList;
    procedure SetSocket(AValue: TAppNetworkThrd);
  public
    Url : string;
    Code : Integer;
    NewHeaders : string;
    Result : TStream;
    WikiList: TWikiList;
    Config : TIniFile;
    Document : TDocument;
    Template : TStringList;
    property Socket : TAppNetworkThrd read FSocket write SetSocket;

    constructor Create;
    destructor Destroy; override;
    procedure ProcessWikiRequest;
    procedure OpenConfig;
  end;

function HandleWikiRequest(Sender : TAppNetworkThrd;Method, URL: string;Headers : TStringList;Input,Output : TMemoryStream): Integer;
var
  lOut, aParameters: TStringList;
  i: Integer;
  aSock: TWikiSession = nil;
begin
  {
  http://www.odata.org/
  /wiki/folder1/page2
  }
  for i := 0 to Sender.Objects.Count-1 do
    if TObject(Sender.Objects[i]) is TWikiSession then
      aSock := TWikiSession(Sender.Objects[i]);
  if not Assigned(aSock) then
    begin
      aParameters := TStringList.Create;
      aParameters.NameValueSeparator:=':';
      aParameters.CaseSensitive:=False;
      aSock := TWikiSession.Create;
      aSock.Socket := Sender;
      Sender.Objects.Add(aSock);
    end;
  Result := 500;
  if copy(lowercase(url),0,6)='/wiki/' then
    begin
      aSock.Url := copy(url,7,length(url));
      Sender.Synchronize(Sender,@aSock.ProcessWikiRequest);
      Result := aSock.Code;
      if Assigned(aSock.Result) then
        Output.CopyFrom(aSock.Result,0);
      Headers.Clear;
      Headers.Add('Content-Type: '+ 'text/html');
      if aSock.NewHeaders<>'' then
        Headers.Add(aSock.NewHeaders);
    end;
end;

{ TWikiSession }

procedure TWikiSession.CreateWikiList;
begin
  if Assigned(uData.Data) then
    begin
      WikiList := TWikiList.Create(nil);
      Document := TDocument.Create(nil);
    end;
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
  FSocket.Synchronize(FSocket,@CreateWikiList);
end;

constructor TWikiSession.Create;
begin
  Code := 500;
  Template := TStringList.Create;
end;

destructor TWikiSession.Destroy;
begin
  FSocket.Synchronize(FSocket,@DestroyWikiList);
  if Assigned(Config) then Config.Free;
  Template.Free;
  inherited Destroy;
end;

procedure TWikiSession.ProcessWikiRequest;
var
  Path, tmp: String;
  sl: TStringList;
  aNumber: integer;
  ms: TMemoryStream;
begin
  Code:=404;
  OpenConfig;
  NewHeaders:='';
  if (Url = '')
  or (Url = '/') then
    begin
      Code := 301;
      NewHeaders:='Location: '+Config.ReadString('wiki','index','index');
      exit;
    end;
  if WikiList.FindWikiPage(Url) then
    begin
      sl := TStringList.Create;
      if pos('%CONTENT%',Template.Text)>0 then
        begin
          sl.Text := StringReplace(Template.Text,'%CONTENT%',WikiList.PageAsHtml(True),[]);
          sl.Text := StringReplace(sl.Text,'%TIME%',DateTimeToStr(Now()),[]);
        end
      else
        sl.Text := WikiList.PageAsHtml;
      result := TMemoryStream.Create;
      sl.SaveToStream(result);
      sl.Free;
      Code := 200;
    end
  else
    begin
      Path := Url;
      if copy(uppercase(Path),0,5)='ICON(' then
        begin
          if TryStrToInt(copy(Path,6,length(Path)-6),aNumber) then
            begin
              ms := TMemoryStream.Create;
              ms.Position:=0;
              Result := ms;
            end;
        end
      else if copy(uppercase(Path),0,12)='HISTORYICON(' then
        begin
          tmp := copy(Path,13,length(Path)-13);
          if TryStrToInt(tmp,aNumber) then
            begin
              ms := TMemoryStream.Create;
              ms.Position:=0;
              Result := ms;
            end;
        end
      else if Assigned(Document) then
        begin
          Document.Filter(Data.QuoteField('TYPE')+'=''W'' and '+Data.QuoteField('NAME')+'='+Data.QuoteValue(copy(ExtractFileName(Path),0,rpos('.',ExtractFileName(Path))-1)),1);
          if Document.DataSet.RecordCount > 0 then
            begin
              ms := TMemoryStream.Create;
              Document.CheckoutToStream(ms);
              ms.Position:=0;
              Result := ms;
              Code := 200;
            end;
        end;
    end;
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

