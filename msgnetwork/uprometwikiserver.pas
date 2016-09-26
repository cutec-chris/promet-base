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
  Utils;

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
    Result : TStream;
    WikiList: TWikiList;
    Document : TDocument;
    property Socket : TAppNetworkThrd read FSocket write SetSocket;

    constructor Create;
    destructor Destroy; override;
    procedure ProcessWikiRequest;
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
    end;
end;

{ TWikiSession }

procedure TWikiSession.CreateWikiList;
begin
  WikiList := TWikiList.Create(nil);
  Document := TDocument.Create(nil);
end;

procedure TWikiSession.DestroyWikiList;
begin
  Document.Free;
  WikiList.Free;
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
end;

destructor TWikiSession.Destroy;
begin
  FSocket.Synchronize(FSocket,@DestroyWikiList);
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
  if WikiList.FindWikiPage(Url) then
    begin
      sl := TStringList.Create;
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
      else
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

initialization
  uappserverhttp.RegisterHTTPHandler(@HandleWikiRequest);
finalization
end.

