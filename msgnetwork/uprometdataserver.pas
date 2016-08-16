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
Created 01.04.2016
*******************************************************************************}
unit uprometdataserver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,fpjson, uBaseDBInterface,uBaseDbClasses,fpsqlparser,
  fpsqlscanner, fpsqltree,httpsend,OpenSSL, jsonparser, db, uappserverhttp,
  uAppServer;

implementation

uses usync,uBaseDatasetInterfaces,uData;

function HandleDataRequest(Sender : TAppNetworkThrd;Method, URL: string;Headers : TStringList;Input,Output : TMemoryStream): Integer;
var
  aClassType: TBaseDBDatasetClass;
  aClass: TBaseDBDataset;
  aList: String;
  aFilter: String;
  FSQLStream: TStringStream;
  QueryFields: TStringList;
  FSQLScanner: TSQLScanner;
  FSQLParser: TSQLParser;
  aStmt: TSQLElement;
  Json: TJSONArray = nil;
  aSeq: String;
  slOutput: TStringList;
begin
  {
  /api/pds/objects/contacts
  /api/pds/objects/contacts('1091')
  /api/pds/objects/contacts('1091')?setstatus=I
  /api/pds/objects/contacts('1091')/address
  http://www.odata.org/
  /wiki/folder1/page2
  }
  Result := 500;
  if (copy(lowercase(url),0,15)='/api/pds/lists/') and (UpperCase(Method)='GET') then
    begin
      url := copy(url,16,length(url));
      aList := copy(url,0,pos('/',url)-1);
      QueryFields := TStringList.Create;
      slOutput := TStringList.Create;
      aFilter := QueryFields.Values['filter'];//TODO:Param filter
      if aList <> '' then
        begin
          if aFilter<>'' then
            aList := aList+' where '+aFilter;
          FSQLStream := TStringStream.Create('select * from '+aList);
        end
      else
        FSQLStream := TStringStream.Create(QueryFields.Values['ql']);
      FSQLScanner := TSQLScanner.Create(FSQLStream);
      FSQLParser := TSQLParser.Create(FSQLScanner);
      try
        aStmt := FSQLParser.Parse;
        if TSQLSelectStatement(aStmt).Tables.Count = 1 then
          aList := TSQLSimpleTableReference(TSQLSelectStatement(aStmt).Tables[0]).ObjectName.Name;
        if Data.DataSetFromName(aList,aClassType) then
          begin
            aClass := aClassType.Create(nil);
            Json := TJSONArray.Create;
            Result:=200;
            Headers.Clear;
            Headers.Add('ContentType: text/javascript;charset=utf-8');
            Headers.Add('Access-Control-Allow-Origin: *');
            aClass.Free;
          end;
      except
        on e : ESQLParser do
          begin
            if e.Col > 0 then
              begin
                Result := 401;
                slOutput.Text:='['+IntToStr(e.Line)+':'+IntToStr(e.Col)+'] '+e.Message+','+FSQLParser.CurSource;
                slOutput.SaveToStream(Output);
              end;
          end;
      end;
      aSeq := QueryFields.Values['sequence'];
      if aSeq='' then aSeq := '0';
      if Assigned(Json) then
        begin
          slOutput.Text:=Json.AsJSON;
          slOutput.SaveToStream(Output);
          Json.Free;
        end;
      slOutput.Free;
      QueryFields.Free;
    end
  else if copy(lowercase(url),0,17)='/api/pds/objects/' then
    begin
      url := copy(url,18,length(url));
    end
  ;
end;

initialization
  uappserverhttp.RegisterHTTPHandler(@HandleDataRequest);
end.

