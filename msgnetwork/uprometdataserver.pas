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
  fpsqlscanner, fpsqltree,httpsend,OpenSSL, jsonparser, db, uappserverhttp;

procedure DataSetToJSON(ADataSet: TDataSet; AJSON: TJSONArray; const ADateAsString: Boolean; Fields: TSQLElementList = nil);
procedure ObjectToJSON(AObject : TBaseDBDataSet; AJSON: TJSONObject;const ADateAsString: Boolean);

implementation

uses usync,uBaseDatasetInterfaces;

function HandleDataRequest(Method, URL: string;Headers : TStringList;Input,Output : TStream): Integer;
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
end;

procedure DataSetToJSON(ADataSet: TDataSet; AJSON: TJSONArray;
  const ADateAsString: Boolean; Fields: TSQLElementList);
var
  VJSON: TJSONObject;
begin
  ADataSet.First;
  while not ADataSet.EOF do
  begin
    VJSON := TJSONObject.Create;
    FieldsToJSON(ADataSet.Fields, VJSON, ADateAsString, Fields);
    AJSON.Add(VJSON);
    ADataSet.Next;
  end;
end;

procedure ObjectToJSON(AObject: TBaseDBDataSet; AJSON: TJSONObject;
  const ADateAsString: Boolean);
var
  aArray: TJSONArray;
  aNewObj: TJSONObject;
  i: Integer;
begin
  aArray := TJSONArray.Create;
  DataSetToJSON(AObject.DataSet,aArray,ADateAsString);
  AJSON.Add('Fields',aArray);
  with AObject as IBaseSubDataSets do
    for i := 0 to GetCount-1 do
      begin
        aNewObj := TJSONObject.Create;
        ObjectToJSON(TBaseDBDataSet(SubDataSet[i]),aNewObj,ADateAsString);
        AJSON.Add(TBaseDBDataSet(SubDataSet[i]).Caption,aNewObj);
      end;
end;

initialization
  uappserverhttp.RegisterHTTPHandler(@HandleDataRequest);
end.

