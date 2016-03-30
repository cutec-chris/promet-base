unit uprometdataserver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,fpjson, uBaseDBInterface,uBaseDbClasses,fpsqlparser,
  fpsqlscanner, fpsqltree,httpsend,OpenSSL, jsonparser, db;

function HandleHTTPRequest(Method,URL : string) : Integer;

procedure DataSetToJSON(ADataSet: TDataSet; AJSON: TJSONArray; const ADateAsString: Boolean; Fields: TSQLElementList = nil);
procedure ObjectToJSON(AObject : TBaseDBDataSet; AJSON: TJSONObject;const ADateAsString: Boolean);

implementation

uses usync,uBaseDatasetInterfaces;

function HandleHTTPRequest(Method, URL: string): Integer;
begin
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

end.

