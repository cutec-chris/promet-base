unit uspreadsheet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,uBaseDbClasses,db,uBaseDatasetInterfaces,variants;
type
  TSpreedsheetCell = class(TBaseDBDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
  end;

  TSpreedsheet = class(TBaseDbList)
  private
    FCells: TSpreedsheetCell;
    function GetCell(Row, Col : Integer): TSpreedsheetCell;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure Open; override;
    procedure DefineFields(aDataSet : TDataSet);override;
    procedure FillDefaults(aDataSet : TDataSet);override;
    property Cells[Row,Col : Integer] : TSpreedsheetCell read GetCell;
  end;

implementation

procedure TSpreedsheetCell.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'SPREADCELL';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('ROW',ftInteger,0,True);
            Add('COL',ftInteger,0,True);
            Add('CONTENT',ftString,100,True);
          end;
      if Assigned(ManagedIndexdefs) then
        with ManagedIndexDefs do
          begin
            Add('COL','COL',[]);
            Add('COL','COL',[]);
          end;
    end;
end;

function TSpreedsheet.GetCell(Row, Col : Integer): TSpreedsheetCell;
begin
  Result := FCells;
  if not FCells.Locate('ROW;CELL',VarArrayOf([Row,Col]),[]) then
    begin
      FCells.Append;
      FCells.FieldByName('ROW').AsInteger:=Row;
      FCells.FieldByName('COL').AsInteger:=Col;
    end;
end;

constructor TSpreedsheet.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FCells := TSpreedsheetCell.CreateEx(Self,DataModule,Connection,DataSet);
end;

destructor TSpreedsheet.Destroy;
begin
  FCells.Destroy;
  inherited Destroy;
end;

procedure TSpreedsheet.Open;
begin
  inherited Open;
  FCells.Open;
end;

procedure TSpreedsheet.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'SPREADSHEET';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('NAME',ftString,80,True);
            Add('PAGE',ftString,80,True);
          end;
      if Assigned(ManagedIndexdefs) then
        with ManagedIndexDefs do
          begin
          end;
    end;
end;

procedure TSpreedsheet.FillDefaults(aDataSet: TDataSet);
begin
  inherited FillDefaults(aDataSet);
end;

end.

