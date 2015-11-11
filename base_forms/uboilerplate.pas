unit uBoilerplate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  DBGrids, ButtonPanel, DbCtrls, uBaseDbClasses;

type
  TfBoilerplate = class(TForm)
    ButtonPanel1: TButtonPanel;
    DBMemo1: TDBMemo;
    DBNavigator1: TDBNavigator;
    dsBoilerplate: TDataSource;
    eFilter: TEdit;
    gList: TDBGrid;
    procedure eFilterChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    FBoilerpl : TBoilerplate;
  public
    { public declarations }
    function Execute(aText : string = '') : Boolean;
    property DataSet : TBoilerplate read FBoilerpl;
  end;

var
  fBoilerplate: TfBoilerplate;

implementation
{$R *.lfm}
uses uData;
procedure TfBoilerplate.FormCreate(Sender: TObject);
begin
  FBoilerpl := TBoilerplate.Create(nil);
  dsBoilerplate.DataSet:=FBoilerpl.DataSet;
end;

procedure TfBoilerplate.eFilterChange(Sender: TObject);
begin
  DataSet.Filter(Data.ProcessTerm('UPPER('+Data.QuoteField('NAME')+')=UPPER('+Data.QuoteValue('*'+eFilter.Text+'*'))+')');
end;

procedure TfBoilerplate.FormDestroy(Sender: TObject);
begin
  FBoilerpl.Free;
end;

function TfBoilerplate.Execute(aText: string): Boolean;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfBoilerplate,FBoilerplate);
      Self := FBoilerplate;
    end;
  eFilter.Text:=trim(aText);
  if trim(eFilter.Text)='' then
    DataSet.Filter('')
  else
    DataSet.Filter(Data.ProcessTerm('UPPER('+Data.QuoteField('NAME')+')=UPPER('+Data.QuoteValue('*'+eFilter.Text+'*'))+')');
  FBoilerpl.Open;
  Result := ShowModal=mrOK;
end;

end.

