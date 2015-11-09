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
    dsBoilerplate: TDataSource;
    eFilter: TEdit;
    gList: TDBGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    FBoilerpl : TBoilerplate;
  public
    { public declarations }
    function Execute : Boolean;
    property DataSet : TBoilerplate read FBoilerpl;
  end;

var
  fBoilerplate: TfBoilerplate;

implementation

{$R *.lfm}

procedure TfBoilerplate.FormCreate(Sender: TObject);
begin
  FBoilerpl := TBoilerplate.Create(nil);
  dsBoilerplate.DataSet:=FBoilerpl.DataSet;
end;

procedure TfBoilerplate.FormDestroy(Sender: TObject);
begin
  FBoilerpl.Free;
end;

function TfBoilerplate.Execute: Boolean;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfBoilerplate,FBoilerplate);
      Self := FBoilerplate;
    end;
  FBoilerpl.Open;
  Result := ShowModal=mrOK;
end;

end.

