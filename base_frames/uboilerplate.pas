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
Created 01.11.2015
*******************************************************************************}
unit uBoilerplate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  DBGrids, ButtonPanel, DbCtrls, ExtCtrls, uBaseDbClasses,uWikiEditor;

type
  TfBoilerplate = class(TForm)
    ButtonPanel1: TButtonPanel;
    DBMemo1: TDBMemo;
    DBNavigator1: TDBNavigator;
    dsBoilerplate: TDataSource;
    eFilter: TEdit;
    gList: TDBGrid;
    pMiddle: TPanel;
    procedure DataSetDataSetAfterScroll(DataSet: TDataSet);
    procedure eFilterChange(Sender: TObject);
    procedure FEditorChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    FBoilerpl : TBoilerplate;
    FEditor: TfWikiEditor;
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
  FEditor := TfWikiEditor.Create(Self);
  FEditor.Parent:=pMiddle;
  FEditor.Align:=alClient;
  FEditor.BorderStyle:=bsNone;
  FEditor.Show;
end;

procedure TfBoilerplate.eFilterChange(Sender: TObject);
begin
  DataSet.Filter(Data.ProcessTerm('UPPER('+Data.QuoteField('NAME')+')=UPPER('+Data.QuoteValue('*'+eFilter.Text+'*'))+')');
end;

procedure TfBoilerplate.FEditorChange(Sender: TObject);
begin
  if FEditor.Id = dataSet.Id.AsVariant then
    begin
      DataSet.Edit;
      DataSet.FieldByName('TEXT').AsString:=FEditor.Text;
    end;
end;

procedure TfBoilerplate.DataSetDataSetAfterScroll(DataSet: TDataSet);
begin
  FEditor.Open(DataSet.FieldByName('TEXT').AsString,FBoilerpl.Id.AsVariant,'B',DataSet.FieldByName('NAME').AsString);
  FEditor.OnChange:=@FEditorChange;
end;

procedure TfBoilerplate.FormDestroy(Sender: TObject);
begin
  FEditor.Free;
  FBoilerpl.Free;
end;

function TfBoilerplate.Execute(aText: string): Boolean;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfBoilerplate,FBoilerplate);
      Self := FBoilerplate;
      FBoilerpl := TBoilerplate.Create(nil);
    end;
  dsBoilerplate.DataSet:=FBoilerpl.DataSet;
  eFilter.Text:=trim(aText);
  if trim(eFilter.Text)='' then
    DataSet.Filter('')
  else
    DataSet.Filter(Data.ProcessTerm('UPPER('+Data.QuoteField('NAME')+')=UPPER('+Data.QuoteValue('*'+eFilter.Text+'*'))+')');
  DataSet.DataSet.AfterScroll:=@DataSetDataSetAfterScroll;
  FBoilerpl.Open;
  Result := ShowModal=mrOK;
  if result and DataSet.CanEdit then
    dataSet.Post;
end;

end.

