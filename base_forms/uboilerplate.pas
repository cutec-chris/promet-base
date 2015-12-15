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
    DBNavigator1: TDBNavigator;
    dsBoilerplate: TDataSource;
    eFilter: TEdit;
    gList: TDBGrid;
    Panel1: TPanel;
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
  FEditor := TfWikiEditor.Create(Self);
  ipHTML.SetHtml(FEditor.GetHTML(strWikiLoadingPage));
  FEditor.Parent:=pMiddle;
  FEditor.Align:=alClient;
  FEditor.BorderStyle:=bsNone;
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

