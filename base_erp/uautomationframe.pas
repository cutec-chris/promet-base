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
Created 01.06.2006
*******************************************************************************}
unit uautomationframe;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, DbCtrls,
  DBGrids, Menus, db, uPrometFramesInplace,uBaseDbClasses,uScriptEditor;

type

  { TfAutomationframe }

  TfAutomationframe = class(TPrometInplaceFrame)
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    pmImage: TPopupMenu;
    Position: TDatasource;
    procedure FEditorOpenUnit(aUnitName: string; X, Y: Integer);
  private
    FDataSet: TBaseDBDataset;
    FEditor: TfScriptEditor;
    procedure SetDataSet(AValue: TBaseDBDataset);
    { private declarations }
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetRights(Editable : Boolean);override;
    property DataSet : TBaseDBDataset read FDataSet write SetDataSet;
  end; 

resourcestring
  strAutomation        = 'Automation';
implementation
{$R *.lfm}
uses uPositionFrame,uBaseERPDBClasses;

procedure TfAutomationframe.FEditorOpenUnit(aUnitName: string; X, Y: Integer);
begin

end;

procedure TfAutomationframe.SetDataSet(AValue: TBaseDBDataset);
begin
  Position.DataSet:=nil;
  if FDataSet=AValue then Exit;
  FDataSet:=AValue;
  if FDataSet is TBaseDBPosition then
    Position.DataSet := TBaseDBPosition(FDataSet).DataSet;
end;

procedure TfAutomationframe.SetRights(Editable: Boolean);
begin
  Enabled := Editable;
end;

constructor TfAutomationframe.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEditor:=TfScriptEditor.Create(Self);
  FEditor.BorderStyle:=bsNone;
  FEditor.Parent:=Self;
  FEditor.Align:=alClient;
  FEditor.acSave.Visible:=False;
  FEditor.OnOpenUnit:=@FEditorOpenUnit;
  FEditor.Visible:=True;
end;

destructor TfAutomationframe.Destroy;
begin
  FEditor.Free;
  inherited Destroy;
end;

end.

