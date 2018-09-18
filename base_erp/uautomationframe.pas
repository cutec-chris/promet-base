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
  DBGrids, Menus, Buttons, ComCtrls, ActnList, uExtControls, db,
  uPrometFramesInplace, uBaseDbClasses, uScriptEditor;

type

  { TfAutomationframe }

  TfAutomationframe = class(TPrometInplaceFrame)
    acEditScript: TAction;
    acEditPrepareScript: TAction;
    ActionList1: TActionList;
    Button1: TButton;
    Button2: TButton;
    eScript: TDBEdit;
    eScript1: TDBEdit;
    eScript2: TDBEdit;
    eScript5: TDBEdit;
    eVersion: TDBEdit;
    GroupBox1: TPanel;
    GroupBox2: TPanel;
    Label1: TLabel;
    Label10: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    Position: TDatasource;
    pToolbar: TPanel;
    SpeedButton3: TBitBtn;
    procedure acEditPrepareScriptExecute(Sender: TObject);
    procedure acEditScriptExecute(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    FDataSet: TBaseDBDataset;
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
uses uPositionFrame,uBaseERPDBClasses,uMasterdata,uprometpascalscript,uData,
  uprometscripts,variants,uBoilerplate;

procedure TfAutomationframe.Button1Click(Sender: TObject);
begin
  if fBoilerplate.Execute(FDataSet.FieldByName('PREPTEXT').AsString) then
    begin
      FDataSet.Edit;
      FDataSet.FieldByName('PREPTEXT').AsString:=fBoilerplate.DataSet.FieldByName('NAME').AsString;
    end;
end;

procedure TfAutomationframe.Button2Click(Sender: TObject);
begin
  if fBoilerplate.Execute(FDataSet.FieldByName('WORKTEXT').AsString) then
    begin
      FDataSet.Edit;
      FDataSet.FieldByName('WORKTEXT').AsString:=fBoilerplate.DataSet.FieldByName('NAME').AsString;
    end;
end;

procedure TfAutomationframe.acEditScriptExecute(Sender: TObject);
var
  aScript: TBaseScript;
  aVer : Variant;
begin
  if eScript.Text='' then
    begin
      FDataSet.Edit;
      if FDataSet is TMasterdataList then
        FDataSet.FieldByName('SCRIPT').AsString:= TMasterdataList(FDataSet).Number.AsString
      else FDataSet.FieldByName('SCRIPT').AsString:=FDataSet.Id.AsString;
    end;
  SetFocus;
  aScript := TBaseScript.Create(nil);
  aScript.SelectByName(FDataSet.FieldByName('SCRIPT').AsString);
  aScript.Open;
  aVer := FDataSet.FieldByName('SCRIPTVER').AsVariant;
  if aVer = '' then aver := Null;
  if (not aScript.Locate('NAME;VERSION',VarArrayOf([FDataSet.FieldByName('SCRIPT').AsString,aVer]),[loCaseInsensitive]))
  then
    begin
      aScript.Append;
      aScript.FieldByName('NAME').AsString:=FDataSet.FieldByName('SCRIPT').AsString;
      aScript.FieldByName('VERSION').AsVariant:=aVer;
      aScript.Post;
    end;
  if aScript.Count>0 then
    data.GotoLink(data.BuildLink(aScript.DataSet));
  aScript.Free;
end;

procedure TfAutomationframe.acEditPrepareScriptExecute(Sender: TObject);
var
  aScript: TBaseScript;
  aVer : Variant;
begin
  if eScript.Text='' then
    begin
      FDataSet.Edit;
      if FDataSet is TMasterdataList then
        FDataSet.FieldByName('PRSCRIPT').AsString:= TMasterdataList(FDataSet).Number.AsString
      else FDataSet.FieldByName('PRSCRIPT').AsString:=FDataSet.Id.AsString;
    end;
  SetFocus;
  aScript := TBaseScript.Create(nil);
  aScript.SelectByName(FDataSet.FieldByName('PRSCRIPT').AsString);
  aScript.Open;
  aVer := FDataSet.FieldByName('PRSCRIPTVER').AsVariant;
  if aVer = '' then aver := Null;
  if (not aScript.Locate('NAME;VERSION',VarArrayOf([FDataSet.FieldByName('PRSCRIPT').AsString,aVer]),[loCaseInsensitive]))
  then
    begin
      aScript.Append;
      aScript.FieldByName('NAME').AsString:=FDataSet.FieldByName('PRSCRIPT').AsString;
      aScript.FieldByName('VERSION').AsVariant:=aVer;
      aScript.Post;
    end;
  if aScript.Count>0 then
    data.GotoLink(data.BuildLink(aScript.DataSet));
  aScript.Free;
end;

procedure TfAutomationframe.SetDataSet(AValue: TBaseDBDataset);
begin
  if FDataSet=AValue then Exit;
  Position.DataSet:=nil;
  FDataSet:=AValue;
  if FDataSet is TBaseDBPosition then
    begin
      Position.DataSet := TBaseDBPosition(FDataSet).DataSet;
    end;
  if FDataSet is TMasterdata then
    begin
      Position.DataSet := TMasterdata(FDataSet).DataSet;
    end;
end;

procedure TfAutomationframe.SetRights(Editable: Boolean);
begin
  Enabled := Editable;
end;

constructor TfAutomationframe.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TfAutomationframe.Destroy;
begin
  inherited Destroy;
end;

end.

