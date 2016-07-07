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
Created 07.07.2016
*******************************************************************************}
unit uCreateProductionOrder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ButtonPanel, EditBtn,uOrder,uMasterdata,variants,uIntfStrConsts;

type

  { TfCreateProductionOrder }

  TfCreateProductionOrder = class(TForm)
    ButtonPanel1: TButtonPanel;
    eMasterdata: TEditButton;
    cbVersion: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure eMasterdataButtonClick(Sender: TObject);
    procedure eMasterdataExit(Sender: TObject);
    function SetOrderfromSearch(aLink: string): Boolean;
  private
    { private declarations }
  public
    { public declarations }
    function Execute(FOrder: TOrder; aID: string; aVersion: Variant): Boolean;
  end;

var
  fCreateProductionOrder: TfCreateProductionOrder;

implementation

uses uSearch;

resourcestring
  strMasterdataWithoutPiecelist                   = 'Der Artikel hat keine Stückliste, wirklich erstellen ?';

{ TfCreateProductionOrder }

procedure TfCreateProductionOrder.eMasterdataButtonClick(Sender: TObject);
begin
  fSearch.SetLanguage;
  fSearch.OnOpenItem:=@SetOrderfromSearch;
  fSearch.AllowSearchTypes(strMasterdata);
  fSearch.Execute(True,'PRODSE','');
end;

procedure TfCreateProductionOrder.eMasterdataExit(Sender: TObject);
var
  aMasterdata: TMasterdata;
begin
  aMasterdata := TMasterdata.Create(nil);
  aMasterdata.Select(eMasterdata.Text);
  aMasterdata.Open;
  if aMasterdata.Count>0 then
    begin
      aMasterdata.Select(aMasterdata.Number.AsString);
      aMasterdata.Open;
      eMasterdata.Text:=aMasterdata.Number.AsString;
      cbVersion.Text:=aMasterdata.Version.AsString;
      cbVersion.Enabled:=aMasterdata.Count>1;
      cbVersion.Items.Clear;
      aMasterdata.First;
      while not aMasterdata.EOF do
        begin
          cbVersion.Items.Add(aMasterdata.Version.AsString);
          aMasterdata.Next;
        end;
      if cbVersion.Enabled then
        begin
          aMasterdata.Locate('ACTIVE','Y',[]);
          cbVersion.Text:=aMasterdata.Version.AsString;
        end;
    end;
  aMasterdata.Free;
end;

function TfCreateProductionOrder.SetOrderfromSearch(aLink: string): Boolean;
var
  aMasterdata: TMasterdata;
begin
  aMasterdata := TMasterdata.Create(nil);
  aMasterdata.SelectFromLink(aLink);
  aMasterdata.Open;
  if aMasterdata.Count>0 then
    begin
      aMasterdata.Select(aMasterdata.Number.AsString);
      aMasterdata.Open;
      eMasterdata.Text:=aMasterdata.Number.AsString;
      cbVersion.Text:=aMasterdata.Version.AsString;
      cbVersion.Enabled:=aMasterdata.Count>1;
      cbVersion.Items.Clear;
      aMasterdata.First;
      while not aMasterdata.EOF do
        begin
          cbVersion.Items.Add(aMasterdata.Version.AsString);
          aMasterdata.Next;
        end;
      if cbVersion.Enabled then
        begin
          aMasterdata.Locate('ACTIVE','Y',[]);
          cbVersion.Text:=aMasterdata.Version.AsString;
        end;
    end;
  aMasterdata.Free;
end;

function TfCreateProductionOrder.Execute(FOrder: TOrder; aID: string;
  aVersion: Variant): Boolean;
var
  aMasterdata: TMasterdata;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfCreateProductionOrder,fCreateProductionOrder);
      Self := fCreateProductionOrder;
    end;
  eMasterdata.Text:=aID;
  eMasterdataExit(nil);
  if aVersion<>Null then
    cbVersion.Text:=aVersion;
  Result := ShowModal=mrOK;
  if Result then
    begin
      //Find Article and Create Order
      aMasterdata := TMasterdata.Create(nil);
      aMasterdata.SelectFromNumber(eMasterdata.Text);
      aMasterdata.Open;
      if aVersion<>Null then
        aMasterdata.Locate('VERSION',cbVersion.Text,[]);
      aMasterdata.Positions.Open;
      if aMasterdata.Positions.Count=0 then
        begin
          if MessageDlg(strMasterdata,strMasterdataWithoutPiecelist,mtWarning,[mbYes,mbNo],0) <> mrYes then
            begin
              Result:=False;
              exit;
            end;
        end;
      if aMasterdata.Count>0 then
        begin
          FOrder.Insert;
          FOrder.Positions.Insert;
          //FOrder.Status.AsString:=FOrder.OrderType.FieldByName('STATUS').AsString;
          FOrder.Positions.Assign(aMasterdata);
          FOrder.Positions.Post;
          FOrder.Post;
        end;
      aMasterdata.Free;
    end;
end;

initialization
  {$I uCreateProductionOrder.lrs}

end.

