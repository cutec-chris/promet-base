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
Created 07.07.2012
*******************************************************************************}
unit uarticlesupplierframe;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, DbCtrls, db,
  uFilterFrame,uPrometFramesInplace, uExtControls,DBGrids, StdCtrls;
type

  { TfArticleSupplierFrame }

  TfArticleSupplierFrame = class(TPrometInplaceFrame)
    Bevel1: TBevel;
    cbSupplierTransportCurrency: TDBComboBox;
    eArticleNumberSupplier: TDBEdit;
    eDelivertime: TDBEdit;
    eSupplierTransportPrice: TDBEdit;
    gSupplier: TDBGrid;
    gSupplierPrices: TDBGrid;
    lArticleNumberSupplier: TLabel;
    lDelivertime: TLabel;
    lDelivertime1: TLabel;
    dnNavigator: TDBNavigator;
    ExtRotatedLabel1: TExtRotatedLabel;
    lPrices: TLabel;
    lTransportSupplier: TLabel;
    Panel1: TPanel;
    Panel3: TPanel;
    pToolbar: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    Supplier: TDatasource;
    SupplierPrices: TDataSource;
    procedure gSupplierDblClick(Sender: TObject);
    procedure gSupplierDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure gSupplierDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
  private
    { private declarations }
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;
    procedure SetRights(Editable : Boolean);override;
  end;
implementation
{$R *.lfm}
uses uMainTreeFrame,uSearch,uPerson,uData,uBaseVisualApplication;
procedure TfArticleSupplierFrame.gSupplierDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := False;
  if Source is TDragEntry then
    begin
      Accept := pos('CUSTOMERS' ,TDragEntry(Source).Links)>0;
      exit;
    end;
 end;
procedure TfArticleSupplierFrame.gSupplierDragDrop(Sender, Source: TObject; X,
  Y: Integer);
  procedure AddPosition(aLink : string);
  var
    aPersons: TPersonList;
    aEmployee: String;
    aName: String;
  begin
    if aLink='' then exit;
    aPersons := TPersonList.CreateEx(Self,Data);
    aPersons.SelectFromLink(aLink);
    aPersons.Open;
    aEmployee := aPersons.FieldByName('ACCOUNTNO').AsString;
    aName := aPersons.FieldByName('NAME').AsString;
    aPersons.free;
    if (aEmployee<>'') and (aName<>'') then
      begin
        Supplier.DataSet.Append;
        Supplier.DataSet.FieldByName('ACCOUNTNO').AsString := aEmployee;
        Supplier.DataSet.FieldByName('NAME').AsString := aName;
        Supplier.DataSet.Post;
      end;
  end;

var
  aLinks: String;
begin
  if Source is TDragEntry then
    begin
      aLinks := TDragEntry(Source).Links;
      while pos(';',aLinks)>0 do
        begin
          AddPosition(copy(aLinks,0,pos(';',aLinks)-1));
          aLinks := copy(aLinks,pos(';',aLinks)+1,length(aLinks));
        end;
      AddPosition(aLinks);
    end;
end;

procedure TfArticleSupplierFrame.gSupplierDblClick(Sender: TObject);
begin
  Data.GotoLink('CUSTOMERS@'+Supplier.DataSet.FieldByName('ACCOUNTNO').AsString);
end;

constructor TfArticleSupplierFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;
destructor TfArticleSupplierFrame.Destroy;
begin
  inherited Destroy;
end;
procedure TfArticleSupplierFrame.SetRights(Editable: Boolean);
begin
  Enabled := Editable;
  ArrangeToolBar(pToolbar,nil,'Supplier');
end;
end.

