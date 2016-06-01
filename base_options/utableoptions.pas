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
*******************************************************************************}
unit utableoptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, DbCtrls,
  Dialogs, Buttons, ExtDlgs, ComCtrls, db, uOptionsFrame, uBaseDBClasses,
  uBaseDBInterface,uBaseDatasetInterfaces;

type

  { TfTableOptions }

  TfTableOptions = class(TOptionsFrame)
    iImage: TDBImage;
    lAdress: TLabel;
    mDBOptions: TDBMemo;
    MandantDetailDS: TDatasource;
  private
    { private declarations }
    aConnection: TComponent;
    aMandant: TMandantDetails;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy;override;
    procedure StartTransaction;override;
    procedure CommitTransaction;override;
    procedure RollbackTransaction;override;
  end;
implementation
{$R *.lfm}
uses uData,uOrder,uDocuments,uBaseERPDBClasses,uImpCSV,Utils;

constructor TfTableOptions.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  aConnection := Data.GetNewConnection;
  aMandant := TMandantDetails.CreateEx(Self,Data,aConnection);
  MandantDetailDS.DataSet := aMandant.DataSet;
end;

destructor TfTableOptions.Destroy;
begin
  aMandant.Destroy;
  aConnection.Destroy;
  inherited Destroy;
end;

procedure TfTableOptions.StartTransaction;
begin
  inherited StartTransaction;
  Data.StartTransaction(aConnection);
  aMandant.CreateTable;
  aMandant.Open;
end;

procedure TfTableOptions.CommitTransaction;
begin
  if aMandant.CanEdit then aMandant.DataSet.Post;
  Data.CommitTransaction(aConnection);
  inherited CommitTransaction;
end;

procedure TfTableOptions.RollbackTransaction;
begin
  Data.RollbackTransaction(aConnection);
  inherited RollbackTransaction;
end;

end.

