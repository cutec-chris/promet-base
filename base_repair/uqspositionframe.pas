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
unit uQSPositionFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls, DBGrids,
  uPositionFrame, db, Grids, Graphics, DbCtrls, Buttons, Clipbrd, uPrometFramesInplace;

type
 { TfQSPositionFrame }

  TfQSPositionFrame = class(TPrometInplaceFrame)
    eSerialSearch: TEdit;
    lTesttime: TDBText;
    lSerial: TDBText;
    lPosNo: TDBText;
    lIdent: TDBText;
    dgSteps: TDBGrid;
    dgTest: TDBGrid;
    OrderPos: TDatasource;
    OrderQMTest: TDatasource;
    OrderQMtestDetails: TDatasource;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    Splitter1: TSplitter;
    procedure dgStepsDrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure eSerialSearchChange(Sender: TObject);
    procedure FrameEnter(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure TfPositionDatasetTOrderPosQMTestDataSetAfterScroll(
      DataSet: TDataSet);
  private
    { private declarations }
  public
    { public declarations }
    procedure SetupDB;
    procedure SetRights(Editable : Boolean);override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

uses uRawData,uOrder;

{ TfQSPositionFrame }

procedure TfQSPositionFrame.FrameEnter(Sender: TObject);
begin
  SetupDB;
end;
procedure TfQSPositionFrame.dgStepsDrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
const
  memowidth = 200;
var
  s: string;
begin
  if not Assigned(Column.Field) then exit;
  with (Sender as TDBGrid) do begin
    if Column.Field.IsBlob then begin
      Canvas.FillRect(Rect);
      s := copy(Column.Field.AsString, 1, memowidth);
      Canvas.TextOut(Rect.Left+2, Rect.Top+2, s);
    end else begin
      DefaultDrawColumnCell(Rect, DataCol, Column, State);
    end;
  end;
end;

procedure TfQSPositionFrame.eSerialSearchChange(Sender: TObject);
begin
  OrderQMTest.DataSet.Locate('SERIAL',eSerialSearch.Text,[loPartialKey]);
end;

procedure TfQSPositionFrame.SpeedButton1Click(Sender: TObject);
begin
  if TfPosition(Owner).Dataset is TOrderPos then
    begin
      if not Assigned(fRawData) then Application.CreateForm(TfRawData,fRawData);
      fRawData.Memo1.Text:=TOrderPos(TfPosition(Owner).Dataset).QMTest.FieldByName('RAWDATA').AsString;
      fRawData.ShowModal;
    end;
end;

procedure TfQSPositionFrame.SpeedButton2Click(Sender: TObject);
begin
  if TfPosition(Owner).Dataset is TOrderPos then
    Clipboard.AsText:=TOrderPos(TfPosition(Owner).Dataset).QMTest.FieldByName('RAWDATA').AsString;
end;

procedure TfQSPositionFrame.SpeedButton3Click(Sender: TObject);
begin
  OrderQMTest.DataSet.Refresh;
end;

procedure TfQSPositionFrame.TfPositionDatasetTOrderPosQMTestDataSetAfterScroll(
  DataSet: TDataSet);
begin
  if Assigned(Self) then
    OrderQMtestDetails.DataSet.Last;
end;

procedure TfQSPositionFrame.SetupDB;
begin
  if TfPosition(Owner).Dataset is TOrderPos then
    with TfPosition(Owner).DataSet as TorderPos do
      begin
        OrderPos.DataSet := DataSet;
        OrderQMTest.DataSet := QMTest.DataSet;
        QMTest.Open;
        OrderQMTestDetails.DataSet := QMTest.Details.DataSet;
        QMTest.Details.Open;
        QMTest.DataSet.AfterScroll:=@TfPositionDatasetTOrderPosQMTestDataSetAfterScroll;
      end;
end;

procedure TfQSPositionFrame.SetRights(Editable: Boolean);
begin
  dgTest.ReadOnly := not Editable;
  dgSteps.ReadOnly := not Editable;
end;

destructor TfQSPositionFrame.Destroy;
begin
  try
    if Assigned(Self) and Assigned(Owner) and (TfPosition(Owner).Dataset is TOrderPos) then
      with TfPosition(Owner).DataSet as TorderPos do
        QMTest.DataSet.AfterScroll:=nil
  except
  end;
  inherited Destroy;
end;

end.

