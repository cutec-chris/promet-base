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
unit uProcessOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, DBGrids, ExtCtrls, StdCtrls,
  DbCtrls, ComCtrls, Buttons, ActnList, uOptionsFrame, db;

type

  { TfProcessOptions }

  TfProcessOptions = class(TOptionsFrame)
    acStartProcess: TAction;
    acStopProcess: TAction;
    ActionList1: TActionList;
    Clients: TDatasource;
    DBGrid1: TDBGrid;
    DBMemo1: TDBMemo;
    DBNavigator1: TDBNavigator;
    DBNavigator2: TDBNavigator;
    gProcesses1: TDBGrid;
    Label1: TLabel;
    Label2: TLabel;
    lProcesses1: TLabel;
    Panel1: TPanel;
    ProcessParameters: TDatasource;
    lProcesses: TLabel;
    Processes: TDatasource;
    gProcesses: TDBGrid;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Timer1: TTimer;
    procedure acStartProcessExecute(Sender: TObject);
    procedure acStopProcessExecute(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure StartTransaction;override;
    procedure CommitTransaction;override;
    procedure RollbackTransaction;override;
  end;

implementation

uses uData, uBaseDbInterface,uProcessManagement,uBaseDatasetInterfaces;
{$R *.lfm}

procedure TfProcessOptions.Timer1Timer(Sender: TObject);
var
  rec: TBookmark;
begin
  if Data.ProcessClient.Processes.CanEdit then exit;
  if Data.ProcessClient.Processes.Parameters.CanEdit then exit;
  Processes.DataSet.DisableControls;
  if Processes.DataSet.Active then
    begin
      rec := Processes.DataSet.GetBookmark;
      Processes.DataSet.Refresh;
      Processes.DataSet.GotoBookmark(rec);
      DBMemo1.SelStart := DBMemo1.GetTextLen;
      DBMemo1.SelLength := 0;
      DBMemo1.ScrollBy(0, DBMemo1.Lines.Count);
      DBMemo1.Refresh;
    end;
  Processes.DataSet.EnableControls;
end;

procedure TfProcessOptions.acStopProcessExecute(Sender: TObject);
begin
  if Processes.DataSet.Active and (Processes.DataSet.RecordCount>0) then
    begin
      if not ((Processes.State = dsEdit) or (Processes.State = dsInsert)) then
        Processes.DataSet.Edit;
      Processes.DataSet.FieldByName('STATUS').AsString:='N';
      Processes.DataSet.Post;
    end;
end;

procedure TfProcessOptions.acStartProcessExecute(Sender: TObject);
begin
  if Processes.DataSet.Active and (Processes.DataSet.RecordCount>0) then
    Data.ProcessClient.Process(True,True);
end;

procedure TfProcessOptions.StartTransaction;
begin
  inherited StartTransaction;
  Clients.DataSet := Data.ProcessClient.DataSet;
  Processes.DataSet := Data.ProcessClient.Processes.DataSet;
  Data.ProcessClient.Open;
  Data.ProcessClient.Processes.Open;
  ProcessParameters.DataSet := Data.ProcessClient.Processes.Parameters.DataSet;
  Data.ProcessClient.Processes.Parameters.DataSet.Open;
  Timer1.Enabled:=True;
  Clients.DataSet.Locate('NAME','*',[]);
end;
procedure TfProcessOptions.CommitTransaction;
begin
  Timer1.Enabled:=False;
  if (Processes.State = dsEdit) or (Processes.State = dsInsert) then
    Processes.DataSet.Post;
  inherited CommitTransaction;
end;
procedure TfProcessOptions.RollbackTransaction;
begin
  Timer1.Enabled:=False;
  if (Processes.State = dsEdit) or (Processes.State = dsInsert) then
    Processes.DataSet.Cancel;
  inherited RollbackTransaction;
end;

end.

