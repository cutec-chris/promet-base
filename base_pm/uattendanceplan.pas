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
Created 05.05.2013
*******************************************************************************}
unit uattendanceplan;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, Menus, ActnList, gsGanttCalendar, uTask, Math, types,
  uPrometFrames, uIntfStrConsts, Variants,ugridview,grids,dbgrids,utasks,
  uTaskPlan;

type

  { TfAttPlan }

  TfAttPlan = class(TPrometMainFrame)
    ActionList1: TActionList;
    bDayView: TSpeedButton;
    Bevel3: TBevel;
    Bevel5: TBevel;
    Bevel7: TBevel;
    bMonthView: TSpeedButton;
    bRefresh: TSpeedButton;
    bToday: TSpeedButton;
    bWeekView: TSpeedButton;
    eSearch: TEdit;
    ExtRotatedLabel1: TLabel;
    IdleTimer1: TTimer;
    iHourglass: TImage;
    Label5: TLabel;
    Label7: TLabel;
    lDate: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    miUserOptions: TMenuItem;
    Panel9: TPanel;
    pgantt: TPanel;
    Panel7: TPanel;
    pmAction: TPopupMenu;
    pNav: TPanel;
    tbTop: TPanel;
    procedure acCancelExecute(Sender: TObject);
    procedure aINewDrawBackground(Sender: TObject; aCanvas: TCanvas;
      aRect: TRect; aStart, aEnd: TDateTime; aDayWidth: Double;aUnfinishedList : TList = nil);
    procedure aINewOpen(Sender: TObject);
    procedure bDayViewClick(Sender: TObject);
    procedure bMonthViewClick(Sender: TObject);
    procedure bRefreshClick(Sender: TObject);
    procedure bShowTasksClick(Sender: TObject);
    procedure bTodayClick(Sender: TObject);
    procedure bWeekViewClick(Sender: TObject);
    procedure eSearchChange(Sender: TObject);
    procedure FGanttCalendarDblClick(Sender: TObject);
    procedure FGanttCalendarMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FGanttCalendarMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FGanttCalendarMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FGanttCalendarMoveOverInterval(Sender: TObject;
      aInterval: TInterval; X, Y: Integer);
    procedure FGanttCalendarShowHint(Sender: TObject; HintInfo: PHintInfo);
    procedure FGanttCalendarStartDateChanged(Sender: TObject);
    procedure FGanttTreeAfterUpdateCommonSettings(Sender: TObject);
    procedure FGanttTreeResize(Sender: TObject);
    procedure IdleTimer1Timer(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure TCollectThreadTerminate(Sender: TObject);
    procedure TIntervalChanged(Sender: TObject);
  private
    { private declarations }
    FSelectedCol: TDateTime;
    FSelectedInterval : TInterval;
    FSelectedRow: Int64;
    FGantt: TgsGantt;
    FThreads : TList;
    FTasks : TTaskList;
    FHintRect : TRect;
    FCollectedTo : TDateTime;
    FCollectedFrom : TDateTime;
    FRow : Integer;
    FUsers : TStringList;
    FOwners : TStringList;
    aClickPoint: types.TPoint;
    FSelectedUser : TInterval;
    procedure QueThread(aPlan : TWinControl;aFrom,aTo : TDateTime;aResource : TRessource;asUser : string;AttatchTo : TInterval = nil;Colorized : Boolean = False);
    procedure CheckThreads;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Populate(aParent : Variant;aUser : Variant);
    procedure CollectResources(aResource : TRessource;aFrom, aTo: TDateTime;asUser : string;aConnection : TComponent = nil;Colorized : Boolean = False);
    function GetIntervalFromCoordinates(Gantt: TgsGantt; X, Y, Index: Integer): TInterval;
    function GetTaskIntervalFromCoordinates(Gantt: TgsGantt; X, Y, Index: Integer): TInterval;
  end;

  { TCollectThread }

  TCollectThread = class(TThread)
  private
    FResource: uTaskPlan.TRessource;
    FUser: String;
    FPlan: TfAttPlan;
    FAttatchTo: TInterval;
    FColorized : Boolean;
    FFrom: TDateTime;
    FTo: TDateTime;
    procedure Attatch;
    procedure Plan;
  public
    procedure Execute; override;
    constructor Create(aPlan : TfAttPlan;aFrom,aTo : TDateTime;aResource : TRessource;asUser : string;AttatchTo : TInterval = nil;Colorized : Boolean = False);
    property User : string read FUser;
  end;

implementation
uses uData,LCLIntf,uBaseDbClasses,uProjects,uTaskEdit,LCLProc,uGanttView,uColors,
  uCalendar,uTaskPlanOptions,uBaseERPDBClasses,uAttStatistic,uEventEdit,uCalendarFrame;
{$R *.lfm}

{ TCollectThread }

procedure TCollectThread.Attatch;
begin
  FAttatchTo.Pointer:=FResource;
  try
    FPlan.Invalidate;
    FPlan.iHourglass.Visible:=False;
    Application.ProcessMessages;
  except
  end;
end;

procedure TCollectThread.Plan;
var
  aConnection: Classes.TComponent;
begin
  aConnection := nil;
  FPlan.CollectResources(FResource,FFrom,FTo,FUser,aConnection,FColorized);
end;

procedure TCollectThread.Execute;
begin
  Synchronize(@Plan);
  Synchronize(@Attatch);
end;

constructor TCollectThread.Create(aPlan: TfAttPlan; aFrom, aTo: TDateTime;
  aResource: TRessource; asUser: string; AttatchTo: TInterval;
  Colorized: Boolean);
begin
  FPlan := aPlan;
  FFrom := aFrom;
  FTo := aTo;
  FResource := aResource;
  FUser := asUser;
  FAttatchTo := AttatchTo;
  FreeOnTerminate:=True;
  FColorized := Colorized;
  inherited Create(True);
end;

procedure TfAttPlan.FGanttTreeAfterUpdateCommonSettings(Sender: TObject);
begin
  fgantt.Tree.ColWidths[0]:=0;
  fgantt.Tree.ColWidths[1]:=0;
  fgantt.Tree.ColWidths[2]:=180;
  fgantt.Tree.Cells[2,0]:=strUsers;
  fgantt.Tree.ColWidths[3]:=0;
  fgantt.Tree.ColWidths[4]:=0;
  fgantt.Tree.ColWidths[5]:=0;
  fgantt.Tree.ColWidths[6]:=0;
  fgantt.Tree.ColWidths[7]:=0;
  FGantt.Tree.Width:=190;
  FGantt.Tree.OnResize:=@FGanttTreeResize;
  FGantt.Tree.ShowHint:=True;
  FGantt.Tree.Options:=FGantt.Tree.Options+[goCellHints];
  FGantt.Tree.Options:=FGantt.Tree.Options-[goHorzLine];
  FGantt.Tree.AlternateColor:=$00FFE6E6;
end;

procedure TfAttPlan.FGanttTreeResize(Sender: TObject);
begin
  fgantt.Tree.ColWidths[0]:=0;
  fgantt.Tree.ColWidths[1]:=0;
  fgantt.Tree.ColWidths[2]:=FGantt.Tree.Width-FGantt.Tree.ColWidths[3]-FGantt.Tree.ColWidths[4]-FGantt.Tree.ColWidths[5];
  fgantt.Tree.ColWidths[6]:=0;
  fgantt.Tree.ColWidths[7]:=0;
end;

procedure TfAttPlan.IdleTimer1Timer(Sender: TObject);
var
  i: Integer;
  FoundInterval : TInterval = nil;
  function SearchInterval(aInterval : TInterval) : Boolean;
  var
    a: Integer;
  begin
    Result := pos(lowercase(eSearch.Text),lowercase(aInterval.Task))>0;
    if not Result then
      begin
        for a := 0 to aInterval.IntervalCount-1 do
          begin
            Result := SearchInterval(aInterval.Interval[a]);
            if Result then
              break;
          end;
      end
    else
      FoundInterval := aInterval;
  end;

begin
  if IdleTimer1.Tag=1 then exit;
  IdleTimer1.Tag:=1;
  IdleTimer1.Enabled:=false;
  if length(eSearch.Text)<3 then
    begin
      IdleTimer1.Tag:=0;
      exit;
    end;
  for i := 0 to FGantt.IntervalCount-1 do
    begin
      if SearchInterval(FGantt.Interval[i]) then break;
    end;
  FGantt.BeginUpdate;
  FGantt.Tree.Row:=FoundInterval.Index;
  while Assigned(FoundInterval) and Assigned(FoundInterval.Parent) do
    begin
      FoundInterval.Parent.Opened:=True;
      FoundInterval := FoundInterval.Parent;
    end;
  FGantt.EndUpdate;
  IdleTimer1.Tag:=0;
end;

procedure TfAttPlan.MenuItem1Click(Sender: TObject);
var
  List: TList;
begin
  List := TList.Create;
  FGantt.MakeIntervalList(List);
  fAttStatistic.Execute(TInterval(List[Fgantt.Tree.Row-1]));
  List.free;
end;

procedure TfAttPlan.TCollectThreadTerminate(Sender: TObject);
begin
  if FThreads.IndexOf(Sender) > -1 then
    FThreads.Remove(Sender);
  FGantt.Invalidate;
  iHourglass.Visible:=FThreads.Count>0;
  if not iHourglass.Visible then
    FGantt.Calendar.Cursor:=crDefault
  else
    FGantt.Calendar.Cursor:=crHourGlass;
  Application.ProcessMessages;
end;

procedure TfAttPlan.TIntervalChanged(Sender: TObject);
begin
  TInterval(TInterval(Sender).Pointer2).StartDate:=TInterval(Sender).StartDate;
  TInterval(TInterval(Sender).Pointer2).FinishDate:=TInterval(Sender).FinishDate;
end;

procedure TfAttPlan.QueThread(aPlan: TWinControl; aFrom, aTo: TDateTime;
  aResource: TRessource; asUser: string; AttatchTo: TInterval;
  Colorized: Boolean);
var
  Found: Boolean;
  i: Integer;
begin
  Found := False;
  for i := 0 to FThreads.Count-1 do
    begin
      if TCollectThread(FThreads[i]).User=asUser then
        begin
          aResource.Abort:=True;
          Found := True;
          break;
        end;
    end;
  FThreads.Add(TCollectThread.Create(Self,aFrom,aTo,aResource,asUser,AttatchTo,Colorized));
  TCollectThread(FThreads[FThreads.Count-1]).OnTerminate:=@TCollectThreadTerminate;
end;

procedure TfAttPlan.CheckThreads;
var
  i: Integer;
begin
  for i := 0 to FThreads.Count-1 do
    TCollectThread(FThreads[i]).Resume;
  iHourglass.Visible:=FThreads.Count>0;
  if not iHourglass.Visible then
    FGantt.Calendar.Cursor:=crDefault
  else
    begin
      FGantt.Calendar.Cursor:=crHourGlass;
      Application.ProcessMessages;
    end;
end;

procedure TfAttPlan.bDayViewClick(Sender: TObject);
begin
  FGantt.MinorScale:=tsDay;
  FGantt.MajorScale:=tsWeekNum;
  FGantt.Calendar.StartDate:=FGantt.Calendar.StartDate;
end;

procedure TfAttPlan.aINewDrawBackground(Sender: TObject; aCanvas: TCanvas;
  aRect: TRect; aStart, aEnd: TDateTime; aDayWidth: Double;
  aUnfinishedList: TList);
var
  TaskPlan : TfTaskPlan;
begin
  Taskplan.aIDrawBackgroundWeekends(Sender,aCanvas,aRect,aStart,aEnd,aDayWidth,$e0e0e0,FSelectedCol);
  Taskplan.aIDrawBackground(Sender,aCanvas,aRect,aStart,aEnd,aDayWidth,clBlue,clLime,clRed,FSelectedCol);
end;

procedure TfAttPlan.aINewOpen(Sender: TObject);
var
  i: Integer;
  tmpRes: TRessource;
begin
  for i := 0 to TInterval(Sender).IntervalCount-1 do
    if TInterval(Sender).Interval[i] is TPInterval then
      begin
        tmpRes := TRessource.Create(nil);
        tmpRes.User:=TPInterval(TInterval(Sender).Interval[i]);
        QueThread(Self,FCollectedFrom,FCollectedTo,tmpRes,TPInterval(TInterval(Sender).Interval[i]).User,TInterval(Sender).Interval[i],(TInterval(Sender).Interval[i].Color=clRed) or (TInterval(Sender).Color=clred));
      end;
  FGanttCalendarStartDateChanged(FGantt.Calendar);
  CheckThreads;
end;

procedure TfAttPlan.acCancelExecute(Sender: TObject);
begin
  bRefreshClick(Sender);
end;

procedure TfAttPlan.bMonthViewClick(Sender: TObject);
begin
  FGantt.MinorScale:=tsDay;
  FGantt.MajorScale:=tsQuarter;
  FGantt.MinorScale:=tsMonth;
  FGantt.Calendar.StartDate:=FGantt.Calendar.StartDate;
end;

procedure TfAttPlan.bRefreshClick(Sender: TObject);
  procedure RefreshRes(aInt : TInterval);
  var
    i: Integer;
    aUser: String;
    tmpRes: TRessource;
  begin
    for i := 0 to aInt.IntervalCount-1 do
      RefreshRes(aInt.Interval[i]);
    if Assigned(aInt.Pointer) then
      begin
        aUser := TPInterval(aInt).User;
        TInterval(aInt.Pointer).Free;
        aInt.Pointer := nil;
        tmpRes := TRessource.Create(nil);
        tmpRes.User:=TPInterval(aInt);
        QueThread(Self,FCollectedFrom,FCollectedTo,tmpRes,TPInterval(TInterval(Sender).Interval[i]).User,TInterval(Sender).Interval[i],(TInterval(Sender).Interval[i].Color=clRed) or (TInterval(Sender).Color=clred));
      end;
  end;
var
  i: Integer;
begin
  for i := 0 to FGantt.IntervalCount-1 do
    RefreshRes(FGantt.Interval[i]);
  CheckThreads;
end;

procedure TfAttPlan.bShowTasksClick(Sender: TObject);
begin
  FGantt.Calendar.Invalidate;
end;

procedure TfAttPlan.bTodayClick(Sender: TObject);
begin
  FGantt.StartDate:=Now();
end;

procedure TfAttPlan.bWeekViewClick(Sender: TObject);
begin
  FGantt.MinorScale:=tsDay;
  FGantt.MajorScale:=tsMonth;
  FGantt.MinorScale:=tsWeekNumPlain;
  FGantt.Calendar.StartDate:=FGantt.Calendar.StartDate;
end;

procedure TfAttPlan.eSearchChange(Sender: TObject);
begin
  IdleTimer1.Enabled:=True;
end;

function IsInRect(X, Y: Integer; R: TRect): Boolean;
begin
  Result := (X >= R.Left) and (X <= R.Right); //and (Y >= R.Top) and (Y <= R.Bottom);
end;
function IsInRow(X, Y: Integer; R: TRect): Boolean;
begin
  Result := (Y >= R.Top) and (Y <= R.Bottom);
end;

procedure TfAttPlan.FGanttCalendarDblClick(Sender: TObject);
var
  aCalFrame: TfCalendarFrame;
  aEventEdit: TfEventEdit;
  List: TList;
  ay: Integer;
  i: Integer;
begin
  if Assigned(FSelectedInterval) then
    begin
      aCalFrame := TfCalendarFrame.Create(nil);
      TCalendar(aCalFrame.DataSet).Select(FSelectedInterval.Id);
      aCalFrame.DataSet.Open;
      aCalFrame.DataStore.LoadEvents;
      aEventEdit := TfEventEdit.Create(Self);
      if aCalFrame.DataStore.Resource.Schedule.EventCount>0 then
        aEventEdit.Execute(aCalFrame.DataStore.Resource.Schedule.GetEvent(0),aCalFrame.DataStore.Resource,aCalFrame.DataStore.Directory,aCalFrame.DataStore);
      aEventEdit.Free;
      aCalFrame.Free;
    end;
end;

procedure TfAttPlan.FGanttCalendarMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  List: TList;
  ay: Integer;
  i: Integer;
begin
  FSelectedInterval:=nil;
  List := TList.Create;
  FGantt.MakeIntervalList(List);
  ay := y -FGantt.Calendar.StartDrawIntervals;
  ay := ay div max(FGantt.Calendar.PixelsPerLine,1);
  ay := ay+(FGantt.Tree.TopRow-1);
  if (ay<List.Count) and (ay>-1) then
    if Assigned(TInterval(List[ay]).Pointer) then
      begin
        for i := 0 to TRessource(TInterval(List[ay]).Pointer).IntervalCount-1 do
          if not TRessource(TInterval(List[ay]).Pointer).Interval[i].IsDrawRectClear then
            if IsInRect(X,y,TRessource(TInterval(List[ay]).Pointer).Interval[i].DrawRect) then
              if TRessource(TInterval(List[ay]).Pointer).Interval[i].Color<>clBlue then
                begin
                  if (TRessource(TInterval(List[ay]).Pointer).Interval[i] is TBackInterval) then
                    begin
                      FSelectedInterval:=TRessource(TInterval(List[ay]).Pointer).Interval[i];
                    end;
                end;
      end;
  List.Free;
end;

procedure TfAttPlan.FGanttCalendarMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if fHintRect.Left>-1 then
    begin
      if (X<FHintRect.Left)
      or (X>FHintRect.Right)
      then
        begin
          FHintRect.Left:=-1;
          Application.CancelHint;
        end;
    end;
end;

procedure TfAttPlan.FGanttCalendarMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  CheckThreads;
  Application.ProcessMessages;
end;

procedure TfAttPlan.FGanttCalendarMoveOverInterval(Sender: TObject;
  aInterval: TInterval; X, Y: Integer);
begin
  if (not Assigned(aInterval)) then
    begin
      FSelectedInterval := nil;
      FGantt.Calendar.Hint:='';
      exit;
    end;
  if FGantt.Calendar.Hint=aInterval.Task then exit;
  FHintRect:=aInterval.DrawRect;
  FGantt.Calendar.Hint:=aInterval.Task+LineEnding+aInterval.Resource;
  FGantt.Calendar.ShowHint:=True;
end;

procedure TfAttPlan.FGanttCalendarShowHint(Sender: TObject;
  HintInfo: PHintInfo);
var
  ay: Integer;
  i: Integer;
  List : TList;
  aPercent : Integer = 0;
  DD: String;
begin
  if HintInfo^.HintStr='' then
    begin
      List := TList.Create;
      FGantt.MakeIntervalList(List);
      ay := HintInfo^.CursorPos.Y-FGantt.Calendar.StartDrawIntervals;
      ay := ay div max(FGantt.Calendar.PixelsPerLine,1);
      ay := ay+(FGantt.Tree.TopRow-1);
      HintInfo^.HintStr := '';
      HintInfo^.HideTimeout:=30000;
      if (ay<List.Count) and (ay>-1) then
        if Assigned(TInterval(List[ay]).Pointer) then
          begin
            for i := 0 to TRessource(TInterval(List[ay]).Pointer).IntervalCount-1 do
              if not TRessource(TInterval(List[ay]).Pointer).Interval[i].IsDrawRectClear then
                if IsInRect(HintInfo^.CursorPos.X,HintInfo^.CursorPos.Y,TRessource(TInterval(List[ay]).Pointer).Interval[i].DrawRect) then
                  if TRessource(TInterval(List[ay]).Pointer).Interval[i].Color<>clBlue then
                    begin
                      aPercent := aPercent+round(TRessource(TInterval(List[ay]).Pointer).Interval[i].PercentUsage*100);
                      if (TRessource(TInterval(List[ay]).Pointer).Interval[i] is TBackInterval) then
                        begin
                          if HintInfo^.HintStr <> '' then HintInfo^.HintStr := HintInfo^.HintStr+lineending;
                          HintInfo^.HintStr := HintInfo^.HintStr+TRessource(TInterval(List[ay]).Pointer).Interval[i].Task;
                          FSelectedInterval:=TRessource(TInterval(List[ay]).Pointer).Interval[i];
                        end;
                    end;
          end;
      List.Free;
    end;
end;

procedure TfAttPlan.FGanttCalendarStartDateChanged(Sender: TObject);
var
  aDiff: Extended;
  procedure RefreshRes(aInt : TInterval);
  var
    i: Integer;
    aUser: String;
    tmpRes: TRessource;
  begin
    for i := 0 to aInt.IntervalCount-1 do
      RefreshRes(aInt.Interval[i]);
    if Assigned(aInt.Pointer) then
      begin
        aUser := TPInterval(aInt).User;
        if aDiff>0 then
          QueThread(Self,FCollectedFrom,FCollectedTo,TRessource(aInt.Pointer),aUser,aInt,(aInt.Color=clRed) or (aInt.Color=clred))
        else
          QueThread(Self,FCollectedFrom,FCollectedTo,TRessource(aInt.Pointer),aUser,aInt,(aInt.Color=clRed) or (aInt.Color=clred));
      end;
  end;
var
  i: Integer;
begin
  if FGantt.Calendar.VisibleFinish>FCollectedTo then
    begin
      aDiff := FGantt.Calendar.VisibleFinish-FCollectedTo;
      if aDiff<30 then aDiff := 30;
      for i := 0 to FGantt.IntervalCount-1 do
        RefreshRes(FGantt.Interval[i]);
      FCollectedTo:=FCollectedTo+aDiff;
    end;
  if FGantt.Calendar.VisibleStart<FCollectedFrom then
    begin
      aDiff := FGantt.Calendar.VisibleStart-FCollectedFrom;
      if aDiff>-30 then aDiff := -30;
      for i := 0 to FGantt.IntervalCount-1 do
        RefreshRes(FGantt.Interval[i]);
      FCollectedFrom:=FCollectedFrom+aDiff;
    end;
end;

constructor TfAttPlan.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FThreads := TList.Create;
  FCollectedFrom:=Now();
  FCollectedTo:=Now();
  FOwners := TStringList.Create;
  FUsers := TStringList.Create;
  FGantt := TgsGantt.Create(Self);
  FSelectedUser := nil;
  FGantt.Parent := pgantt;
  FGantt.Tree.TaskEditable:=False;
  FGantt.Align:=alClient;
  FGantt.Tree.AfterUpdateCommonSettings:=@FGanttTreeAfterUpdateCommonSettings;
  FGantt.Calendar.OnMoveOverInterval:=@FGanttCalendarMoveOverInterval;
  FGantt.Calendar.OnShowHint:=@FGanttCalendarShowHint;
  FGantt.Calendar.OnMouseMove:=@FGanttCalendarMouseMove;
  FGantt.Calendar.OnStartDateChanged:=@FGanttCalendarStartDateChanged;
  FGantt.Calendar.OnMouseUp:=@FGanttCalendarMouseUp;
  FGantt.Calendar.OnMouseDown:=@FGanttCalendarMouseDown;
  FGantt.Calendar.OnDblClick:=@FGanttCalendarDblClick;
  FGantt.Tree.PopupMenu := pmAction;
  bDayViewClick(nil);
  FGantt.Calendar.ShowHint:=True;
end;

destructor TfAttPlan.Destroy;
  procedure ClearResources(aInt : TInterval);
  var
    i: Integer;
    aInt1: TInterval;
  begin
    for i := 0 to aInt.IntervalCount-1 do
      ClearResources(aInt.Interval[i]);
    if Assigned(aInt.Pointer) then
      begin
        TInterval(aInt.Pointer).Free;
        aInt.Pointer := nil;
      end;
  end;
var
  i: Integer;
begin
  for i := 0 to FGantt.IntervalCount-1 do
    ClearResources(FGantt.Interval[i]);
  if Assigned(FDataSet) then
    begin
      FreeAndNil(FDataSet);
    end;
  while FThreads.Count>0 do
    Application.ProcessMessages;
  FThreads.Free;
  FOwners.Free;
  FUsers.Free;
  inherited Destroy;
end;

procedure TfAttPlan.Populate(aParent: Variant;aUser : Variant);
var
  aTasks: TTaskList;
var
  aNewInterval: TInterval;
  aTask: TTask;
  aInterval: TInterval;
  aDep: TInterval;
  i: Integer;
  aUserFilter : string ='';
  aRoot: TUser;
  aIRoot: TInterval;
  ColorUser : Variant;
  HighestInterval : TInterval = nil;

  procedure CollectUsers(aIParent : TInterval;bParent : Variant;Colorized : Boolean = False);
  var
    aUsers: TUser;
    aINew: TPInterval;
    tmpRes: TRessource;
    aActive: TActiveUsers;
  begin
    aUsers := TUser.Create(nil);
    Data.SetFilter(aUsers,Data.QuoteField('PARENT')+'='+Data.QuoteValue(bParent));
    aUsers.First;
    aActive := TActiveUsers.Create(nil);
    while not aUsers.EOF do
      begin
        if aUsers.FieldByName('TYPE').AsString='G' then
          begin
            aINew := TPInterval.Create(FGantt);
            aINew.Task:=aUsers.FieldByName('NAME').AsString;
            aINew.StartDate:=Now()-(365*10);
            aINew.FinishDate:=Now()-(365*10);
            aINew.Visible:=True;
            aINew.Style:=isNone;
            aIParent.AddInterval(aINew);
            aINew.Opened:=False;
            aINew.OnExpand:=@aINewOpen;
            if (aUsers.Id.AsVariant=ColorUser) or Colorized then
              begin
                aINew.Color:=clred;
                CollectUsers(aINew,aUsers.Id.AsVariant,(aUsers.Id.AsVariant=ColorUser) or Colorized);
                HighestInterval := aINew;
              end
            else
              begin
                CollectUsers(aINew,aUsers.Id.AsVariant,(aUsers.Id.AsVariant=ColorUser) or Colorized);
                aINew.Visible:=False;
              end;
            aINew.OnDrawBackground:=@aINewDrawBackground;
          end
        else if not ((aUsers.FieldByName('LEAVED').AsString<>'') and (aUsers.FieldByName('LEAVED').AsDateTime<Now())) then
          begin
            aINew := TPInterval.Create(FGantt);
            aINew.Task:=aUsers.FieldByName('NAME').AsString;
            aINew.StartDate:=Now()-(365*10);
            aINew.FinishDate:=Now()-(365*10);
            aINew.SetUser(aUsers.FieldByName('ACCOUNTNO').AsString,nil);
            aIParent.AddInterval(aINew);
            Data.SetFilter(aActive,Data.QuoteField('NAME')+'='+Data.QuoteValue(aUsers.FieldByName('NAME').AsString));
            if aActive.Count>0 then
              begin
                aINew.Styles:=[fsBold];
              end;
            if (aUsers.Id.AsVariant=ColorUser) or Colorized then
              begin
                aINew.Color:=clred;
                HighestInterval:=aINew;
              end;
            aINew.OnDrawBackground:=@aINewDrawBackground;
          end;
        aUsers.Next;
      end;
    aActive.Free;
    aUsers.Free;
  end;
begin
  if Data.Users.FieldByName('POSITION').AsString='LEADER' then
    ColorUser := Data.Users.FieldByName('PARENT').AsVariant
  else ColorUser := Data.Users.Id.AsVariant;
  while FGantt.IntervalCount>0 do
    FGantt.DeleteInterval(0);
  aRoot := TUser.Create(nil);
  aRoot.SelectByParent(Null);
  aRoot.Open;
  Application.ProcessMessages;
  FGantt.BeginUpdate;
  while not aRoot.EOF do
    begin
      aIRoot := TInterval.Create(FGantt);
      aIRoot.Task:=aRoot.FieldByName('NAME').AsString;
      aIRoot.Visible:=True;
      aIRoot.StartDate:=Now()-1;
      aIRoot.FinishDate:=Now()-1;
      FGantt.AddInterval(aIRoot);
      CollectUsers(aIRoot,aRoot.Id.AsVariant,aRoot.Id.AsVariant=aUser);
      aRoot.DataSet.Next;
      aIRoot.OnDrawBackground:=@aINewDrawBackground;
    end;
  while Assigned(HighestInterval) do
    begin
      HighestInterval.Opened:=False;
      HighestInterval.Opened:=True;
      HighestInterval := HighestInterval.Parent;
    end;
  aRoot.Free;
  FGantt.EndUpdate;
  FGantt.Tree.TopRow:=1;
  FGantt.StartDate:=Now();
end;

procedure TfAttPlan.CollectResources(aResource: TRessource; aFrom,
  aTo: TDateTime; asUser: string; aConnection: TComponent; Colorized: Boolean);
var
  aUser: TUser;
  bTasks: TTaskList;
  bInterval: TPInterval;
  aDue: System.TDateTime;
  aStart: System.TDateTime;
  aCalendar: TCalendar;
  gView : TfGanttView;
  aCat: TCategory;
begin
  aUser := TUser.CreateEx(Self,Data,aConnection);
  aUser.SelectByAccountno(asUser);
  aUser.Open;
  aResource.Resource := aUser.Text.AsString;
  aUser.Free;
  aResource.Accountno := asUSer;
  aCalendar := TCalendar.CreateEx(Self,Data,aConnection);
  aCalendar.SelectPlanedByUserAndTime(asUser,aFrom,aTo);
  aCalendar.Open;
  with aCalendar.DataSet do
    begin
      First;
      while not EOF do
        begin
          bInterval := TBackInterval.Create(nil);
          bInterval.StartDate:=aCalendar.FieldByName('STARTDATE').AsDateTime;
          bInterval.FinishDate:=aCalendar.FieldByName('ENDDATE').AsDateTime;
          if aCalendar.FieldByName('ALLDAY').AsString = 'Y' then
            begin
              bInterval.StartDate := trunc(bInterval.StartDate);
              bInterval.FinishDate := trunc(bInterval.FinishDate+1);
            end;
          if Colorized then
            begin
              aCat := TCategory.Create(nil);
              Data.SetFilter(aCat,Data.QuoteField('TYPE')+'='+Data.QuoteValue('C')+' AND '+Data.QuoteField('NAME')+'='+Data.QuoteValue(aCalendar.FieldByName('CATEGORY').AsString));
              if aCat.Count>0 then
                bInterval.Color:=StringToColor(aCat.FieldByName('COLOR').AsString);
              aCat.Free;
            end;
          bInterval.Task:=aCalendar.FieldByName('SUMMARY').AsString;
          bInterval.Project:=aCalendar.FieldByName('CATEGORY').AsString;
          bInterval.Id:=aCalendar.Id.AsVariant;
          aResource.AddInterval(bInterval);
          bInterval.Changed:=False;
          Next;
        end;
    end;
  aCalendar.Free;
  aResource.Sort;
end;

function TfAttPlan.GetIntervalFromCoordinates(Gantt: TgsGantt; X, Y,Index : Integer): TInterval;
var
  List: TList;
  aId : Variant;
  ay: Integer;
  i: Integer;
  aIdx : Integer = 0;
  function IsInRect(aX, aY: Integer; R: TRect): Boolean;
  begin
    Result := (aX >= R.Left) and (aX <= R.Right);
  end;
begin
  Result := nil;
  aId := Null;
  List := TList.Create;
  Gantt.MakeIntervalList(List);
  ay := Y-Gantt.Calendar.StartDrawIntervals;
  ay := ay div max(Gantt.Calendar.PixelsPerLine,1);
  ay := ay+(Gantt.Tree.TopRow-1);
  if (ay<List.Count) and (ay>-1) then
    begin
      if IsInRect(X,Y,TInterval(List[ay]).DrawRect) then
        aId := TInterval(List[ay]).Id;
      if Assigned(TInterval(List[ay]).Pointer) then
        begin
          for i := 0 to TRessource(TInterval(List[ay]).Pointer).IntervalCount-1 do
            if not TRessource(TInterval(List[ay]).Pointer).Interval[i].IsDrawRectClear then
              if IsInRect(X,Y,TRessource(TInterval(List[ay]).Pointer).Interval[i].DrawRect)
              and (not (TRessource(TInterval(List[ay]).Pointer).Interval[i] is TBackInterval))
              then
                begin
                  if aIdx = Index then
                    begin
                      Result := TRessource(TInterval(List[ay]).Pointer).Interval[i];
                      break;
                    end;
                  inc(aIdx);
                end;
        end;
    end;
  List.Free;
end;

function TfAttPlan.GetTaskIntervalFromCoordinates(Gantt: TgsGantt; X, Y,
  Index: Integer): TInterval;
var
  List: TList;
  aId : Variant;
  ay: Integer;
  i: Integer;
  aIdx : Integer = 0;
  function IsInRect(aX, aY: Integer; R: TRect): Boolean;
  begin
    Result := (aX >= R.Left) and (aX <= R.Right);
  end;
begin
  Result := nil;
  aId := Null;
  List := TList.Create;
  Gantt.MakeIntervalList(List);
  ay := Y-Gantt.Calendar.StartDrawIntervals;
  ay := ay div max(Gantt.Calendar.PixelsPerLine,1);
  ay := ay+(Gantt.Tree.TopRow-1);
  if (ay<List.Count) and (ay>-1) then
    begin
      if IsInRect(X,Y,TInterval(List[ay]).DrawRect) then
        Result := TInterval(List[ay]);
    end;
  List.Free;
end;

end.

