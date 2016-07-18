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
unit uPrometFrames;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, Forms, uBaseDbInterface, uBaseDbClasses, uExtControls,
  Dialogs, Controls, ExtCtrls,uQuickHelpFrame,LCLProc, ActnList,db,contnrs;
type

  { TPrometMainFrame }

  TPrometMainFrame = class(TExtControlFrame)
    procedure FwindowClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    FLink: string;
    FListAction: TAction;
    FNewAction: TAction;
    FUseTransactions: Boolean;
  protected
    FDataSet: TBaseDBDataSet;
    FConnection: TComponent;
    FHelpPanel : TPanel;
    Fwindow : TForm;
    FQuickHelpFrame: TfQuickHelpFrame;
    procedure SetDataSet(const AValue: TBaseDBDataset);virtual;
    procedure SetConnection(AValue: TComponent);virtual;
    procedure DoCloseFrame(Data : PtrInt);
    procedure DoWindowize(Data : PtrInt);
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure DoOpen;virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;

    property Connection : TComponent read FConnection write SetConnection;
    property DataSet : TBaseDBDataSet read FDataSet write SetDataSet;
    procedure CloseConnection(Ask : Boolean = True);virtual;
    procedure OpenConnection;virtual;
    procedure Save;virtual;
    procedure Abort;virtual;
    function CanHandleLink(aLink : string) : Boolean;virtual;abstract;
    function OpenFromLink(aLink : string) : Boolean;virtual;
    procedure New;virtual;

    function DefineMenuEntry(aParent : Variant;aName : string;aLink : string;aIcon : Integer) : Variant;

    procedure ListFrameAdded(aFrame : TObject);virtual;abstract;//Configure your List/Filter Frame
    function HasListFrame : Boolean;virtual;
    procedure DoSetup;virtual;abstract;

    property Link : string read FLink;
    procedure SetLanguage;virtual;abstract;
    function GetType : string;virtual;abstract;
    procedure CloseFrame;
    procedure Windowize;
    function ShowHint(var HintStr: string;var CanShow: Boolean; var HintInfo: THintInfo) : Boolean;virtual;
    function HasHelp : Boolean;
    procedure ArrangeToolBar(Control: TPanel; ActionList: TActionList; aName: string);
    procedure AddHelp(aWindow : TWinControl);
    property HelpView : TfQuickHelpFrame read FQuickHelpFrame write FQuickHelpFrame;
    property UseTransactions : Boolean read FUseTransactions write FUseTransactions;
  end;
  TPrometMainFrameClass = class of TPrometMainFrame;

implementation
uses ComCtrls, uIntfStrConsts,LCLType,LCLIntf,uWiki,uData,uBaseApplication;

procedure TPrometMainFrame.FwindowClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  with Application as IBaseDbInterface do
    DBConfig.WriteRect('WNDPOS:'+ClassName,Fwindow.BoundsRect);
  CloseAction:=caFree;
end;

procedure TPrometMainFrame.SetConnection(AValue: TComponent);
begin
  if FConnection=AValue then Exit;
  FConnection:=AValue;
end;

procedure TPrometMainFrame.SetDataSet(const AValue: TBaseDBDataset);
begin
  if FDataSet=AValue then exit;
  FDataSet:=AValue;
end;
procedure TPrometMainFrame.DoCloseFrame(Data: PtrInt);
begin
  try
    CloseConnection;
    TExtMenuPageControl(TTabSheet(Data).PageControl).CloseFrameClick(TTabSheet(Data));
  except
  end;
end;

procedure TPrometMainFrame.DoWindowize(Data: PtrInt);
var
  aRect: TRect;
begin
  try
    FWindow := TForm.Create(Application);
    Self.Parent := Fwindow;
    Fwindow.Show;
    Fwindow.OnClose:=@FwindowClose;
    FWindow.Caption:=TTabSheet(Data).Caption;
    with Application as IBaseDbInterface do
      DBConfig.ReadRect('WNDPOS:'+ClassName,aRect,Fwindow.BoundsRect);
    Fwindow.BoundsRect:=aRect;
    TExtMenuPageControl(TTabSheet(Data).PageControl).CloseFrameClick(TTabSheet(Data));
  except
  end;
end;

procedure TPrometMainFrame.DoEnter;
var
  i: Integer;
begin
  inherited DoEnter;
  for i := 0 to ComponentCount-1 do
    if Components[i] is TActionList then
      TActionList(Components[i]).State:=asNormal;
end;

procedure TPrometMainFrame.DoExit;
var
  i: Integer;
begin
  inherited DoExit;
  for i := 0 to ComponentCount-1 do
    if Components[i] is TActionList then
      TActionList(Components[i]).State:=asSuspended;
  if Assigned(Parent) and (Parent is TTabSheet) and (TTabSheet(Parent).Visible) and (TTabSheet(Parent).PageControl is TExtMenuPageControl) and Parent.CanFocus then
    TTabSheet(Parent).PageControl.SetFocus;
end;

procedure TPrometMainFrame.DoOpen;
var
  aHistory: TAccessHistory;
  i: Integer;
begin
  if HasHelp then AddHelp(Self);
  for i := 0 to ComponentCount-1 do
    if Components[i] is TActionList then
      TActionList(Components[i]).State:=asNormal;
end;

constructor TPrometMainFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConnection := nil;
  FQuickHelpFrame:=nil;
  FUseTransactions:=False;
end;
destructor TPrometMainFrame.Destroy;
begin
  CloseConnection;
  if Assigned(FConnection) and (FConnection<>Data.MainConnection) then
    begin
      FreeAndNil(FConnection);
    end;
  if Assigned(FDataSet) then
    begin
      with BaseApplication as IBaseApplication do
        Debug('DataSet Assigned !'+Self.ClassName);
    end;
  inherited Destroy;
end;
procedure TPrometMainFrame.New;
begin
  CloseConnection;
  OpenConnection;
end;

function TPrometMainFrame.HasListFrame: Boolean;
begin
  Result := False;
end;

procedure TPrometMainFrame.CloseConnection(Ask : Boolean = True);
begin
  try
  if (not Assigned(FConnection)) and FUseTransactions then exit;
  if Assigned(DataSet) and DataSet.Changed and DataSet.Active then
    begin
      if Ask and (MessageDlg(strItem+' '+TBaseDBList(DataSet).Text.AsString+' ('+TBaseDbList(DataSet).Number.AsString+')',strItemnotSaved,mtInformation,[mbYes,mbNo],0) = mrYes) then
        begin
          DataSet.CascadicPost;
          with Application as IBaseDbInterface do
            begin
              if FUseTransactions then
                Data.CommitTransaction(FConnection);
            end;
        end
      else
        begin
          DataSet.CascadicCancel;
          with Application as IBaseDbInterface do
            begin
              if FUseTransactions then
                Data.RollbackTransaction(FConnection);
            end;
        end;
    end
  else
    begin
      with Application as IBaseDbInterface do
        if FUseTransactions then
          Data.RollbackTransaction(FConnection);
    end;
  if FUseTransactions then
    with Application as IBaseDbInterface do
      Data.Disconnect(FConnection);
  except
  end;
end;

procedure TPrometMainFrame.OpenConnection;
begin
  if UseTransactions then
    begin
      CloseConnection;
      if not Assigned(FConnection) then
        FConnection := Data.GetNewConnection;
      Data.StartTransaction(FConnection);
    end
  else FConnection := Data.MainConnection;
end;

procedure TPrometMainFrame.Save;
begin
  if Assigned(FConnection) then
    begin
      FDataSet.CascadicPost;
      if UseTransactions then
        begin
          Data.CommitTransaction(FConnection);
          Data.StartTransaction(FConnection);
        end;
    end;
end;

procedure TPrometMainFrame.Abort;
begin
  if Assigned(FConnection) then
    begin
      FDataSet.CascadicCancel;
      if UseTransactions then
        begin
          Data.RollbackTransaction(FConnection);
          Data.StartTransaction(FConnection);
        end;
    end;
end;

function TPrometMainFrame.DefineMenuEntry(aParent: Variant; aName: string;
  aLink: string; aIcon: Integer): Variant;
begin
  with Data.Tree do
    begin
      if not Locate('LINK',aLink,[]) then
        begin
          Append;
          FieldByName('PARENT').AsVariant:=aParent;
          FieldByName('NAME').AsString:=aName;
          FieldByName('LINK').AsString:=aLink;
          FieldByName('ICON').AsInteger:=aIcon;
          FieldByName('TYPE').AsString:='';
          Post;
        end
      else Result := Id.AsVariant;
    end;
end;

function TPrometMainFrame.OpenFromLink(aLink: string): Boolean;
begin
  FLink := aLink;
  OpenConnection;
end;

procedure TPrometMainFrame.CloseFrame;
begin
  if (Parent is TTabSheet) and (TTabSheet(Parent).Visible) and (TTabSheet(Parent).PageControl is TExtMenuPageControl) then
    begin
      if TTabSheet(Parent).PageControl.ActivePage <> TTabSheet(Parent) then exit;
      Application.QueueAsyncCall(@DoCloseFrame,PtrInt(Parent));
    end;
end;

procedure TPrometMainFrame.Windowize;
begin
  if (Parent is TTabSheet) and (TTabSheet(Parent).Visible) and (TTabSheet(Parent).PageControl is TExtMenuPageControl) then
    begin
      if TTabSheet(Parent).PageControl.ActivePage <> TTabSheet(Parent) then exit;
      Application.QueueAsyncCall(@DoWindowize,PtrInt(Parent));
    end;
end;

function TPrometMainFrame.ShowHint(var HintStr: string; var CanShow: Boolean;
  var HintInfo: THintInfo): Boolean;
begin
  Result := False;
end;

function TPrometMainFrame.HasHelp: Boolean;
var
  aWiki: TWikiList;
begin
  with Application as IBaseApplication do
    if not QuickHelp then
      begin
        Result := False;
        exit;
      end;
  with Application as IBaseDbInterface do
    if DBConfig.ReadString('QUICKHELP','YES') = 'NO'  then
      begin
        Result := False;
        exit;
      end;
  aWiki := TWikiList.Create(nil);
  with BaseApplication as IBaseApplication do
    Result := aWiki.FindWikiPage(Appname+'-Help/workflows/'+lowercase(ClassName));
  aWiki.Free;
end;

procedure TPrometMainFrame.ArrangeToolBar(Control: TPanel;
  ActionList: TActionList; aName: string);
begin
  with Application as IBaseDbInterface do
    if DBConfig.ReadBoolean('TBLEFT',True) then
      Control.Align:=alLeft
    else Control.Align:=alRight;
end;

procedure TPrometMainFrame.AddHelp(aWindow: TWinControl);
var
  aWiki: TWikiList;
begin
  if Assigned(FQuickHelpFrame) then exit;
  aWiki := TWikiList.Create(nil);
  with BaseApplication as IBaseApplication do
  if aWiki.FindWikiPage(Appname+'-Help/workflows/'+lowercase(ClassName)) then
    begin
      FQuickHelpFrame := TfQuickHelpFrame.Create(Self);
      if not FQuickHelpFrame.OpenWikiPage(aWiki) then
        FreeAndNil(FQuickHelpFrame)
      else
        begin
          FQuickHelpFrame.Parent:=Self;
          FQuickHelpFrame.Align:=alTop;
          FQuickHelpFrame.BorderSpacing.Around:=8;
        end;
    end;
  aWiki.Free;
end;

end.

