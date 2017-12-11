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
Created 01.07.2006
*******************************************************************************}
unit uWikiFrame;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, DbCtrls, Buttons,
  StdCtrls, ExtCtrls, IpHtml, db, uPrometFrames, uExtControls, Graphics,
  DBGrids, ActnList, Dialogs, Menus, uImageCache, uBaseDbClasses, LCLProc,
  Clipbrd, contnrs,Aspell,uprometscripts,uWikiEditor;
type
  THistory = class(TStringList)
  private
    FFFWdAction: TAction;
    FHIndex : Integer;
    FRewAction: TAction;
    DontAdd : Boolean;
    procedure SetIndex(const AValue: Integer);
  public
    constructor Create;
    function Add(const S: string): Integer; override;
    procedure Clear; override;
    procedure GoBack;
    procedure GoFwd;
    property HistoryIndex : Integer read FHIndex write SetIndex;
    property FwdAction : TAction read FFFWdAction write FFFWdAction;
    property RewAction : TAction read FRewAction write FRewAction;
  end;

  { TfWikiFrame }

  TfWikiFrame = class(TPrometMainFrame)
    acBack: TAction;
    acForward: TAction;
    acIndex: TAction;
    acScreenshot: TAction;
    acImage: TAction;
    acSpellCheck: TAction;
    acExport: TAction;
    acEdit: TAction;
    acRefresh: TAction;
    acCopy: TAction;
    acSave: TAction;
    acCancel: TAction;
    acDelete: TAction;
    ActionList: TActionList;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel4: TBevel;
    Bevel6: TBevel;
    bTransfer: TSpeedButton;
    cbLanguage: TComboBox;
    DBGrid1: TDBGrid;
    eName: TDBEdit;
    ExtRotatedLabel1: TExtRotatedLabel;
    ExtRotatedLabel2: TExtRotatedLabel;
    Keywords: TDatasource;
    Label1: TDBText;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem8: TMenuItem;
    miDelete: TMenuItem;
    Panel9: TPanel;
    pEntry: TPanel;
    Panel7: TPanel;
    pEdit2: TPanel;
    pmAction: TPopupMenu;
    pMiddle: TPanel;
    pLeft: TPanel;
    Panel3: TPanel;
    pmHistory: TPopupMenu;
    pmContext: TPopupMenu;
    pToolbar1: TPanel;
    pEdit1: TPanel;
    SaveDialog1: TSaveDialog;
    RefreshTimer: TTimer;
    sbMenue: TSpeedButton;
    tbMenue1: TToolButton;
    tbToolBar1: TToolBar;
    ToolButton10: TSpeedButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TSpeedButton;
    ToolButton6: TSpeedButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TSpeedButton;
    Wiki: TDatasource;
    ipHTML: TIpHtmlPanel;
    pTop: TPanel;
    pToolbar: TPanel;
    tbMenue: TToolButton;
    tbToolBar: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    procedure acBackExecute(Sender: TObject);
    procedure acCancelExecute(Sender: TObject);
    procedure acCopyExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acEditExecute(Sender: TObject);
    procedure acExportExecute(Sender: TObject);
    procedure acForwardExecute(Sender: TObject);
    procedure acIndexExecute(Sender: TObject);
    procedure acRefreshExecute(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
    procedure aScriptWrite(const s: string);
    procedure aScriptWriteln(const s: string);
    procedure ipHTMLHotClick(Sender: TObject);
    procedure OpenHistoryItemClick(Sender: TObject);
    procedure pmHistoryPopup(Sender: TObject);
    procedure RefreshTimerTimer(Sender: TObject);
    procedure sbMenueClick(Sender: TObject);
    procedure tsViewShow(Sender: TObject);
    procedure WikiDataChange(Sender: TObject; Field: TField);
    procedure WikiStateChange(Sender: TObject);
  private
    { private declarations }
    FHistory : THistory;
    lastRefresh: TDateTime;
    FEditable: Boolean;
    FVariables: TStrings;
    aDataThere : Boolean;
    FScriptContent : string;
    FEditor : TfWikiEditor;
    procedure FEditorChange(Sender: TObject);
    function GetLeftBar: Boolean;
    procedure SetLeftBar(AValue: Boolean);
    procedure AddDocuments(Sender: TObject);
    procedure DoView;
    procedure DoEdit;
    procedure DoOpen;
  protected
    procedure DoEnter; override;
    procedure DoExit; override;
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;

    function CanHandleLink(aLink : string): Boolean; override;
    function OpenFromLink(aLink : string) : Boolean;override;
    procedure New;override;

    procedure SetLanguage;override;
    procedure SetRights(Editable : Boolean);
    function OpenWikiPage(PageName : string;CreateIfNotExists : Boolean = False) : Boolean;
    procedure Refresh;
    procedure ShowFrame;override;
    procedure DoRefresh(ForceRefresh : Boolean = False); override;
    property Variables : TStrings read FVariables;
    property LeftBar : Boolean read GetLeftBar write SetLeftBar;
  end;
implementation
uses uWiki,uData,WikiToHTML,uDocuments,Utils,LCLIntf,Variants,
  uBaseDbInterface,uscreenshotmain,uMessages,uDocumentFrame,sqlparser,
  sqlscanner, sqltree,uBaseVisualApplication,uStatistic,uspelling,uBaseApplication,
  uBaseVisualControls,uRTFtoTXT,uIntfStrConsts,uprometpascalscript,LCLType;
var
  ClipbrdFmtHTML:TClipboardFormat;
procedure THistory.SetIndex(const AValue: Integer);
begin
  Move(AValue,Count-1);
  FHIndex := Count-1;
  FRewAction.Enabled := FHIndex > 0;
  FFFWdAction.Enabled := FHIndex < Count-1;
  FRewAction.Enabled := FHIndex > 0;
  FFFWdAction.Enabled := True;
  DontAdd := True;
  Data.GotoLink(Strings[FHIndex]);
  DontAdd := False;
end;
constructor THistory.Create;
begin
  DontAdd := False;
end;
function THistory.Add(const S: string): Integer;
begin
  if S = '' then exit;
  if DontAdd then exit;
  if Count > 0 then
    if s = Strings[Count-1] then exit;
  while FHIndex<Count-1 do
    Delete(Count-1);
  Result:=inherited Add(S);
  FRewAction.Enabled := True;
  FFFWdAction.Enabled := False;
  FHIndex := Result;
end;
procedure THistory.Clear;
begin
  inherited Clear;
  FRewAction.Enabled := false;
  FFFWdAction.Enabled := false;
  FHIndex := -1;
end;
procedure THistory.GoBack;
begin
  if FHIndex = 0 then exit;
  dec(FHIndex);
  FRewAction.Enabled := FHIndex > 0;
  FFFWdAction.Enabled := FHIndex < Count-1;
  FRewAction.Enabled := FHIndex > 0;
  FFFWdAction.Enabled := True;
  DontAdd := True;
  Data.GotoLink(Strings[FHIndex]);
  DontAdd := False;
end;
procedure THistory.GoFwd;
begin
  if FHIndex >= Count-1 then exit;
  inc(FHIndex);
  FRewAction.Enabled := FHIndex > 0;
  FFFWdAction.Enabled := FHIndex < Count-1;
  FRewAction.Enabled := True;
  FFFWdAction.Enabled := FHIndex < Count-1;
  DontAdd := True;
  Data.GotoLink(Strings[FHIndex]);
  DontAdd := False;
end;
constructor TfWikiFrame.Create(AOwner: TComponent);
var
  aHTML: TSimpleIpHtml;
  ss: TStringStream;
begin
  inherited Create(AOwner);
  FEditor := TfWikiEditor.Create(Self);
  ipHTML.SetHtml(FEditor.GetHTML(strWikiLoadingPage));
  FEditor.Parent:=pMiddle;
  FEditor.Align:=alClient;
  FEditor.BorderStyle:=bsNone;
  FEditor.OnChange:=@FEditorChange;
  FVariables := TStringList.Create;
  FVariables.Values['USER'] := Data.Users.Id.AsString;
  FVariables.Values['ACCOUNTNO'] := Data.Users.FieldByName('ACCOUNTNO').AsString;
  FVariables.Values['IDCODE'] := Data.Users.FieldByName('IDCODE').AsString;
  FVariables.Values['GROUPID'] := Data.Users.FieldByName('PARENT').AsString;
  FEditable:=False;
  DataSet := TWikiList.CreateEx(Self,Data);
  FHistory := THistory.Create;
  FHistory.FFFWdAction := acForward;
  FHistory.RewAction := acBack;
  Wiki.DataSet := DataSet.DataSet;
  Keywords.DataSet := TWikiList(DataSet).Keywords.DataSet;
  {$ifdef DARWIN}
  ipHTML.DefaultFontSize:=14;
  {$endif}
end;
destructor TfWikiFrame.Destroy;
begin
  FHistory.Destroy;
  FVariables.Free;
  try
    DataSet.Destroy;
    DataSet := nil;
  except
  end;
  try
  inherited Destroy;
  except
  end;
end;

function TfWikiFrame.CanHandleLink(aLink: string): Boolean;
begin
  Result := (copy(aLink,0,4) = 'WIKI');
end;

function TfWikiFrame.OpenFromLink(aLink: string) : Boolean;
begin
  if not CanHandleLink(aLink) then exit;
  if rpos('{',aLink) > 0 then
    aLink := copy(aLink,0,rpos('{',aLink)-1)
  else if rpos('(',aLink) > 0 then
    aLink := copy(aLink,0,rpos('(',aLink)-1);
  if pos('.ID',aLink)>0 then
    begin
      DataSet.Select(copy(aLink, pos('@', aLink) + 1, length(aLink)));
      DataSet.Open;
      Result := DataSet.Count>0;
      if Result then DoOpen;
    end
  else
    Result := OpenWikiPage(copy(aLink, pos('@', aLink) + 1, length(aLink)),Data.Users.Rights.Right('WIKI')>RIGHT_READ);
end;
procedure TfWikiFrame.tsViewShow(Sender: TObject);
begin
  Refresh;
end;

procedure TfWikiFrame.WikiDataChange(Sender: TObject; Field: TField);
begin
  DataSet.Change;
end;

procedure TfWikiFrame.WikiStateChange(Sender: TObject);
begin
  if DataSet.DataSet.State = dsInsert then
    DoEdit;
  acSave.Enabled := DataSet.CanEdit;
  acCancel.Enabled:= DataSet.CanEdit;
end;

procedure TfWikiFrame.ipHTMLHotClick(Sender: TObject);
var
  PageName: String;
  aParent : Integer = TREE_ID_WIKI_UNSORTED;
  ID: Integer;
  i: Integer;
  aLink: String;
begin
  if Assigned(IpHtml.HotNode) and (ipHTML.HotNode is TIpHtmlNodeA) then
    begin
      aLink := StringReplace(TIpHtmlNodeA(IpHtml.HotNode).HRef,'\','/',[rfReplaceAll]);//Ipro changes Links from / to \ on windows automatically
      PageName := SysToUni(StringReplace(aLink,' ','_',[rfReplaceAll]));
      for i := 0 to FVariables.Count-1 do
        pageName := StringReplace(PageName,'@VARIABLES.'+FVariables.Names[i]+'@',FVariables.ValueFromIndex[i],[rfReplaceAll,rfIgnoreCase]);
      if OpenWikiPage(PageName) or OpenWikiPage(lowercase(PageName)) then
      else if (pos('@',PageName)>0) and Data.GotoLink(PageName) then
        begin
        end
      else if ((Pos('://', aLink) > 0) or (pos('www',lowercase(aLink)) > 0)) then
        OpenURL(aLink)
      else if (pos('@',PageName) = 0) and FEditable then
        begin
          OpenWikiPage(PageName,True);
        end;
    end;
end;
procedure TfWikiFrame.OpenHistoryItemClick(Sender: TObject);
begin
  FHistory.HistoryIndex:=TMenuItem(Sender).Tag;
end;
procedure TfWikiFrame.pmHistoryPopup(Sender: TObject);
var
  aItem: TMenuItem;
  i: Integer;
begin
  pmHistory.Items.Clear;
  for i := FHistory.Count-1 downto 0 do
    begin
      aItem := TMenuItem.Create(pmHistory);
      aItem.Caption:=Data.GetLinkDesc(FHistory[i]);
      aItem.OnClick:=@OpenHistoryItemClick;
      if i = FHistory.HistoryIndex then
        aItem.Default := True;
      aItem.Tag := i;
      pmHistory.Items.Add(aItem);
    end;
end;

procedure TfWikiFrame.RefreshTimerTimer(Sender: TObject);
begin
  RefreshTimer.Enabled:=False;
  try
    Refresh;
    lastRefresh := Now();
  except
  end;
end;

procedure TfWikiFrame.sbMenueClick(Sender: TObject);
begin
  TSpeedButton(Sender).PopupMenu.PopUp(TSpeedButton(Sender).ClientOrigin.x,TSpeedButton(Sender).ClientOrigin.y+TSpeedButton(Sender).Height);
end;

procedure TfWikiFrame.acBackExecute(Sender: TObject);
begin
  FHistory.GoBack;
end;

procedure TfWikiFrame.acCancelExecute(Sender: TObject);
begin
  FDataSet.CascadicCancel;
  if Assigned(FConnection) then
    begin
      if UseTransactions then
        begin
          Data.RollbackTransaction(FConnection);
          Data.StartTransaction(FConnection);
        end;
    end;
end;

procedure TfWikiFrame.acCopyExecute(Sender: TObject);
var
  HTMLSource: String;
begin
  ipHTML.CopyToClipboard;
  //HTMLSource := '<b>Formatted</b> text'; // text with formatrings
  //Clipboard.AddFormat(ClipbrdFmtHTML, HTMLSource[1], Length(HTMLSource));
end;

procedure TfWikiFrame.acDeleteExecute(Sender: TObject);
begin
  DataSet.Delete;
end;

procedure TfWikiFrame.acEditExecute(Sender: TObject);
begin
  if acEdit.Checked then
    DoEdit
  else DoView;
end;

procedure TfWikiFrame.acExportExecute(Sender: TObject);
var
  aFN: String;
begin
  if SaveDialog1.Execute then
    begin
      aFN := SaveDialog1.FileName;
      if lowercase(ExtractFileExt(aFN))<>'.html' then
        aFN := aFN+'.html';
      TWikiList(DataSet).ExportToHTML(aFN,OnWikiInclude);
    end;
end;

procedure TfWikiFrame.acForwardExecute(Sender: TObject);
begin
  FHistory.GoFwd;
end;
procedure TfWikiFrame.acIndexExecute(Sender: TObject);
begin
  OpenWikiPage('INDEX');
end;

procedure TfWikiFrame.acRefreshExecute(Sender: TObject);
begin
  Refresh;
end;

procedure TfWikiFrame.acSaveExecute(Sender: TObject);
begin
  FDataSet.CascadicPost;
  if Assigned(FConnection) then
    begin
      if UseTransactions then
        begin
          Data.CommitTransaction(FConnection);
          Data.StartTransaction(FConnection);
        end;
    end;
end;

procedure TfWikiFrame.aScriptWrite(const s: string);
begin
  FScriptContent+=s;
end;

procedure TfWikiFrame.aScriptWriteln(const s: string);
begin
  FScriptContent+=s+LineEnding;
end;

type
  TPlainProcedure = procedure;

procedure TfWikiFrame.SetLeftBar(AValue: Boolean);
begin
  if not Assigned(pLeft) then exit;
  pLeft.Visible:=aValue;
  pTop.Visible:=not AValue;
end;

function TfWikiFrame.GetLeftBar: Boolean;
begin
  Result := pLeft.Visible;
end;

procedure TfWikiFrame.FEditorChange(Sender: TObject);
begin
  if FEditor.Changed then
    begin
      DataSet.Edit;
      DataSet.FieldByName('DATA').AsString:=FEditor.Text;
    end;
end;

procedure TfWikiFrame.AddDocuments(Sender: TObject);
var
  aDocuments: TDocuments;
begin
  if not Assigned(TfDocumentFrame(Sender).DataSet) then
    begin
      aDocuments := TDocuments.CreateEx(Self,Data);
      TfDocumentFrame(Sender).DataSet := aDocuments;
      TfDocumentFrame(Sender).Refresh(DataSet.Id.AsVariant,'W',DataSet.FieldByName('NAME').AsString,Null,Null,0);
    end;
end;

procedure TfWikiFrame.DoView;
begin
  eName.Enabled:=False;
  if FEditor.Changed and (FEditor.Id=DataSet.Id.AsVariant) then
    begin
      DataSet.Edit;
      DataSet.FieldByName('DATA').AsString:=FEditor.Text;
    end;
  acEdit.Checked:=False;
  ipHTML.Visible:= True;
  FEditor.Visible := False;
  Refresh;
end;

procedure TfWikiFrame.DoEdit;
var
  tmp: String;
begin
  ipHTML.Visible:= False;
  FEditor.Visible := True;
  eName.Enabled:=True;
  tmp := DataSet.FieldByName('DATA').AsString;
  FEditor.Open(tmp,DataSet.Id.AsVariant,'W',DataSet.FieldByName('NAME').AsString);
  acEdit.Checked:=True;
  pEntry.Visible:=True;
end;

procedure TfWikiFrame.DoOpen;
begin
  TWikiList(DataSet).Variables.Assign(FVariables);
  ipHTML.SetHtml(FEditor.GetHTML(TWikiList(DataSet).PageAsHtml(True,False)));
end;

procedure TfWikiFrame.DoEnter;
begin
end;

procedure TfWikiFrame.DoExit;
begin
end;

procedure TfWikiFrame.New;
begin
end;
procedure TfWikiFrame.SetLanguage;
begin
end;
procedure TfWikiFrame.SetRights(Editable : Boolean);
begin
  FEditable := Editable;
  pEdit1.Visible:=Editable;
  pEdit2.Visible:=Editable;
end;

function TfWikiFrame.OpenWikiPage(PageName: string;CreateIfNotExists : Boolean = False) : Boolean;
var
  aParent : Variant;
  aPageIndex: Integer;
  aDocPage: TTabSheet;
  aWiki: TWikiList;
begin
  PageName:=HTMLDecode(SysToUni(PageName));
  with BaseApplication as IBaseApplication do
    Debug('OpenWikiPage:'+PageName);
  aWiki := TWikiList.Create(nil);
  Result := aWiki.FindWikiPage(pageName);
  aWiki.Free;
  DoView;
  if Result then
    begin
      TWikiList(DataSet).FindWikiPage(PageName,True);
      aParent := TWikiList(DataSet).ActiveTreeID;
      Screen.Cursor := crHourglass;
      DoOpen;
      Screen.Cursor := crDefault;
      FHistory.Add(Data.BuildLink(DataSet.DataSet));
    end
  else if CreateIfNotExists then
    begin
      TWikiList(DataSet).FindWikiPage(PageName,True);
      aParent := TWikiList(DataSet).ActiveTreeID;
      DataSet.Insert;
      DataSet.FieldByName('NAME').AsString := copy(Utils.HTMLDecode(PageName),rpos('/',Utils.HTMLDecode(PageName))+1,length(Utils.HTMLDecode(PageName)));
      DataSet.FieldByName('TREEENTRY').AsVariant := aParent;
      DoEdit;
   end;
  with BaseApplication as IBaseApplication do
    Debug('OpenWikiPage:'+PageName+' -> done');
end;

procedure TfWikiFrame.Refresh;
var
  aHTML: TIpHtml;
  aCanvas: TBitmap;
begin
  if (not Assigned(DataSet)) or (not DataSet.DataSet.Active) then exit;
  TWikiList(dataSet).Variables.Assign(FVariables);
  aHTML := FEditor.GetHTML(TWikiList(dataSet).PageAsHtml(True,False));
  ipHTML.SetHtml(aHTML);
  aCanvas := TBitmap.Create;
  acanvas.Width:=200;
  acanvas.Height:=200;
  aHTML.Render(aCanvas.Canvas,Rect(0,0,200,200),false,Point(0,0));
  aCanvas.Free;
end;

procedure TfWikiFrame.ShowFrame;
begin
  inherited ShowFrame;
  DoRefresh;
end;

procedure TfWikiFrame.DoRefresh(ForceRefresh: Boolean);
begin
  inherited DoRefresh(ForceRefresh);
  if Now()-LastRefresh>60*1000 then
    if TWikiList(DataSet).isDynamic then
      RefreshTimer.Enabled:=True;
end;

initialization
//  TBaseVisualApplication(Application).RegisterForm(TfWikiFrame);
  ClipbrdFmtHTML := RegisterClipboardFormat('text/html');

{$R *.lfm}
end.

