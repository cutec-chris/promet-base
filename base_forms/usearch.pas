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
unit uSearch;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils,  Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, Buttons, Grids, uIntfStrConsts, ExtCtrls, CheckLst, db,
  LMessages, Spin, LCLtype, ActnList, Menus, LCLIntf, Utils, FPCanvas,
  CustomTimer, Themes, uBaseSearch, uBaseDbClasses, Clipbrd;
type
  TOpenItemEvent = function(aLink : string) : Boolean of Object;
  THackCustomDrawGrid = class(TCustomDrawGrid);

  { TfSearch }

  TfSearch = class(TForm)
    acOpen: TAction;
    acCopyLink: TAction;
    acSaveToLink: TAction;
    acSearchContained: TAction;
    acInformwithexternMail: TAction;
    acInformwithinternMail: TAction;
    ActionList1: TActionList;
    bClose: TBitBtn;
    bEditFilter: TSpeedButton;
    Bevel1: TBevel;
    bSearch: TBitBtn;
    bOpen: TBitBtn;
    bSearchFurther: TButton;
    cbMaxResults: TCheckBox;
    cbSearchType: TCheckListBox;
    IdleTimer: TTimer;
    lHint: TLabel;
    cbSearchIn: TCheckListBox;
    cbAutomaticsearch: TCheckBox;
    eContains: TEdit;
    Image2: TImage;
    LinkSaveDialog: TSaveDialog;
    lSearchtype: TLabel;
    lResults: TLabel;
    lSearchIn: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    Panel1: TPanel;
    Panel5: TPanel;
    pTop: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    cbContains: TRadioButton;
    cbWildgards: TRadioButton;
    Panel4: TPanel;
    PopupMenu1: TPopupMenu;
    seMaxresults: TSpinEdit;
    sgResults: TStringGrid;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    procedure acCopyLinkExecute(Sender: TObject);
    procedure acInformwithexternMailExecute(Sender: TObject);
    procedure acOpenExecute(Sender: TObject);
    procedure acSaveToLinkExecute(Sender: TObject);
    procedure acSearchContainedExecute(Sender: TObject);
    procedure ActiveSearchBeginItemSearch(Sender: TObject);
    procedure ActiveSearchEndItemSearch(Sender: TObject);
    procedure bCloseClick(Sender: TObject);
    procedure bEditFilterClick(Sender: TObject);
    procedure bSearchFurtherClick(Sender: TObject);
    procedure cbSearchInClickCheck(Sender: TObject);
    procedure DoSearch(Sender: TObject);
    procedure cbAutomaticsearchChange(Sender: TObject);
    procedure cbWildgardsChange(Sender: TObject);
    procedure DataSearchresultItem(aIdent: string; aName: string;
      aStatus: string;aActive : Boolean; aLink: string;aPrio : Integer;aItem : TBaseDBList = nil);
    procedure eContainsChange(Sender: TObject);
    procedure eContainsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FastSearchEnd(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormHide(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    function fSearchOpenUserItem(aLink: string): Boolean;
    procedure IdleTimerTimer(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure seMaxresultsChange(Sender: TObject);
    procedure sgResultsDblClick(Sender: TObject);
    procedure sgResultsDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure sgResultsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure sgResultsMouseLeave(Sender: TObject);
    procedure sgResultsMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure sgResultsResize(Sender: TObject);
    procedure sgResultsStartDrag(Sender: TObject; var DragObject: TDragObject);
  private
    Fdone: TNotifyEvent;
    FItemFound: TSearchResultItem;
    FSearcheMail : string;
    FOpenItem: TOpenItemEvent;
    FValidItem: TOpenItemEvent;
    { private declarations }
    SearchText: String;
    HintY: LongInt;
    FModal : Boolean;
    FLastSearch : string;
    FOptionSet : string;
    ActCount: Integer;
    procedure WMCloseQuery(var message: TLMessage); message LM_CLOSEQUERY;
  public
    { public declarations }
    ActiveSearch : TSearch;
    SearchLevel: Integer;
    SearchTypes : TFullTextSearchTypes;
    SearchLocations : TSearchLocations;
    function Execute(Modal : Boolean;OptionSet : string;aHint : string) : Boolean;
    procedure SetUpSearch;
    function ShowHint(var HintStr: string;var CanShow: Boolean; var HintInfo: THintInfo) : Boolean;
    procedure SetLanguage;
    procedure LoadOptions(OptionSet : string);
    procedure SaveOptions;
    function GetLink(Multi : Boolean = False) : string;
    property OnOpenItem : TOpenItemEvent read FOpenItem write FOpenItem;
    property OnValidateItem : TOpenItemEvent read FValidItem write FValidItem;
    property OnItemFound : TSearchResultItem read FItemFound write FItemFound;
    property OnSearchDone : TNotifyEvent read Fdone write Fdone;
    procedure AllowSearchTypes(aTypes : string);
  end;

  { TSearchHintWindow }

  TSearchHintWindow = class(THintWindow)
  private
    FActivating: Boolean;
    function GetDrawTextFlags: Cardinal;
  public
    //procedure ActivateHint(const AHint: String); override;
    procedure ActivateHint(ARect: TRect; const AHint: string); override;
    procedure Paint; override;
  published
    property Caption;
  end;
var
  fSearch: TfSearch;
implementation
{$R *.lfm}
uses uBaseDBInterface,uBaseApplication,uBaseVisualControls,uFormAnimate,
  uBaseVisualApplication,uData,uBaseERPDBClasses,uMasterdata,LCLProc,uSendMail,
  uPerson,variants
  ;
resourcestring
  strSearchfromOrderMode        = 'Diese Suche wurde aus der Vorgangsverwaltung gestartet, wenn Sie einen Eintrag öffnen wird dieser automatisch in den aktuellen Vorgang übernommen.';
  strDoSearch                   = 'suchen';
procedure TfSearch.bCloseClick(Sender: TObject);
begin
  Hide;
  Close;
end;
procedure TfSearch.bEditFilterClick(Sender: TObject);
var
  Animate: TAnimationController;
begin
  bEditFilter.Enabled:=False;
  Animate := TAnimationController.Create(pTop);
  Application.ProcessMessages;
  if bEditFilter.Down then
    Animate.AnimateControlHeight(111)
  else
    Animate.AnimateControlHeight(55);
  bEditFilter.Enabled:=True;
  Animate.Free;
end;

procedure TfSearch.bSearchFurtherClick(Sender: TObject);
begin
  bSearch.Click;
end;

procedure TfSearch.cbSearchInClickCheck(Sender: TObject);
begin
  SearchLevel:=0;
  SetUpSearch;
  sgResults.RowCount:=sgResults.FixedRows;
  if (eContains.Text<>'') and (cbSearchType.Tag=0) then
    DoSearch(nil);
end;

procedure TfSearch.acOpenExecute(Sender: TObject);
var
  aSearchHist: TSearchHistory;
begin
  SearchText := '';
  if Assigned(ActiveSearch) and ActiveSearch.Active then
    begin
      ActiveSearch.Abort;
      while ActiveSearch.Active do Application.ProcessMessages;
    end;
  if (trim(eContains.Text)<>'') then
    begin
      aSearchHist := TSearchHistory.Create(nil);
      aSearchHist.Add(eContains.Text,GetLink);
      aSearchHist.Free;
    end;
 if Assigned(FOpenItem) then
   begin
     if FModal then
       ModalResult := mrOK
     else Close;
     FOpenItem(GetLink);
     exit;
   end;
  with Application as IBaseDBInterface do
    begin
      if Data.GotoLink(GetLink) then
        begin
          if FModal then
            ModalResult := mrOK
          else
            Close;
        end;
    end;
end;

procedure TfSearch.acSaveToLinkExecute(Sender: TObject);
var
  sl: TStringList;
  tmp: String;
begin
 if LinkSaveDialog.Execute then
   begin
     sl := TStringList.Create;
     sl.Add(GetLink);
     tmp := LinkSaveDialog.FileName;
     if lowercase(ExtractFileExt(tmp)) <> '.plink' then
       tmp := tmp+'.plink';
     sl.SaveToFile(tmp);
     sl.Free;
   end;
end;

procedure TfSearch.acSearchContainedExecute(Sender: TObject);
var
  aPos: TBaseDBPosition;
  aMS: TMasterdata;
  aActive: Boolean;
begin
  aMS := TMasterdata.Create(nil);
  aMS.SelectFromLink(GetLink());
  aMS.Open;
  if aMS.Count>0 then
    begin
      sgResults.RowCount:=sgResults.FixedRows;
      aPos := TMDPos.Create(nil);
      Data.SetFilter(aPos,Data.QuoteField('IDENT')+'='+Data.QuoteValue(aMS.Number.AsString));
      with aPos do
        begin
          First;
          while not EOF do
            begin
              aMS := TMasterdata.Create(nil);
              aMS.Select(aPos.FieldByName('REF_ID').AsVariant);
              aMS.Open;
              if aMS.Count>0 then
                begin
                  aActive := Data.States.DataSet.Locate('STATUS',aMS.Status.AsString,[loCaseInsensitive]);
                  if aActive then
                    aActive := aActive and (Data.States.FieldByName('ACTIVE').AsString='Y');
                  DataSearchresultItem(aMS.FieldByName('ID').AsString,aMS.FieldByName('SHORTTEXT').AsString,aMs.Status.AsString,aActive,Data.BuildLink(aMS.DataSet),0,aMS);
                end;
              Next;
            end;
        end;
      aPos.Free;
    end;
  aMS.Free;
end;

procedure TfSearch.ActiveSearchBeginItemSearch(Sender: TObject);
begin
  sgResults.BeginUpdate;
end;

procedure TfSearch.ActiveSearchEndItemSearch(Sender: TObject);
begin
  sgResults.EndUpdate;
end;
procedure TfSearch.acCopyLinkExecute(Sender: TObject);
var
  Stream: TStringStream;
begin
  if sgResults.RowCount <= 1 then exit;
  Stream := TStringStream.Create(GetLink(True));
  Clipboard.AddFormat(LinkClipboardFormat,Stream);
  Stream.Free;
end;

procedure TfSearch.acInformwithexternMailExecute(Sender: TObject);
var
  i: Integer;
  aLink: String;
  aFile: String;
  sl: TStringList;
begin
  fSearch.SetLanguage;
  AllowSearchTypes(strUsers+','+strCustomers);
  fSearch.eContains.Clear;
  fSearch.sgResults.RowCount:=1;
  fSearch.OnOpenItem:=@fSearchOpenUserItem;
  FSearcheMail:='';
  fSearch.Execute(True,'LISTU',strSearchFromMailSelect);
  fSearch.SetLanguage;
  aLink := GetLink;
  if (aLink<>'') then
    begin
      with BaseApplication as IBaseApplication do
        aFile := GetInternalTempDir+ValidateFileName(Data.GetLinkDesc(aLink))+'.plink';
      sl := TStringList.Create;
      sl.Add(aLink);
      sl.SaveToFile(aFile);
      sl.Free;
      DoSendMail(Data.GetLinkDesc(aLink),Data.GetLinkLongDesc(aLink), aFile,'','','',FSearcheMail);
    end;
end;

procedure TfSearch.DoSearch(Sender: TObject);
begin
  if (bSearch.Caption = strAbort) then
    begin
      SearchLevel := 0;
      if Sender = nil then exit;
      SearchText := '';
      if Assigned(ActiveSearch) then
        ActiveSearch.Abort;
      bSearch.Caption := strDoSearch;
      bSearchFurther.Visible:=bSearch.Caption=strContinueSearch;
      bOpen.Enabled:=True;
      {$IFDEF MAINAPP}
      if (fsSerial in SearchTypes) then
        fOrders.acViewList.Execute;
      {$ENDIF}
      exit;
    end;
  bOpen.Enabled:=False;
  bSearch.Caption := strAbort;
  bSearchFurther.Visible:=bSearch.Caption=strContinueSearch;
  SetUpSearch;
  if not ActiveSearch.Start(eContains.Text,SearchLevel) then
    begin
      bSearch.Caption:=strDoSearch;
      bSearchFurther.Visible:=bSearch.Caption=strContinueSearch;
      SearchLevel := 0;
      FreeAndNil(ActiveSearch);
    end;
end;
procedure TfSearch.cbAutomaticsearchChange(Sender: TObject);
begin
//  bSearch.Visible:=not cbAutomaticsearch.Checked;
  with Application as IBaseDBInterface do
    begin
      if cbAutomaticsearch.Checked then
        DBConfig.WriteString('SEARCHWHILETYPING','YES')
      else
        begin
          DBConfig.WriteString('SEARCHWHILETYPING','NO');
          bSearch.Default:=True;
          bOpen.Default:=True;
        end;
    end;
end;
procedure TfSearch.cbWildgardsChange(Sender: TObject);
begin
  with Application as IBaseDBInterface do
    begin
      if cbWildgards.Checked then
        DBConfig.WriteString('WILDGARDSEARCH','YES')
      else
        DBConfig.WriteString('WILDGARDSEARCH','NO');
      if cbWildgards.Checked then
        cbAutomaticSearch.Checked:=False;
    end;
end;
procedure TfSearch.DataSearchresultItem(aIdent: string; aName: string;
  aStatus: string; aActive: Boolean; aLink: string; aPrio: Integer;
  aItem: TBaseDBList);
var
  i: Integer;
  aRec: String;
  gSel: TGridRect;
begin
  if Assigned(FValidItem) then
    if not FValidItem(aLink) then exit;
  for i := 1 to sgResults.RowCount-1 do
    if sgResults.Cells[4,i] = aLink then
      exit;
  if aActive then
    begin
      for i := 0 to sgResults.RowCount-1 do
        if sgResults.Cells[5,i] = 'N' then
          break;
    end
  else
    i := sgResults.RowCount;
  i := i+1;
  if i<1 then i := 1;
  if i>sgResults.RowCount then
    i := sgResults.RowCount;
  sgResults.InsertColRow(False,i);
  sgResults.Cells[1,i] := aIdent;
  sgResults.Cells[2,i] := aName;
  sgResults.Cells[3,i] := aStatus;
  sgResults.Cells[4,i] := aLink;
  if aActive then
    sgResults.Cells[5,i] := 'Y'
  else
    sgResults.Cells[5,i] := 'N';
  sgResults.Cells[6,i] := IntToStr(aPrio);
  if Assigned(FItemFound) then
    FItemFound(aIdent,aName,aStatus,aActive,aLink,aPrio,aItem);
end;
procedure TfSearch.eContainsChange(Sender: TObject);
begin
  IdleTimer.Enabled:=False;
  if Assigned(ActiveSearch) then
    begin
      ActiveSearch.Abort;
      if bSearch.Caption=strAbort then
        bSearch.Click;
    end;
  IdleTimer.Enabled:=True;
end;
procedure TfSearch.eContainsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
  VK_PRIOR,
  VK_NEXT,
  VK_UP,
  VK_DOWN:
    begin
      THackCustomDrawGrid(sgResults).KeyDown(Key,Shift);
      Key := 0;
    end;
  VK_RETURN:
    begin
      if bSearch.Visible then exit;
      THackCustomDrawGrid(sgResults).KeyDown(Key,Shift);
      Key := 0;
    end;
  end;
end;

procedure TfSearch.FastSearchEnd(Sender: TObject);
begin
  inc(SearchLevel);
  acOpen.Enabled:=True;
  Application.ProcessMessages;
  bOpen.Enabled:=True;
  bSearch.Caption:=strContinueSearch;
  bSearchFurther.Visible:=bSearch.Caption=strContinueSearch;
  if sgResults.RowCount > ActCount then
    begin
      bSearch.Default:=False;
      bOpen.Default:=True;
    end
  else DoSearch(bSearch);
  if Assigned(Fdone) then
    Fdone(Self);
end;

procedure TfSearch.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Hide;
  FreeAndNil(ActiveSearch);
  SaveOptions;
  CloseAction:=caHide;
end;

procedure TfSearch.FormHide(Sender: TObject);
begin
  IdleTimer.Enabled:=False;
  Application.CancelHint;
end;
procedure TfSearch.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    begin
      Key := 0;
      Close;
    end;
end;
procedure TfSearch.FormShow(Sender: TObject);
begin
  with Application as IBaseDBInterface do
    begin
      cbAutomaticSearch.Checked:=DBConfig.ReadString('SEARCHWHILETYPING','YES') <> 'NO';
      cbAutomaticSearch.OnChange(Self);
      cbWildgards.Checked:=DBConfig.ReadString('WILDGARDSEARCH','NO') = 'YES';
      eContains.SetFocus;
      eContains.SelectAll;
    end;
end;

function TfSearch.fSearchOpenUserItem(aLink: string): Boolean;
var
  aUser: TUser;
  aCont: TPerson;
  aFile: String;
  sl: TStringList;
begin
  if pos('USERS',aLink)>0 then
    begin
      aUser := TUser.Create(nil);
      aUser.SelectFromLink(aLink);
      aUser.Open;
      if aUser.Count>0 then
        begin
          FSearcheMail := trim(aUser.FieldByName('EMAIL').AsString);
        end;
      aUser.Free;
    end
  else
    begin
      aCont := TPerson.Create(nil);
      aCont.SelectFromLink(aLink);
      aCont.Open;
      if aCont.Count>0 then
        begin
          aCont.ContactData.Open;
          if aCont.ContactData.Locate('TYPE;ACTIVE',VarArrayOf(['EM','Y']),[loPartialKey]) then
            FSearcheMail := aCont.ContactData.FieldByName('DATA').AsString;
        end;
      aCont.Free;
    end;
end;

procedure TfSearch.IdleTimerTimer(Sender: TObject);
begin
  if eContains.Text = '' then exit;
  SearchLevel:=0;
  FreeAndNil(ActiveSearch);
  if FLastSearch = eContains.Text then IdleTimer.Enabled:=False;
  if cbAutomaticSearch.Checked then
    begin
      SearchText := '';
      if Assigned(ActiveSearch) then
        ActiveSearch.Abort;
      FLastSearch := eContains.Text;
      DoSearch(nil);
    end;
end;

procedure TfSearch.PopupMenu1Popup(Sender: TObject);
begin
  acSearchContained.Enabled:=copy(GetLink(),0,10)='MASTERDATA';
end;

procedure TfSearch.seMaxresultsChange(Sender: TObject);
begin
  FreeAndNil(ActiveSearch);
  with Application as IBaseDBInterface do
    begin
      if cbMaxresults.Checked then
        DBConfig.WriteInteger('SEARCHMAXRESULTS',seMaxResults.Value)
      else
        DBConfig.WriteString('SEARCHMAXRESULTS','100');
    end;
  if seMaxresults.Tag=0 then
    DoSearch(nil);
end;
procedure TfSearch.sgResultsDblClick(Sender: TObject);
begin
  acOpen.Execute;
end;
procedure TfSearch.sgResultsDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
begin
  if aRow < 1 then
    sgResults.DefaultDrawCell(aCol,aRow,aRect,aState)
  else if aCol = 0 then
    begin
      sgResults.Canvas.Brush.Color := clWindow;
      sgResults.Canvas.Brush.Style:=bsSolid;
      sgResults.Canvas.FillRect(aRect);
      with Application as IBaseDBInterface do
        fVisualControls.Images.Draw(sgResults.Canvas,aRect.Left+2,aRect.Top+2,Data.GetLinkIcon(sgResults.Cells[4,aRow]),sgResults.Cells[5,aRow]='Y');
    end;
end;
procedure TfSearch.sgResultsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  {
  IF (Key = VK_RETURN) then
    begin
      acOpen.Execute;
      Key := 0;
    end;}
end;
procedure TfSearch.sgResultsMouseLeave(Sender: TObject);
begin
  Application.CancelHint;
end;
procedure TfSearch.sgResultsMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if sgResults.MouseToCell(Mouse.CursorPos).y <> HintY then
    begin
      Application.CancelHint;
      sgResults.ShowHint:=False;
      HintY := sgResults.MouseToCell(Mouse.CursorPos).y;
      with Application as IBaseDBInterface do
        sgResults.Hint:=Data.GetLinkDesc(sgResults.Cells[4,HintY]);
      sgResults.ShowHint:=True;
    end;
end;
procedure TfSearch.sgResultsResize(Sender: TObject);
begin
  sgResults.Columns[2].Width := sgResults.Width-(sgResults.Columns[0].Width+sgResults.Columns[1].Width+sgResults.Columns[3].Width)-25;
end;

procedure TfSearch.sgResultsStartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
  try
    DragObject := TDragEntry.Create(Sender as TControl);
    if Assigned(DragObject) then
      with DragObject as TDragEntry do
        Links := GetLink(True);
  except
  end;
end;

procedure TfSearch.WMCloseQuery(var message: TLMessage);
begin //Workaround for #0012552
  Close;
end;
function TfSearch.Execute(Modal : Boolean;OptionSet : string;aHint : string): Boolean;
var
  ActControl: TWinControl;
  Options: String;
  i: Integer;
begin
  SearchLevel:=0;
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfSearch,fSearch);
      Self := fSearch;
    end;
  if Self.Visible and Modal then Self.Hide;
  if sgResults.RowCount = 1 then
    begin
      bSearch.Default:=True;
      bOpen.Default:=False;
    end;
  FModal := Modal;
  bEditFilterClick(nil);
  lHint.Visible := False;
  if aHint <> '' then
    begin
      lHint.Caption:=aHint;
      lHint.Visible:=True;
      lHint.Top:=0;
    end;
  LoadOptions(OptionSet);
  if not Modal then
    begin
      Show;
    end
  else
    begin
      Result := Showmodal = mrOK;
    end;
end;

procedure TfSearch.SetUpSearch;
var
  i: Integer;
  aItems: TSearchLocations;
  a: Integer;
begin
  SearchTypes:= [];
  for i := low(uBaseSearch.SearchLocations) to High(uBaseSearch.SearchLocations) do
    if cbSearchIn.Items.IndexOf(uBaseSearch.SearchLocations[i]) >= 0 then
      if cbSearchIn.Checked[cbSearchIn.Items.IndexOf(uBaseSearch.SearchLocations[i])] then
        SearchTypes := SearchTypes+[TFullTextSearchType(i)];
  aItems := GetSearchAbleItems;
  SetLength(SearchLocations,0);
  for i := 0 to length(aItems)-1 do
    for a := 0 to cbSearchtype.Items.Count-1 do
      if (aItems[i]=cbSearchtype.Items[a]) and (cbSearchtype.Checked[a]) then
        begin
          SetLength(SearchLocations,length(SearchLocations)+1);
          SearchLocations[length(SearchLocations)-1] := aItems[i];
        end;
  if SearchLevel=0 then
    begin
      sgResults.RowCount := sgResults.FixedRows;
      FreeAndNil(ActiveSearch);
    end;
  ActCount := sgResults.RowCount;
  SearchText := eContains.Text;
  if not Assigned(ActiveSearch) then
    begin
      SearchLevel:=0;
      if cbMaxResults.Checked then
        ActiveSearch := TSearch.Create(SearchTypes,SearchLocations,cbContains.Checked,seMaxResults.Value)
      else
        ActiveSearch := TSearch.Create(SearchTypes,SearchLocations,cbContains.Checked,0);
      ActiveSearch.OnItemFound:=@DataSearchresultItem;
      ActiveSearch.OnBeginItemSearch:=@ActiveSearchBeginItemSearch;
      ActiveSearch.OnEndItemSearch:=@ActiveSearchEndItemSearch;
      ActiveSearch.OnEndSearch:=@FastSearchEnd;
    end;
end;

function TfSearch.ShowHint(var HintStr: string; var CanShow: Boolean;
  var HintInfo: THintInfo) : Boolean;
var
  aLongDesc: String;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfSearch,fSearch);
      Self := fSearch;
    end;
  Result := False;
  if HintInfo.HintControl = sgResults then
    begin
      Result := True;
      HintInfo.HintWindowClass:=TSearchHintWindow;
//      if sgResults.MouseToCell(HintInfo.CursorPos).y = HintY then exit;
      HintY := sgResults.MouseToCell(HintInfo.CursorPos).y;
      with Application as IBaseDBInterface do
        begin
          HintInfo.HintStr := Data.GetLinkDesc(sgResults.Cells[4,HintY]);
          aLongDesc := Data.GetLinkLongDesc(sgResults.Cells[4,HintY]);
        end;
      if aLongDesc <> '' then
        HintInfo.HintStr := HintInfo.HintStr +lineending+ aLongDesc;
      HintInfo.HideTimeout:=15000;
    end;
end;
procedure TfSearch.SetLanguage;
var
  aLocations: TSearchLocations;
  i: Integer;
  found: Boolean;
  a: Integer;
begin
  if not Assigned(fSearch) then
    begin
      Application.CreateForm(TfSearch,fSearch);
      Self := fSearch;
    end;
  sgResults.Columns[0].Title.Caption := strType;
  sgResults.Columns[1].Title.Caption := strID;
  sgResults.Columns[2].Title.Caption := strShortname;

  cbSearchIn.Items.Clear;
  for i := low(uBaseSearch.SearchLocations) to High(uBaseSearch.SearchLocations) do
    begin
      cbSearchIn.Items.Add(uBaseSearch.SearchLocations[i]);
      if SearchLocDefault[i] then
        cbSearchIn.Checked[cbSearchIn.Items.Count-1] := True;
    end;
  aLocations := GetSearchAbleItems;
  cbSearchtype.Items.Clear;
  for i := 0 to length(aLocations)-1 do
    begin
      found := false;
      for a := 0 to cbSearchType.Items.Count-1 do
        if pos(aLocations[i],cbSearchType.items[a])>0 then
          found := True;
      if not Found then
        cbSearchType.Items.Add(aLocations[i]);
    end;
end;
procedure TfSearch.LoadOptions(OptionSet : string);
var
  Options: String;
  i: Integer;
  SearchInClear: Boolean = True;
begin
  FreeAndNil(ActiveSearch);
//  SetLanguage; Dont do this here since Allow SearchTypes has no eefect with this
  Options := '';
  with Application as IBaseDbInterface do
    begin
      if DBConfig.ReadString('SEARCHMAXRESULTS','ON') = 'OFF' then
        cbMaxresults.Checked := False
      else
        begin
          seMaxresults.Tag:=1;
          cbMaxresults.Checked := true;
          seMaxResults.Value:=DBConfig.ReadInteger('SEARCHMAXRESULTS',10);
          seMaxresults.Tag:=0;
        end;
      Options := DBConfig.ReadString('SEARCHTP:'+OptionSet,Options);
    end;
  with BaseApplication as IBaseApplication do
    Debug('Search:Loading Options for '+OptionSet+':'+Options);
  i := 0;
  for i := 0 to cbSearchType.Items.Count-1 do
    cbSearchType.Checked[i]:=False;
  while pos(';',Options) > 0 do
    begin
      cbSearchType.Tag:=1;
      if cbSearchType.Items.IndexOf(copy(Options,0,pos(';',Options)-1)) <> -1 then
        begin
          cbSearchType.Checked[cbSearchType.Items.IndexOf(copy(Options,0,pos(';',Options)-1))] := True;
          SearchInClear := False;
        end;
      cbSearchType.Tag:=0;
      Options := copy(Options,pos(';',Options)+1,length(Options));
      inc(i);
    end;
  if SearchInClear then
    begin
      for i := 0 to cbSearchType.Items.Count-1 do
        if cbSearchType.Items[i]<>strHistory then
          cbSearchType.Checked[i]:=True;
    end;
  Options := '';
  for i := 0 to cbSearchIn.Items.Count-1 do
    begin
      if cbSearchIn.Checked[i] then
        Options := Options+cbSearchIn.Items[i]+';';
      cbSearchIn.Checked[i] := False;
    end;
  with Application as IBaseDbInterface do
    Options := DBConfig.ReadString('SEARCHIN:'+OptionSet,Options);
  with BaseApplication as IBaseApplication do
    Debug('Search:Loading Searchin for '+OptionSet+':'+Options);
  while pos(';',Options) > 0 do
    begin
      if cbSearchin.Items.IndexOf(copy(Options,0,pos(';',Options)-1)) <> -1 then
        begin
          cbSearchIn.Checked[cbSearchIn.Items.IndexOf(copy(Options,0,pos(';',Options)-1))] := True;
        end;
      Options := copy(Options,pos(';',Options)+1,length(Options));
    end;
  FoptionSet := OptionSet;
  SetUpSearch;
end;

procedure TfSearch.SaveOptions;
var
  Options: String;
  i: Integer;
begin
  Options := '';
  for i := 0 to cbSearchType.Items.Count-1 do
    begin
      if cbSearchType.Checked[i] then
        Options := Options+cbSearchType.Items[i]+';';
    end;
  with Application as IBaseDbInterface do
    DBConfig.WriteString('SEARCHTP:'+FOptionSet,Options);
  Options := '';
  for i := 0 to cbSearchIn.Items.Count-1 do
    begin
      if cbSearchIn.Checked[i] then
        Options := Options+cbSearchIn.Items[i]+';';
    end;
  with Application as IBaseDbInterface do
    DBConfig.WriteString('SEARCHIN:'+FOptionSet,Options);
end;

function TfSearch.GetLink(Multi: Boolean): string;
var
  i: LongInt;
  aClass: TBaseDBDatasetClass;
  aDS: TBaseDBDataset;
  tmp: String;
begin
  Result := '';
  if Multi then
    begin
      for i := sgResults.Selection.Top to sgResults.Selection.Bottom do
        begin
          tmp := sgResults.Cells[4,i];
          if Data.ListDataSetFromLink(tmp,aClass) then
            begin
              aDS := aClass.Create(nil);
              if Assigned(aDS) then
                begin
                  tBaseDbList(aDS).SelectFromLink(tmp);
                  aDS.Open;
                  if aDS.Count>0 then
                    begin
                      tmp := Data.BuildLink(aDS.DataSet);
                      Result := Result+tmp+';'
                    end;
                  FreeAndNil(aDS);
                end;
            end;
        end;
    end
  else if (sgResults.RowCount > 0) and (sgResults.Row > -1) then
    begin
      Result := sgResults.Cells[4,sgResults.Row];
      if Data.ListDataSetFromLink(Result,aClass) then
        begin
          aDS := aClass.Create(nil);
          if Assigned(aDS) then
            begin
              tBaseDbList(aDS).SelectFromLink(Result);
              aDS.Open;
              if aDS.Count>0 then
                Result := Data.BuildLink(aDS.DataSet);
              FreeAndNil(aDS);
            end;
        end;
    end;
end;

procedure TfSearch.AllowSearchTypes(aTypes: string);
var
  i: Integer;
  found: Boolean;
  tmp: String;
begin
  SetLanguage;
  Self := fSearch;
  i := 0;
  while i < fSearch.cbSearchType.Items.Count do
    begin
      if pos(fSearch.cbSearchType.Items[i],aTypes)=0 then
        fSearch.cbSearchType.Items.Delete(i)
      else
        inc(i);
    end;
  cbSearchType.Tag:=1;
  if fSearch.cbSearchType.Items.Count=1 then
    fSearch.cbSearchType.Checked[0]:=true;
  cbSearchType.Tag:=0;
  FreeAndNil(ActiveSearch);
end;

function TSearchHintWindow.GetDrawTextFlags: Cardinal;
var
  EffectiveAlignment: TAlignment;
begin
  Result := DT_NOPREFIX or DT_VCENTER or DT_WORDBREAK;
  EffectiveAlignment := taLeftJustify;
  if BiDiMode <> bdLeftToRight then
  begin
    Result := Result or DT_RTLREADING;
  end;
  Result := Result or DT_LEFT;
end;

{procedure TSearchHintWindow.ActivateHint(const AHint: String);
begin
  inherited ActivateHint(AHint);
  Caption := AHint;
  //AMonitor := Screen.MonitorFromPoint(ARect.TopLeft);
end;
}
procedure TSearchHintWindow.ActivateHint(ARect: TRect; const AHint: string);
var
  AMonitor: TMonitor;
  ABounds: TRect;
begin
  FActivating := True;
  try
    Caption := AHint;
    AMonitor := Screen.MonitorFromPoint(ARect.TopLeft);
    ABounds := AMonitor.BoundsRect;

    ARect.Right:=ARect.Right+18;

    // limit width, height by monitor
    if (ARect.Right - ARect.Left) > (ABounds.Right - ABounds.Left) then
      ARect.Right := ARect.Left + (ABounds.Right - ABounds.Left);

    if (ARect.Bottom - ARect.Top) > (ABounds.Bottom - ABounds.Top) then
      ARect.Bottom := ARect.Top + (ABounds.Bottom - ABounds.Top);
    // offset hint to fit into monitor
    if ARect.Bottom > ABounds.Bottom then
    begin
      ARect.Top := ABounds.Bottom - (ARect.Bottom - ARect.Top);
      ARect.Bottom := ABounds.Bottom;
    end;
    if ARect.Right > ABounds.Right then
    begin
      ARect.Left := ABounds.Right - (ARect.Right - ARect.Left);
      ARect.Right := ABounds.Right;
    end;
    if ARect.Left < ABounds.Left then ARect.Left := ABounds.Left;
    if ARect.Top < ABounds.Top then ARect.Top := ABounds.Top;

    SetBounds(ARect.Left, ARect.Top,
              ARect.Right - ARect.Left, ARect.Bottom - ARect.Top);
    Visible := True;
    Invalidate;
  finally
    FActivating := False;
  end;
end;

procedure TSearchHintWindow.Paint;
var
  R: TRect;
  bmp: TBitmap;
  Details: TThemedElementDetails;
begin
  R := ClientRect;
  if (Color = clInfoBk) or (Color = clDefault) then // draw using themes
  begin
    Details := ThemeServices.GetElementDetails(tttStandardLink);
    ThemeServices.DrawElement(Canvas.Handle, Details, R);
  end
  else
  begin
    Canvas.Brush.Color := Color;
    Canvas.Pen.Width := 1;
    Canvas.FillRect(R);
    DrawEdge(Canvas.Handle, R, BDR_RAISEDOUTER, BF_RECT);
  end;

  InflateRect(R, - 2 * BorderWidth, - 2 * BorderWidth);
  Canvas.Rectangle(R.Left,R.Top,18,R.Bottom);

  Inc(R.Left, 2);
  Inc(R.Top, 2);

  R.Left:=R.Left+18;
  DrawText(Canvas.GetUpdatedHandle([csFontValid]), PChar(Caption),
    Length(Caption), R, GetDrawTextFlags);
end;
initialization
end.

