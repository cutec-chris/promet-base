{*******************************************************************************
  Copyright (C) Christian Ulrich info@cu-tec.de

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free                                                 f
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
Created 26.03.2013
*******************************************************************************}
unit umanagedocframe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, DbCtrls,
  Buttons, ComCtrls, ActnList, thumbcontrol, uPrometFrames, uBaseDocPages,
  uBaseDBInterface, threadedimageLoader, uDocumentFrame, DBZVDateTimePicker,
  PReport, Dialogs, PairSplitter, Menus, ExtDlgs, LCLType, uIntfStrConsts,
  uGeneralStrConsts, uBaseDbClasses, variants, types, uTimeLine, uPreviewFrame,
  uOCR, uExtControls,syncobjs,uBaseDatasetInterfaces;

type
  TImageItem = class(TObject)
  public
    Done : Boolean;
  end;

  { TfManageDocFrame }

  TfManageDocFrame = class(TPrometMainFrame)
    acDelete: TAction;
    acRefresh: TAction;
    acSetTag: TAction;
    acRebuildThumb: TAction;
    acEdit: TAction;
    acSaveAll: TAction;
    acSave: TAction;
    acImport: TAction;
    acRotate: TAction;
    acOCR: TAction;
    acMarkAsDone: TAction;
    acFindSubject: TAction;
    acFindDate: TAction;
    acSaveasPDF: TAction;
    acRename: TAction;
    acSetLink: TAction;
    acImageImport: TAction;
    acShowDetails: TAction;
    acFileImport: TAction;
    acOptimizeDocument: TAction;
    acSetDate: TAction;
    acAquire: TAction;
    acCreateFromTemplate: TAction;
    ActionList1: TActionList;
    bEditFilter: TSpeedButton;
    Bevel1: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Bevel5: TBevel;
    Bevel6: TBevel;
    Bevel7: TBevel;
    Bevel8: TBevel;
    Bevel9: TBevel;
    bImport1: TSpeedButton;
    bImport2: TSpeedButton;
    bImport3: TSpeedButton;
    bShowDetail: TSpeedButton;
    bRefresh2: TSpeedButton;
    bRefresh3: TSpeedButton;
    bRefresh4: TSpeedButton;
    bTag: TSpeedButton;
    bTag1: TSpeedButton;
    bZoomIn: TSpeedButton;
    bZoomOut: TSpeedButton;
    cbFilter: TComboBox;
    Documents: TDatasource;
    DBEdit1: TDBEdit;
    IdleTimer1: TTimer;
    MenuItem11: TMenuItem;
    mFulltext: TMemo;
    MenuItem10: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    DBZVDateTimePicker1: TDBZVDateTimePicker;
    eSearch: TEdit;
    ExtRotatedLabel1: TLabel;
    ExtRotatedLabel2: TLabel;
    ExtRotatedLabel3: TLabel;
    ExtRotatedLabel4: TLabel;
    ExtRotatedLabel5: TLabel;
    ExtRotatedLabel6: TLabel;
    ExtRotatedLabel7: TLabel;
    ExtRotatedLabel8: TExtRotatedLabel;
    iNoThumbnail: TImage;
    Label1: TLabel;
    Label2: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    OpenDialog1: TOpenDialog;
    OpenPictureDialog1: TOpenPictureDialog;
    Panel2: TPanel;
    pcPages: TPageControl;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    Panel1: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    pDocFrame: TPanel;
    pLeft: TPanel;
    pNav: TPanel;
    pNav1: TPanel;
    pmPopup: TPopupMenu;
    pNav2: TPanel;
    pSave: TPanel;
    pNav4: TPanel;
    pRight: TPanel;
    pThumb: TPanel;
    pToolbar: TPanel;
    SaveDialog1: TSaveDialog;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    spPages: TSplitter;
    tbMenue2: TToolButton;
    tbToolBar1: TToolBar;
    tstext: TTabSheet;
    tbMenue1: TToolButton;
    tbToolBar: TToolBar;
    ThumbControl1: TThumbControl;
    tsDocument: TTabSheet;
    tsFiles: TTabSheet;
    procedure acAquireExecute(Sender: TObject);
    procedure acCreateFromTemplateExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acEditExecute(Sender: TObject);
    procedure acFileImportExecute(Sender: TObject);
    procedure acImageImportExecute(Sender: TObject);
    procedure acFindDateExecute(Sender: TObject);
    procedure acFindSubjectExecute(Sender: TObject);
    procedure acImportExecute(Sender: TObject);
    procedure acMarkAsDoneExecute(Sender: TObject);
    procedure acOCRExecute(Sender: TObject);
    procedure acOpenExecute(Sender: TObject);
    procedure acOptimizeDocumentExecute(Sender: TObject);
    procedure acRebuildThumbExecute(Sender: TObject);
    procedure acRefreshExecute(Sender: TObject);
    procedure acRenameExecute(Sender: TObject);
    procedure acRotateExecute(Sender: TObject);
    procedure acSaveAllExecute(Sender: TObject);
    procedure acSaveasPDFExecute(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
    procedure acSetDateExecute(Sender: TObject);
    procedure acSetLinkExecute(Sender: TObject);
    procedure acSetTagExecute(Sender: TObject);
    procedure acShowDetailsExecute(Sender: TObject);
    procedure bShowDetailClick(Sender: TObject);
    procedure bZoomInClick(Sender: TObject);
    procedure bZoomOutClick(Sender: TObject);
    procedure DoAOpen(Data: PtrInt);
    procedure DoOnDropFiles(Sender: TObject; const FileNames: array of String);
    procedure eSearchChange(Sender: TObject);
    procedure FDocFrameAftercheckInFiles(Sender: TObject);
    procedure FrameEnter(Sender: TObject);
    procedure FrameExit(Sender: TObject);
    procedure FTimeLineSetMarker(Sender: TObject);
    procedure IdleTimer1Timer(Sender: TObject);
    procedure PreviewFrameAfterGetText(Sender: TObject);
    function SetLinkfromSearch(aLink: string): Boolean;
    procedure tbMenue1Click(Sender: TObject);
    procedure ThumbControl1AfterDraw(Sender: TObject; Item: TThreadedImage;
      aRect: Trect);
    procedure ThumbControl1DblClick(Sender: TObject);
    procedure ThumbControl1DrawCaption(Sender: TObject; Item: TThreadedImage;
      aRect: TRect);
    procedure ThumbControl1ImageLoaderManagerBeforeStartQueue(Sender: TObject);
    procedure ThumbControl1ItemIndexChanged(Sender: TObject;
      Item: TThreadedImage);
    procedure ThumbControl1LoadFile(Sender: TObject; URL: string; out
      Stream: TStream);
    procedure ThumbControl1Scrolled(Sender: TObject);
    procedure tstextShow(Sender: TObject);
  private
    { private declarations }
    FFullDataSet : TDocPages;
    FLast : string;
    FFetchDS : TDataSet;
    FFetchSQL : string;
    FTyp: string;
    FURL : string;
    aTag: String;
    aDate: String;
    FirstEnter : Boolean;
    FtempPath : string;
    FDocFrame: TfDocumentFrame;
    FTimeLine: TTimeLine;
    PreviewFrame: TfPreview;
    SelectedItem: TThreadedImage;
    FFilter: String;
    CSLoad: TCriticalSection;
    loadedDocument: UTF8String;
    procedure FetchNext;
    function GetTyp: string;
    procedure SetTyp(AValue: string);
    procedure WaitForImage;
    procedure RebuidThumb;
    procedure ShowDocument;
  protected
    procedure DoEnter; override;
    procedure DoExit; override;
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoRefresh(ForceRefresh : Boolean = False); override;

    function GotoCurrentItem: Boolean;
    procedure Open(aType : string);
    procedure OpenDir(aDir : Variant);
    property Typ : string read GetTyp write SetTyp;
    procedure ShowFrame; override;
    property FullDataSet : TDocPages read FFullDataSet;
  end;

  { TImportCheckTherad }

  TImportCheckThread = class(TThread)
  private
    FDoc: TfManageDocFrame;
    procedure CheckImport;
  public
    procedure Execute;override;
    constructor Create(aDocFrame : TfManageDocFrame);
  end;

procedure AddToMainTree(Node : TTReeNode;aType : string = 'D');
const
  MAX_IMAGES = 50;
implementation
{$R *.lfm}
uses uData,udocuments,uWait,LCLIntf,Utils,uFormAnimate,uImportImages,
  ProcessUtils,uMainTreeFrame,ucameraimport,FPimage,FPReadJPEG,FPCanvas,
  FPWriteJPEG,LCLProc,uthumbnails,uBaseVisualControls,updfexport,uSearch,
  usimpleprocess,uImaging,uBaseApplication,uDocumentAcquire,Graphics,PdfDoc,
  PdfImages,uBaseVisualApplication,uSelectTemplate;
resourcestring
  strTag                   = 'Tag';
  strSetTag                = 'alle markierten setzen';
  strSetDate               = 'Soll das Datum %s als Belegdatum gesetzt werden ?';
  strMakeLinkToDocuments   = 'Soll ein Verweis in dne Dateien des Eintrags angelegt werden ?'+LineEnding+'So können Sie auch vom Eintrag aus das Dokument schnell finden';
  strNoText                = 'kein Text gefunden, oder keine OCR Anwendung installiert !';
  strNewDoc                = 'Neues Dokument';

procedure AddToMainTree(Node: TTReeNode;aType : string = 'D');
var
  Node1: TTreeNode;
begin
  TTreeEntry(Node.Data).Typ := etDocuments;
  Data.Tree.DataSet.Filter:='(('+Data.QuoteField('PARENT')+'=0) and ('+Data.QuoteField('TYPE')+'='+Data.QuoteValue(aType)+'))';
  Data.Tree.DataSet.Filtered:=True;
  Data.Tree.DataSet.First;
  while not Data.Tree.dataSet.EOF do
    begin
      Node1 := fMainTreeFrame.tvMain.Items.AddChildObject(Node,'',TTreeEntry.Create);
      TTreeEntry(Node1.Data).Rec := Data.GetBookmark(Data.Tree);
      TTreeEntry(Node1.Data).DataSource := Data.Tree;
      TTreeEntry(Node1.Data).Text[0] := Data.Tree.FieldByName('NAME').AsString;
      TTreeEntry(Node1.Data).Typ := etDocumentDir;
      fMainTreeFrame.tvMain.Items.AddChildObject(Node1,'',TTreeEntry.Create);
      Data.Tree.DataSet.Next;
    end;
  Data.Tree.DataSet.Filtered:=False;
end;

{ TImportCheckTherad }

procedure TImportCheckThread.CheckImport;
begin
  FDoc.acImport.Enabled := fCameraimport.ImportAvalibe;
end;

procedure TImportCheckThread.Execute;
begin
  Synchronize(@CheckImport);
end;

constructor TImportCheckThread.Create(aDocFrame: TfManageDocFrame);
begin
  FDoc := aDocFrame;
  FreeOnTerminate:=True;
  if not BaseApplication.HasOption('disablethreads') then
    inherited Create(false)
  else
    Execute;
end;

procedure TfManageDocFrame.ThumbControl1LoadFile(Sender: TObject; URL: string;
  out Stream: TStream);
begin
  CSLoad.Enter;
  try
  FUrl := URL;
  if FLast=URL then
    begin
      if Assigned(TThreadedImage(Sender).Thread) then
        TThread(TThreadedImage(Sender).Thread).Synchronize(TThreadedImage(Sender).Thread,@FetchNext)
      else fetchNext;
    end;
  if Assigned(TThreadedImage(Sender).Thread) then
    TThread(TThreadedImage(Sender).Thread).Synchronize(TThreadedImage(Sender).Thread,@WaitForImage)
  else WaitForImage;
  Stream := TfileStream.Create(FtempPath+URL,fmOpenRead);
  finally
    CSLoad.Leave;
  end;
end;

procedure TfManageDocFrame.ThumbControl1Scrolled(Sender: TObject);
var
  aItem: TThreadedImage;
  ItemPos: Integer;
  DayHeight: Extended;
  aRec: db.LargeInt;
begin
  aItem := ThumbControl1.ItemFromPoint(point(ThumbControl1.Left+(ThumbControl1.ThumbWidth div 2),ThumbControl1.Top+(ThumbControl1.ThumbHeight div 2)));
  if Assigned(aItem) then
    begin
      aRec := DataSet.GetBookmark;
      if DataSet.DataSet.Locate('SQL_ID',copy(aItem.URL,0,pos('.',aItem.URL)-1),[]) then
        begin
          ItemPos := aItem.Area.Top+((aItem.Area.Bottom-aItem.Area.Top) div 2);
          DayHeight := (FTimeLine.Height/FTimeLine.DateRange);
          FTimeLine.StartDate:=DataSet.FieldByName('ORIGDATE').AsDateTime+round((ItemPos/DayHeight))+30;
          FTimeLine.PointerDate:=DataSet.FieldByName('ORIGDATE').AsDateTime+round((ItemPos/DayHeight));
        end;
      DataSet.GotoBookmark(aRec);
    end;
end;

procedure TfManageDocFrame.tstextShow(Sender: TObject);
var
  ss: TStringStream;
begin
  if GotoCurrentItem then
    begin
      ss := TStringStream.Create('');
      Data.BlobFieldToStream(DataSet.DataSet,'FULLTEXT',ss);
      mFulltext.Text:=ss.DataString;
      ss.Free;
    end;
end;

procedure TfManageDocFrame.ThumbControl1ImageLoaderManagerBeforeStartQueue(
  Sender: TObject);
var
  i: Integer;
  aId: String;
  aImg: TThreadedImage;
begin
  //if ThumbControl1.ImageLoaderManager.Queue.Count=0 then exit;
  FFetchSQL:='';
  for i := 0 to ThumbControl1.ImageLoaderManager.Queue.Count-1 do
    begin
      aImg := TThreadedImage(ThumbControl1.ImageLoaderManager.Queue[i]);
      try
        if Assigned(aImg) then
          if not FileExists(FtempPath+aImg.URL) then
            begin
              aId := copy(aImg.URL,0,pos('.',aImg.URL)-1);
              if IsNumeric(aID) then
                FFetchSQL:=FFetchSQL+' or '+Data.QuoteField('SQL_ID')+'='+Data.QuoteValue(aId);
            end;
      except
        ThumbControl1.ImageLoaderManager.Queue.Delete(i);
        break;
      end;
    end;
  FFetchSQL:=copy(FFetchSQL,4,length(FFetchSQL));
  if FFetchSQL <> '' then
    begin
      with DataSet.DataSet as IBaseManageDB do
        FFetchSQL:='select '+Data.QuoteField('SQL_ID')+','+Data.QuoteField('THUMBNAIL')+' from '+Data.QuoteField(TableName)+' where '+FFetchSQL+' order by '+Data.QuoteField('ORIGDATE')+' desc';
      FFetchDS := Data.GetNewDataSet(FFetchSQL);
      FFetchDS.Open;
      if not DirectoryExists(UniToSys(FtempPath)) then
        ForceDirectories(UniToSys(FtempPath));
      while not FFetchDS.EOF do
        begin
          Data.BlobFieldToFile(FFetchDS,'THUMBNAIL',FtempPath+FFetchDS.FieldByName('SQL_ID').AsString+'.jpg');
          if FileSize(FtempPath+FFetchDS.FieldByName('SQL_ID').AsString+'.jpg')=0 then
            begin
              iNoThumbnail.Picture.SaveToFile(FtempPath+FFetchDS.FieldByName('SQL_ID').AsString+'.jpg');
            end;
          FFetchDS.Next;
        end;
      FreeAndNil(FFetchDS);
    end;
end;
procedure TfManageDocFrame.ThumbControl1ItemIndexChanged(Sender: TObject;
  Item: TThreadedImage);
var
  i: Integer;
  tmp: String;
  ItemPos: Integer;
  DayHeight: Extended;
  aItem: TThreadedImage;
begin
  SelectedItem := Item;
  if Item=nil then exit;
  FDocFrame.Refresh(copy(Item.URL,0,pos('.',Item.URL)-1),'S');
  aItem := ThumbControl1.ItemFromPoint(point(ThumbControl1.Left+(ThumbControl1.ThumbWidth div 2),ThumbControl1.Top+(ThumbControl1.ThumbHeight div 2)));
  if Assigned(aItem) then
    begin
      if DataSet.DataSet.Locate('SQL_ID',copy(aItem.URL,0,pos('.',aItem.URL)-1),[]) then
        begin
          ItemPos := aItem.Area.Top+((aItem.Area.Bottom-aItem.Area.Top) div 2);
          DayHeight := (FTimeLine.Height/FTimeLine.DateRange);
          FTimeLine.StartDate:=DataSet.FieldByName('ORIGDATE').AsDateTime+round((ItemPos/DayHeight))+30;
          FTimeLine.PointerDate:=DataSet.FieldByName('ORIGDATE').AsDateTime+round((ItemPos/DayHeight));
        end;
    end;
  DataSet.DataSet.Locate('SQL_ID',copy(Item.URL,0,pos('.',Item.URL)-1),[]);
  FFullDataSet.Select(copy(Item.URL,0,pos('.',Item.URL)-1));
  FFullDataSet.Open;
  FTimeLine.MarkerDate:=DataSet.FieldByName('ORIGDATE').AsDateTime;
  if bShowDetail.Down then
    ShowDocument;
end;

procedure TfManageDocFrame.FrameEnter(Sender: TObject);
var
  aSheet: TTabSheet;
  aParent: TWinControl;
  aForm: TForm;
begin
  if Assigned(Parent) and (Parent is TTabSheet) then
    begin
      aSheet := Parent as TTabSheet;
      aParent := aSheet.PageControl.Parent;
      while Assigned(aParent) and (not (aParent is TForm)) do
        begin
          aParent := aParent.Parent;
        end;
      if Assigned(aParent) and (aParent is TForm) then
        aForm := aParent as TForm;
      if Assigned(aForm) then
        begin
          aForm.OnDropFiles:=@DoOnDropFiles;
          aForm.AllowDropFiles:=True;
        end;
    end;
  if FirstEnter then
    begin
      FirstEnter := False;
      DoRefresh;
    end;
end;
procedure TfManageDocFrame.FrameExit(Sender: TObject);
var
  aSheet: TTabSheet;
  aParent: TWinControl;
  aForm: TForm;
begin
  if Assigned(Parent) and (Parent is TTabSheet) then
    begin
      aSheet := Parent as TTabSheet;
      aParent := aSheet.PageControl.Parent;
      while Assigned(aParent) and (not (aParent is TForm)) do
        begin
          aParent := aParent.Parent;
        end;
      if Assigned(aParent) and (aParent is TForm) then
        aForm := aParent as TForm;
      if Assigned(aForm) then
        begin
          aForm.OnDropFiles:=nil;
          aForm.AllowDropFiles:=False;
        end;
    end;
end;
procedure TfManageDocFrame.FTimeLineSetMarker(Sender: TObject);
var
  OldIdx: Integer;
  i: Integer;
  arec: LargeInt;
begin
  if not ((not FDataSet.EOF) and ((FDataSet.FieldByName('ORIGDATE').IsNull) or (FDataSet.FieldByName('ORIGDATE').AsDateTime>FTimeline.MarkerDate))) then
    begin
      FDataSet.First;
      i := 0;
    end
  else i := ThumbControl1.ImageLoaderManager.ActiveIndex;
  while (not FDataSet.EOF) and ((FDataSet.FieldByName('ORIGDATE').IsNull) or (FDataSet.FieldByName('ORIGDATE').AsDateTime>FTimeline.MarkerDate)) do
    begin
      inc(i);
      arec:=FDataSet.GetBookmark;
      if i>ThumbControl1.ImageLoaderManager.CountItems then
        FetchNext;
      FDataSet.GotoBookmark(arec);
      FDataSet.Next;
    end;
  ThumbControl1.ImageLoaderManager.ActiveIndex:=i;
  ThumbControl1.ScrollIntoView;
end;
procedure TfManageDocFrame.IdleTimer1Timer(Sender: TObject);
function EncodeField(Val : string) : string;
begin
  Result := 'UPPER('+Data.QuoteField(Val)+')';
end;

function EncodeValue(Val : string) : string;
begin
  Result := Data.QuoteValue('*'+Data.EscapeString(Val)+'*');
  Result := 'UPPER('+Result+')';
end;

function CastText(Val : string) : string;
begin
  Result := 'UPPER(CAST('+Data.QuoteField(Val)+' as CHAR(8000)))';
end;
var
  aOldEntry: UTF8String;
  tmp: TCaption;
  aFilter: String;
  procedure AddFilter(aTmp : string);
  begin
    if atmp <> '' then
      begin
        if aFilter <> '' then
          aFilter := aFilter+' AND ';
        with DataSet.DataSet as IBaseDbFilter do
          aFilter := aFilter+
                 '('+Data.ProcessTerm(EncodeField('TAGS')+'='+EncodeValue(atmp))+' OR '+
                     Data.ProcessTerm(EncodeField('NAME')+'='+EncodeValue(atmp))+' OR '+
                     Data.ProcessTerm(EncodeField('LINK')+'='+EncodeValue(atmp))+' OR '+
                     Data.ProcessTerm(CastText('FULLTEXT')+'='+EncodeValue(atmp))+')';
      end;
  end;

begin
  if IdleTimer1.Tag=1 then exit;
  IdleTimer1.Tag:=1;
  IdleTimer1.Enabled:=false;
  if Assigned(ThumbControl1.ImageLoaderManager.ActiveItem) then
    aOldEntry := copy(ThumbControl1.ImageLoaderManager.ActiveItem.URL,0,rpos('.',ThumbControl1.ImageLoaderManager.ActiveItem.URL)-1);
  TDocPages(DataSet).PrepareDataSet;
  with DataSet.DataSet as IBaseDbFilter do
    begin
      SortFields := 'ORIGDATE';
      SortDirection:=sdDescending;
      if trim(eSearch.Text)<>'' then
        begin
          Limit := 1000;
          if Uppercase(eSearch.Text)='NULL' then
            begin
              aFilter := '('+Data.ProcessTerm(Data.QuoteField('TAGS')+'='+Data.QuoteValue(''))+') AND ('+Data.QuoteField('TYPE')+'='+Data.QuoteValue(TDocPages(DataSet).Typ)+')';
            end
          else
            begin
              tmp := eSearch.Text;
              aFilter := '';
              while pos(',',tmp) > 0 do
                begin
                  AddFilter(copy(tmp,0,pos(',',tmp)-1));
                  tmp := copy(tmp,pos(',',tmp)+1,length(tmp));
                end;
              AddFilter(tmp);
              aFilter := '('+aFilter+') AND ('+Data.QuoteField('TYPE')+'='+Data.QuoteValue(TDocPages(DataSet).Typ)+')';
            end;
          Filter :=  aFilter;
        end
      else
        begin
          Filter := Data.QuoteField('TYPE')+'='+Data.QuoteValue(TDocPages(DataSet).Typ);
          Limit := 0;
        end;
    end;
  DataSet.Open;
  FLast:='';
  ThumbControl1.URLList:='';
  SelectedItem:=nil;
  Documents.DataSet := FFullDataSet.DataSet;
  FetchNext;
  ThumbControl1.Invalidate;
  //Application.ProcessMessages;
  IdleTimer1.Tag:=0;
end;

procedure TfManageDocFrame.PreviewFrameAfterGetText(Sender: TObject);
var
  aDocument: TDocument;
begin
  TDocPages(FFullDataSet).Select(DataSet.Id.AsVariant);
  TDocPages(FFullDataSet).Open;
  if TDocPages(FFullDataSet).Count>0 then
    begin
      aDocument := TDocument.Create(nil);
      aDocument.SelectByID(FDocFrame.DataSet.Id.AsVariant);
      aDocument.Open;
      TDocPages(FFullDataSet).Edit;
      TDocPages(FFullDataSet).FieldByName('FULLTEXT').AsString:=aDocument.FieldByName('FULLTEXT').AsString;
      TDocPages(FFullDataSet).Post;
      aDocument.Free;
    end;
end;

function TfManageDocFrame.SetLinkfromSearch(aLink: string): Boolean;
var
  Item: TThreadedImage;
  i: Integer;
  procedure SetLink;
  var
    aEntryClass: TBaseDBDatasetClass;
    aEntry: TBaseDBDataset;
    aDocument: TDocument;
    aDocLink: String;
    i: Integer;
    aName: String;
    DoLinkDocuments: Boolean;
  begin
    DataSet.DataSet.Locate('SQL_ID',copy(Item.URL,0,pos('.',Item.URL)-1),[]);
    DoLinkDocuments := MessageDlg(strMakeLinkToDocuments,mtInformation,[mbYes,mbNo],0) = mrYes;
    TDocPages(DataSet).Edit;
    TDocPages(DataSet).FieldByName('LINK').AsString:=aLink;
    TDocPages(DataSet).Post;
    SelectedItem.Name:=TDocPages(DataSet).FieldByName('NAME').AsString+LineEnding+Data.GetLinkDesc(TDocPages(DataSet).FieldByName('LINK').AsString);
    if Data.DataSetFromLink(aLink,aEntryClass) then
      begin
        if DoLinkDocuments then
          begin
            aEntry := aEntryClass.Create(nil);
            if aEntry is TBaseDbList then
              begin
                TBaseDbList(aEntry).SelectFromLink(aLink);
                TBaseDbList(aEntry).Open;
                if TBaseDbList(aEntry).Count>0 then
                  begin
                    aDocument := TDocument.CreateEx(Self,Data);
                    aDocument.Select(0);
                    aDocument.Open;
                    aDocument.Ref_ID:=aEntry.Id.AsVariant;
                    aDocument.BaseID:=aEntry.Id.AsVariant;
                    aDocument.BaseTyp:=TBaseDbList(aEntry).GetTyp;
                    if Assigned(TBaseDbList(aEntry).FieldByName('LANGUAGE')) then
                      aDocument.BaseLanguage:=TBaseDbList(aEntry).FieldByName('LANGUAGE').AsString;
                    if Assigned(TBaseDbList(aEntry).FieldByName('VERSION')) then
                      aDocument.BaseLanguage:=TBaseDbList(aEntry).FieldByName('VERSION').AsString;
                    for i := 0 to FDocFrame.lvDocuments.Items.Count-1 do
                      begin
                        if FDocFrame.GotoEntry(FDocFrame.lvDocuments.Items[i]) then
                          begin
                            aDocLink := Data.BuildLink(FDocFrame.DataSet.DataSet);
                            break;
                          end;
                      end;
                    aDocument.AddFromLink(aDocLink);
                    aDocument.Edit;
                    aName := TDocPages(DataSet).FieldByName('NAME').AsString;
                    if rpos('.',aName)>0 then
                      aName := copy(aName,0,rpos('.',aName)-1);
                    aDocument.FieldByName('NAME').AsString:=aName;
                    aDocument.FieldByName('DATE').AsVariant:=TDocPages(DataSet).FieldByName('ORIGDATE').AsVariant;
                    aDocument.Post;
                    aDocument.Free;
                  end;
              end;
            aEntry.Free;
          end;
      end;
  end;

begin
  if ThumbControl1.SelectedList.Count=0 then
    begin
      Item := SelectedItem;
      SetLink;
    end
  else for i := 0 to ThumbControl1.SelectedList.Count-1 do
    begin
      Item := TThreadedImage(ThumbControl1.SelectedList[i]);
      SetLink;
    end;
end;

procedure TfManageDocFrame.tbMenue1Click(Sender: TObject);
begin
  TSpeedButton(Sender).PopupMenu.PopUp(TSpeedButton(Sender).ClientOrigin.x,TSpeedButton(Sender).ClientOrigin.y+TSpeedButton(Sender).Height);
end;

procedure TfManageDocFrame.ThumbControl1AfterDraw(Sender: TObject;
  Item: TThreadedImage;aRect : Trect);
begin
  if not Assigned(Item.Pointer) then
    begin
      if DataSet.Locate('SQL_ID',copy(Item.URL,0,pos('.',Item.URL)-1),[]) then
        begin
          Item.Pointer := TImageItem.Create;
          TImageItem(Item.Pointer).Done:=DataSet.FieldByName('DONE').AsString='Y';
        end;
    end;
  if Assigned(Item.Pointer) then
    begin
      if TImageItem(Item.Pointer).Done then
        fVisualControls.Images.Draw(TThumbControl(Sender).Canvas,aRect.Right-16,aRect.Bottom-16,74);
    end;
  with TThumbControl(Sender).Canvas do
    begin
      Brush.Style:=bsClear;
      aRect := Item.Area;
      aRect.Left:=aRect.Left+Item.Left-ThumbControl1.HScrollPosition;
      aRect.Top:=aRect.Top+Item.Top-ThumbControl1.VScrollPosition;
      aRect.Right:=aRect.Right+Item.Left-ThumbControl1.HScrollPosition;
      aRect.Bottom:=aRect.Bottom+Item.Top-ThumbControl1.VScrollPosition;
      Rectangle(aRect);
      Brush.Style:=bsSolid;
    end;
end;

procedure TfManageDocFrame.ThumbControl1DblClick(Sender: TObject);
begin
  acEdit.Execute;
end;

procedure TfManageDocFrame.ThumbControl1DrawCaption(Sender: TObject;
  Item: TThreadedImage; aRect: TRect);
begin
  ThumbControl1.Canvas.Font.Color:=ThumbControl1.Font.Color;
  if pos(#10,Item.Name)>0 then
    begin
      ThumbControl1.Canvas.TextRect(aRect,aRect.Left,aRect.Top,copy(Item.Name,0,pos(#10,Item.Name)-1));
      ThumbControl1.Canvas.TextRect(aRect,aRect.Left,aRect.Top+ThumbControl1.Canvas.GetTextHeight(Item.Name),copy(Item.Name,pos(#10,Item.Name)+1,length(Item.Name)));
    end
  else
    ThumbControl1.Canvas.TextRect(aRect,aRect.Left,aRect.Top,Item.Name);
end;

procedure TfManageDocFrame.DoOnDropFiles(Sender: TObject;
  const FileNames: array of String);
var
  i: Integer;
  NewFileName: String = '';
  aFile: String;
  extn: String;
  aSecFile: String;
  iFile: String;
begin
  Screen.Cursor:=crHourGlass;
  Application.ProcessMessages;
  if (not Assigned(Sender)) or fPicImport.Execute then
    begin
      if Assigned(fWaitform) then
        begin
          fWaitForm.ShowInfo(ExtractFileName(Filenames[0]));
          fWaitform.ProgressBar1.Max:=length(FileNames);
          fWaitform.ProgressBar1.Position:=0;
          fWaitform.ProgressBar1.Style:=pbstNormal;
          fWaitForm.Show;
        end;
      Application.ProcessMessages;
      for i := 0 to length(FileNames)-1 do
        begin
          iFile := UniToSys(FileNames[i]);
          if not FileExists(iFile) then
            begin
              NewFileName := AppendPathDelim(GetTempPath)+ExtractFileName(iFile);
              {$ifdef linux}
              ExecProcess('gvfs-copy "'+iFile+'" "'+NewFileName+'"');
              {$endif}
              if not FileExists(NewFileName) then
                Showmessage(Format(strCantAccessFile,[iFile]));
            end
          else NewFileName:=iFile;
          if FileExists(NewFileName) then
            begin
              if Assigned(fWaitform) then
                fWaitForm.ShowInfo(ExtractFileName(NewFileName));
              TDocPages(FFullDataSet).AddFromFile(SysToUni(NewFileName));
              TDocPages(FFullDataSet).Edit;
              if Assigned(Sender) then
                TDocPages(FFullDataSet).FieldByName('TAGS').AsString:=fPicImport.eTags.Text;
              TDocPages(FFullDataSet).FieldByName('TYPE').AsString:=FTyp;
              TDocPages(FFullDataSet).Post;
              if Assigned(Sender) and (fPicImport.cbDelete.Checked) then
                begin
                  aFile := NewFileName;
                  extn :=  AnsiString(AnsiLowerCase(ExtractFileExt(aFile)));
                  if (extn = '.cr2')
                  or (extn = '.crw')
                  or (extn = '.dng')
                  or (extn = '.raw')
                  or (extn = '.erf')
                  or (extn = '.raf')
                  or (extn = '.3fr')
                  or (extn = '.fff')
                  or (extn = '.dcr')
                  or (extn = '.dcs')
                  or (extn = '.kdc')
                  or (extn = '.rwl')
                  or (extn = '.mef')
                  or (extn = '.mfw')
                  or (extn = '.iiq')
                  or (extn = '.mrw')
                  or (extn = '.mdc')
                  or (extn = '.nef')
                  or (extn = '.nrw')
                  or (extn = '.orf')
                  or (extn = '.rw2')
                  or (extn = '.pef')
                  or (extn = '.srw')
                  or (extn = '.x3f')
                  or (extn = '.cs1')
                  or (extn = '.cs4')
                  or (extn = '.cs16')
                  or (extn = '.srf')
                  or (extn = '.sr2')
                  or (extn = '.arw')
                  then
                    begin
                      if FileExists(UniToSys(copy(aFile,0,rpos('.',aFile)-1)+'.jpg')) then
                        aSecFile := copy(aFile,0,rpos('.',aFile)-1)+'.jpg'
                      else if FileExists(UniToSys(copy(aFile,0,rpos('.',aFile)-1)+'.JPG')) then
                        aSecFile := copy(aFile,0,rpos('.',aFile)-1)+'.JPG'
                      else if FileExists(UniToSys(copy(aFile,0,rpos('.',aFile)-1)+'.Jpg')) then
                        aSecFile := copy(aFile,0,rpos('.',aFile)-1)+'.Jpg'
                      else aSecFile:='';
                      if aSecFile <> '' then
                        begin
                          {$ifdef linux}
                          try
                            ExecProcess('gvfs-rm "'+aSecFile+'"');
                          except
                            DeleteFile(UniToSys(aSecFile));
                          end;
                          {$else}
                          DeleteFile(UniToSys(aSecFile));
                          {$endif}
                        end;
                    end;
                  if FileExists(UniToSys(copy(NewFileName,0,length(NewFileName)-length(extn))+'.ufraw')) then
                    DeleteFile(UniToSys(copy(NewFileName,0,length(NewFileName)-length(extn))+'.ufraw'));
                  DeleteFile(UniToSys(NewFileName));
                  if NewFileName<>iFile then
                    begin
                      {$ifdef linux}
                      ExecProcess('gvfs-rm "'+iFile+'"');
                      {$endif}
                    end;
                end;
            end;
          if Assigned(fWaitform) then
            fWaitform.ProgressBar1.Position:=fWaitform.ProgressBar1.Position+1;
        end;
      if Assigned(fWaitform) then
        begin
          fWaitform.ProgressBar1.Style:=pbstMarquee;
          fWaitform.Hide;
        end;
      acRefresh.Execute;
    end;
  Screen.Cursor:=crDefault;
end;
procedure TfManageDocFrame.eSearchChange(Sender: TObject);
begin
  IdleTimer1.Enabled:=True;
end;
procedure TfManageDocFrame.FDocFrameAftercheckInFiles(Sender: TObject);
begin
  RebuidThumb;
end;
procedure TfManageDocFrame.acDeleteExecute(Sender: TObject);
var
  aItem: TThreadedImage;
  i: Integer;

  procedure DeleteItem;
  begin
    DataSet.DataSet.Locate('SQL_ID',copy(aItem.URL,0,pos('.',aItem.URL)-1),[]);
    TDocPages(FFullDataSet).Select(DataSet.Id.AsVariant);
    TDocPages(FFullDataSet).Open;
    if TDocPages(FFullDataSet).Count>0 then
      begin
        while FDocFrame.DataSet.Count>0 do
          TDocuments(FDocFrame.DataSet).Delete;
        DataSet.Delete;
        ThumbControl1.ImageLoaderManager.List.Delete(ThumbControl1.ImageLoaderManager.ActiveIndex);
      end;
  end;

begin
  if GotoCurrentItem and (MessageDlg(strRealdelete,mtInformation,[mbYes,mbNo],0) = mrYes) then
    begin
      if ThumbControl1.SelectedList.Count=0 then
        begin
          aItem := SelectedItem;
          DeleteItem;
        end
      else for i := 0 to ThumbControl1.SelectedList.Count-1 do
        begin
          aItem := TThreadedImage(ThumbControl1.SelectedList[i]);
          DeleteItem;
        end;
      ThumbControl1.Arrange;
      ThumbControl1.Invalidate;
      acRefresh.Execute;
    end;
end;

procedure TfManageDocFrame.acAquireExecute(Sender: TObject);
var
  NewImage : TJpegImage;
  i: Integer;
  aTop: Integer;
  Stream: TMemoryStream;
  Extension: String;
  Report: TPdfDoc;
  Page: Integer;
  aTitle : string;
  aText : string;
  aDocument: TDocument;
begin
  Application.Processmessages;
  if fAcquire.Execute then
    begin
      if fAcquire.cbType.Text = 'JPEG' then
        begin
          //Draw all images (Pages) to one big image
          NewImage := TJpegImage.Create;
          NewImage.Width := 0;
          NewImage.Height := 0;
          NewImage.CompressionQuality:=65;
          for i := 0 to length(fAcquire.Images)-1 do
            begin
              if fAcquire.Images[i].Width > NewImage.Width then
                NewImage.Width := fAcquire.Images[i].Width;
              NewImage.Height := NewImage.Height+fAcquire.Images[i].Height;
            end;
          NewImage.Canvas.Brush.Color := clWhite;
          NewImage.Canvas.Pen.Color := clWhite;
          NewImage.Canvas.Rectangle(0,0,NewImage.Width,NewImage.Height);
          aTop := 0;
          for i := 0 to length(fAcquire.Images)-1 do
            begin
              NewImage.Canvas.Draw(0,aTop,fAcquire.Images[i].Bitmap);
              inc(aTop,fAcquire.Images[i].Height);
            end;
          //Save it to Doc
          Stream := TMemoryStream.Create;
          NewImage.SaveToStream(Stream);
          Stream.Position := 0;
          NewImage.Free;
          Extension := 'jpeg';
        end
      else if fAcquire.cbType.Text = 'PDF' then
        begin
          Report := TPdfDoc.Create;
          Report.NewDoc;
          NewImage := TJpegImage.Create;
          NewImage.CompressionQuality:=65;
          for i := 0 to length(fAcquire.Images)-1 do
            begin
              Report.AddPage;
              Report.Canvas.PageWidth:=fAcquire.Images[i].Width+1;
              Report.Canvas.PageHeight:=fAcquire.Images[i].Height+1;
  //              Report.Canvas.PageHeight := trunc(Report.Canvas.PageHeight*0.8);
              NewImage.Width := fAcquire.Images[i].Width;
              NewImage.Height := fAcquire.Images[i].Height;
              NewImage.Canvas.Brush.Color := clWhite;
              NewImage.Canvas.Pen.Color := clWhite;
              NewImage.Canvas.Rectangle(0,0,NewImage.Width,NewImage.Height);
              NewImage.Canvas.Draw(0,0,fAcquire.Images[i].Bitmap);
              Report.AddXObject('Image'+IntToStr(i), CreatePdfImage(NewImage, 'Pdf-Jpeg'));
              Report.Canvas.DrawXObject(0, -1, fAcquire.Images[i].Width, fAcquire.Images[i].Height+1, 'Image'+IntToStr(i));
            end;
          NewImage.Free;
          Stream := TMemoryStream.Create;
          Report.SaveToStream(Stream);
          Stream.Position := 0;
          Report.Free;
          Extension := 'pdf';
        end;
      aText := '';
      aTitle := '';
      for Page := 0 to fAcquire.Texts.Count-1 do
        begin
          uOCR.FixText(TStringList(fAcquire.Texts[Page]));
          if aTitle = '' then
            aTitle := uOCR.GetTitle(TStringList(fAcquire.Texts[Page]));
          aText := aText+TStringList(fAcquire.Texts[Page]).Text;
        end;
      TDocPages(FFullDataSet).Insert;
      TDocPages(FFullDataSet).Post;
      aDocument := TDocument.CreateEx(Self,Data);
      aDocument.Select(0);
      aDocument.Open;
      aDocument.Ref_ID:=TDocPages(FFullDataSet).Id.AsLargeInt;
      aDocument.BaseID:=TDocPages(FFullDataSet).Id.AsString;
      aDocument.BaseTyp:='S';
      aDocument.BaseLanguage:=Null;
      aDocument.BaseLanguage:=Null;
      aDocument.ParentID:=0;
      aDocument.AddFromStream('scan',
                              Extension,
                              Stream,
                              aText,
                              Now());
      Stream.Free;
      for i := 0 to fAcquire.Texts.Count-1 do
        TStringList(fAcquire.Texts[i]).Free;
      fAcquire.Texts.Clear;
      aDocument.Free;
    end;
end;

procedure TfManageDocFrame.acCreateFromTemplateExecute(Sender: TObject);
var
  Stream: TMemoryStream;
  i: Integer;
  a: Integer;
  tmp: string;
  aDocument: TDocument;
  aFiles : array of string;
begin
  FSelectTemplate.DataSet := TDocumentTemplates.Create(nil);
  FSelectTemplate.DataSet.CreateTable;
  aDocument := TDocument.CreateEx(Self,Data);
  aDocument.Select(0);
  if fSelectTemplate.Execute(FTyp,aDocument) then
    begin
      Stream := TMemoryStream.Create;
      Data.BlobFieldToFile(FSelectTemplate.DataSet.DataSet,'DOCUMENT',GetTempDir+FSelectTemplate.eName.Text+'.'+FSelectTemplate.DataSet.DataSet.FieldByName('EXTENSION').AsString);
      Setlength(aFiles,1);
      aFiles[length(aFiles)-1] := GetTempDir+FSelectTemplate.eName.Text+'.'+FSelectTemplate.DataSet.DataSet.FieldByName('EXTENSION').AsString;
      DoOnDropFiles(nil,aFiles);
      DeleteFile(UniToSys(GetTempDir+FSelectTemplate.eName.Text+'.'+FSelectTemplate.DataSet.DataSet.FieldByName('EXTENSION').AsString));
      RebuidThumb;
      acEdit.Execute;
    end;
  aDocument.Free;
  FSelectTemplate.DataSet.Free;
end;

procedure TfManageDocFrame.acEditExecute(Sender: TObject);
var
  i: Integer;
  a: Integer;
  Found: Boolean = False;
begin
  ShowDocument;
  for i := 0 to FDocFrame.lvDocuments.Items.Count-1 do
    if (lowercase(copy(FDocFrame.lvDocuments.Items[i].SubItems[0],0,4)) = 'jpg ')
    or (lowercase(copy(FDocFrame.lvDocuments.Items[i].SubItems[0],0,5)) = 'jpeg ')
    then
      begin
        FDocFrame.lvDocuments.ItemIndex:=i;
        FDocFrame.acViewFile.Execute;
        for a := 0 to FDocFrame.lvDocuments.Items.Count-1 do
          begin
            if FDocFrame.GotoEntry(FDocFrame.lvDocuments.Items[a]) then
              if PreviewFrame.CanHandleType(Uppercase(FDocFrame.DataSet.FieldByName('EXTENSION').AsString)) then
                begin
                  PreviewFrame.LoadFromDocuments(TDocuments(FDocFrame.DataSet).Id.AsVariant);
                  break;
                end;
          end;
        Found := True;
        break;
      end;
  if not Found then
    begin
      if FDocFrame.lvDocuments.Items.Count=0 then exit;
      FDocFrame.acViewFile.Execute;
    end;
end;

procedure TfManageDocFrame.acFileImportExecute(Sender: TObject);
var
  aFiles : array of string;
  i: Integer;
begin
  if OpenDialog1.Execute then
    begin
      for i := 0 to OpenDialog1.Files.Count-1 do
        begin
          Setlength(aFiles,length(aFiles)+1);
          aFiles[length(aFiles)-1] := OpenDialog1.Files[i];
        end;
      DoOnDropFiles(Self,aFiles);
    end;
end;

procedure TfManageDocFrame.acImageImportExecute(Sender: TObject);
var
  aFiles : array of string;
  i: Integer;
begin
  if OpenPictureDialog1.Execute then
    begin
      for i := 0 to OpenPictureDialog1.Files.Count-1 do
        begin
          Setlength(aFiles,length(aFiles)+1);
          aFiles[length(aFiles)-1] := OpenPictureDialog1.Files[i];
        end;
      DoOnDropFiles(Self,aFiles);
    end;
end;

procedure TfManageDocFrame.acFindDateExecute(Sender: TObject);
var
  bDate: TDateTime;
  aStart: Integer;
  aLen: Integer;
begin
  bDate := uOCR.GetDateEx(mFulltext.Lines,aStart,aLen);
  if bDate > 0 then
    begin
      mFulltext.SelStart:=aStart;
      mFulltext.SelLength:=aLen;
      if MessageDlg(Format(strSetDate,[DateToStr(bDate)]),mtInformation,[mbYes,mbNo],0) = mrYes then
        begin
          TDocPages(FFullDataSet).Select(DataSet.Id.AsVariant);
          TDocPages(FFullDataSet).Open;
          if TDocPages(FFullDataSet).Count>0 then
            begin
              TDocPages(FFullDataSet).Edit;
              TDocPages(FFullDataSet).FieldByName('ORIGDATE').AsDateTime:=bDate;
              TDocPages(FFullDataSet).Post;
            end;
        end;
    end;
end;

procedure TfManageDocFrame.acFindSubjectExecute(Sender: TObject);
var
  aStart: Integer;
  aLen: Integer;
  aText: String;
begin
  aText := uOCR.GetTitleEx(mFulltext.Lines,0,aStart,aLen);
  if aText <> '' then
    begin
      mFulltext.SelStart:=aStart;
      mFulltext.SelLength:=aLen;
    end;
end;

procedure TfManageDocFrame.acImportExecute(Sender: TObject);
begin
  if fCameraimport.Execute(Self,FTyp) then
    acRefresh.Execute;
end;

procedure TfManageDocFrame.acMarkAsDoneExecute(Sender: TObject);
var
  Item : TThreadedImage;
  i: Integer;
  procedure ToggleDone;
  begin
    DataSet.DataSet.Locate('SQL_ID',copy(Item.URL,0,pos('.',Item.URL)-1),[]);
    TDocPages(FFullDataSet).Select(DataSet.Id.AsVariant);
    TDocPages(FFullDataSet).Open;
    if TDocPages(FFullDataSet).Count>0 then
      begin
        TDocPages(FFullDataSet).Edit;
        if TDocPages(FFullDataSet).FieldByName('DONE').AsString='Y' then
          TDocPages(FFullDataSet).FieldByName('DONE').Clear
        else
          TDocPages(FFullDataSet).FieldByName('DONE').AsString:='Y';
        TDocPages(FFullDataSet).Post;
      end;
  end;

begin
  if ThumbControl1.SelectedList.Count=0 then
    begin
      Item := SelectedItem;
      ToggleDone;
    end
  else for i := 0 to ThumbControl1.SelectedList.Count-1 do
    begin
      Item := TThreadedImage(ThumbControl1.SelectedList[i]);
      ToggleDone;
    end;
  ThumbControl1.Arrange;
  ThumbControl1.Invalidate;
  acRefresh.Execute;
end;

procedure TfManageDocFrame.acOCRExecute(Sender: TObject);
var
  aDoc: TDocument;
  Texts: TOCRPages;
  aText: TStringList;
  i: Integer;
begin
  aDoc := TDocument.Create(nil);
  aDoc.SelectByReference(TDocPages(DataSet).Id.AsVariant);
  aDoc.Open;
  if aDoc.Count>0 then
    begin
      try
        Texts := DoOCR(aDoc);
        aText := TStringList.Create;
        for i := 0 to Texts.Count-1 do
          begin
            FixText(TStringList(Texts[i]));
            atext.AddStrings(TStringList(Texts[i]));
          end;
        if trim(aText.Text) <> '' then
          begin
            TDocPages(FFullDataSet).Select(DataSet.Id.AsVariant);
            TDocPages(FFullDataSet).Open;
            if TDocPages(FFullDataSet).Count>0 then
              begin
                TDocPages(FFullDataSet).Edit;
                TDocPages(FFullDataSet).FieldByName('FULLTEXT').AsString:=aText.Text;
                mFulltext.Text:=aText.Text;
                TDocPages(FFullDataSet).Post;
              end;
          end
        else Showmessage(strNoText);
        aText.Free;
        for i := 0 to Texts.Count-1 do
          TStringList(Texts[i]).Free;
        Texts.Free;
      except
        raise;
      end;
    end;
  aDoc.Free;
end;

procedure TfManageDocFrame.acOpenExecute(Sender: TObject);
begin

end;

procedure TfManageDocFrame.acOptimizeDocumentExecute(Sender: TObject);
var
  i: Integer;
  Img: TFPMemoryImage;
  reader: TFPReaderJPEG;
  aDoc: TDocument;
  aFullStream: TMemoryStream;
  Img2: TFPMemoryImage;
  wr: TFPWriterJPEG;
  x: Integer;
  y: Integer;
  a: Integer;
begin
  for i := 0 to FDocFrame.lvDocuments.Items.Count-1 do
    if (lowercase(copy(FDocFrame.lvDocuments.Items[i].SubItems[0],0,4)) = '.jpg')
    or (lowercase(copy(FDocFrame.lvDocuments.Items[i].SubItems[0],0,5)) = '.jpeg')
    then
      begin
        FDocFrame.lvDocuments.ItemIndex:=i;
        if FDocFrame.GotoEntry(FDocFrame.lvDocuments.Items[i]) then
          begin
            Img := TFPMemoryImage.Create(0, 0);
            Img.UsePalette := false;
            reader := TFPReaderJPEG.Create;
            try
              aFullStream := TMemoryStream.Create;
              aDoc := TDocument.Create(nil);
              aDoc.SelectByID(TDocuments(FDocFrame.DataSet).Id.AsVariant);
              aDoc.Open;
              aDoc.CheckoutToStream(aFullStream);
              aFullStream.Position:=0;
              Img.LoadFromStream(aFullStream, reader);
              reader.Free;
              if Assigned(Img) then
                begin
                  uImaging.Delight(Img);
                  wr := TFPWriterJPEG.Create;
                  wr.ProgressiveEncoding:=True;
                  aFullStream.Size:=0;
                  aFullStream.Position:=0;
                  Img.SaveToStream(aFullStream,wr);
                  wr.Free;
                  aFullStream.Position:=0;
                  aDoc.CheckInFromStream(aFullStream);
                  aDoc.Free;
                  aFullStream.Free;
                end;
            finally
              Img.Free;
            end;

            for a := 0 to FDocFrame.lvDocuments.Items.Count-1 do
              begin
                if FDocFrame.GotoEntry(FDocFrame.lvDocuments.Items[a]) then
                  if PreviewFrame.CanHandleType(Uppercase(FDocFrame.DataSet.FieldByName('EXTENSION').AsString)) then
                    begin
                      PreviewFrame.LoadFromDocuments(TDocuments(FDocFrame.DataSet).Id.AsVariant);
                      break;
                    end;
              end;
            acRebuildThumb.Execute;
          end;
        break;
      end;
end;

procedure TfManageDocFrame.acRebuildThumbExecute(Sender: TObject);
var
  i: Integer;
  Item: TThreadedImage;
begin
  if ThumbControl1.SelectedList.Count=0 then
    RebuidThumb
  else for i := 0 to ThumbControl1.SelectedList.Count-1 do
    begin
      Item := TThreadedImage(ThumbControl1.SelectedList[i]);
      DataSet.DataSet.Locate('SQL_ID',copy(Item.URL,0,pos('.',Item.URL)-1),[]);
      FDocFrame.Refresh(copy(Item.URL,0,pos('.',Item.URL)-1),'S');
      RebuidThumb;
    end;
  acRefresh.Execute;
end;
procedure TfManageDocFrame.acRefreshExecute(Sender: TObject);
var
  OldIdx: Integer;
begin
  if not Self.DataSet.Active then exit;
  OldIdx:=ThumbControl1.ImageLoaderManager.ActiveIndex;
  DataSet.DataSet.Refresh;
  FLast:='';
  ThumbControl1.URLList:='';
  SelectedItem:=nil;
  Documents.DataSet := DataSet.DataSet;
  FetchNext;
  while ThumbControl1.ImageLoaderManager.CountItems<OldIdx do
    FetchNext;
  ThumbControl1.ImageLoaderManager.ActiveIndex:=OldIdx;
  ThumbControl1.ScrollIntoView;
  bShowDetail.Enabled:=DataSet.Count>0;
  if not bShowDetail.Enabled then
    begin
      bShowDetail.Down:=false;
      bShowDetailClick(nil);
    end;
  pSave.Enabled:=DataSet.Count>0;
  ThumbControl1.Arrange;
  ThumbControl1.Invalidate;
  ThumbControl1ItemIndexChanged(ThumbControl1,ThumbControl1.ImageLoaderManager.ActiveItem);
end;

procedure TfManageDocFrame.acRenameExecute(Sender: TObject);
var
  aValue: String;
begin
  if GotoCurrentItem then
    if InputQuery(strRename,strName,aValue) then
      begin
        TDocPages(DataSet).Edit;
        TDocPages(DataSet).FieldByName('NAME').AsString:=aValue;
        TDocPages(DataSet).Post;
        SelectedItem.Name:=TDocPages(DataSet).FieldByName('NAME').AsString+LineEnding+Data.GetLinkDesc(TDocPages(DataSet).FieldByName('LINK').AsString);
      end;
end;

procedure TfManageDocFrame.acRotateExecute(Sender: TObject);
var
  i: Integer;
  Img: TFPMemoryImage;
  reader: TFPReaderJPEG;
  aDoc: TDocument;
  aFullStream: TMemoryStream;
  Img2: TFPMemoryImage;
  wr: TFPWriterJPEG;
  x: Integer;
  y: Integer;
  a: Integer;
begin
  for i := 0 to FDocFrame.lvDocuments.Items.Count-1 do
    if (lowercase(copy(FDocFrame.lvDocuments.Items[i].SubItems[0],0,4)) = '.jpg')
    or (lowercase(copy(FDocFrame.lvDocuments.Items[i].SubItems[0],0,5)) = '.jpeg')
    then
      begin
        FDocFrame.lvDocuments.ItemIndex:=i;
        if FDocFrame.GotoEntry(FDocFrame.lvDocuments.Items[i]) then
          begin
            Img := TFPMemoryImage.Create(0, 0);
            Img.UsePalette := false;
            reader := TFPReaderJPEG.Create;
            try
              aFullStream := TMemoryStream.Create;
              aDoc := TDocument.Create(nil);
              aDoc.SelectByID(TDocuments(FDocFrame.DataSet).Id.AsVariant);
              aDoc.Open;
              aDoc.CheckoutToStream(aFullStream);
              aFullStream.Position:=0;
              Img.LoadFromStream(aFullStream, reader);
              reader.Free;
              if Assigned(Img) then
                begin
                  Img2 := TFPMemoryImage.create(0,0);
                  Img2.Width:=Img.Height+1;
                  Img2.Height:=Img.Width+1;
                  Img2.UsePalette := false;
                  for x := 0 to Img.Width-1 do
                    for y := 0 to Img.Height-1 do
                      begin
                        Img2.Colors[Img.Height-y,x] := Img.Colors[x,y];
                      end;
                  wr := TFPWriterJPEG.Create;
                  wr.ProgressiveEncoding:=True;
                  aFullStream.Size:=0;
                  aFullStream.Position:=0;
                  Img2.SaveToStream(aFullStream,wr);
                  wr.Free;
                  aFullStream.Position:=0;
                  aDoc.CheckInFromStream(aFullStream);
                  aDoc.Free;
                  aFullStream.Free;
                  Img2.Free;
                end;
            finally
              Img.Free;
            end;

            for a := 0 to FDocFrame.lvDocuments.Items.Count-1 do
              begin
                if FDocFrame.GotoEntry(FDocFrame.lvDocuments.Items[a]) then
                  if PreviewFrame.CanHandleType(Uppercase(FDocFrame.DataSet.FieldByName('EXTENSION').AsString)) then
                    begin
                      PreviewFrame.LoadFromDocuments(TDocuments(FDocFrame.DataSet).Id.AsVariant);
                      break;
                    end;
              end;
            acRebuildThumb.Execute;
          end;
        break;
      end;
end;

procedure TfManageDocFrame.acSaveAllExecute(Sender: TObject);
var
  ARect: TRect;
  Dum: TRect;
  i: Integer;
  a: Integer;
begin
  ARect.Left := 0;
  ARect.Top := 0;
  ARect.Bottom:=ThumbControl1.Height;
  Arect.Right:=ThumbControl1.Width;
  if not SelectDirectoryDialog1.Execute then exit;
  fWaitForm.ShowInfo(strSave);
  for i := 0 to ThumbControl1.ImageLoaderManager.List.Count - 1 do
    if IntersectRect(Dum, ARect, TThreadedImage(ThumbControl1.ImageLoaderManager.List[i]).Rect) then
      begin
        ThumbControl1.ImageLoaderManager.ActiveIndex:=i;
        ThumbControl1ItemIndexChanged(ThumbControl1,TThreadedImage(ThumbControl1.ImageLoaderManager.List[i]));
        for a := 0 to FDocFrame.lvDocuments.Items.Count-1 do
          if (lowercase(copy(FDocFrame.lvDocuments.Items[a].SubItems[0],0,4)) = 'jpg ')
          or (lowercase(copy(FDocFrame.lvDocuments.Items[a].SubItems[0],0,5)) = 'jpeg ')
          then
            begin
              FDocFrame.lvDocuments.ItemIndex:=a;
              FDocFrame.SaveFileToDir(SelectDirectoryDialog1.FileName);
            end;
      end;
  fWaitForm.Hide;
end;

procedure TfManageDocFrame.acSaveasPDFExecute(Sender: TObject);
var
  aDocument: TDocument;
  aNumber: String;
  aFullStream: TMemoryStream;
  i: Integer;
begin
  if SaveDialog1.Execute then
    begin
      if not Assigned(fpdfexport) then
        Application.CreateForm(Tfpdfexport,fpdfexport);
      for i := 0 to FDocFrame.lvDocuments.Items.Count-1 do
        begin
          if (lowercase(copy(FDocFrame.lvDocuments.Items[i].SubItems[0],0,4)) = 'jpg ')
          or (lowercase(copy(FDocFrame.lvDocuments.Items[i].SubItems[0],0,5)) = 'jpeg ')
          then
          if FDocFrame.GotoEntry(FDocFrame.lvDocuments.Items[i]) then
            begin
              aDocument := TDocument.Create(nil);
              aDocument.SelectByID(FDocFrame.DataSet.Id.AsVariant);
              aDocument.Open;
              aNumber := aDocument.FieldByName('NUMBER').AsString;
              aDocument.SelectByNumber(aNumber);
              aDocument.Open;
              aFullStream := TMemoryStream.Create;
              aDocument.CheckoutToStream(aFullStream);
              aFullStream.Position:=0;
              aDocument.Free;
              fpdfexport.Image.Picture.LoadFromStreamWithFileExt(aFullStream,'.jpg');
              fpdfexport.Image.Repaint;
              fpdfexport.Pdf.FileName:=SaveDialog1.FileName;
              fpdfexport.Pdf.BeginDoc;
              fpdfexport.pdf.Print(fpdfexport.Page);
              fpdfexport.Pdf.EndDoc;
              aFullStream.Free;
            end;
        end;
    end;
end;

procedure TfManageDocFrame.acSaveExecute(Sender: TObject);
var
  a: Integer;
begin
  ThumbControl1ItemIndexChanged(ThumbControl1,TThreadedImage(ThumbControl1.ImageLoaderManager.List[ThumbControl1.ImageLoaderManager.ActiveIndex]));
  for a := 0 to FDocFrame.lvDocuments.Items.Count-1 do
    if (lowercase(copy(FDocFrame.lvDocuments.Items[a].SubItems[0],0,4)) = 'jpg ')
    or (lowercase(copy(FDocFrame.lvDocuments.Items[a].SubItems[0],0,5)) = 'jpeg ')
    then
      begin
        FDocFrame.lvDocuments.ItemIndex:=a;
        FDocFrame.acSaveToFile.Execute;
      end;
end;

procedure TfManageDocFrame.acSetDateExecute(Sender: TObject);
var
  Item : TThreadedImage;
  i: Integer;
  procedure ToggleDone;
  var
    tmp: String;
  begin
    DataSet.DataSet.Locate('SQL_ID',copy(Item.URL,0,pos('.',Item.URL)-1),[]);
    if (aDate <> '') then
      begin
        if not DataSet.CanEdit then
          DataSet.DataSet.Edit;
        DataSet.FieldByName('ORIGDATE').AsString := aDate;
      end;
  end;

begin
  if InputQuery(strDate,strSetTag,aDate) then
    begin
      if ThumbControl1.SelectedList.Count=0 then
        begin
          Item := SelectedItem;
          ToggleDone;
        end
      else for i := 0 to ThumbControl1.SelectedList.Count-1 do
        begin
          Item := TThreadedImage(ThumbControl1.SelectedList[i]);
          ToggleDone;
        end;
    end;
end;

procedure TfManageDocFrame.acSetLinkExecute(Sender: TObject);
begin
  if GotoCurrentItem then
    begin
      fSearch.SetLanguage;
      fSearch.OnOpenItem:=@SetLinkfromSearch;
      fSearch.Execute(True,'DOCLINK',strSearchfromDocumentsMode);
    end;
end;

procedure TfManageDocFrame.acSetTagExecute(Sender: TObject);
var
  Item : TThreadedImage;
  i: Integer;
  procedure ToggleDone;
  var
    tmp: String;
  begin
    DataSet.DataSet.Locate('SQL_ID',copy(Item.URL,0,pos('.',Item.URL)-1),[]);
    if (aTag <> '') and (pos(aTag,DataSet.FieldByName('TAGS').AsString)=0) then
      begin
        if not DataSet.CanEdit then
          DataSet.DataSet.Edit;
        tmp := DataSet.FieldByName('TAGS').AsString;
        if (copy(tmp,length(tmp)-1,1) <> ',') and (trim(tmp) <> '') then
          tmp := tmp+',';
        tmp := tmp+aTag;
        DataSet.FieldByName('TAGS').AsString := tmp;
      end;
  end;

begin
  if InputQuery(strTag,strSetTag,aTag) then
    begin
      if ThumbControl1.SelectedList.Count=0 then
        begin
          Item := SelectedItem;
          ToggleDone;
        end
      else for i := 0 to ThumbControl1.SelectedList.Count-1 do
        begin
          Item := TThreadedImage(ThumbControl1.SelectedList[i]);
          ToggleDone;
        end;
    end;
end;
procedure TfManageDocFrame.acShowDetailsExecute(Sender: TObject);
begin
  if (not bShowDetail.Down) and (bShowDetail.Enabled) then
    begin
      bShowDetail.Down:=True;
      bShowDetailClick(nil);
    end;
end;
procedure TfManageDocFrame.bShowDetailClick(Sender: TObject);
begin
  if bShowDetail.Down then
    begin
      Panel1.Align:=alLeft;
      Panel1.Width:=330;
      pcPages.Visible:=True;
      spPages.Visible:=True;
      pcPages.Align:=alClient;
    end
  else
    begin
      Panel1.Align:=alClient;
      pcPages.Visible:=False;
      spPages.Visible:=False;
    end;
  ThumbControl1.Arrange;
  ThumbControl1.Invalidate;
  ShowDocument;
end;
procedure TfManageDocFrame.bZoomInClick(Sender: TObject);
begin
  ThumbControl1.ThumbHeight:=ThumbControl1.ThumbHeight+20;
  ThumbControl1.ThumbWidth:=ThumbControl1.ThumbWidth+20;
  acRefresh.Execute;
end;
procedure TfManageDocFrame.bZoomOutClick(Sender: TObject);
begin
  ThumbControl1.ThumbHeight:=ThumbControl1.ThumbHeight-20;
  ThumbControl1.ThumbWidth:=ThumbControl1.ThumbWidth-20;
  acRefresh.Execute;
end;

procedure TfManageDocFrame.DoAOpen(Data: PtrInt);
var
  aRefThread: TImportCheckThread;
begin
  aRefThread := TImportCheckThread.Create(Self);
end;

procedure TfManageDocFrame.FetchNext;
var
  i: Integer;
  aItem: TThreadedImage;
begin
  i := 0;
  if DataSet.DataSet.Locate('SQL_ID',copy(FLast,0,pos('.',FLast)-1),[]) then
    DataSet.Next
  else
    DataSet.First;
  while (not DataSet.EOF) and (i<MAX_IMAGES) do
    begin
      inc(i);
      FLast := DataSet.Id.AsString+'.jpg';
      aItem := ThumbControl1.ImageLoaderManager.AddImage(FLast);
      aItem.Name:=TDocPages(DataSet).FieldByName('NAME').AsString+LineEnding+Data.GetLinkDesc(TDocPages(DataSet).FieldByName('LINK').AsString);
      if not Assigned(SelectedItem) then
        SelectedItem := aItem;
      DataSet.Next;
    end;
  ThumbControl1.Arrange;
end;

function TfManageDocFrame.GetTyp: string;
begin
  Result := FTyp;
end;

procedure TfManageDocFrame.SetTyp(AValue: string);
begin
  FTyp := AValue;
  if AValue='D' then
    begin
      bImport3.Action:=acAquire;
      tbMenue2.Action:=acCreateFromTemplate;
      bImport3.Visible:=True;
    end
  else
    begin
      bImport3.Action:=acImport;
      tbMenue2.Action:=acImport;
      bImport3.Visible:=False;
    end;

end;

procedure TfManageDocFrame.WaitForImage;
var
  URL: String;
  aTime: DWORD;
begin
  URL := FURL;
  with BaseApplication as IBaseApplication do
    Debug('WaitForImage   :'+URL);
  aTime := GetTickCount;
  while (not FileExists(FtempPath+URL)) do
    begin
      if GetTickCount-aTime>1000 then break;
      Application.ProcessMessages;
    end;
  with BaseApplication as IBaseApplication do
    Debug('WaitForImageEnd:'+URL);
end;
procedure TfManageDocFrame.RebuidThumb;
var
  aDocument: TDocument;
  aFullStream: TMemoryStream;
  i: Integer;
  aStream: TMemoryStream;
  aNumber: String;
  aSStream: TStringStream;
  aFS: TFileStream;
  extn: String;
  aText: string;
begin
  Screen.Cursor:=crHourGlass;
  Application.ProcessMessages;
  for i := 0 to FDocFrame.lvDocuments.Items.Count-1 do
    begin
      if FDocFrame.GotoEntry(FDocFrame.lvDocuments.Items[i]) then
        begin
          aDocument := TDocument.Create(nil);
          aDocument.SelectByID(FDocFrame.DataSet.Id.AsVariant);
          aDocument.Open;
          aNumber := aDocument.FieldByName('NUMBER').AsString;
          aDocument.SelectByNumber(aNumber);
          aDocument.Open;
          aFullStream := TMemoryStream.Create;
          aStream := TMemoryStream.Create;
          aDocument.CheckoutToStream(aFullStream);
          aFullStream.Position:=0;
          aSStream := TStringStream.Create('');
          extn :=  AnsiString(AnsiLowerCase(ExtractFileExt(aDocument.filename)));
          GetContentText(aFullStream,extn,aText);
          if GenerateThumbNail(ExtractFileExt(aDocument.FileName),aFullStream,aStream,aText) then
            begin
              if aStream.Size>0 then
                begin
                  Data.StreamToBlobField(aStream,DataSet.DataSet,'THUMBNAIL');
                  aStream.Position:=0;
                  aFS := TFileStream.Create(FtempPath+DataSet.FieldByName('SQL_ID').AsString+'.jpg',fmCreate);
                  aFS.CopyFrom(aStream,0);
                  aFS.Free;
                end;
            end;
          TDocPages(FFullDataSet).Select(DataSet.Id.AsVariant);
          TDocPages(FFullDataSet).Open;
          if TDocPages(FFullDataSet).Count>0 then
            begin
              if TDocPages(FFullDataSet).FieldByName('FULLTEXT').AsString<>aText then
                begin
                  TDocPages(FFullDataSet).Edit;
                  TDocPages(FFullDataSet).FieldByName('FULLTEXT').AsString:=aText;
                  TDocPages(FFullDataSet).Post;
                end;
            end;
          aSStream.Free;
          aDocument.Free;
          aFullStream.Free;
          aStream.Free;
          ThumbControl1.ImageLoaderManager.ActiveItem.LoadState:=lsEmpty;
          ThumbControl1.Invalidate;
        end;
    end;
  Screen.Cursor:=crDefault;
end;

procedure TfManageDocFrame.ShowDocument;
var
  aStream: TFileStream;
  i: Integer;
begin
  if bShowDetail.Down then
    begin
      if Assigned(ThumbControl1.ImageLoaderManager.ActiveItem) then
        begin
          if loadedDocument<>ThumbControl1.ImageLoaderManager.ActiveItem.URL then
            begin
              try
                aStream := TFileStream.Create(FtempPath+ThumbControl1.ImageLoaderManager.ActiveItem.URL,fmOpenRead);
                PreviewFrame.LoadFromStream(aStream,'JPG');
                loadedDocument:=ThumbControl1.ImageLoaderManager.ActiveItem.URL;
                aStream.Free;
              except
              end;
            end;
        end
      else
        begin
          if ThumbControl1.ImageLoaderManager.CountItems>0 then
            ThumbControl1.ImageLoaderManager.ActiveIndex:=0;
        end;
      Application.ProcessMessages;
      for i := 0 to FDocFrame.lvDocuments.Items.Count-1 do
        begin
          if FDocFrame.GotoEntry(FDocFrame.lvDocuments.Items[i]) then
            if FDocFrame.DataSet.FieldByName('SIZE').AsInteger<(2*1024*1024) then
              if PreviewFrame.CanHandleType(Uppercase(FDocFrame.DataSet.FieldByName('EXTENSION').AsString)) then
                begin
                  PreviewFrame.LoadFromDocuments(TDocuments(FDocFrame.DataSet).Id.AsVariant);
                  break;
                end;
        end;
      acRotate.Enabled:=False;
      for i := 0 to FDocFrame.lvDocuments.Items.Count-1 do
        if (lowercase(copy(FDocFrame.lvDocuments.Items[i].SubItems[0],0,4)) = '.jpg')
        or (lowercase(copy(FDocFrame.lvDocuments.Items[i].SubItems[0],0,5)) = '.jpeg')
        then
          begin
            acRotate.Enabled:=True;
          end;
      if tstext.Visible then
        tstext.OnShow(tsText);
    end;
end;

procedure TfManageDocFrame.DoEnter;
begin
end;

procedure TfManageDocFrame.DoExit;
begin
end;

constructor TfManageDocFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ThumbControl1.ClearImageList;
  CSLoad := TCriticalSection.Create;
  FFilter := '';
  SelectedItem:=nil;
  if BaseApplication.HasOption('disablethreads') then
    ThumbControl1.MultiThreaded:=False;
  ThumbControl1.BorderStyle:=bsNone;
  DataSet := TDocPagesList.Create(nil);
  FTempPath := uthumbnails.GetThumbTempDir;
  ForceDirectories(UniToSys(FtempPath));
  FtempPath := AppendPathDelim(FtempPath);
  FDocFrame := TfDocumentFrame.Create(Self);
  FDocFrame.DataSet := TDocuments.Create(nil);
  FDocFrame.Parent := pDocFrame;
  FDocFrame.Align:=alClient;
  FDocFrame.HasPreview:=False;
  FDocFrame.AftercheckInFiles:=@FDocFrameAftercheckInFiles;
  FTimeLine := TTimeLine.Create(Self);
  fTimeLine.Parent:=pRight;
  fTimeLine.Align:=alClient;
  fTimeLine.OnSetMarker:=@FTimeLineSetMarker;
  FTimeLine.Increment:=-16;
  FTimeLine.UseLongMonth:=False;
  PreviewFrame := TfPreview.Create(Self);
  PreviewFrame.Parent := tsDocument;
  PreviewFrame.Align := alClient;
  PreviewFrame.Show;
  PreviewFrame.AddToolbarAction(acEdit);
  PreviewFrame.AddToolbarAction(acRotate);
  PreviewFrame.AddToolbarAction(acOptimizeDocument);
  PreviewFrame.AfterGetText:=@PreviewFrameAfterGetText;
  FirstEnter:=True;
end;
destructor TfManageDocFrame.Destroy;
begin
  try
    CSLoad.Free;
  except
  end;
  FFullDataSet.Free;
  FTimeLine.Free;
  FreeAndNil(FDataSet);
  FDocFrame.Free;
  PreviewFrame.Free;
  inherited Destroy;
  ClearThumbDir;
end;
procedure TfManageDocFrame.Open(aType: string);
var
  aRefThread: TImportCheckThread;
begin
  FTyp := aType;
  ThumbControl1.OnLoadFile:=@ThumbControl1LoadFile;
  ThumbControl1.ImageLoaderManager.BeforeStartQueue:=@ThumbControl1ImageLoaderManagerBeforeStartQueue;
  PreviewFrame.ZoomWidth:=aType='D';
  TDocPages(DataSet).Typ:=aType;
  FFilter := Data.QuoteField('TYPE')+'='+Data.QuoteValue(aType);
  with DataSet.DataSet as IBaseDbFilter do
    begin
      SortFields := 'ORIGDATE';
      SortDirection:=sdDescending;
      Limit := 0;
      FetchRows:=5;
      Filter :=  FFilter;
    end;
  TDocPages(DataSet).PrepareDataSet;
  ThumbControl1.URLList:='';
  SelectedItem:=nil;
  Documents.DataSet := DataSet.DataSet;
  bShowDetail.Down:=False;
  bShowDetailClick(nil);
  bShowDetail.Enabled:=DataSet.Count>0;
  pSave.Enabled:=DataSet.Count>0;
  acOptimizeDocument.Visible:=aType='D';
  Application.QueueAsyncCall(@DoAOpen,0);
end;
procedure TfManageDocFrame.DoRefresh(ForceRefresh: Boolean);
begin
  ThumbControl1.Invalidate;
end;

function TfManageDocFrame.GotoCurrentItem : Boolean;
begin
  Result := false;
  if SelectedItem=nil then exit;
  Result := DataSet.DataSet.Locate('SQL_ID',copy(SelectedItem.URL,0,pos('.',SelectedItem.URL)-1),[]);
end;

procedure TfManageDocFrame.OpenDir(aDir: Variant);
begin
  with BaseApplication as IBaseApplication do
    Debug('TfManageDocFrame:OpenDir enter');
  if aDir = Null then
    FFilter := Data.QuoteField('TYPE')+'='+Data.QuoteValue(FTyp)
  else
    FFilter := '('+Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(aDir)+') AND ('+Data.QuoteField('TYPE')+'='+Data.QuoteValue(FTyp)+')';
  if not Assigned(FFullDataSet) then
    FFullDataSet := TDocPages.Create(nil);
  with FFullDataSet.DataSet as IBaseDbFilter do
    begin
      SortFields := 'ORIGDATE';
      SortDirection:=sdDescending;
      Limit := 1;
      Filter :=  FFilter;
    end;
  FFullDataSet.Open;
  with DataSet.DataSet as IBaseDbFilter do
    begin
      Fields:=Data.QuoteField('SQL_ID')+','+Data.QuoteField('ORIGDATE')+','+Data.QuoteField('TAGS')+','+Data.QuoteField('NAME')+','+Data.QuoteField('LINK')+','+Data.QuoteField('TYPE')+','+Data.QuoteField('DONE');
      SortFields := 'ORIGDATE';
      SortDirection:=sdDescending;
      FetchRows:=100;
      Limit := 0;
      Filter :=  FFilter;
    end;
  DataSet.Open;
  FTimeLine.StartDate:=DataSet.FieldByName('ORIGDATE').AsDateTime;
  FLast:='';
  ThumbControl1.URLList:='';
  SelectedItem:=nil;
  Documents.DataSet := DataSet.DataSet;
  FetchNext;
  if Assigned(IdleTimer1) then
    IdleTimer1.Tag:=0;
  bShowDetail.Enabled:=DataSet.Count>0;
  pSave.Enabled:=DataSet.Count>0;
  ThumbControl1.Arrange;
  with BaseApplication as IBaseApplication do
    Debug('TfManageDocFrame:OpenDir leave');
end;

procedure TfManageDocFrame.ShowFrame;
begin
  inherited ShowFrame;
  if not Assigned(Self) then exit;
  if not Assigned(Self.DataSet) then exit;
  acRefresh.Execute;
end;

initialization
//  TBaseVisualApplication(Application).RegisterForm(TfManageDocFrame);
end.

