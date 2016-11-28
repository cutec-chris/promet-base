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
    procedure fWikiFrameWikiInclude(Inp: string; var Outp: string; aLevel: Integer=0
      );
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
    function Wiki2HTML(input: string): TIPHtml;
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
procedure TfWikiFrame.fWikiFrameWikiInclude(Inp: string; var Outp: string;aLevel : Integer = 0);
var
  aList: TMessageList;
  aMessage: TMessage;
  aCount : Integer;
  ss: TStringStream;
  aNewList: TWikiList;
  FSQLStream: TStringStream;
  FSQLScanner: TSQLScanner;
  FSQLParser: TSQLParser;
  bStmt: TSQLElement;
  aTableName: TSQLStringType;
  aClass: TBaseDBDatasetClass;
  aDs: TBaseDbDataSet;
  aFilter: TSQLStringType;
  aRight: String;
  aLimit: Integer = 10;
  i: Integer;
  aLimitS: String;
  aElem: TSQLElement;
  aName: TSQLStringType;
  aStatistic: TStatistic;
  aSQL: String;
  aRDs: TDataSet = nil;
  tmp: String;
  IncHeader: Boolean;
  aConditionOK : Boolean = True;
  aCondition: String = '';
  aTmpFloat: Extended;
  IsForm: Boolean;
  aInclude: String;
  nInp: String;
  ConvertRTF: Boolean = False;
  aScript: TBaseScript;
  bScript: String;
  Found: Boolean;
  procedure BuildLinkRow(aBDS : TDataSet);
  var
    aLink: String;
  begin
    aLink := Data.BuildLink(aBDS);
    Outp+='<li><a href="'+aLink+'" title="'+Data.GetLinkDesc(aLink)+#10+Data.GetLinkLongDesc(aLink)+'">'+HTMLEncode(Data.GetLinkDesc(aLink))+'</a></li>';
  end;
  function BuildTableRow(aBDS : TDataSet;aStmt : TSQLElement) : string;
  var
    aLink: String;
    i: Integer;
    a: Integer;
    aName: TSQLStringType;
    aElem: TSQLElement;
    aLinkBase: String;
  begin
    result := '';
    aLink := Data.BuildLink(aBDS);
    Result+='<tr valign="top">';
    if TSQLSelectStatement(aStmt).All then
      begin
        for a := 0 to aDS.DataSet.FieldCount-1 do
          begin
            if (aBDS.Fields[a].DataType=ftString) or (aBDS.Fields[a].DataType=ftMemo) or (aBDS.Fields[a].DataType=ftWideString) then
              Result+='<td align="left">'+aBDS.Fields[a].AsString+'</td>'
            else
              Result+='<td align="left">'+aBDS.Fields[a].Text+'</td>';
          end;
      end
    else
      begin
        for i := 0 to TSQLSelectStatement(aStmt).Fields.Count-1 do
          begin
            aElem := TSQLSelectStatement(aStmt).Fields[i];
            if aElem is TSQLSelectField then
              begin
                aName := TSQLSelectField(aElem).Expression.GetAsSQL([]);
                if copy(uppercase(aName),0,5)='LINK(' then
                  begin
                    aName := copy(aName,6,length(aName)-6);
                    if pos('(',aName)>0 then
                      begin
                        aLinkBase := copy(aName,0,pos('(',aName)-1);
                        aName := copy(aName,pos('(',aName)+1,length(aname));
                        aName := copy(aName,0,length(aName)-1);
                        aLink := aLinkBase+copy(aLink,pos('@',aLink),length(aLink));
                      end;
                    if pos('.',aName)>0 then
                      aName := copy(aName,rpos('.',aName)+1,length(aName));
                    if (aBDS.FieldDefs.IndexOf(aName)>-1) then
                      Result+='<td align="left"><a href="'+aLink+'" title="'+Data.GetLinkDesc(aLink)+#10+Data.GetLinkLongDesc(aLink)+'">'+HTMLEncode(aBDS.Fields[aBDS.FieldDefs.IndexOf(aName)].AsString)+'</a></td>'
                  end
                else if copy(uppercase(aName),0,4)='RTF(' then
                  begin
                    aName := copy(aName,5,length(aName)-5);
                    if pos('(',aName)>0 then
                      begin
                        aName := copy(aName,pos('(',aName)+1,length(aname));
                        aName := copy(aName,0,length(aName)-1);
                      end;
                    Result+='<td align="left">'+HTMLEncode(RTF2Plain(aBDS.Fields[aBDS.FieldDefs.IndexOf(aName)].AsString))+'</td>';
                  end
                else if copy(uppercase(aName),0,5)='ICON(' then
                  begin
                    aName := copy(aName,6,length(aName)-6);
                    if pos('(',aName)>0 then
                      begin
                        aName := copy(aName,pos('(',aName)+1,length(aname));
                        aName := copy(aName,0,length(aName)-1);
                      end;
                    Result+='<td align="left"><img src="ICON('+aBDS.Fields[aBDS.FieldDefs.IndexOf(aName)].AsString+')"></img></td>';
                  end
                else if copy(uppercase(aName),0,12)='HISTORYICON(' then
                  begin
                    aName := copy(aName,12,length(aName)-11);
                    if pos('(',aName)>0 then
                      begin
                        aName := copy(aName,pos('(',aName)+1,length(aname));
                        aName := copy(aName,0,length(aName)-1);
                      end;
                    Result+='<td align="left"><img src="HISTORYICON('+aBDS.Fields[aBDS.FieldDefs.IndexOf(aName)].AsString+')"></img></td>';
                  end
                else if (aBDS.FieldDefs.IndexOf(copy(aName,rpos('.',aName)+1,length(aName)))>-1) then
                  begin
                    if pos('.',aName)>0 then
                      aName := copy(aName,rpos('.',aName)+1,length(aName));
                    if (aBDS.Fields[aBDS.FieldDefs.IndexOf(aName)].DataType=ftFloat)
                    or (aBDS.Fields[aBDS.FieldDefs.IndexOf(aName)].DataType=ftInteger)
                    or (aBDS.Fields[aBDS.FieldDefs.IndexOf(aName)].DataType=ftDateTime)
                    then
                      Result+='<td align="right">'+HTMLEncode(aBDS.Fields[aBDS.FieldDefs.IndexOf(aName)].Text)+'</td>'
                    else
                      Result+='<td align="left">'+HTMLEncode(aBDS.Fields[aBDS.FieldDefs.IndexOf(aName)].AsString)+'</td>';
                  end
                else if Assigned(TSQLSelectField(aElem).AliasName) then
                  begin
                    aName := TSQLSelectField(aElem).AliasName.GetAsSQL([]);
                    if (aBDS.FieldDefs.IndexOf(aName)>-1) then
                      begin
                        if (aBDS.Fields[aBDS.FieldDefs.IndexOf(aName)].DataType=ftString) or (aBDS.Fields[aBDS.FieldDefs.IndexOf(aName)].DataType=ftMemo) or (aBDS.Fields[aBDS.FieldDefs.IndexOf(aName)].DataType=ftWideString) then
                          Result+='<td align="left">'+HTMLEncode(aBDS.Fields[aBDS.FieldDefs.IndexOf(aName)].AsString)+'</td>'
                        else
                          Result+='<td align="left">'+HTMLEncode(aBDS.Fields[aBDS.FieldDefs.IndexOf(aName)].Text)+'</td>';
                      end;
                  end;
              end;
          end;
      end;
    Result+='</tr>';
  end;
  procedure AddHeader(aStmt : TSQLElement);
  var
    b: Integer;
  begin
    Outp+='<thead><tr>';
    for b := 0 to TSQLSelectStatement(aStmt).Fields.Count-1 do
      begin
        aElem := TSQLSelectStatement(aStmt).Fields[b];
        if aElem is TSQLSelectField then
          begin
            if Assigned(TSQLSelectField(aElem).AliasName) then
              Outp+='<th><b>'+HTMLEncode(StringReplace(TSQLSelectField(aElem).AliasName.GetAsSQL([sfoSingleQuoteIdentifier]),'''','',[rfReplaceAll]))+'</b></th>'
            else
              begin
                aName := TSQLSelectField(aElem).Expression.GetAsSQL([sfoSingleQuoteIdentifier]);
                if copy(uppercase(aName),0,5)='LINK(' then
                  begin
                    aName := copy(aName,6,length(aName)-6);
                    if pos('(',aName)>0 then
                      begin
                        aName := copy(aName,pos('(',aName)+1,length(aname));
                        aName := copy(aName,0,length(aName)-1);
                      end;
                  end;
                if pos('.',aName)>0 then
                  aName := copy(aName,pos('.',aName)+1,length(aName));
                Outp+='<th><b>'+HTMLEncode(StringReplace(aName,'''','',[rfReplaceAll]))+'</b></th>';
              end;
          end;
      end;
    Outp+='</tr></thead>';
  end;
  procedure FilterSQL(aType : Integer;IncHeader : Boolean = False);
  var
    a: Integer;
    aOrderDir: TSQLOrderDirection;
    aOrder: string = '';
    aOrderDirStr: String = 'ASC';
    aRDS: TDataSet;
    i: Integer;
    aTable: TSQLElement;
    NoRights: Boolean;
    aStmt: TSQLElement;
    aRightInt: Integer;
  begin
    FSQLStream := TStringStream.Create(Inp+';');
    FSQLScanner := TSQLScanner.Create(FSQLStream);
    FSQLParser := TSQLParser.Create(FSQLScanner);
    try
      aFilter:='';
      aStmt := FSQLParser.Parse;
      a := 0;
      aTable := TSQLSelectStatement(aStmt).Tables[a];
      if aTable is TSQLSimpleTableReference then
       begin
         aTableName := TSQLSimpleTableReference(aTable).ObjectName.Name;
         aRight := UpperCase(aTableName);
       end;
      if  (aType <> 3)
      and (aType <> 4)
      then
        begin
          if Data.ListDataSetFromLink(aTableName+'@',aClass) then
            begin
              if IncHeader then
                AddHeader(aStmt);
              if aType=1 then Outp+='<tbody align="left" valign="top" align="left">';
              aDs := TBaseDBDataset(aClass.Create(nil));
              if Assigned(TSQLSelectStatement(aStmt).Where) then
                aFilter:=TSQLSelectStatement(aStmt).Where.GetAsSQL([sfoDoubleQuoteIdentifier]);
              if Assigned(TSQLSelectStatement(aStmt).Orderby) and (TSQLSelectStatement(aStmt).Orderby.Count>0) then
                begin
                  aOrder:=TSQLIdentifierName(TSQLOrderByElement(TSQLSelectStatement(aStmt).Orderby.Elements[0]).Field).Name;
                  aOrderDir := TSQLOrderByElement(TSQLSelectStatement(aStmt).Orderby.Elements[0]).OrderBy;
                end;
              if ((data.Users.Rights.Right(aRight)>RIGHT_READ) or (data.Users.Rights.Right(aRight)=-1)) and (Assigned(aDS)) then
                begin
                  if aOrder<>'' then
                    begin
                      if aOrderDir=obAscending then
                        aOrderDirStr := 'ASC'
                      else aOrderDirStr := 'DESC';
                    end;
                  if (aDs.ActualFilter<>'') and (aFilter<>'') then
                    aDs.Filter('('+aDs.ActualFilter+') AND ('+aFilter+')',aLimit)
                  else if (aFilter = '') and (aDs.ActualFilter<>'') then
                    aDs.FilterEx('('+aDs.ActualFilter+')',aLimit,aOrder,aOrderDirStr)
                  else
                    aDs.FilterEx(aFilter,aLimit,aOrder,aOrderDirStr);
                  while not aDS.EOF do
                    begin
                      case aType of
                      0:BuildLinkRow(aDs.DataSet);
                      1:Outp+=BuildTableRow(aDs.DataSet,aStmt);
                      end;
                      aDataThere:=True;
                      aDs.Next;
                    end;
                end;
              aDS.Free;
              if aType=1 then Outp+='</tbody>';
            end
          else //pure SQL
            begin //TODO:better rights check ??
              NoRights := False;
              for a := 0 to TSQLSelectStatement(aStmt).Tables.Count-1 do
                begin
                  aTable := TSQLSelectStatement(aStmt).Tables[a];
                  if aTable is TSQLSimpleTableReference then
                    begin
                      aTableName := TSQLSimpleTableReference(aTable).ObjectName.Name;
                      aRight := UpperCase(aTableName);
                      aRightInt := Data.Users.Rights.Right(aRight);
                      if (aRightInt>-1) and (aRightInt<RIGHT_READ) then
                        NoRights := True;
                    end;
                end;
              if not NoRights then
                begin
                  aSQL := TSQLSelectStatement(aStmt).GetAsSQL([sfoDoubleQuoteIdentifier]);
                  aSQL := ReplaceSQLFunctions(aSQL);
                  if aLimit>0 then
                    aSQL := AddSQLLimit(aSQL,aLimit);
                  aRDS := Data.GetNewDataSet(aSQL);
                  try
                    aRDS.Open;
                    if IncHeader then
                      AddHeader(aStmt);
                    if aType=1 then Outp+='<tbody align="left" valign="top">';
                    while not aRDS.EOF do
                      begin
                        case aType of
                        0:BuildLinkRow(aRDS);
                        1:Outp+=BuildTableRow(aRDs,aStmt);
                        end;
                        aDataThere:=True;
                        aRDs.Next;
                      end;
                  except
                    on e : Exception do
                      begin
                        Outp+='error:'+e.Message+'<br>';
                        aDataThere:=True;
                      end;
                  end;
                end;
            end;
        end
      else
        begin
          aSQL := TSQLSelectStatement(aStmt).GetAsSQL([sfoDoubleQuoteIdentifier]);
          aSQL := ReplaceSQLFunctions(aSQL);
          if aLimit>0 then
            aSQL := AddSQLLimit(aSQL,aLimit);
          aRDS := Data.GetNewDataSet(aSQL);
          aRDS.Open;
          while not aRDS.EOF do
            begin
              if ((data.Users.Rights.Right(aRight)>RIGHT_READ) or (data.Users.Rights.Right(aRight)=-1)) and (Assigned(aRDS)) then
                begin
                  for i := 0 to aRDS.FieldCount-1 do
                    begin
                      if aType=3 then
                        begin
                          if i>0 then Outp+=',';
                          tmp := HTMLEncode(aRDS.Fields[i].AsString);
                          Outp += tmp;
                          if TryStrToFloat(tmp,aTmpFloat) then
                            begin
                              if aTmpFloat<>0 then
                                aDataThere:=True;
                            end
                          else if tmp<>'' then aDataThere:=True;
                        end
                      else if (aType=4) then
                        begin
                          tmp := aRDS.Fields[i].FieldName;
                          Variables.Values[tmp]:=aRDS.Fields[i].AsString;
                          aDataThere:=True;
                        end;
                    end;
                end;
              if aType = 4 then
                begin
                  aNewList := TWikiList.CreateEx(Self,Data);
                  if aNewList.FindWikiPage(aInclude) then
                    begin
                      Inp := aNewList.FieldByName('DATA').AsString;
                      for i := 0 to FVariables.Count-1 do
                        begin
                          Inp := StringReplace(Inp,'@VARIABLES.'+FVariables.Names[i]+'@',FVariables.ValueFromIndex[i],[rfReplaceAll,rfIgnoreCase]);
                          Inp := StringReplace(Inp,'@VARIABLES.'+FVariables.Names[i]+':HTTP@',HTTPEncode(PChar(FVariables.ValueFromIndex[i])),[rfReplaceAll,rfIgnoreCase]);
                        end;
                      Outp:=Outp+WikiText2HTML(Inp,'','',True);
                    end;
                  aNewList.Free;
                end;
              aRDS.Next;
            end;
          aRDS.Free;
        end;
      FreeAndNil(aStmt);
    except
      on e : Exception do
        begin
          Outp+='error:'+e.Message+'<br>';
          aDataThere:=True;
        end;
    end;
    FSQLScanner.Free;
    FSQLParser.Free;
    FSQLStream.Free;
  end;
begin
  with BaseApplication as IBaseApplication do
    Debug('WikiInclude:'+Inp);
  if pos('datathere(',lowercase(Inp))>0 then
    aDataThere:=False;
  if copy(lowercase(Inp),0,3)='if(' then
    begin
      aCondition := copy(Inp,4,pos(';',Inp)-4);
      Inp := copy(Inp,pos(';',Inp)+1,length(Inp));
      Inp := copy(Inp,0,length(Inp)-1);
      if copy(lowercase(aCondition),0,6)='right(' then
        begin
          aConditionOK:=Data.Users.Rights.Right(copy(aCondition,7,length(aCondition)-7))>=RIGHT_READ;
        end;
    end;
  if copy(lowercase(Inp),0,4)='rtf(' then
    begin
      Inp := copy(Inp,5,length(Inp)-5);
      ConvertRTF := True;
    end;
  if not aConditionOK then exit;
  for i := 0 to FVariables.Count-1 do
    begin
      Inp := StringReplace(Inp,'@VARIABLES.'+FVariables.Names[i]+'@',FVariables.ValueFromIndex[i],[rfReplaceAll,rfIgnoreCase]);
      Inp := StringReplace(Inp,'@VARIABLES.'+FVariables.Names[i]+':HTTP@',HTTPEncode(PChar(FVariables.ValueFromIndex[i])),[rfReplaceAll,rfIgnoreCase]);
    end;
  if Uppercase(copy(Inp,0,6)) = 'BOARD(' then
    begin
      Inp := copy(Inp,7,length(Inp));
      Data.SetFilter(Data.Tree,'',0,'','ASC',False,True,True);
      if Data.Tree.DataSet.Locate('NAME',copy(Inp,0,pos(',',Inp)-1),[loCaseInsensitive]) then
        begin
          Inp := copy(Inp,pos(',',Inp)+1,length(Inp));
          Inp := copy(Inp,0,pos(')',Inp)-1);
          if not  TryStrToInt(Inp,aCount) then aCount := 30;
          aList := TMessageList.Create(nil);
          Data.SetFilter(aList,Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(VarToStr(Data.Tree.Id.AsVariant)));
          while not aList.DataSet.EOF do
            begin
              if aCount <= 0 then break;
              aMessage := TMessage.CreateEx(Self,Data);
              aMessage.Select(aList.Id.AsVariant);
              aMessage.Open;
              if aMessage.Count > 0 then
                begin
                  Outp := Outp+'<b>'+aMessage.FieldByName('SUBJECT').AsString+'</b>';
                  aMessage.Content.Open;
                  if aMessage.Content.Count > 0 then
                    begin
                      ss := TStringStream.Create('');
                      Data.BlobFieldToStream(aMessage.Content.DataSet,'DATA',ss);
                      Outp := Outp+'<br>'+WikiText2HTML(ss.DataString,'','',True)+'<br>'+DateTimeToStr(aMessage.FieldByName('SENDDATE').AsDateTime)+'<br>';
                      ss.Free;
                    end;
                end;
              aMessage.Free;
              aList.DataSet.Next;
            end;
          aList.Free;
        end;
    end
  else if Uppercase(copy(Inp,0,9)) = 'SQLLINKS(' then
    begin
      Inp := copy(Inp,10,length(Inp)-10);
      if pos(';',Inp)>0 then
        begin
          aLimitS := copy(Inp,rpos(';',Inp)+1,length(Inp));
          if IsNumeric(aLimitS) then
            begin
              Inp := copy(Inp,0,rpos(';',Inp)-1);
              aLimit := StrToIntDef(aLimitS,10);
            end;
        end;
      Outp+='<ol>';
      FilterSQL(0);
      Outp+='</ol>';
    end
  else if Uppercase(copy(Inp,0,9)) = 'SQLTABLE(' then
    begin
      Inp := copy(Inp,10,length(Inp)-10);
      if pos(';',Inp)>0 then
        begin
          aLimitS := copy(Inp,rpos(';',Inp)+1,length(Inp));
          if IsNumeric(aLimitS) then
            begin
              Inp := copy(Inp,0,rpos(';',Inp)-1);
              aLimit := StrToIntDef(aLimitS,10);
            end;
        end;
      Outp+='<table>';
      FilterSQL(1);
      Outp+='</table>';
      if pos('error:',Outp)>0 then
        Outp := StringReplace(Outp,'table>','p>',[rfReplaceAll]);
    end
  else if Uppercase(copy(Inp,0,10)) = 'SQLTABLEH(' then
    begin
      Inp := copy(Inp,11,length(Inp)-11);
      if pos(';',Inp)>0 then
        begin
          aLimitS := copy(Inp,rpos(';',Inp)+1,length(Inp));
          if IsNumeric(aLimitS) then
            begin
              Inp := copy(Inp,0,rpos(';',Inp)-1);
              aLimit := StrToIntDef(aLimitS,10);
            end;
        end;
      Outp+='<table>';
      FilterSQL(1,True);
      Outp+='</table>';
      if pos('error:',Outp)>0 then
        Outp := StringReplace(Outp,'table>','p>',[rfReplaceAll]);
    end
  else if (Uppercase(copy(Inp,0,10)) = 'STATISTIC(')
       or (Uppercase(copy(Inp,0,11)) = 'STATISTICH(')
  then
    begin
      Inp := copy(Inp,11,length(Inp)-11);
      IncHeader := False;
      if copy(Inp,0,1)='(' then
        begin
          Inp := copy(Inp,2,length(Inp));
          IncHeader := True;
        end;
      if pos(';',Inp)>0 then
        begin
          aLimitS := copy(Inp,rpos(';',Inp)+1,length(Inp));
          if IsNumeric(aLimitS) then
            begin
              Inp := copy(Inp,0,rpos(';',Inp)-1);
              aLimit := StrToIntDef(aLimitS,10);
            end;
        end;
      aSQL := copy(Inp,0,rpos(' ',Inp)-1);
      if aSQL <> '' then
        aSQL := aSQL+' STATISTICS';
      FSQLStream := TStringStream.Create(aSQL);
      FSQLScanner := TSQLScanner.Create(FSQLStream);
      FSQLParser := TSQLParser.Create(FSQLScanner);
      Inp := copy(Inp,rpos(' ',Inp)+1,length(Inp));
      aStatistic := nil;
      try
        aFilter:='';
        bStmt := FSQLParser.Parse;
        aStatistic := TStatistic.Create(nil);
        if pos('(',Inp)>0 then
          begin
            tmp := copy(Inp,pos('(',Inp)+1,length(Inp)-1);
            tmp := copy(tmp,0,length(tmp)-1);
            Variables.Values[copy(tmp,0,pos('=',tmp)-1)] := copy(tmp,pos('=',tmp)+1,length(tmp));
            Inp := copy(Inp,0,pos('(',Inp)-1);
          end;
        aStatistic.SelectFromLink(Inp);
        aStatistic.Open;
        if aStatistic.Count>0 then
          begin
            aSQL := aStatistic.BuildQuerry(Variables);
            aSQL := ReplaceSQLFunctions(aSQL);
            aRDs := Data.GetNewDataSet(aSQL);
            try
              aRDS.Open;
            except
              on e : Exception do
                begin
                  Outp+='error:'+e.Message+'<br>';
                  aDataThere:=True;
                end;
            end;
            Outp+='<table>';
            if IncHeader then
              AddHeader(bStmt);
            Outp+='<tbody align="left" valign="top">';
            while (not aRDS.EOF) and (aLimit>0) do
              begin
                aDataThere:=True;
                Outp+=BuildTableRow(aRDs,bStmt);
                dec(aLimit,1);
                aRDS.Next;
              end;
            Outp+='</tbody>';
            Outp+='</table>';
          end;
      finally
        FreeAndNil(aRds);
        FreeAndNil(aStatistic);
      end;
      FreeAndNil(bStmt);
      FSQLScanner.Free;
      FSQLParser.Free;
      FSQLStream.Free;
    end
  else if Uppercase(copy(Inp,0,15)) = 'STATISTICVALUE(' then
    begin
      Inp := copy(Inp,16,length(Inp)-16);
      if pos(';',Inp)>0 then
        begin
          aLimitS := copy(Inp,rpos(';',Inp)+1,length(Inp));
          if IsNumeric(aLimitS) then
            begin
              Inp := copy(Inp,0,rpos(';',Inp)-1);
              aLimit := StrToIntDef(aLimitS,10);
            end;
        end;
      aSQL := copy(Inp,0,rpos(' ',Inp)-1);
      if aSQL <> '' then
        aSQL := aSQL+' STATISTICS';
      FSQLStream := TStringStream.Create(aSQL);
      FSQLScanner := TSQLScanner.Create(FSQLStream);
      FSQLParser := TSQLParser.Create(FSQLScanner);
      Inp := copy(Inp,rpos(' ',Inp)+1,length(Inp));
      try
        aFilter:='';
        bStmt := FSQLParser.Parse;
        aStatistic := TStatistic.Create(nil);
        aStatistic.SelectFromLink(Inp);
        aStatistic.Open;
        if aStatistic.Count>0 then
          begin
            aRDs := Data.GetNewDataSet(aStatistic.BuildQuerry(Variables));
            try
              aRDS.Open;
            except
              on e : Exception do
                begin
                  Outp+='error:'+e.Message+'<br>';
                  aDataThere:=True;
                end;
            end;
            tmp := BuildTableRow(aRDs,bStmt);
            tmp := StringReplace(tmp,'<tr>','',[rfReplaceall]);
            tmp := StringReplace(tmp,'</tr>','',[rfReplaceall]);
            tmp := StringReplace(tmp,'<td>','',[rfReplaceall]);
            tmp := StringReplace(tmp,'</td>','',[rfReplaceall]);
            Outp+=tmp;
            if TryStrToFloat(tmp,aTmpFloat) then
              if aTmpFloat<>0 then
                aDataThere:=True;
          end;
      finally
        FreeAndNil(aRds);
        FreeAndNil(aStatistic);
      end;
      FreeAndNil(bStmt);
      FSQLScanner.Free;
      FSQLParser.Free;
      FSQLStream.Free;
    end
  else if (Uppercase(copy(Inp,0,4)) = 'SQL(')
       or (Uppercase(copy(Inp,0,5)) = 'FORM(') then
    begin
      IsForm := (Uppercase(copy(Inp,0,5)) = 'FORM(');
      Inp := copy(Inp,pos('(',Inp)+1,length(Inp)-(pos('(',Inp)+1));
      if IsForm then
        begin
          aInclude := copy(Inp,0,pos(';',Inp)-1);
          Inp := copy(Inp,pos(';',Inp)+1,length(Inp));
        end;
      if pos(';',Inp)>0 then
        begin
          aLimitS := copy(Inp,rpos(';',Inp)+1,length(Inp));
          if IsNumeric(aLimitS) then
            begin
              Inp := copy(Inp,0,rpos(';',Inp)-1);
              aLimit := StrToIntDef(aLimitS,10);
            end;
        end;
      if not IsForm then
        FilterSQL(3)
      else
        begin
          FilterSQL(4);
        end;
    end
  else if (Uppercase(copy(Inp,0,7)) = 'SCRIPT(') then
    begin
      Inp := copy(Inp,pos('(',Inp)+1,length(Inp)-(pos('(',Inp)+1));
      aScript := TBaseScript.Create(nil);
      aScript.Write:=@aScriptWrite;
      aScript.Writeln:=@aScriptWriteln;
      FScriptContent:='';
      Found := False;
      if pos('(',Inp)=1 then
        begin
          bScript := copy(Inp,0,pos('(',Inp)-1);
          aScript.SelectByName(bScript);
          aScript.Open;
          if aScript.Count>0 then
            begin
              Inp := copy(Inp,pos('(',Inp)+1,length(Inp)-(pos('(',Inp)+1));
              aScript.Execute(Inp);
              Found := True;
            end;
        end;
      if (not Found) and Assigned(aScript.Script) then
        begin
          aScript.Script.Source:=Inp;
          if not aScript.Script.Execute(Null) then
            FScriptContent:=aScript.Script.Results;
        end;
      aScript.Free;
      Outp := Outp+FScriptContent;
    end
  else
    begin
      aNewList := TWikiList.CreateEx(Self,Data);
      nInp := Inp;
      if pos('|',nInp) > 0 then nInp := copy(nInp,0,pos('|',nInp)-1);
      nInp := StringReplace(nInp,'%username%',Data.Users.Text.AsString,[]);
      if aNewList.FindWikiPage(nInp) and (aLevel < 150) then
        begin
          Outp := Outp+WikiText2HTML(aNewList.FieldByName('DATA').AsString,'','',True,aLevel+1);
        end;
      aNewList.Free;
    end;
  if copy(lowercase(aCondition),0,10)='datathere(' then
    begin
      if not aDataThere then Outp := '';
    end;
  if ConvertRTF then
    begin
      Outp:=RTF2Plain(OutP);
    end;
end;

function TfWikiFrame.Wiki2HTML(input: string): TIPHtml;
var
  ss: TStringStream;
begin
  aDataThere:=False;
  WikiToHTML.OnWikiInclude:=@fWikiFrameWikiInclude;
  Result := FEditor.GetHTML(WikiText2HTML(input,'','',True));
end;

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
      TfDocumentFrame(Sender).Refresh(DataSet.Id.AsVariant,'W',DataSet.FieldByName('NAME').AsString,Null,Null);
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
  ipHTML.SetHtml(Wiki2HTML(DataSet.FieldByName('DATA').AsString));
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
  with BaseApplication as IBaseApplication do
    Debug('OpenWikiPage:'+PageName);
  aWiki := TWikiList.Create(nil);
  Result := aWiki.FindWikiPage(pageName);
  aWiki.Free;
  DoView;
  if Result then
    begin
      TWikiList(DataSet).FindWikiPage(PageName);
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
      DataSet.FieldByName('NAME').AsString := copy(PageName,rpos('/',PageName)+1,length(PageName));
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
  aHTML := Wiki2HTML(DataSet.FieldByName('DATA').AsString);
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

