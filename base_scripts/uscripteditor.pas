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
Created 08.08.2014
*******************************************************************************}
unit uScriptEditor;

interface

uses
  SysUtils, Classes, types, db, Graphics, Controls, Forms, Dialogs, Menus,
  ExtCtrls, StdCtrls, ComCtrls, ActnList, DbCtrls, DBGrids, SynEdit,
  SynEditTypes, SynHighlighterPas, SynCompletion, LCLType, uPSComponent_Default,
  RegExpr, LResources, SynEditRegexSearch, SynEditSearch, SynEditMiscClasses,
  SynEditHighlighter, SynGutterBase, SynEditMarks, SynEditMarkupSpecialLine,
  SynHighlighterSQL, SynHighlighterPython, SynHighlighterCpp,
  SynHighlighterJScript, uprometscripts, LCLIntf, Buttons, StdActns, genscript,
  uBaseDbClasses, variants, uIntfStrConsts,genpascalscript
  ;

type
  TOpenUnitEvent = procedure(UnitName : string;X,Y : Integer) of object;

  TBreakpoint = class
  public
    Module : string;
    Line : Integer;
  end;

  { TOwnLiemark }

  TOwnLinemark = class(TSynEditMark)
  public
    destructor Destroy; override;
  end;

  { TfScriptEditor }

  TfScriptEditor = class(TForm)
    acNew: TAction;
    acRun: TAction;
    acPause: TAction;
    acReset: TAction;
    acSyntaxcheck: TAction;
    acSave: TAction;
    acDecompile: TAction;
    acStepover: TAction;
    acStepinto: TAction;
    acLogout: TAction;
    acRunRemote: TAction;
    acGotoUnit: TAction;
    ActionList1: TActionList;
    cbSyntax: TDBComboBox;
    cbClient: TComboBox;
    DataSource: TDataSource;
    eRunFunction: TEdit;
    EditCopy1: TEditCopy;
    EditPaste1: TEditPaste;
    eSearchC: TEdit;
    GutterImages: TImageList;
    Label2: TLabel;
    MenuItem10: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    Panel4: TPanel;
    Panel5: TPanel;
    PopupMenu2: TPopupMenu;
    PopupMenu3: TPopupMenu;
    SelectData: TDatasource;
    gResults: TDBGrid;
    DBGrid1: TDBGrid;
    FindDialog: TFindDialog;
    ilImageList: TImageList;
    Label1: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    messages: TListBox;
    Panel1: TPanel;
    pLeft: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    pDetail: TPanel;
    PopupMenu1: TPopupMenu;
    BreakPointMenu: TMenuItem;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    ReplaceDialog1: TReplaceDialog;
    Run1: TMenuItem;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    Splitter2: TSplitter;
    StepOver1: TMenuItem;
    StepInto1: TMenuItem;
    N1: TMenuItem;
    Reset1: TMenuItem;
    N2: TMenuItem;
    Run2: TMenuItem;
    Splitter1: TSplitter;
    Open1: TMenuItem;
    Save1: TMenuItem;
    StatusBar: TStatusBar;
    Decompile1: TMenuItem;
    N5: TMenuItem;
    Pause1: TMenuItem;
    ed: TSynEdit;
    HigC: TSynCppSyn;
    SynEditRegexSearch: TSynEditRegexSearch;
    Search1: TMenuItem;
    Find1: TMenuItem;
    Replace1: TMenuItem;
    Searchagain1: TMenuItem;
    N6: TMenuItem;
    Gotolinenumber1: TMenuItem;
    HigSQL: TSynSQLSyn;
    HigPascal: TSynPasSyn;
    HigPython: TSynPythonSyn;
    HigJavaScript: TSynJScriptSyn;
    Syntaxcheck1: TMenuItem;
    tmDebug: TTimer;
    ToolBar1: TToolBar;
    tbTools: TToolBar;
    bRun: TToolButton;
    tbCollapse: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    procedure aButtonClick(Sender: TObject);
    procedure acDecompileExecute(Sender: TObject);
    procedure acGotoUnitExecute(Sender: TObject);
    procedure acLogoutExecute(Sender: TObject);
    procedure acNewExecute(Sender: TObject);
    procedure acPauseExecute(Sender: TObject);
    procedure acResetExecute(Sender: TObject);
    procedure acRunExecute(Sender: TObject);
    procedure acRunRemoteExecute(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
    procedure acStepintoExecute(Sender: TObject);
    procedure acStepoverExecute(Sender: TObject);
    procedure acSyntaxcheckExecute(Sender: TObject);
    procedure aScriptIdle(Sender: TObject);
    procedure aScriptRunLine(Sender: TScript; Module: string; aPosition, Row,
      Col: Integer);
    procedure cbSyntaxSelect(Sender: TObject);
    procedure edChange(Sender: TObject);
    procedure edGutterClick(Sender: TObject; X, Y, Line: integer;
      mark: TSynEditMark);
    procedure EditCopy1Execute(Sender: TObject);
    procedure EditPaste1Execute(Sender: TObject);
    procedure edShowHint(Sender: TObject; HintInfo: PHintInfo);
    procedure edSpecialLineColors(Sender: TObject; Line: Integer; var Special: Boolean; var FG, BG: TColor);
    procedure BreakPointMenuClick(Sender: TObject);
    procedure eSearchCEnter(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure FDataSetDataSetAfterScroll(DataSet: TDataSet);
    procedure FDataSetDataSetBeforeScroll(DataSet: TDataSet);
    procedure FDataSetWriteln(const s: string);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure edStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    function DebuggerNeedFile(Sender: TObject; const OrginFileName: String; var FileName, Output: String): Boolean;
    procedure FscriptCompileMessage(Sender: TScript; Module, Message: string;
      aPosition, Row, Col: Integer);
    procedure FSynCompletionExecute(Sender: TObject);
    procedure FSynCompletionSearchPosition(var APosition: integer);
    procedure FSynCompletionUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char
      );
    procedure MenuItem6Click(Sender: TObject);
    procedure messagesDblClick(Sender: TObject);
    procedure Gotolinenumber1Click(Sender: TObject);
    procedure Find1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure Searchagain1Click(Sender: TObject);
    procedure Replace1Click(Sender: TObject);
    procedure edDropFiles(Sender: TObject; X, Y: Integer;
      AFiles: TStrings);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure tbCollapseClick(Sender: TObject);
    procedure tmDebugTimer(Sender: TObject);
    procedure TPascalScriptToolRegistering(const s: string);
    function TPascalScriptUses(Sender: TPascalScript; const aName: String;
      OnlyAdditional: Boolean): Boolean;
  private
    FOpenUnit: TOpenUnitEvent;
    FSearchFromCaret: boolean;
    FOldStatus : string;
    FActiveLine: Longint;
    FResume: Boolean;
    FSynCompletion: TSynCompletion;
    FActiveFile: string;
    FDataSet : TBaseDBDataset;
    Fuses : TBaseScript;
    FWasRunning: Boolean;
    Linemark : TSynEditMark;
    LastStepTime : Int64;
    FBreakPoints : TList;
    Fscript : TScript;
    procedure InitEvents;
    procedure SetScript(AValue: TScript);
    procedure UpdateStatus;
    procedure ButtonStatus(Status : TScriptStatus);
    function Compile: Boolean;
    function Execute: Boolean;
    function GetBreak(Idx : Integer): TBreakpoint;
    function GetBreakPointCount: Integer;

    function InternalParamStr(Param : Integer) : String;
    function InternalParamCount : Integer;
    procedure SetActiveFile(const Value: string);

    procedure DoSearchReplaceText(AReplace: boolean; ABackwards: boolean);
    procedure SetDataSet(AValue: TBaseDBDataSet);

    property ActiveFile: string read FActiveFile write SetActiveFile;
  public
    FLastCompileStamp: int64;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property DataSet : TBaseDBDataSet read FDataSet write SetDataSet;
    property BreakPointCount : Integer read GetBreakPointCount;
    property BreakPoints[Idx : Integer] : TBreakpoint read GetBreak;
    function HasBreakPoint(Module : string;Line : Integer) : Boolean;
    procedure ClearBreakPoint(Module : string;Line : Integer);
    procedure SetBreakPoint(Module : string;Line : Integer);
    function SaveCheck: Boolean;
    function Execute(aScript: string;aVersion:Variant; aConnection: TComponent = nil;DefScript : string=''): Boolean;
    property OnOpenUnit : TOpenUnitEvent read FOpenUnit write FOpenUnit;
    property Script : TScript read FScript write SetScript;
  end;
  TMessageObject = class
  public
    X : Integer;
    Y : Integer;
    ErrorType : string;
    ModuleName : string;
  end;

var
  fScriptEditor: TfScriptEditor;
  fLastScriptEditor: TfScriptEditor;

implementation

uses
  uFrmGotoLine,uData,uBaseApplication,Utils,uSystemMessage,uStatistic,
  Clipbrd,uprometpascalscript,uprometpythonscript,uprometcscript,uvideofunctions,
  uVideoTest,uPSUtils;

{$R *.lfm}

function GetCurWord(Sender : TCustomSynEdit):string;
var
  S:string;
  i,j:integer;
begin
  Result:='';
  with Sender do
    begin
      S:=Trim(Copy(LineText, 1, CaretX));
      I:=Length(S);
      while (i>0) and (S[i]<>'.') do Dec(I);
      if (I>0) then
      begin
        J:=i-1;
        //Get table name
        while (j>0) and (S[j] in ['A'..'z','"']) do Dec(j);
        Result:=trim(Copy(S, j+1, i-j-1));
      end;
    end;
end;

// options - to be saved to the registry
var
  gbSearchBackwards: boolean;
  gbSearchCaseSensitive: boolean;
  gbSearchFromCaret: boolean;
  gbSearchSelectionOnly: boolean;
  gbSearchTextAtCaret: boolean;
  gbSearchWholeWords: boolean;
  gbSearchRegex: boolean;
  gsSearchText: string;
  gsSearchTextHistory: string;
  gsReplaceText: string;
  gsReplaceTextHistory: string;

resourcestring
  STR_TEXT_NOTFOUND = 'Text not found';
  STR_UNNAMED = 'Unnamed';
  STR_COMPILE_ERROR = 'Fehler beim kompilieren !';
  STR_SUCCESSFULLY_EXECUTED = 'Erfolgreich ausgeführt';
  STR_FORM_TITLE = 'Editor';
  STR_FORM_TITLE_RUNNING = 'Editor - Running';
  STR_INPUTBOX_TITLE = 'Script';
  STR_NOTSAVED = 'Script wurde noch nicht gespeichert, jetzt speichern?';
  strScriptRunning       = 'Das Script wurde gestartet';

procedure DoSleep(aTime: LongInt); StdCall;
var
  bTime: QWord;
begin
  bTime := GetTickCount64;
  while GetTickCount64-bTime < aTime do
    Application.ProcessMessages;
end;

{ TOwnLiemark }

destructor TOwnLinemark.Destroy;
begin
  inherited Destroy;
end;

procedure TfScriptEditor.DoSearchReplaceText(AReplace: boolean; ABackwards: boolean);
var
  Options: TSynSearchOptions;
begin
  Statusbar.SimpleText := '';
  if AReplace then
    Options := [ssoPrompt, ssoReplace, ssoReplaceAll]
  else
    Options := [];
  if ABackwards then
    Include(Options, ssoBackwards);
  if gbSearchCaseSensitive then
    Include(Options, ssoMatchCase);
  if not fSearchFromCaret then
    Include(Options, ssoEntireScope);
  if gbSearchSelectionOnly then
    Include(Options, ssoSelectedOnly);
  if gbSearchWholeWords then
    Include(Options, ssoWholeWord);
  if ed.SearchReplace(gsSearchText, gsReplaceText, Options) = 0 then
  begin
    //MessageBeep(MB_ICONASTERISK);
    Statusbar.SimpleText := STR_TEXT_NOTFOUND;
    if ssoBackwards in Options then
      ed.BlockEnd := ed.BlockBegin
    else
      ed.BlockBegin := ed.BlockEnd;
    ed.CaretXY := ed.BlockBegin;
  end;

end;

procedure TfScriptEditor.SetDataSet(AValue: TBaseDBDataSet);
begin
  if FDataSet=AValue then Exit;
  FDataSet:=AValue;
  if AValue = nil then exit;
  DataSource.DataSet := FDataSet.DataSet;
  FDataSet.DataSet.BeforeScroll:=@FDataSetDataSetBeforeScroll;
  FDataSet.DataSet.AfterScroll:=@FDataSetDataSetAfterScroll;
  FDataSet.DataSet.AfterCancel:=@FDataSetDataSetAfterScroll;
end;

constructor TfScriptEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FBreakPoints:=TList.Create;
  FSynCompletion := TSynCompletion.Create(Self);
  FSynCompletion.CaseSensitive := False;
  FSynCompletion.AddEditor(ed);
  FSynCompletion.OnExecute:=@FSynCompletionExecute;
  FSynCompletion.OnUTF8KeyPress:=@FSynCompletionUTF8KeyPress;
  FSynCompletion.OnSearchPosition:=@FSynCompletionSearchPosition;
  genpascalscript.DoSleep:=@DoSleep;
  Linemark := TOwnLinemark.Create(ed);
  Linemark.ImageList:=GutterImages;
  Linemark.ImageIndex:=8;
  ed.Marks.Add(Linemark);
end;

destructor TfScriptEditor.Destroy;
begin
  FBreakPoints.Free;
  FSynCompletion.Free;
  inherited Destroy;
end;

function TfScriptEditor.HasBreakPoint(Module: string; Line: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to BreakPointCount-1 do
    if (BreakPoints[i].Module=Module)
    and (BreakPoints[i].Line=Line) then
      begin
        Result:=True;
        exit;
      end;
end;

procedure TfScriptEditor.ClearBreakPoint(Module: string; Line: Integer);
var
  i: Integer;
begin
  if not HasBreakPoint(Module,Line) then exit;
  for i := 0 to BreakPointCount-1 do
    if (BreakPoints[i].Module=Module)
    and (BreakPoints[i].Line=Line) then
      begin
        FBreakPoints.Delete(i);
        exit;
      end;
end;

procedure TfScriptEditor.SetBreakPoint(Module: string; Line: Integer);
var
  aBreak: TBreakpoint;
begin
  if HasBreakPoint(Module,Line) then exit;
  aBreak := TBreakpoint.Create;
  aBreak.Module:=Module;
  aBreak.Line:=Line;
  FBreakPoints.Add(aBreak);
end;

procedure TfScriptEditor.edSpecialLineColors(Sender: TObject; Line: Integer;
  var Special: Boolean; var FG, BG: TColor);
begin
  if HasBreakPoint(ActiveFile, Line) then
  begin
    Special := True;
    if Line = FActiveLine then
    begin
      BG := clWhite;
      FG := clRed;
    end else
    begin
      FG := clWhite;
      BG := clRed;
    end;
  end else
  if Line = FActiveLine then
  begin
    Special := True;
    FG := clWhite;
    bg := clBlue;
  end else Special := False;
end;

procedure TfScriptEditor.edGutterClick(Sender: TObject; X, Y, Line: integer;
  mark: TSynEditMark);
var
  i: Integer;
  m: TSynEditMark;
begin
  if (ed.Highlighter=HigPascal)
  or (ed.Highlighter=HigPython)
  then
    begin
      if HasBreakPoint(ActiveFile, Line) then
        begin
          ClearBreakPoint(ActiveFile, Line);
          i := 0;
          while i<ed.Marks.Count do
            begin
              if (ed.Marks[i].Line=Line) and (ed.Marks[i] <> Linemark) then
                ed.Marks[i].Free
              else inc(i);
            end;
        end
      else
        begin
          SetBreakPoint(ActiveFile, Line);
          m := TSynEditMark.Create(ed);
          m.Line := Line;
          m.ImageList := GutterImages;
          m.ImageIndex := 1;
          m.Visible := true;
          ed.Marks.Add(m);
        end;
    end;
  ed.Refresh;
end;

procedure TfScriptEditor.EditCopy1Execute(Sender: TObject);
begin
  if ed.Focused then
    ed.CopyToClipboard;
end;

procedure TfScriptEditor.EditPaste1Execute(Sender: TObject);
begin
  if ed.Focused then
    ed.PasteFromClipboard;
end;

procedure TfScriptEditor.edShowHint(Sender: TObject; HintInfo: PHintInfo);
var
  ASynEdit: TSynEdit;
  EditPos: types.TPoint;
  EditCaret: Classes.TPoint;
  aWord: String;
  i: Integer;
  aCont: String;
begin
  ASynEdit:=ed;
  EditPos:=HintInfo^.CursorPos;
  HintInfo^.HideTimeout:=30000;
  if not PtInRect(ASynEdit.ClientRect,EditPos) then exit;
  EditCaret:=ASynEdit.PhysicalToLogicalPos(ASynEdit.PixelsToRowColumn(EditPos));
  if (EditCaret.Y<1) then exit;
  aWord := ASynEdit.GetWordAtRowCol(EditCaret);
  for i := 0 to LoadedLibs.Count-1 do
    if lowercase(TLoadedLib(LoadedLibs[i]).Name)=lowercase(aWord) then
      begin
        HintInfo^.HintStr:=TLoadedLib(LoadedLibs[i]).Code;
        exit;
      end;
  try
    if Assigned(Script) then
      begin
        aCont := Script.GetVarContents(aWord);
        if aCont<>'' then
          begin
            HintInfo^.HintStr:=aWord+':'+aCont;
          end;
      end;
  except
  end;
end;

procedure TfScriptEditor.acSyntaxcheckExecute(Sender: TObject);
begin
 Compile;
end;

procedure TfScriptEditor.aScriptIdle(Sender: TObject);
begin
  Application.ProcessMessages;
  if (not Linemark.Visible) then
    begin
      ButtonStatus(TBaseScript(FDataSet).Script.Status);
    end;
  if FResume then
    begin
      FResume := False;
      if Assigned(Script) then
        Script.Resume;
      FActiveLine := 0;
      Linemark.Visible:=False;
      ed.Refresh;
    end;
end;

procedure TfScriptEditor.aScriptRunLine(Sender: TScript; Module: string;
  aPosition, Row, Col: Integer);
var
  mo: TMessageObject;
begin
 if Module='' then Module:=ActiveFile;
 try
   if Assigned(Data) and (not FDataSet.Active) then exit;
   if (Module=ActiveFile) and ((Script.Status<>ssRunning) or HasBreakPoint(Module, Row)) then
     begin
       Linemark.Visible:=True;
       Linemark.Line:=Row;
       if Assigned(BaseApplication) then with BaseApplication as IBaseApplication do
         begin
           Debug('Script:'+Module+':'+IntToStr(Row));
         end;
       if HasBreakPoint(Module, Row) then
         begin
           Linemark.ImageIndex:=9;
           if Assigned(Script) then
             if not Script.Pause then
               Script.Stop;
           Linemark.Visible:=False;
         end
       else Linemark.ImageIndex:=8;
       //Mark active Line
       FActiveLine := Row;
       if (FActiveLine < ed.TopLine +2) or (FActiveLine > Ed.TopLine + Ed.LinesInWindow -2) then
       begin
         Ed.TopLine := FActiveLine - (Ed.LinesInWindow div 2);
       end;
       ed.CaretY := FActiveLine;
       ed.CaretX := 1;
       ed.Refresh;
       ButtonStatus(Script.Status);
       Application.ProcessMessages;
     end
   else
     begin
       if Assigned(BaseApplication) then with BaseApplication as IBaseApplication do Debug('Script:'+Module+':'+IntToStr(Row));
       if LineMark.Visible then
         begin
           mo := TMessageObject.Create;
           mo.ModuleName:=Module;
           mo.Y:=Row;
           messages.AddItem('Unit:'+Module+':'+IntToStr(Row),mo);
           messages.ItemIndex:=messages.Items.Count-1;
           messages.MakeCurrentVisible;
         end;
       if GetTickCount-LastStepTime > 10 then
         begin
           Application.ProcessMessages;
           LastStepTime:=GetTickCount;
         end;
     end;
 except
 end;
end;

procedure TfScriptEditor.cbSyntaxSelect(Sender: TObject);
begin
  ed.Highlighter := TSynCustomHighlighter(FindComponent('Hig'+cbSyntax.Text));
  acStepinto.Enabled:=(ed.Highlighter=HigPascal) or (ed.Highlighter=HigPython) and (cbClient.Text='');
  acStepover.Enabled:=(ed.Highlighter=HigPascal) or (ed.Highlighter=HigPython) and (cbClient.Text='');
  TBaseScript(FDataSet).ResetScript;
  Script := nil;
end;

procedure TfScriptEditor.edChange(Sender: TObject);
begin
 if Assigned(Data) then
   begin
     FDataSet.Edit;
     FDataSet.FieldByName('SCRIPT').AsString:=ed.Lines.Text;
     FDataSet.FieldByName('FOLDSTATE').AsString:=ed.FoldState;
   end;
 UpdateStatus;
end;

procedure TfScriptEditor.acDecompileExecute(Sender: TObject);
begin
 if ed.Highlighter=HigPascal then
   begin
     {
      if Compile then
        begin
          Debugger.GetCompiled(s);
          IFPS3DataToText(s, s);
          messages.AddItem(s,nil);
        end;
        }
   end
 else if ed.Highlighter=HigSQL then
   begin
     messages.AddItem(ReplaceSQLFunctions(ed.Lines.Text),nil);
   end;
end;

procedure TfScriptEditor.acGotoUnitExecute(Sender: TObject);
begin
 if Assigned(OnOpenUnit) then
   begin
     OnOpenUnit(Self.ed.GetWordAtRowCol(Self.ed.CaretXY),0,0);
   end;
end;

procedure TfScriptEditor.aButtonClick(Sender: TObject);
begin
  if (FDataSet is TBaseScript) and (Script is TPascalScript) then
    TPascalScript(Script).OpenTool(TToolButton(Sender).Caption);
end;

procedure TfScriptEditor.acLogoutExecute(Sender: TObject);
begin
  with Application as IBaseApplication do
    Logout;
  Application.Terminate;
end;

procedure TfScriptEditor.acNewExecute(Sender: TObject);
begin
  FDataSet.Insert;
end;

procedure TfScriptEditor.acPauseExecute(Sender: TObject);
begin
  if Assigned(script) then
    if script.Pause then
      ButtonStatus(ssPaused);
end;

procedure TfScriptEditor.acResetExecute(Sender: TObject);
begin
  if Assigned(Script) then
    if Script.Stop then
      begin
        DoCleanUp;
        ButtonStatus(ssNone);
        Linemark.Visible:=False;
        FActiveLine := 0;
      end;
end;

procedure TfScriptEditor.acRunExecute(Sender: TObject);
var
  sl: TStringList;
  i: Integer;
  tmp: TCaption;
  tmp1: String;
  aParams : array of Variant;
begin
  Linemark.Visible:=False;
  Linemark.Line:=0;
  //Application.Processmessages;
  if Assigned(Script) and (Script.Status<>ssNone) then
    begin
      FActiveLine := 0;
      Script.Resume;
      ButtonStatus(ssRunning);
    end
  else
    begin
      ButtonStatus(ssCompiling);
      LastStepTime := GetTickCount;
      gResults.Visible := False;
      messages.Visible := True;
      fLastScriptEditor := Self;
      sl := TStringList.Create;
      sl.Text:=ed.Text;
      i := 0;
      while i<sl.Count do
        begin
          if (copy(sl[i],0,2)='--')
          or (trim(sl[i])='')
          then
            sl.Delete(i)
          else inc(i);
        end;
      if Assigned(SelectData.DataSet) then
        begin
          SelectData.DataSet.Free;
          SelectData.DataSet := nil;
        end;
      InitEvents;
      messages.Clear;
      acStepinto.Enabled:=False;
      acStepover.Enabled:=False;
      if (ed.Highlighter=HigSQL) and (pos('SELECT ',Uppercase(sl[0]))>0) then
        begin
          gResults.Visible := True;
          messages.Visible := false;
          SelectData.DataSet := Data.GetNewDataSet(ed.Text);
          ButtonStatus(ssRunning);
          Application.ProcessMessages;
          try
            SelectData.DataSet.Open;
          except
            on e : Exception do
              begin
                gResults.Visible := False;
                messages.Visible := True;
                messages.AddItem(e.Message,nil);
              end;
          end;
          ButtonStatus(ssNone);
        end
      else if (FDataSet is TBaseScript) then
        begin
          TBaseScript(FDataSet).writeln := @FDataSetWriteln;
          TBaseScript(FDataSet).debugln := @FDataSetWriteln;
          Script := TBaseScript(FDataSet).Script;
          if Assigned(Script) then
            begin
              Script.OnRunLine:=@aScriptRunLine;
              Script.OnIdle:=@aScriptIdle;
              Script.OnCompileMessage:=@FscriptCompileMessage;
            end;
          if ed.ChangeStamp<>FLastCompileStamp then
            begin
              if Assigned(DataSet) and (DataSet.State<>dsInsert) then
                acSave.Execute;
              if Script is TByteCodeScript then
                TByteCodeScript(Script).ByteCode:='';
            end;
          Setlength(aParams,0);
          try
            if TByteCodeScript(Script).ByteCode='' then
              begin
                TBaseScript(FDataSet).Compile;
                FLastCompileStamp:=ed.ChangeStamp;
              end;
            ButtonStatus(ssRunning);
            Application.ProcessMessages;
            if eRunFunction.Text<>'' then
              begin
                tmp := eRunFunction.Text;
                if pos('(',tmp) > 0 then
                  begin
                    tmp1 := copy(tmp,0,pos('(',tmp)-1);
                    tmp := copy(tmp,pos('(',tmp)+1,length(tmp)-(pos('(',tmp)+1));
                  end
                else
                  begin
                    tmp1 := tmp;
                    tmp := '';
                  end;
                while pos(',',tmp)>0 do
                  begin
                    Setlength(aParams,length(aparams)+1);
                    aParams[length(aParams)-1]:=copy(tmp,0,pos(',',tmp)-1);
                    tmp := copy(tmp,pos(',',tmp)+1,length(tmp));
                  end;
                if tmp<>'' then
                  begin
                    Setlength(aParams,length(aparams)+1);
                    aParams[length(aParams)-1]:=tmp;
                  end;
                TBaseScript(FDataSet).Script.RunScriptFunction(aParams,tmp1);
              end
            else if not TBaseScript(FDataSet).Execute(Null,True) then
              begin
                messages.AddItem('failed to Load Compiled Data',nil);
                if TBaseScript(FDataSet).Script.Results<>'' then
                  messages.AddItem(TBaseScript(FDataSet).Script.Results,nil);;
              end;
          finally
            ButtonStatus(ssNone);
            TBaseScript(FDataSet).Script.Stop;
          end;
        end;
      sl.Free;
    end;
end;

procedure TfScriptEditor.acRunRemoteExecute(Sender: TObject);
begin
  acSave.Execute;
  {
  with BaseApplication as IBaseApplication do
    TMessageHandler(GetMessageManager).SendCommand(cbClient.Text,'ExecuteScript('+eName.Text+')');
  acRunRemote.Enabled:=false;
  FDataSet.DataSet.Refresh;
  FOldStatus := FDataSet.FieldByName('STATUS').AsString;
  FWasRunning:=False;
  tmDebug.Enabled:=True;
  }
end;

procedure TfScriptEditor.acSaveExecute(Sender: TObject);
begin
  if Assigned(Data) then
    begin
      FDataSet.CascadicPost;
    end
  else
    begin

    end;
  ed.Modified := False;
  ed.MarkTextAsSaved;
end;

procedure TfScriptEditor.acStepintoExecute(Sender: TObject);
begin
  if Assigned(Script) then
    Script.StepInto;
end;

procedure TfScriptEditor.acStepoverExecute(Sender: TObject);
begin
  if Assigned(Script) then
    Script.StepOver;
end;

procedure TfScriptEditor.BreakPointMenuClick(Sender: TObject);
var
  Line: Longint;
  m: TSynEditMark;
  i: Integer;
begin
  Line := Ed.CaretY;
  if HasBreakPoint(ActiveFile, Line) then
    begin
      ClearBreakPoint(ActiveFile, Line);
      i := 0;
      while i<ed.Marks.Count-1 do
        begin
          if ed.Marks[i].Line=Line then
            ed.Marks.Delete(i)
          else inc(i);
        end;
    end
  else
    begin
      SetBreakPoint(ActiveFile, Line);
      m := TSynEditMark.Create(ed);
      m.Line := Line;
      m.ImageList := GutterImages;
      m.ImageIndex := 1;
      m.Visible := true;
      ed.Marks.Add(m);
    end;
  ed.Refresh;
end;

procedure TfScriptEditor.eSearchCEnter(Sender: TObject);
begin
 if trim(eSearchC.Text)='<'+strSearch+'>' then
   eSearchC.Clear;
 eSearchC.Font.Color:=clDefault;
end;

procedure TfScriptEditor.Exit1Click(Sender: TObject);
begin
  acReset.Execute; //terminate any running script
  if SaveCheck then //check if script changed and not yet saved
    Close;
end;

function TfScriptEditor.Compile: Boolean;
var
  i: Longint;
  mo: TMessageObject;
begin
  Result := False;
  try
    Result := Result or TBaseScript(FDataSet).Compile;
  except
    on e : Exception do
      Messages.Items.Add(e.Message);
  end;
  messages.Clear;
  if not Result then
    begin
      Messages.Items.Add(STR_COMPILE_ERROR);
    end;
end;

function TfScriptEditor.Execute: Boolean;
begin
  if TBaseScript(FDataSet).Execute(Null) then
  begin
    Messages.Items.Add(STR_SUCCESSFULLY_EXECUTED);
    Result := True; 
  end else
  begin
    Result := False;
  end;
end;

function TfScriptEditor.GetBreak(Idx : Integer): TBreakpoint;
begin
  Result := TBreakpoint(FBreakPoints[Idx]);
end;

function TfScriptEditor.GetBreakPointCount: Integer;
begin
  Result := FBreakPoints.Count;
end;

procedure TfScriptEditor.FDataSetDataSetAfterScroll(DataSet: TDataSet);
begin
 try
   TBaseScript(FDataSet).ResetScript;
   Fscript:=nil;
   ed.Lines.Text:=FDataSet.FieldByName('SCRIPT').AsString;
   ActiveFile := FDataSet.FieldByName('NAME').AsString;
   ed.FoldState := FDataSet.FieldByName('FOLDSTATE').AsString;
   cbSyntaxSelect(cbSyntax);
 except
 end;
end;

procedure TfScriptEditor.FDataSetDataSetBeforeScroll(DataSet: TDataSet);
begin
  SaveCheck;
end;

procedure TfScriptEditor.FDataSetWriteln(const s: string);
begin
 messages.Items.Add(S);
 messages.ItemIndex:=messages.Items.Count-1;
 messages.MakeCurrentVisible;
end;

procedure TfScriptEditor.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  acReset.Execute;
  try
    while Assigned(Fscript) and (Fscript.Status <> ssNone) do
      Application.ProcessMessages;
  except
  end;
end;

procedure TfScriptEditor.FormCreate(Sender: TObject);
begin
  FDataSet:=nil;
  Fuses := nil;
end;

function TfScriptEditor.InternalParamStr(Param: Integer): String;
begin
  result := '';
end;

function TfScriptEditor.InternalParamCount: Integer;
begin
  Result := 0;
end;

//check if script changed and not yet saved//
function TfScriptEditor.SaveCheck: Boolean;
begin
  if ed.Modified then
    begin
      case MessageDlg(STR_NOTSAVED, mtConfirmation, mbYesNoCancel, 0) of
        mrYes:
          begin
            acSave.Execute;
            Result := ActiveFile <> '';
          end;
        mrNo: Result := True;
        else
          Result := False;
      end;
    end else Result := True;
end;

function TfScriptEditor.Execute(aScript: string; aVersion: Variant;
  aConnection: TComponent; DefScript: string): Boolean;
var
  aCreate: Boolean;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfScriptEditor,fScriptEditor);
      Self := fScriptEditor;
    end;
  if Assigned(Data) then  //Database Handling
    begin
      if not Assigned(FDataSet) then
        begin
          FDataSet := TBaseScript.CreateEx(nil,Data,aConnection);
          FDataSet.CreateTable;
        end;
      TBaseScript(FDataSet).SelectByName(aScript);
      FDataSet.Open;
      DataSource.DataSet := FDataSet.DataSet;
      FDataSet.DataSet.BeforeScroll:=@FDataSetDataSetBeforeScroll;
      FDataSet.DataSet.AfterScroll:=@FDataSetDataSetAfterScroll;
      FDataSet.DataSet.AfterCancel:=@FDataSetDataSetAfterScroll;
      aCreate := False;
      if (aVersion=Null) or (aVersion='') then
        if (not FDataSet.Locate('NAME;ACTIVE',VarArrayOf([aScript,'Y']),[loCaseInsensitive])) then
          if (not FDataSet.Locate('NAME',VarArrayOf([aScript]),[loCaseInsensitive])) then
            aCreate := True;
      if not aCreate then
        if (not FDataSet.Locate('NAME;VERSION',VarArrayOf([aScript,aVersion]),[loCaseInsensitive])) or (aScript='') then
          aCreate := True;
      if aCreate or (aScript='') then
        begin
          FDataSet.Insert;
          FDataSet.FieldByName('NAME').AsString:=aScript;
          FDataSet.FieldByName('VERSION').AsVariant:=aVersion;
          if DefScript<>'' then
            FDataSet.FieldByName('SCRIPT').AsString:=DefScript;
        end
      else
        FDataSetDataSetAfterScroll(FDataSet.DataSet);
      Script := TBaseScript(FDataSet).Script;
    end
  else //File Handling
    begin
      cbSyntax.Enabled:=False;
      ed.Highlighter:=HigPascal;
      //TODO:load from file
    end;
  Result := Showmodal = mrOK;
  if Assigned(Data) then
    begin
      if Result then
        FDataSet.Post;
      FDataSet.Close;
    end;
end;

procedure TfScriptEditor.edStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  UpdateStatus;
  acSave.Enabled := ed.Modified or (Assigned(FDataset) and (FDataSet.CanEdit));
end;

function TfScriptEditor.DebuggerNeedFile(Sender: TObject; const OrginFileName: String;
  var FileName, Output: String): Boolean;
var
  path: string;
begin
  if Assigned(Data) then
    begin
      if not Assigned(Fuses) then
        begin
          Fuses := TBaseScript.Create(nil);
          Fuses.Open;
        end
      else Fuses.DataSet.Refresh;
      Result := Fuses.Locate('NAME',FileName,[loCaseInsensitive]);
      if Result then
        Output:=Fuses.FieldByName('SCRIPT').AsString;
    end
  else
    begin
      //TODO:File Handling
    end;
end;

procedure TfScriptEditor.FscriptCompileMessage(Sender: TScript; Module,
  Message: string; aPosition, Row, Col: Integer);
begin
  Messages.Items.AddObject(Message,nil);
end;

procedure TfScriptEditor.FSynCompletionExecute(Sender: TObject);
var
  i: Integer;
  aStatement: String;
  s: String;
  aStrings: TStrings;
  ps : PChar;
  a: Integer;
  sl: TStringList;
begin
  with FSynCompletion.ItemList do
    begin
      Clear;
      s := GetCurWord(TSynCompletion(Sender).Editor);
      if cbSyntax.Text='SQL' then
        begin
          if s = '' then
            begin
              for i := 0 to Data.Tables.Count-1 do
                Add(Data.Tables[i]);
              Add('select');
              Add('insert');
              Add('update');
              Add('delete');
              Add('from');
              Add('where');
              Add('into');
              Add('order by');
              Add('group by');
            end;
        end
      else if cbSyntax.Text='Pascal' then
        begin
          sl := TStringList.Create;
          try
            SplitRegExpr('(.*):(.*);',FDataSet.FieldByName('SCRIPT').AsString,sl);
            for a := 0 to sl.Count-1 do
              Add(sl[a]);
          except
          end;
          sl.Free;
        end;
    end;
end;

procedure TfScriptEditor.FSynCompletionSearchPosition(var APosition: integer);
var
  i: Integer;
begin
  for i := 0 to FSynCompletion.ItemList.Count-1 do
    if Uppercase(copy(FSynCompletion.ItemList[i],0,length(FSynCompletion.CurrentString))) = Uppercase(FSynCompletion.CurrentString) then
      begin
        aPosition := i;
        FSynCompletion.TheForm.Position:=i-1;
        FSynCompletion.TheForm.Position:=i;
        exit;
      end;
end;

procedure TfScriptEditor.FSynCompletionUTF8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
begin
  if (length(UTF8Key)=1) and (System.Pos(UTF8Key[1],FSynCompletion.EndOfTokenChr)>0) then
    begin
      FSynCompletion.TheForm.OnValidate(Sender,UTF8Key,[]);
      UTF8Key:='';
    end
end;

procedure TfScriptEditor.MenuItem6Click(Sender: TObject);
begin
  if messages.ItemIndex>-1 then
    Clipboard.AsText:=messages.Items[messages.ItemIndex];
end;

procedure TfScriptEditor.SetActiveFile(const Value: string);
begin
  FActiveFile := Value;
end;

function GetErrorRowCol(const inStr: string): TPoint;
var
  Row:string;
  Col:string;
  p1,p2,p3:integer;
begin
  p1:=Pos('(',inStr);
  p2:=Pos(':',inStr);
  p3:=Pos(')',inStr);
  if (p1>0) and (p2>p1) and (p3>p2) then
  begin
    Row := Copy(inStr, p1+1,p2-p1-1);
    Col := Copy(inStr, p2+1,p3-p2-1);
    Result.x := StrToInt(Trim(Col));
    Result.y := StrToInt(Trim(Row));
  end
  else
  begin
    Result.x := 1;
    Result.y := 1;
  end
end;

procedure TfScriptEditor.messagesDblClick(Sender: TObject);
var
  mo: TObject;
  Found: Boolean = False;
begin
 mo := messages.Items.Objects[messages.ItemIndex];
 try
 if Assigned(mo) then
   begin
     ed.CaretY:=TMessageObject(mo).Y;
     ed.CaretX:=TMessageObject(mo).X;
     if TMessageObject(mo).ModuleName<>ActiveFile then
       if Assigned(OnOpenUnit) then
         begin
           OnOpenUnit(TMessageObject(mo).ModuleName,TMessageObject(mo).X,TMessageObject(mo).Y);
           Found := True;
         end;
     if not Found then
       begin
         if ed.CanFocus then
           ed.SetFocus;
       end;
   end;
 except
 end;
end;

procedure TfScriptEditor.Gotolinenumber1Click(Sender: TObject);
begin
  with TfrmGotoLine.Create(self) do
  try
    Char := ed.CaretX;
    Line := ed.CaretY;
    ShowModal;
    if ModalResult = mrOK then
      ed.CaretXY := CaretXY;
  finally
    Free;
    try
      ed.SetFocus;
    except
    end;
  end;
end;

procedure TfScriptEditor.Find1Click(Sender: TObject);
begin
end;

procedure TfScriptEditor.Open1Click(Sender: TObject);
begin

end;

procedure TfScriptEditor.Searchagain1Click(Sender: TObject);
begin
  DoSearchReplaceText(FALSE, FALSE);
end;

procedure TfScriptEditor.Replace1Click(Sender: TObject);
begin
end;

procedure TfScriptEditor.edDropFiles(Sender: TObject; X, Y: Integer;
  AFiles: TStrings);
begin
 if AFiles.Count>=1 then
  if SaveCheck then //check if script changed and not yet saved
    begin
      FDataSet.Insert;
      ed.ClearAll;
      ed.Lines.LoadFromFile(AFiles[0]);
      ed.Modified := True;
      ActiveFile := AFiles[0];
    end;
end;

procedure TfScriptEditor.SpeedButton1Click(Sender: TObject);
begin
 ed.SearchReplace(eSearchC.Text,'',[ssoFindContinue]);
end;

procedure TfScriptEditor.SpeedButton2Click(Sender: TObject);
begin
 ed.SearchReplace(eSearchC.Text,'',[ssoFindContinue,ssoBackwards]);
end;

procedure TfScriptEditor.tbCollapseClick(Sender: TObject);
begin
  if tbCollapse.Down then
    ed.UnfoldAll
  else
    ed.FoldAll(1);
  ed.Modified:=True;
  edChange(ed);
end;

procedure TfScriptEditor.tmDebugTimer(Sender: TObject);
var
  sl: TStringList;
  i: Integer;
begin
 FDataSet.DataSet.Refresh;
 if FDataSet.FieldByName('STATUS').AsString=FOldStatus then exit;
 FOldStatus:=FDataSet.FieldByName('STATUS').AsString;
 if FDataSet.FieldByName('STATUS').AsString='R' then
   begin
     messages.AddItem(strScriptRunning,nil);
     FWasRunning := True;
   end
 else if FWasRunning then
   begin
     acRunRemote.Enabled:=True;
     tmDebug.Enabled:=False;
     sl := TStringList.Create;
     sl.Text:=FDataSet.FieldByName('LASTRESULT').AsString;
     for i := 0 to sl.Count-1 do
       messages.AddItem(sl[i],nil);
     sl.Free;
   end;
end;

procedure TfScriptEditor.TPascalScriptToolRegistering(const s: string);
var
  i: Integer;
  aButton: TToolButton;
begin
  for i := 0 to tbTools.ButtonList.Count-1 do
    if tbTools.Buttons[i].Caption=s then
      exit;
  aButton := TToolButton.Create(tbTools);
  aButton.Parent:=tbTools;
  aButton.Caption:=s;
  aButton.ShowCaption:=True;
  aButton.Hint:=s;
  aButton.OnClick:=@aButtonClick;
end;

function TfScriptEditor.TPascalScriptUses(Sender: TPascalScript;
  const aName: String; OnlyAdditional: Boolean): Boolean;
begin
  Result:=False;
  if lowercase(aName) = 'video' then
    begin
      Sender.Compiler.AddTypeS('TFPColor','record red : word;green : word;blue : word;alpha : word; end;');
      Sender.Compiler.AddTypeS('THLSColor','record h : word;l : word;s : word;end;');
      Sender.AddFunctionEx(@CopyToWorkArea,'procedure CopyToWorkArea(x,y,width,height : Integer);',cdStdCall);
      Sender.AddFunctionEx(@ScaleImage,'procedure ScaleImage(NewWidth : Integer;NewHeight : Integer);',cdStdCall);
      Sender.AddFunctionEx(@ImageWidth,'function ImageWidth : Integer;stdcall;',cdStdCall);
      Sender.AddFunctionEx(@ImageHeight,'function ImageHeight : Integer;stdcall;',cdStdCall);
      Sender.AddFunctionEx(@SetPixel,'procedure SetPixel(x,y : Integer; r,g,b : word);',cdStdCall);
      Sender.AddFunctionEx(@SetPixelHLS,'procedure SetPixelHLS(x,y : Integer; h,l,s : word);',cdStdCall);
      Sender.AddFunctionEx(@GetPixel,'function GetPixel(x,y : Integer) : TFPColor;stdcall;',cdStdCall);
      Sender.AddFunctionEx(@GetPixelHLS,'function GetPixelHLS(x,y : Integer) : THLSColor;',cdStdCall);
      Sender.AddFunctionEx(@RefreshImageVT,'procedure RefreshImage;stdcall;',cdStdCall);
      Sender.AddFunctionEx(@LoadImageVT,'function LoadImage(aFile : PChar) : Boolean;',cdStdCall);
      Sender.AddFunctionEx(@SaveImage,'function SaveImage(aFile : PChar) : Boolean;',cdStdCall);
      Sender.AddFunctionEx(@ReloadWorkImage,'function ReloadWorkImage(aFile : PChar) : Boolean;',cdStdCall);
      Sender.AddFunctionEx(@SaveWorkImage,'function SaveWorkImage(aFile : PChar) : Boolean;',cdStdCall);
      Sender.AddFunctionEx(@CaptureImageVT,'function CaptureImage(dev: PChar;Width,Height : Integer): Boolean;',cdStdCall);
      Result := True;
    end;
end;

procedure TfScriptEditor.InitEvents;
var
  aScript: TScript;
begin
  aScript := Script;
  if Assigned(aScript) then
    begin
      aScript.OnIdle:=@aScriptIdle;
      aScript.OnRunLine:=@aScriptRunLine;
      if aScript is TPascalScript then
       begin
         TPascalScript(aScript).OnToolRegistering:=@TPascalScriptToolRegistering;
         TPascalScript(aScript).OnUses:=@TPascalScriptUses;
       end;
    end;
end;

procedure TfScriptEditor.SetScript(AValue: TScript);
begin
  if FScript=AValue then Exit;
  FScript:=AValue;
end;

procedure TfScriptEditor.UpdateStatus;
begin
  StatusBar.Panels[0].Text:=Format('%d:%d',[ed.CaretX, ed.CaretY]);
  StatusBar.Panels[1].Text:=IntToStr(Length(ed.Lines.Text));
end;

procedure TfScriptEditor.ButtonStatus(Status: TScriptStatus);
begin
  case Status of
  ssCompiling:
    begin
      bRun.Action:=acSyntaxcheck;
      acSyntaxcheck.Enabled:=False;
      acPause.Enabled:=False;
      acReset.Enabled:=False;
      acStepinto.Enabled:=False;
      acStepover.Enabled:=False;
    end;
  ssRunning:
    begin
      acSyntaxcheck.Enabled:=True;
      bRun.Action:=acRun;
      acPause.Enabled:=True;
      acReset.Enabled:=True;
      acRun.Enabled:=False;
      acStepinto.Enabled:=False;
      acStepover.Enabled:=False;
    end;
  ssPaused:
    begin
      acSyntaxcheck.Enabled:=True;
      bRun.Action:=acRun;
      acPause.Enabled:=False;
      acReset.Enabled:=True;
      acRun.Enabled:=True;
      acStepinto.Enabled:=True;
      acStepover.Enabled:=True;
    end;
  ssNone:
    begin
      acSyntaxcheck.Enabled:=True;
      bRun.Action:=acRun;
      acPause.Enabled:=False;
      acReset.Enabled:=False;
      acRun.Enabled:=True;
      acStepinto.Enabled:=True;
      acStepover.Enabled:=False;
    end;
  end;
  Application.ProcessMessages;
end;

initialization
  RegisterPropertyToSkip(TSynEdit, 'MouseTextActions', 'Not available in Laz 1.0', '');
  RegisterPropertyToSkip(TSynEdit, 'VisibleSpecialChars', 'Not available in Laz 1.0', '');
  DoInputBox:=@InputBox;
end.

