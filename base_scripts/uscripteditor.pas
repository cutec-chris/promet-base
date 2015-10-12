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
  RegExpr, LResources, uPSRuntime, uPSDisassembly, uPSUtils, uPSComponent,
  uPSDebugger, uPSComponent_DB, SynEditRegexSearch, SynEditSearch,
  SynEditMiscClasses, SynEditHighlighter, SynGutterBase, SynEditMarks,
  SynEditMarkupSpecialLine, SynHighlighterSQL, SynHighlighterPython,
  SynHighlighterCpp, uPSCompiler, uprometscripts, LCLIntf, genscript,
  uBaseDbClasses, variants;

type
  TOpenUnitEvent = procedure(UnitName : string;X,Y : Integer) of object;

  TBreakpoint = class
  public
    Module : string;
    Line : Integer;
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
    ActionList1: TActionList;
    cbSyntax: TDBComboBox;
    cbClient: TComboBox;
    DataSource: TDataSource;
    GutterImages: TImageList;
    Label2: TLabel;
    MenuItem6: TMenuItem;
    PopupMenu2: TPopupMenu;
    SelectData: TDatasource;
    gResults: TDBGrid;
    DBGrid1: TDBGrid;
    FindDialog: TFindDialog;
    IFPS3DllPlugin1: TPSDllPlugin;
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
    HigCJavaScript: TSynCppSyn;
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
    Syntaxcheck1: TMenuItem;
    tmDebug: TTimer;
    ToolBar1: TToolBar;
    tbTools: TToolBar;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    procedure aButtonClick(Sender: TObject);
    procedure acDecompileExecute(Sender: TObject);
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
    procedure DebuggerExecImport(Sender: TObject; se: TPSExec;
      x: TPSRuntimeClassImporter);
    function DebuggerGetNotificationVariant(Sender: TPSScript;
      const aName: tbtstring): Variant;
    procedure edChange(Sender: TObject);
    procedure edGutterClick(Sender: TObject; X, Y, Line: integer;
      mark: TSynEditMark);
    procedure edShowHint(Sender: TObject; HintInfo: PHintInfo);
    procedure edSpecialLineColors(Sender: TObject; Line: Integer; var Special: Boolean; var FG, BG: TColor);
    procedure BreakPointMenuClick(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure DebuggerCompile(Sender: TPSScript);
    procedure FDataSetDataSetAfterScroll(DataSet: TDataSet);
    procedure FDataSetDataSetBeforeScroll(DataSet: TDataSet);
    procedure FDataSetWriteln(const s: string);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure edStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    function DebuggerNeedFile(Sender: TObject; const OrginFileName: String; var FileName, Output: String): Boolean;
    procedure DebuggerBreakpoint(Sender: TObject; const FileName: String; bPosition, Row, Col: Cardinal);
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
    procedure tmDebugTimer(Sender: TObject);
    procedure TPascalScriptToolRegistering(const s: string);
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
    FOldUses : TPSOnUses;
    FWasRunning: Boolean;
    Linemark : TSynEditMark;
    LastStepTime : Int64;
    FBreakPoints : TList;
    Fscript : TScript;
    ClassImporter: uPSRuntime.TPSRuntimeClassImporter;
    procedure InitEvents;
    procedure ButtonStatus(Status : TScriptStatus);
    function Compile: Boolean;
    function Execute: Boolean;
    function GetBreak(Idx : Integer): TBreakpoint;
    function GetBreakPointCount: Integer;

    function InternalParamStr(Param : Integer) : String;
    function InternalParamCount : Integer;
    procedure InternalSleep(MiliSecValue: LongInt);
    procedure SetActiveFile(const Value: string);

    procedure DoSearchReplaceText(AReplace: boolean; ABackwards: boolean);
    procedure SetDataSet(AValue: TBaseDBDataSet);

    property ActiveFile: string read FActiveFile write SetActiveFile;
  public
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
  uFrmGotoLine,uData,uBaseApplication,genpascalscript,Utils,uSystemMessage,uStatistic,
  Clipbrd,uprometpascalscript,uprometpythonscript;

{$R *.lfm}

const
  isRunningOrPaused = [isRunning, isPaused];

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
  STR_RUNTIME_ERROR='[Laufzeitfehler] %s(%d:%d), bytecode(%d:%d): %s'; //Birb
  STR_FORM_TITLE = 'Editor';
  STR_FORM_TITLE_RUNNING = 'Editor - Running';
  STR_INPUTBOX_TITLE = 'Script';
  STR_NOTSAVED = 'Script wurde noch nicht gespeichert, jetzt speichern?';
  strScriptRunning       = 'Das Script wurde gestartet';
function OnUses(Sender: TPSPascalCompiler; const Name: tbtString): Boolean;
begin
  Result := False;
  if (Assigned(flastScriptEditor) and Assigned(Data)) and (fLastScriptEditor.FDataSet is TBaseScript) and (TBaseScript(fLastScriptEditor.FDataSet).Script is TPascalScript) then
    Result := TPascalScript(TBaseScript(flastScriptEditor.FDataSet).Script).InternalUses(Sender,Name);
  if not Result and (Assigned(flastScriptEditor) and Assigned(Data)) and (fLastScriptEditor.FDataSet is TBaseScript) and (TBaseScript(fLastScriptEditor.FDataSet).Script is TPascalScript) then
    Result := TPascalScript(TBaseScript(fLastScriptEditor.FDataSet).Script).InternalUses(Sender,Name)
end;

procedure DoSleep(aTime: LongInt); StdCall;
var
  bTime: QWord;
begin
 bTime := GetTickCount64;
 while GetTickCount64-bTime < aTime do
   Application.ProcessMessages;
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
  if ed.Highlighter=HigPascal then
    begin
      if HasBreakPoint(ActiveFile, Line) then
        begin
          ClearBreakPoint(ActiveFile, Line);
          i := 0;
          while i<ed.Marks.Count do
            begin
              if ed.Marks[i].Line=Line then
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

procedure TfScriptEditor.edShowHint(Sender: TObject; HintInfo: PHintInfo);
var
  ASynEdit: TSynEdit;
  EditPos: types.TPoint;
  EditCaret: Classes.TPoint;
  aWord: String;
  i: Integer;
  aCont: TbtString;
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
      end;
 {
 if Debugger.Running then
   begin
     aCont := Debugger.GetVarContents(aWord);
     if aCont<>'' then
       begin
         HintInfo^.HintStr:=aWord+':'+aCont;
       end;
   end;
 }
end;

procedure TfScriptEditor.acSyntaxcheckExecute(Sender: TObject);
begin
 Compile;
end;

procedure TfScriptEditor.aScriptIdle(Sender: TObject);
begin
  Application.ProcessMessages;
  if FResume then
    begin
      FResume := False;
      if Assigned(FScript) then
        FScript.Resume;
      FActiveLine := 0;
      ed.Refresh;
    end;
end;

procedure TfScriptEditor.aScriptRunLine(Sender: TScript; Module: string;
  aPosition, Row, Col: Integer);
begin
 try
   if Assigned(Data) and (not FDataSet.Active) then exit;
   if ((Module=ActiveFile) or (Module='')) and (FScript.Status<>ssRunning) then
     begin
       //Set mark
       if not Assigned(LineMark) then
         begin
           Linemark := TSynEditMark.Create(ed);
           Linemark.ImageList:=GutterImages;
           Linemark.ImageIndex:=8;
           ed.Marks.Add(Linemark);
           Linemark.Visible:=True;
         end;
       Linemark.Line:=Row;
       if HasBreakPoint(Module, Row) then
         begin
           Linemark.ImageIndex:=9;
           if Assigned(FScript) then
             if not FScript.Pause then
               FScript.Stop;
         end
       else Linemark.ImageIndex:=8;
       with BaseApplication as IBaseApplication do
         begin
           Debug('Script:'+Module+':'+IntToStr(Row));
         end;
       //Mark active Line
       FActiveLine := Row;
       if (FActiveLine < ed.TopLine +2) or (FActiveLine > Ed.TopLine + Ed.LinesInWindow -2) then
       begin
         Ed.TopLine := FActiveLine - (Ed.LinesInWindow div 2);
       end;
       ed.CaretY := FActiveLine;
       ed.CaretX := 1;
       ed.Refresh;
       Application.ProcessMessages;
     end
   else
     begin
       with BaseApplication as IBaseApplication do Debug('Script:'+Module+':'+IntToStr(Row));
       if GetTickCount-LastStepTime > 10 then
         Application.ProcessMessages;
       LastStepTime:=GetTickCount;
     end;
 except
 end;
end;

procedure TfScriptEditor.cbSyntaxSelect(Sender: TObject);
begin
  ed.Highlighter := TSynCustomHighlighter(FindComponent('Hig'+cbSyntax.Text));
  acStepinto.Enabled:=(ed.Highlighter=HigPascal) and (cbClient.Text='');
  acStepover.Enabled:=(ed.Highlighter=HigPascal) and (cbClient.Text='');
end;

procedure TfScriptEditor.DebuggerExecImport(Sender: TObject; se: TPSExec;
  x: TPSRuntimeClassImporter);
begin
  if Assigned(Data) and (FDataSet is TBaseScript) and (FScript is TPascalScript) then
    TPascalScript(FScript).ClassImporter:=x;
end;

function TfScriptEditor.DebuggerGetNotificationVariant(Sender: TPSScript;
  const aName: tbtstring): Variant;
begin
  Showmessage(aName);
end;

procedure TfScriptEditor.edChange(Sender: TObject);
begin
 if Assigned(Data) then
   begin
     FDataSet.Edit;
     FDataSet.FieldByName('SCRIPT').AsString:=ed.Lines.Text;
     FDataSet.FieldByName('FOLDSTATE').AsString:=ed.FoldState;
   end;
end;

procedure TfScriptEditor.acDecompileExecute(Sender: TObject);
var
  s: tbtstring;
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

procedure TfScriptEditor.aButtonClick(Sender: TObject);
begin
  if (FDataSet is TBaseScript) and (FScript is TPascalScript) then
    TPascalScript(FScript).OpenTool(TToolButton(Sender).Caption);
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
  if ed.Highlighter=HigPascal then
    begin
      if (FDataSet is TBaseScript) and (FScript is TPascalScript) then
        with TPascalScript(FScript) do
          begin
            if Runtime is TPSDebugExec then
              begin
                TPSDebugExec(Runtime).Pause;
                TPSDebugExec(Runtime).StepInto;
                acRun.Enabled:=True;
              end;
          end;
      {
      if Debugger.Exec.Status = isRunning then
        begin
          Debugger.Pause;
          Debugger.StepInto;
          acRun.Enabled:=True;
        end;
       }
      acStepinto.Enabled:=acPause.Enabled;
      acStepover.Enabled:=acPause.Enabled;
    end
  else
    begin
      acStepinto.Enabled:=False;
      acStepover.Enabled:=False;
    end;
end;

procedure TfScriptEditor.acResetExecute(Sender: TObject);
begin
  if ed.Highlighter=HigPascal then
    begin
      if Assigned(FScript) then
        if FScript.Stop then
          begin
            DoCleanUp;
            acRun.Enabled:=True;
            acPause.Enabled:=false;
            acReset.Enabled:=false;
            acStepinto.Enabled:=acPause.Enabled or acRun.Enabled;
            acStepover.Enabled:=acPause.Enabled or acRun.Enabled;
          end;
    end
  else
    begin
      acStepinto.Enabled:=False;
      acStepover.Enabled:=False;
    end;
end;

procedure TfScriptEditor.acRunExecute(Sender: TObject);
var
  sl: TStringList;
  i: Integer;
begin
  LastStepTime := GetTickCount;
  gResults.Visible := False;
  messages.Visible := True;
  fLastScriptEditor := Self;
  sl := TStringList.Create;
  sl.Text:=ed.Text;
  i := 0;
  while i<sl.Count do
    begin
      if copy(sl[i],0,2)='--' then
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
  if (FDataSet is TBaseScript) then
    begin
      TBaseScript(FDataSet).writeln := @FDataSetWriteln;
      acSave.Execute;
      ButtonStatus(ssRunning);
      if not TBaseScript(FDataSet).Execute(Null,True) then
        messages.AddItem('failed to executing',nil);
      ButtonStatus(FScript.Status);
    end;
  sl.Free;
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
  if Assigned(FScript) then
    FScript.StepInto;
  ButtonStatus(FScript.Status);
end;

procedure TfScriptEditor.acStepoverExecute(Sender: TObject);
begin
  if Assigned(FScript) then
    FScript.StepOver;
  ButtonStatus(FScript.Status);
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
  aMsg: TPSPascalCompilerMessage;
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
      {
      for i := 0 to Debugger.CompilerMessageCount -1 do
        begin
          aMsg := Debugger.CompilerMessages[i];
          mo := TMessageObject.Create;
          mo.X:=aMsg.Col;
          mo.Y:=aMsg.Row;
          mo.ModuleName:=aMsg.ModuleName;
          mo.ErrorType := aMsg.ErrorType;
          Messages.Items.AddObject(aMsg.MessageToString,mo);
        end;
      if Debugger.CompilerMessageCount=0 then
        messages.Items.Add(Debugger.ExecErrorToString);
      }
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
    //messages.Items.Add(Format(STR_RUNTIME_ERROR, [extractFileName(aFile), Debugger.ExecErrorRow,Debugger.ExecErrorCol,Debugger.ExecErrorProcNo,Debugger.ExecErrorByteCodePosition,Debugger.ExecErrorToString])); //Birb
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

procedure TfScriptEditor.DebuggerCompile(Sender: TPSScript);
begin
  FOldUses:=Sender.Comp.OnUses;
  if Assigned(Data) and (FDataSet is TBaseScript) then
    TBaseScript(FDataSet).Writeln:=@FDataSetWriteln;
  Sender.Comp.OnUses:=@OnUses;
  OnUses(Sender.Comp,'SYSTEM');
end;

procedure TfScriptEditor.FDataSetDataSetAfterScroll(DataSet: TDataSet);
begin
 try
   TBaseScript(FDataSet).ResetScript;
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

procedure TfScriptEditor.InternalSleep(MiliSecValue: LongInt);
begin
  genpascalscript.DoSleep(MiliSecValue);
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
      FScript := TBaseScript(FDataSet).Script;
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
  StatusBar.Panels[0].Text := IntToStr(ed.CaretY)+':'+IntToStr(ed.CaretX);
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

procedure TfScriptEditor.DebuggerBreakpoint(Sender: TObject; const FileName: String; bPosition, Row,
  Col: Cardinal);
begin
 acRun.Enabled:=True;
 acPause.Enabled:=False;
  FActiveLine := Row;
  if (FActiveLine < ed.TopLine +2) or (FActiveLine > Ed.TopLine + Ed.LinesInWindow -2) then
  begin
    Ed.TopLine := FActiveLine - (Ed.LinesInWindow div 2);
  end;
  ed.CaretY := FActiveLine;
  ed.CaretX := 1;

  ed.Refresh;
  acStepinto.Enabled:=acPause.Enabled or acRun.Enabled;
  acStepover.Enabled:=acPause.Enabled or acRun.Enabled;
end;

procedure TfScriptEditor.FSynCompletionExecute(Sender: TObject);
function GetCurWord:string;
var
  S:string;
  i,j:integer;
begin
  Result:='';
  with TSynCompletion(Sender).Editor do
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
var
  i: Integer;
  aStatement: String;
  s: String;
  aStrings: TStrings;
  ps : PChar;
  aTp: TPSTypeRec;
  aTyp: TPSType;
  a: Integer;
  aVar: PIFVariant;
  sl: TStringList;
begin
  with FSynCompletion.ItemList do
    begin
      Clear;
      s := GetCurWord;
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
         ed.SetFocus;
       end;
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

procedure TfScriptEditor.InitEvents;
var
  aScript: TScript;
begin
  aScript := FScript;
  if Assigned(aScript) then
    begin
      aScript.OnIdle:=@aScriptIdle;
      aScript.OnRunLine:=@aScriptRunLine;
    end;
end;

procedure TfScriptEditor.ButtonStatus(Status: TScriptStatus);
begin
  case Status of
  ssRunning:
    begin
      acPause.Enabled:=True;
      acReset.Enabled:=True;
      acRun.Enabled:=False;
      acStepinto.Enabled:=False;
      acStepover.Enabled:=False;
    end;
  ssPaused:
    begin
      acPause.Enabled:=False;
      acReset.Enabled:=True;
      acRun.Enabled:=True;
      acStepinto.Enabled:=True;
      acStepover.Enabled:=True;
    end;
  ssNone:
    begin
      acPause.Enabled:=False;
      acReset.Enabled:=False;
      acRun.Enabled:=True;
      acStepinto.Enabled:=True;
      acStepover.Enabled:=False;
    end;
  end;
end;

initialization
  RegisterPropertyToSkip(TSynEdit, 'MouseTextActions', 'Not available in Laz 1.0', '');
  RegisterPropertyToSkip(TSynEdit, 'VisibleSpecialChars', 'Not available in Laz 1.0', '');
end.

