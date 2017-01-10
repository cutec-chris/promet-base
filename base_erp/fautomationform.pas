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
Created 08.10.2015
*******************************************************************************}
unit fautomationform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, IpHtml, LResources, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, StdCtrls, Buttons, ActnList, ComCtrls, Menus, DbCtrls,
  Spin, DBGrids, uBaseDbClasses, uBaseERPDBClasses, uprometscripts, uDocuments,
  uprometpascalscript, genpascalscript, genscript, db, simpleipc, blcksock,
  synsock, uprometscriptprinting, uImageCache,base64;

type
  TTCPCommandDaemon = class(TThread)
  private
    FOnData: TNotifyEvent;
    FSocket:TTCPBlockSocket;
    FData : string;
    procedure DoData;
  public
    ConnectionSocket: TTCPBlockSocket;
    constructor Create;
    Destructor Destroy; override;
    procedure Execute; override;
    property Data : string read FData;
    property OnData : TNotifyEvent read FOnData write FOnData;
  end;

  { TFAutomation }

  TFAutomation = class(TForm)
    acAbort: TAction;
    acExecuteStep: TAction;
    acPrepare: TAction;
    acProduce: TAction;
    acReady: TAction;
    acSave: TAction;
    acExecutePrepareStep: TAction;
    acDebugLog: TAction;
    acEdit: TAction;
    acRefresh: TAction;
    ActionList1: TActionList;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Bevel7: TBevel;
    BitBtn1: TBitBtn;
    bExecute: TSpeedButton;
    BitBtn3: TSpeedButton;
    BitBtn5: TSpeedButton;
    bResults: TSpeedButton;
    cbCategory: TComboBox;
    cbCreateTask: TCheckBox;
    DBGrid1: TDBGrid;
    ipHTML: TIpHtmlPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    lQuit: TLabel;
    lStatusProblems: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lStep: TLabel;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    miExtended: TMenuItem;
    mNotes: TMemo;
    PageControl1: TPageControl;
    Panel1: TPanel;
    pBDE: TPanel;
    Panel3: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    pNav1: TPanel;
    PopupMenu1: TPopupMenu;
    rbArticle: TRadioButton;
    rbList: TRadioButton;
    rbNoData: TRadioButton;
    rbOrder: TRadioButton;
    sbMenue: TSpeedButton;
    sbMenue1: TSpeedButton;
    sbMenue2: TSpeedButton;
    sbMenue4: TSpeedButton;
    sbMenue5: TSpeedButton;
    bNet: TToggleBox;
    seProblemTime: TSpinEdit;
    spBDE: TSplitter;
    tbButtons: TToolBar;
    tmQuit: TTimer;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    tsOldProblems: TTabSheet;
    tsBDE: TTabSheet;
    tsProblems: TTabSheet;
    ToolBar1: TPanel;
    ToolButton1: TSpeedButton;
    ToolButton2: TSpeedButton;
    tvStep: TTreeView;
    procedure aButtonClick(Sender: TObject);
    procedure acDebugLogExecute(Sender: TObject);
    procedure acEditExecute(Sender: TObject);
    procedure acExecutePrepareStepExecute(Sender: TObject);
    procedure acExecuteStepExecute(Sender: TObject);
    procedure acPrepareExecute(Sender: TObject);
    procedure acProduceExecute(Sender: TObject);
    procedure acReadyExecute(Sender: TObject);
    procedure acRefreshExecute(Sender: TObject);
    procedure bNetChange(Sender: TObject);
    procedure bResultsClick(Sender: TObject);
    procedure cbCategoryChange(Sender: TObject);
    function FCacheGetFile(URL: string; var NewPath: string): TStream;
    procedure fLogWaitFormbAbortClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FSocketTerminate(Sender: TObject);
    procedure ipHTMLHotClick(Sender: TObject);
    procedure tmQuitTimer(Sender: TObject);
    procedure TreeDataScriptScriptRunLine(Sender: TScript; Module: string;
      aPosition, aRow, aCol: Integer);
    procedure tvStepSelectionChanged(Sender: TObject);
    function ExecuteServerFunction(aFunc : string) : Variant;
  private
    FActNode: TIpHtmlNode;
    FDataSet: TBaseDBPosition;
    FSelStep: TNotifyEvent;
    fScript : TBaseScript;
    nComm : TTreeNode;
    FSocket : TTCPCommandDaemon;
    FCache : TFileCache;
    DoCompileScript : ^TDataEvent;
    procedure FSocketData(Sender: TObject);
    procedure SetDataSet(AValue: TBaseDBPosition);
    function FindNextStep: Boolean;
    function LoadStep : Boolean;
    { private declarations }
  public
    { public declarations }
    property DataSet : TBaseDBPosition read FDataSet write SetDataSet;
    procedure Clear;
    procedure DoOpen;
    procedure ClearScreen;
    procedure DoAddPosition;
    procedure SelectFirstStep;
    property Script : TBaseScript read fScript;
    property OnSelectStep : TNotifyEvent read FSelStep write FSelStep;
  end;

  { TProdTreeData }

  TProdTreeData = class
    procedure CompileScript(Data: PtrInt);
    procedure DebugCompileMessage(Sender: TScript; Module, Message: string;
      Position, Row, Col: Integer);
    procedure ScriptDebugln(const s: string);
    procedure ScriptPrepareWriteln(const s: string);
    procedure ScriptWriteln(const s: string);
    procedure ShowNewData(Data: PtrInt);
  public
    EditParent : Int64;
    Position : Int64;
    Script,Preparescript : TBaseScript;
    Documents,PrepDocuments : TDocument;
    Func,PrepareFunc : string;
    actualHtml: String;

    PreText : TStringList;
    WorkText : TStringList;
    ScriptOutput,PrepareOutput : TStringList;
    Prepared : Boolean;
    constructor Create;
    destructor Destroy; override;
    function CheckContent : Boolean;
    function ShowData : Boolean;
    procedure LoadScript(aScript : string;aVersion : Variant);
    procedure LoadPrepareScript(aScript : string;aVersion : Variant);
    procedure LoadDocuments(aID : largeInt;aType : string;aTID : string;aVersion : Variant;aLanguage : Variant);
    procedure LoadPrepDocuments(aID : largeInt;aType : string;aTID : string;aVersion : Variant;aLanguage : Variant);
  end;
  TSimpleIpHtml = class(TIpHtml)
    procedure SimpleIpHtmlGetImageX(Sender: TIpHtmlNode; const URL: string;
      var Picture: TPicture);
  protected
  public
    property OnGetImageX;
    constructor Create;
  end;

var
  FAutomation: TFAutomation;

implementation

uses Utils,uBaseVisualControls,uMasterdata,uData,uOrder,variants,uLogWait,
  uautomationframe,wikitohtml,LCLIntf,uBaseApplication,ubaseconfig,utask,uBaseDBInterface
  {$IFDEF WINDOWS}
  ,Windows
  {$ENDIF}
  ;

resourcestring
  strDoPick                             = 'kommissionieren';
  strRunning                            = 'wird ausgeführt...';
  strRun                                = 'Ausführen';
  strNotmoreSteps                       = 'Es sind keine (weiteren) Arbeitschritte vorhanden.<br><br>Um einen neuen Auftrag auswählen zu können müssen Sie den Auftrag (ab)schließen';
  strPartiallyProblematic               = 'Achtung Teile des Auftrages sind in nicht freigegebenem Zustand !';
  strLoading                            = 'Auftragsdaten werden geladen...';
  strNumberSetEmpty                     = 'Nummernkreis erschöpft';
  strNoOrderLoaded                      = 'Es ist kein Auftrag geladen oder das Problem wurde nicht gefunden !';
  strNewNumbers                         = 'Code';
  strProblemSend                        = 'Die Störung wurde Eingetragen !';

procedure TTCPCommandDaemon.DoData;
begin
  if Assigned(FOnData) then
    FOnData(Self);
end;

constructor TTCPCommandDaemon.Create;
begin
  FSocket:=TTCPBlockSocket.create;
  FreeOnTerminate:=true;
  inherited Create(false);
end;

destructor TTCPCommandDaemon.Destroy;
begin
  FSocket.Free;
  inherited Destroy;
end;

procedure TTCPCommandDaemon.Execute;
var
  ClientSock: TSocket;
begin
  ConnectionSocket := TTCPBlockSocket.Create;
  FSocket.CreateSocket;
  FSocket.setLinger(true,10);
  FSocket.Bind('127.0.0.1','9874');
  FSocket.Listen;
  if FSocket.LastError = 0 then
    begin
      while not Terminated do
        begin
          if FSocket.CanRead(300) then
            begin
              ConnectionSocket.Socket := FSocket.accept;
              FData := ConnectionSocket.RecvTerminated(600,CRLF);
              try
                if FData<>'' then
                  Synchronize(@DoData);
              except
              end;
              ConnectionSocket.CloseSocket;
            end;
        end;
   end;
end;

procedure TFAutomation.FSocketData(Sender: TObject);
var
  aRes : Variant;
  aFunction: String;
begin
  aFunction := TTCPCommandDaemon(Sender).Data;
  ares := ExecuteServerFunction(aFunction);
  if (ares <> Null) then
    TTCPCommandDaemon(Sender).ConnectionSocket.SendString(VarToStr(ares)+CRLF);
end;

procedure TFAutomation.acExecuteStepExecute(Sender: TObject);
var
  TreeData: TProdTreeData;
  aRes : Variant;
begin
  fLogWaitForm.Clear;
  if Assigned(tvStep.Selected) then
    begin
      TreeData := TProdTreeData(tvStep.Selected.Data);
      if acExecuteStep.Caption=strRun then
        begin
          Application.ProcessMessages;
          acExecuteStep.Checked:=True;
          acExecuteStep.Caption:=strRunning;
          Application.ProcessMessages;
          TreeData.ScriptOutput.Clear;
          TreeData.Script.ActualObject := DataSet.Parent;
          TreeData.Script.Script.OnRunLine:=@TreeDataScriptScriptRunLine;
          if Assigned(TreeData.Script) then
            begin
              FScript := TreeData.Script;
              tvStep.Enabled:=False;
              if TreeData.Func<>'' then
                begin
                  aRes := FScript.Script.RunScriptFunction([],TreeData.Func);
                end
              else if not FScript.Execute(Null) then
                begin
                  if not Assigned(TreeData.Script.Script) then
                    TreeData.ScriptOutput.Add('<b>Ausführung fehlgeschlagen:Scripttyp unbekannt</b>')
                  else
                    TreeData.ScriptOutput.Add('<b>Ausführung fehlgeschlagen:'+TreeData.Script.Script.Results+'</b>');
                  TreeData.ShowNewData(0);
                end;
            end;
          acExecuteStep.Checked:=False;
          acExecuteStep.Caption:=strRun;
          tvStep.Enabled:=True;
        end
      else
        begin
          TreeData.Script.Script.Stop;
          acExecuteStep.Caption:=strRun;
        end;
    end;
end;

procedure TFAutomation.acExecutePrepareStepExecute(Sender: TObject);
var
  TreeData: TProdTreeData;
begin
  fLogWaitForm.Clear;
  if Assigned(tvStep.Selected) then
    begin
      TreeData := TProdTreeData(tvStep.Selected.Data);
      if not acExecutePrepareStep.Checked then
        begin
          Application.ProcessMessages;
          acExecutePrepareStep.Checked:=True;
          Application.ProcessMessages;
          TreeData.ScriptOutput.Clear;
          TreeData.Preparescript.ActualObject := DataSet.Parent;
          TreeData.Preparescript.Script.OnRunLine:=@TreeDataScriptScriptRunLine;
          TreeData.PrepareOutput.Clear;
          if Assigned(TreeData.Preparescript) then
            if not TreeData.Preparescript.Execute(Null) then
              begin
                if not Assigned(TreeData.Preparescript.Script) then
                  TreeData.ScriptOutput.Add('<b>Ausführung fehlgeschlagen:Scripttyp unbekannt</b>')
                else
                  TreeData.ScriptOutput.Add('<b>Ausführung fehlgeschlagen:'+TreeData.Preparescript.Script.Results+'</b>');
                TreeData.ShowNewData(0);
              end;
          acExecutePrepareStep.Checked:=False;
        end
      else
        begin
          TreeData.Preparescript.Script.Stop;
        end;
    end;
end;

procedure TFAutomation.acDebugLogExecute(Sender: TObject);
begin
  fLogWaitForm.SetLanguage;
  fLogWaitForm.Show;
end;

procedure TFAutomation.aButtonClick(Sender: TObject);
var
  aImages: TOrderRepairImages;
  aTask: TTask;
begin
  aImages := TOrderRepairImages.Create(nil);
  aImages.Open;
  if aImages.Locate('NAME',TToolButton(Sender).Caption,[]) then
    begin
      if Assigned(FDataSet) then
        begin
          TOrderPos(DataSet).Repair.Insert;
          TOrderPos(DataSet).Repair.FieldByName('INTNOTES').AsString:=mNotes.Text;
          TOrderPos(DataSet).Repair.FieldByName('ERRIMAGE').AsVariant:=aImages.Id.AsVariant;
          TOrderPos(DataSet).Repair.FieldByName('IMAGENAME').AsString:=aImages.FieldByName('NAME').AsString;
          TOrderPos(DataSet).Repair.FieldByName('TIME').AsFloat:=seProblemTime.Value/MinsPerDay;
          TOrderPos(DataSet).Repair.Post;
          with TOrderPos(DataSet).Repair do
            begin
              Details.Open;
              while Details.Count>0 do Details.Delete;
              aImages.RepairDetail.Open;
              aImages.RepairDetail.First;
              while not aImages.RepairDetail.EOF do
                begin
                  Details.Insert;
                  Details.FieldByName('ASSEMBLY').AsString := aImages.RepairDetail.FieldByName('ASSEMBLY').AsString;
                  Details.FieldByName('PART').AsString := aImages.RepairDetail.FieldByName('PART').AsString;
                  Details.FieldByName('ERROR').AsString := aImages.RepairDetail.FieldByName('ERROR').AsString;
                  Details.Post;
                  aImages.RepairDetail.Next;
                end;
            end;
          TOrderPos(DataSet).Repair.Post;
          try
            aImages.Edit;
            aImages.FieldByName('COUNTER').AsInteger:=aImages.FieldByName('COUNTER').AsInteger+1;
            aImages.Post;
          except
          end;
          seProblemTime.Value:=5;
          lQuit.Caption := strProblemSend;
          lQuit.Visible := True;
          tmQuit.Enabled:=True;
        end
      else
        Showmessage(strNoOrderLoaded);
      if cbCreateTask.Checked then
        begin
          aTask := TTask.Create(nil);
          aTask.Append;
          aTask.Text.AsString:=aImages.FieldByName('NAME').AsString;
          aTask.FieldByName('DESC').AsString:=mNotes.Text;
          if aImages.FieldByName('USER').AsString<>'' then
            aTask.FieldByName('USER').AsString:=aImages.FieldByName('USER').AsString;
          aTask.Post;
        end;
      mNotes.Clear;
    end;
  aImages.Free;
end;

procedure TFAutomation.acEditExecute(Sender: TObject);
begin

end;

procedure TFAutomation.acPrepareExecute(Sender: TObject);
var
  TreeData: TProdTreeData;
begin
  FAutomation.lStatusProblems.Visible:=False;
  if Assigned(tvStep.Selected) then
    begin
      TreeData := TProdTreeData(tvStep.Selected.Data);
      TreeData.Prepared:=False;
      TreeData.ShowData;
    end;
end;

procedure TFAutomation.acProduceExecute(Sender: TObject);
var
  TreeData: TProdTreeData;
begin
  if Assigned(tvStep.Selected) then
    begin
      TreeData := TProdTreeData(tvStep.Selected.Data);
      TreeData.Prepared:=True;
      TreeData.ShowData;
    end;
end;

procedure TFAutomation.acReadyExecute(Sender: TObject);
begin
  FindNextStep;
end;

procedure TFAutomation.acRefreshExecute(Sender: TObject);
begin
  LoadStep;
end;

procedure TFAutomation.bNetChange(Sender: TObject);
begin
  if bNet.Checked then
    if not Assigned(FSocket) then
      begin
        FSocket := TTCPCommandDaemon.Create;
        FSocket.OnData:=@FSocketData;
        FSocket.OnTerminate:=@FSocketTerminate;
      end;
end;

procedure TFAutomation.bResultsClick(Sender: TObject);
var
  aImages: TOrderRepairImages;
  aButton: TToolButton;
begin
  pBDE.Visible:=bResults.Down;
  spBDE.Visible:=bResults.Down;
  if pBDE.Visible then
    begin
      while tbButtons.ButtonCount>0 do
        tbButtons.Buttons[0].Free;
      if Assigned(FDataSet) then
        begin
          TOrderPos(FDataSet).Repair.Open;
        end;
     aImages := TOrderRepairImages.Create(nil);
     aImages.Filter(Data.ProcessTerm(Data.QuoteField('CATEGORY')+'='+Data.QuoteValue('*'+cbCategory.Text+'*')));
     while not aImages.EOF do
       begin
         aButton := TToolButton.Create(tbButtons);
         aButton.Caption:=aImages.FieldByName('NAME').AsString;
         aButton.Parent:=tbButtons;
         aButton.OnClick:=@aButtonClick;
         aButton.Tag:=aImages.Id.AsInteger;
         aImages.Next;
       end;
     aImages.Free;
    end;
end;

procedure TFAutomation.cbCategoryChange(Sender: TObject);
begin
  with BaseApplication as IBaseConfig do
    Config.WriteString('FA_CATEGORY',cbCategory.Text);
end;

function TFAutomation.FCacheGetFile(URL: string; var NewPath: string): TStream;
var
  TreeData: TProdTreeData;
  ms: TMemoryStream;
  aPicture: TPicture;
  aURL: String;
  Path: String;
  aNumber: integer;
  tmp: String;
  aDoc: TDocument;
  Picture: TPicture;
  tmpURL: String;
  pic: TPicture;
begin
  Result := nil;
  DoCompileScript := nil;
  try
  if Assigned(FAutomation.tvStep.Selected) then
    begin
      TreeData := TProdTreeData(FAutomation.tvStep.Selected.Data);
      if not Assigned(TreeData.Documents) then exit;
      Path := URL;
      if copy(uppercase(Path),0,5)='ICON(' then
        begin
          if TryStrToInt(copy(Path,6,length(Path)-6),aNumber) then
            begin
              ms := TMemoryStream.Create;
              Picture := TPicture.Create;
              fVisualControls.Images.GetBitmap(aNumber,Picture.Bitmap);
              Picture.SaveToStreamWithFileExt(ms,'png');
              NewPath := Copy(Path,0,length(path)-length(ExtractFileExt(Path)))+'.png';
              ms.Position:=0;
              Result := ms;
              ms.Position:=0;
              Result := ms;
              Picture.Free;
            end;
        end
      else if copy(uppercase(Path),0,12)='HISTORYICON(' then
        begin
          tmp := copy(Path,13,length(Path)-13);
          if TryStrToInt(tmp,aNumber) then
            begin
              ms := TMemoryStream.Create;
              Picture := TPicture.Create;
              fVisualControls.HistoryImages.GetBitmap(aNumber,Picture.Bitmap);
              Picture.SaveToStreamWithFileExt(ms,'png');
              NewPath := Copy(Path,0,length(path)-length(ExtractFileExt(Path)))+'.png';
              ms.Position:=0;
              Result := ms;
              ms.Position:=0;
              Result := ms;
              Picture.Free;
            end;
        end
      else if copy(uppercase(Path),0,7)='FILE://' then
        begin
          tmpURL := UniToSys(copy(Path,8,length(Path)));
          if FileExists(tmpURL) then
            begin
              pic := TPicture.Create;
              pic.LoadFromFile(tmpUrl);
              ms := TMemoryStream.Create;
              pic.SaveToStreamWithFileExt(ms,'jpg');
              ms.Position:=0;
              Result := ms;
              pic.Free;
            end;
        end
      else
        begin
          TreeData.Documents.Open;
          aURL := copy(URL,0,rpos('.',URL)-1);
          if TreeData.Documents.Locate('NAME',aURL,[loCaseInsensitive]) then
            begin
              ms := TMemoryStream.Create;
              aDoc := TDocument.Create(nil);
              aDoc.SelectByNumber(TreeData.Documents.FieldByName('NUMBER').AsVariant);
              aDoc.Open;
              aDoc.CheckoutToStream(ms);
              aDoc.Free;
              ms.Position:=0;
              Result := ms;
            end
          else if Assigned(TreeData.PrepDocuments) then
            begin
              TreeData.PrepDocuments.Open;
              if TreeData.PrepDocuments.Locate('NAME',aURL,[loCaseInsensitive]) then
                begin
                  ms := TMemoryStream.Create;
                  aDoc := TDocument.Create(nil);
                  aDoc.SelectByNumber(TreeData.PrepDocuments.FieldByName('NUMBER').AsVariant);
                  aDoc.Open;
                  aDoc.CheckoutToStream(ms);
                  aDoc.Free;
                  ms.Position:=0;
                  Result := ms;
                end;
            end;
        end;
    end;
  except
    Result := nil;
  end;
  if not Assigned(Result) then
    begin
      aURL := copy(URL,0,rpos('.',URL)-1);
      aDoc := TDocument.Create(nil);
      aDoc.Filter(Data.QuoteField('TYPE')+'='+Data.QuoteValue('W')+' AND '+Data.QuoteField('NAME')+'='+Data.QuoteValue(aURL));
      if aDoc.Count>0 then
       begin
         ms := TMemoryStream.Create;
         aDoc.CheckoutToStream(ms);
         ms.Position:=0;
         if TIpHtmlNodeIMG(FActNode).Width.LengthType = hlAbsolute then
           begin
             try
               aPicture := TPicture.Create;
               aPicture.LoadFromStreamWithFileExt(ms,aDoc.FieldByName('EXTENSION').AsString);
               Picture := TPicture.Create;
               Picture.Bitmap.Width := TIpHtmlNodeIMG(FActNode).Width.LengthValue;
               Aspect := aPicture.Height/aPicture.Width;
               Picture.Bitmap.Height := round(TIpHtmlNodeIMG(FActNode).Width.LengthValue*Aspect);
               Picture.Bitmap.Canvas.AntialiasingMode:= amOn;
               Picture.Bitmap.Canvas.StretchDraw(Classes.Rect(0,0,Picture.Width,Picture.Height),aPicture.Graphic);
               aPicture.Free;
               ms.Free;
               ms := TMemoryStream.Create;
               Picture.SaveToStreamWithFileExt(ms,'png');
               NewPath := Copy(Path,0,length(path)-length(ExtractFileExt(Path)))+'.png';
               ms.Position:=0;
               Picture.Free;
             except
             end;
           end;
         Result := ms;
       end;
      aDoc.Free;
    end;
end;

procedure TFAutomation.fLogWaitFormbAbortClick(Sender: TObject);
begin
  fLogWaitForm.Close;
end;

procedure TFAutomation.FormCreate(Sender: TObject);
begin
  FSocket := TTCPCommandDaemon.Create;
  FSocket.OnData:=@FSocketData;
  FSocket.OnTerminate:=@FSocketTerminate;
  bNet.Checked:=True;
  FCache := TFileCache.Create(30);
  FCache.OnGetFile:=@FCacheGetFile;
  with BaseApplication as IBaseConfig do
    cbCategory.Text:=Config.ReadString('FA_CATEGORY','');
end;

procedure TFAutomation.FormDestroy(Sender: TObject);
begin
  with Application as IBaseApplication do
    SaveConfig;
  FCache.Free;
  if Assigned(FSocket) then
    begin
      FSocket.Terminate;
      FSocket.OnData:=nil;
      FSocket.WaitFor;
    end;
end;

procedure TFAutomation.FSocketTerminate(Sender: TObject);
begin
  bNet.Checked:=False;
  FSocket := nil;
end;

procedure TFAutomation.ipHTMLHotClick(Sender: TObject);
var
  PageName: String;
  ID: Integer;
  i: Integer;
  aLink: String;
begin
  if Assigned(IpHtml.HotNode) and (ipHTML.HotNode is TIpHtmlNodeA) then
    begin
      aLink := TIpHtmlNodeA(IpHtml.HotNode).HRef;
      PageName := StringReplace(aLink,' ','_',[rfReplaceAll]);
      for i := 0 to FVariables.Count-1 do
        pageName := StringReplace(PageName,'@VARIABLES.'+FVariables.Names[i]+'@',FVariables.ValueFromIndex[i],[rfReplaceAll,rfIgnoreCase]);
      if Data.GotoLink('WIKI@'+PageName) then
      else if (pos('@',PageName)>0) and Data.GotoLink(PageName) then
        begin
        end
      else if ((Pos('://', aLink) > 0) or (pos('www',lowercase(aLink)) > 0)) then
        OpenURL(aLink);
    end;
end;

procedure TFAutomation.tmQuitTimer(Sender: TObject);
begin
  tmQuit.Enabled:=False;
  lQuit.Visible:=False;
end;

procedure TFAutomation.TreeDataScriptScriptRunLine(Sender: TScript;
  Module: string; aPosition, aRow, aCol: Integer);
begin
  Application.ProcessMessages;
end;

procedure TFAutomation.tvStepSelectionChanged(Sender: TObject);
var
  Res: Boolean;
begin
  if Assigned(FSelStep) then FSelStep(tvStep);
  if Assigned(tvStep.Selected) then
    begin
      acExecuteStep.Enabled:=False;
      if DataSet.Locate('SQL_ID',TProdTreeData(tvStep.Selected.Data).Position,[]) then
        Res := LoadStep
      else Res := False;
      if not Res then
        begin
          lStep.Caption:=tvStep.Selected.Text;
          ipHTML.SetHtml(nil);
          rbNoData.Checked:=True;
          acProduce.Enabled:=False;
          acPrepare.Enabled:=False;
          acReady.Enabled:=False;
        end;
    end;
end;

function TFAutomation.ExecuteServerFunction(aFunc: string): Variant;
var
  aFunction: String;
  aParams : array of Variant;
  tmp: String;
begin
  Result := Null;
  Setlength(aParams,0);
  aFunction := aFunc;
  if pos('(',aFunction)>0 then
    begin
      tmp := copy(aFunction,pos('(',aFunction)+1,length(aFunction));
      tmp := copy(tmp,0,length(tmp)-2);
      aFunction:=copy(aFunction,0,pos('(',aFunction)-1);
    end;
  try
    if Assigned(FAutomation.Script) then
      Result := FAutomation.Script.Script.RunScriptFunction(aParams,aFunction);
  except
    on E : Exception do
      begin
        Result := e.Message+CRLF;
      end;
  end;
end;

procedure TFAutomation.SetDataSet(AValue: TBaseDBPosition);
begin
  if FDataSet=AValue then Exit;
  FDataSet:=AValue;
end;

function TFAutomation.FindNextStep: Boolean;
var
  aHTML: TSimpleIpHtml;
  ss: TStringStream;
begin
  result := False;
  while (Assigned(tvStep.Selected) and (not Result)) do
    begin
      if tvStep.Selected.ImageIndex=43 then //Kommissionieren
        tvStep.Selected:=tvStep.Selected.GetNextSibling
      else
        tvStep.Selected:=tvStep.Selected.GetNext;
      while Assigned(tvStep.Selected) and (tvStep.Selected.ImageIndex=43) do
        tvStep.Selected:=tvStep.Selected.GetNextSibling;
      if Assigned(tvStep.Selected) then
        begin
          Result := LoadStep;// or (tvStep.Selected.ImageIndex=49);
          if Result then acReady.Enabled:=True;
          if Result then break;
        end;
    end;
  if not Result then
    begin
      aHTML := TSimpleIPHtml.Create;
      ss := TStringStream.Create('<body>'+UniToSys(strNotmoreSteps)+'</body>');
      aHTML.LoadFromStream(ss);
      ss.Free;
      ipHTML.SetHtml(aHTML);
    end;
  if Result then
    if Assigned(FSelStep) then FSelStep(tvStep);
  if not Result then acExecuteStep.Enabled:=False;
end;

function TFAutomation.LoadStep: Boolean;
var
  aMasterdata: TMasterdata;
  TreeData : TProdTreeData;
  aPosID: String;
  aTexts: TBoilerplate;
  nOrder: TOrder;
  aVersion : Variant;
begin
  lStatusProblems.Visible:=False;
  if not Assigned(tvStep.Selected) then exit;
  lStep.Caption:=tvStep.Selected.Text;
  Result := False;
  acExecuteStep.Enabled:=False;
  acExecutePrepareStep.Enabled:=False;
  rbNoData.Checked:=True;
  TreeData := TProdTreeData(tvStep.Selected.Data);
  TreeData.WorkText.Clear;
  TreeData.PreText.Clear;
  FreeAndNil(TreeData.Documents);
  FreeAndNil(TreeData.Preparescript);
  FreeAndNil(TreeData.Script);
  //Information in Order
  if (Assigned(DataSet.FieldByName('PRSCRIPT')) and (DataSet.FieldByName('PRSCRIPT').AsString<>''))
  or (DataSet.FieldByName('SCRIPT').AsString<>'')
  or (DataSet.FieldByName('TEXT').AsString<>'')
  or (DataSet.FieldByName('WORKTEXT').AsString<>'') then
    begin
      TreeData.LoadScript(DataSet.FieldByName('SCRIPT').AsString,DataSet.FieldByName('SCRIPTVER').AsVariant);
      if Assigned(DataSet.FieldByName('PRSCRIPT')) then
        TreeData.LoadPrepareScript(DataSet.FieldByName('PRSCRIPT').AsString,DataSet.FieldByName('PRSCRIPTVER').AsVariant);
      if DataSet.DataSet.FieldDefs.IndexOf('ORDERNO') <> -1 then
        aPosID := DataSet.FieldByName('ORDERNO').AsString+DataSet.FieldByName('POSNO').AsString
      else
        aPosID := DataSet.FieldByName('SQL_ID').AsString+DataSet.FieldByName('POSNO').AsString;
      TreeData.Func:=DataSet.FieldByName('SCRIPTFUNC').AsString;
      TreeData.PrepareFunc:=DataSet.FieldByName('PRSCRIPTFUNC').AsString;
      TreeData.WorkText.Text:=WikiText2HTML(DataSet.FieldByName('TEXT').AsString);
      if DataSet.FieldByName('PREPTEXT').AsString<>'' then
        begin
          aTexts := TBoilerplate.Create(nil);
          aTexts.Filter(Data.QuoteField('NAME')+'='+Data.QuoteValue(DataSet.FieldByName('PREPTEXT').AsString));
          if aTexts.Count>0 then
            TreeData.PreText.Text:=WikiText2HTML(aTexts.FieldByName('TEXT').AsString);
          TreeData.LoadPrepDocuments(aTexts.Id.AsVariant,'B',aTexts.FieldByName('NAME').AsString,Null,Null);
          aTexts.Free;
        end;
      if DataSet.FieldByName('WORKTEXT').AsString<>'' then
        begin
          aTexts := TBoilerplate.Create(nil);
          aTexts.Filter(Data.QuoteField('NAME')+'='+Data.QuoteValue(DataSet.FieldByName('WORKTEXT').AsString));
          if aTexts.Count>0 then
            begin
              TreeData.WorkText.Text:=WikiText2HTML(aTexts.FieldByName('TEXT').AsString);
              TreeData.LoadDocuments(aTexts.Id.AsVariant,'B',aTexts.FieldByName('NAME').AsString,Null,Null);
            end;
          aTexts.Free;
        end
      else
        TreeData.LoadDocuments(DataSet.Id.AsVariant,'P',aPosId,Null,Null);
      Result := TreeData.CheckContent;
      rbOrder.Checked:=True;
    end;
  //Information in Piecelist
  if (not Result) and (Assigned(DataSet.Parent)) then
    begin
      nOrder := TOrder.Create(nil);
      //Unseren Auftrag nochmal öffnen damit wir die Position wechseln können
      nOrder.Select(DataSet.Parent.Id.AsVariant);
      nOrder.Open;
      nOrder.Positions.Open;
      if nOrder.Positions.Locate('SQL_ID',DataSet.FieldByName('PARENT').AsVariant,[]) then //Vorgängerposition finden (zu fertigender Artikel)
        begin
          aMasterdata := TMasterdata.Create(nil);
          aMasterdata.Select(nOrder.Positions.FieldByName('IDENT').AsString);
          aMasterdata.Open;
          aVersion := nOrder.Positions.FieldByName('VERSION').AsVariant;
          if aVersion = '' then
            aVersion := Null;
          //nach Artikel/Version/Sprache suchen
          if not aMasterdata.Locate('ID;VERSION;LANGUAGE',VarArrayOf([nOrder.Positions.FieldByName('IDENT').AsString,aVersion,nOrder.Positions.FieldByName('LANGUAGE').AsVariant]),[]) then
            begin
              if not (((nOrder.Positions.FieldByName('VERSION').AsString='') or (nOrder.Positions.FieldByName('VERSION').IsNull))
              and (aMasterdata.Locate('ID;VERSION;LANGUAGE',VarArrayOf([nOrder.Positions.FieldByName('IDENT').AsString,'',nOrder.Positions.FieldByName('LANGUAGE').AsString]),[]))) then
                //nach Artikel/Version suchen (Sprache ignorieren)
                if not aMasterdata.Locate('ID;VERSION',VarArrayOf([nOrder.Positions.FieldByName('IDENT').AsString,aVersion]),[]) then
                  aMasterdata.Close;
            end;
          if aMasterdata.Active then
            begin
              aMasterdata.Positions.Open;
              //Position in Stückliste finden
              if aMasterdata.Positions.Locate('POSNO;SHORTTEXT',VarArrayOf([DataSet.FieldByName('TPOSNO').AsString,DataSet.FieldByName('SHORTTEXT').AsString]),[])
              or aMasterdata.Positions.Locate('SHORTTEXT',VarArrayOf([DataSet.FieldByName('SHORTTEXT').AsString]),[])
              then
                begin
                  TreeData.LoadScript(aMasterdata.Positions.FieldByName('SCRIPT').AsString,aMasterdata.Positions.FieldByName('SCRIPTVER').AsVariant);
                  TreeData.Func:=aMasterdata.Positions.FieldByName('SCRIPTFUNC').AsString;
                  TreeData.PrepareFunc:=aMasterdata.Positions.FieldByName('PRSCRIPTFUNC').AsString;
                  if Assigned(aMasterdata.Positions.FieldByName('PRSCRIPT')) then
                    TreeData.LoadPrepareScript(aMasterdata.Positions.FieldByName('PRSCRIPT').AsString,aMasterdata.Positions.FieldByName('PRSCRIPTVER').AsVariant);
                  TreeData.WorkText.Text:=WikiText2HTML(aMasterdata.Positions.FieldByName('TEXT').AsString);
                  if aMasterdata.Positions.DataSet.FieldDefs.IndexOf('ORDERNO') <> -1 then
                    aPosID := aMasterdata.Positions.FieldByName('ORDERNO').AsString+aMasterdata.Positions.FieldByName('POSNO').AsString
                  else
                    aPosID := aMasterdata.Positions.FieldByName('SQL_ID').AsString+aMasterdata.Positions.FieldByName('POSNO').AsString;
                  if aMasterdata.Positions.FieldByName('PREPTEXT').AsString<>'' then
                    begin
                      aTexts := TBoilerplate.Create(nil);
                      aTexts.Filter(Data.QuoteField('NAME')+'='+Data.QuoteValue(aMasterdata.Positions.FieldByName('PREPTEXT').AsString));
                      if aTexts.Count>0 then
                        TreeData.PreText.Text:=WikiText2HTML(aTexts.FieldByName('TEXT').AsString);
                      TreeData.LoadPrepDocuments(aTexts.Id.AsVariant,'B',aTexts.FieldByName('NAME').AsString,Null,Null);
                      aTexts.Free;
                    end;
                  if aMasterdata.Positions.FieldByName('WORKTEXT').AsString<>'' then
                    begin
                      aTexts := TBoilerplate.Create(nil);
                      aTexts.Filter(Data.QuoteField('NAME')+'='+Data.QuoteValue(aMasterdata.Positions.FieldByName('WORKTEXT').AsString));
                      if aTexts.Count>0 then
                        TreeData.WorkText.Text:=WikiText2HTML(aTexts.FieldByName('TEXT').AsString);
                      TreeData.LoadDocuments(aTexts.Id.AsVariant,'B',aTexts.FieldByName('NAME').AsString,Null,Null);
                      aTexts.Free;
                    end
                  else
                    TreeData.LoadDocuments(aMasterdata.Positions.Id.AsVariant,'P',aPosId,Null,Null);
                  Result := TreeData.CheckContent;
                  rbList.Checked:=Result;
                end;
            end;
          aMasterdata.Free;
        end;
      nOrder.Free;
    end;
  if not Result then
    begin
      aMasterdata := TMasterdata.Create(nil);
      aMasterdata.Select(DataSet.FieldByName('IDENT').AsString);
      aMasterdata.Open;
      if not aMasterdata.Locate('ID;VERSION;LANGUAGE',VarArrayOf([DataSet.FieldByName('IDENT').AsString,DataSet.FieldByName('VERSION').AsVariant,DataSet.FieldByName('LANGUAGE').AsString]),[]) then
        begin
          //nach Artikel/Version suchen (Sprache ignorieren)
          if not aMasterdata.Locate('ID;VERSION',VarArrayOf([DataSet.FieldByName('IDENT').AsString,DataSet.FieldByName('VERSION').AsVariant]),[]) then
            if (DataSet.FieldByName('VERSION').AsString='') and (not aMasterdata.Locate('ID;VERSION',VarArrayOf([DataSet.FieldByName('IDENT').AsString,Null]),[])) then
              aMasterdata.Close
            else if (DataSet.FieldByName('VERSION').AsString<>'') then
              aMasterdata.Close;
        end;
      if aMasterdata.Active then
        begin
          TreeData.LoadScript(aMasterdata.FieldByName('SCRIPT').AsString,aMasterdata.FieldByName('SCRIPTVER').AsVariant);
          TreeData.Func:=aMasterdata.FieldByName('SCRIPTFUNC').AsString;
          TreeData.PrepareFunc:=aMasterdata.FieldByName('PRSCRIPTFUNC').AsString;
          if Assigned(aMasterdata.FieldByName('PRSCRIPT')) then
            TreeData.LoadPrepareScript(aMasterdata.FieldByName('PRSCRIPT').AsString,aMasterdata.FieldByName('PRSCRIPTVER').AsVariant);
          if not Assigned(uBaseERPDBClasses.TextTyp) then
            uBaseERPDBClasses.TextTyp := TTextTypes.Create(nil);
          Texttyp.Open;
          if TextTyp.Locate('TYP','7',[]) then
            begin
              aMasterdata.Texts.Open;
              if aMasterdata.Texts.Locate('TEXTTYPE',TextTyp.DataSet.RecNo,[]) then
                TreeData.WorkText.Text:=WikiText2HTML(aMasterdata.Texts.FieldByName('TEXT').AsString);
            end;
          if aMasterdata.FieldByName('PREPTEXT').AsString<>'' then
            begin
              aTexts := TBoilerplate.Create(nil);
              aTexts.Filter(Data.QuoteField('NAME')+'='+Data.QuoteValue(aMasterdata.FieldByName('PREPTEXT').AsString));
              if aTexts.Count>0 then
                TreeData.PreText.Text:=WikiText2HTML(aTexts.FieldByName('TEXT').AsString);
              TreeData.LoadPrepDocuments(aTexts.Id.AsVariant,'B',aTexts.FieldByName('NAME').AsString,Null,Null);
              aTexts.Free;
            end;
          if aMasterdata.FieldByName('WORKTEXT').AsString<>'' then
            begin
              aTexts := TBoilerplate.Create(nil);
              aTexts.Filter(Data.QuoteField('NAME')+'='+Data.QuoteValue(aMasterdata.FieldByName('WORKTEXT').AsString));
              if aTexts.Count>0 then
                TreeData.WorkText.Text:=WikiText2HTML(aTexts.FieldByName('TEXT').AsString);
              TreeData.LoadDocuments(aTexts.Id.AsVariant,'B',aTexts.FieldByName('NAME').AsString,Null,Null);
              aTexts.Free;
            end
          else
            TreeData.LoadDocuments(aMasterdata.Id.AsVariant,aMasterdata.GetTyp,aMasterdata.FieldByName('ID').AsString,aMasterdata.FieldByName('VERSION').AsVariant,aMasterdata.FieldByName('LANGUAGE').AsVariant);
          Result := TreeData.CheckContent;
          rbArticle.Checked:=Result;
        end;
      aMasterdata.Free;
    end;
  if Result then
    begin
      FAutomation.ipHTML.Visible:=False;
      TreeData.ShowData;
      FAutomation.ipHTML.Visible:=True;
      FAutomation.ipHTML.Repaint;
      Application.ProcessMessages;
      if DoCompileScript<>nil then
        begin
          FAutomation.lStatusProblems.Color:=clInfoBk;
          FAutomation.lStatusProblems.Font.Color:=clInfoText;
          FAutomation.lStatusProblems.Visible:=True;
          FAutomation.lStatusProblems.Caption:=strLoading;
          Application.QueueAsyncCall(@TreeData.CompileScript,0);
          if Data.Users.Rights.Right('PRODUCTION')<=RIGHT_WRITE then
            begin
              FAutomation.acExecutePrepareStep.Enabled:=FAutomation.acExecutePrepareStep.Enabled and (not FAutomation.lStatusProblems.Visible);
              FAutomation.acExecuteStep.Enabled:=FAutomation.acExecuteStep.Enabled and (not FAutomation.lStatusProblems.Visible);
            end;
        end;
    end;
end;

procedure TFAutomation.Clear;
begin
  fLogWaitForm.SetLanguage;
  fLogWaitForm.Caption:='Debug';
  fLogWaitForm.bAbort.Kind:=bkClose;
  fLogWaitForm.bAbort.OnClick:=@fLogWaitFormbAbortClick;
  tvStep.Items.Clear;
  ipHTML.SetHtml(nil);
  nComm := nil;
  bExecute.Enabled:=False;
end;

procedure TSimpleIpHtml.SimpleIpHtmlGetImageX(Sender: TIpHtmlNode;
  const URL: string; var Picture: TPicture);
var
  aPicture: TPicture = nil;
  aFile: TMemoryStream = nil;
  NewURL : string = '';
begin
  FAutomation.FActNode := Sender;
  aFile := FAutomation.FCache.GetFile(URL,NewURL);
  if Assigned(aFile) then
    begin
      Picture := TPicture.Create;
      aFile.Position := 0;
      try
        Picture.LoadFromStreamWithFileExt(aFile,ExtractFileExt(NewURL));
      except
        FreeAndNil(Picture);
      end;
    end;
end;
constructor TSimpleIpHtml.Create;
begin
  inherited;
  OnGetImageX:=@SimpleIpHtmlGetImageX;
end;
var IsCompiling : Boolean;
procedure TProdTreeData.CompileScript(Data: PtrInt);
begin
  Screen.Cursor:=crHourGlass;
  Script.Writeln:=@ScriptWriteln;
  if not Assigned(Script.Script) then exit;
  Script.Script.OnCompileMessage:=@DebugCompileMessage;
  if IsCompiling then exit;
  IsCompiling := True;
  if (not Script.Compile) then
    begin
      FAutomation.lStatusProblems.Caption:='Failed to compile Script !';
      fLogWaitForm.ShowInfo(Script.Script.Results);
      FAutomation.lStatusProblems.Visible:=True;
      Screen.Cursor:=crDefault;
      IsCompiling := False;
      exit;
    end;
  IsCompiling := False;
  if (Script.StatusProblems.Text<>'') and Assigned(FAutomation) then
    begin
      FAutomation.lStatusProblems.Color:=clred;
      FAutomation.lStatusProblems.Font.Color:=clWhite;
      FAutomation.lStatusProblems.Caption:=strPartiallyProblematic+LineEnding+trim(Script.StatusProblems.Text);
      FAutomation.lStatusProblems.Visible:=True;
      Screen.Cursor:=crDefault;
    end
  else
    begin
      FAutomation.lStatusProblems.Visible:=False;
      Screen.Cursor:=crDefault;
    end;
end;

procedure TProdTreeData.DebugCompileMessage(Sender: TScript; Module,
  Message: string; Position, Row, Col: Integer);
begin
  fLogWaitForm.ShowInfo(Message);
end;

procedure TProdTreeData.ScriptDebugln(const s: string);
begin
  fLogWaitForm.ShowInfo(s);
end;

procedure TProdTreeData.ScriptPrepareWriteln(const s: string);
var
  aTxt: String;
begin
  fLogWaitForm.ShowInfo(s);
  if copy(s,0,7)='**STEP ' then
    PrepareOutput.Add('<img src="ICON(22)"></img><span>'+copy(s,8,length(s))+'</span><br>')
  else if copy(s,0,10)='**STEPEND ' then
    begin
      while (PrepareOutput.Count>0) and (copy(PrepareOutput[PrepareOutput.Count-1],0,15)<>'<img src="ICON(') do
        PrepareOutput.Delete(PrepareOutput.Count-1);
      aTxt := PrepareOutput[PrepareOutput.Count-1];
      aTxt := copy(aTxt,pos('<span>',aTxt)+6,length(aTxt));
      aTxt := copy(aTxt,0,pos('</span>',aTxt)-1);
      PrepareOutput.Delete(PrepareOutput.Count-1);
      PrepareOutput.Add('<img src="ICON(74)"></img><span>'+aTxt+'</span> -> '+copy(s,11,length(s))+'<br>')
    end
  else if copy(s,0,8)='**ERROR ' then
    begin
      while (PrepareOutput.Count>0) and (copy(PrepareOutput[PrepareOutput.Count-1],0,15)<>'<img src="ICON(') do
        PrepareOutput.Delete(PrepareOutput.Count-1);
      aTxt := PrepareOutput[PrepareOutput.Count-1];
      aTxt := copy(aTxt,pos('<i>',aTxt)+3,length(aTxt));
      aTxt := copy(aTxt,0,pos('</i>',aTxt)-1);
      PrepareOutput.Delete(PrepareOutput.Count-1);
      PrepareOutput.Add('<img src="ICON(75)"></img><b><span>'+aTxt+'</span> -> '+copy(s,9,length(s))+'</b><br>')
    end
  else PrepareOutput.Add(s);
  Application.QueueAsyncCall(@ShowNewData,0);
end;

procedure TProdTreeData.ScriptWriteln(const s: string);
var
  aTxt: String;
begin
  fLogWaitForm.ShowInfo(s);
  if copy(s,0,7)='**STEP ' then
    ScriptOutput.Add('<img src="ICON(22)"></img><i><span>'+copy(s,8,length(s))+'</span></i><br>')
  else if copy(s,0,10)='**STEPEND ' then
    begin
      while (ScriptOutput.Count>0) and (copy(ScriptOutput[ScriptOutput.Count-1],0,15)<>'<img src="ICON(') do
        ScriptOutput.Delete(ScriptOutput.Count-1);
      aTxt := ScriptOutput[ScriptOutput.Count-1];
      aTxt := copy(aTxt,pos('<span>',aTxt)+6,length(aTxt));
      aTxt := copy(aTxt,0,pos('</span>',aTxt)-1);
      ScriptOutput.Delete(ScriptOutput.Count-1);
      ScriptOutput.Add('<img src="ICON(74)"></img><span>'+aTxt+'</span> -> '+copy(s,11,length(s))+'<br>')
    end
  else if copy(s,0,8)='**ERROR ' then
    begin
      while (ScriptOutput.Count>0) and (copy(ScriptOutput[ScriptOutput.Count-1],0,15)<>'<img src="ICON(') do
        ScriptOutput.Delete(ScriptOutput.Count-1);
      aTxt := ScriptOutput[ScriptOutput.Count-1];
      aTxt := copy(aTxt,pos('<i>',aTxt)+3,length(aTxt));
      aTxt := copy(aTxt,0,pos('</i>',aTxt)-1);
      ScriptOutput.Delete(ScriptOutput.Count-1);
      ScriptOutput.Add('<img src="ICON(75)"></img><b><span>'+aTxt+'</span> -> '+copy(s,9,length(s))+'</b><br>')
    end
  else ScriptOutput.Add(s);
  Application.QueueAsyncCall(@ShowNewData,0);
end;

procedure TProdTreeData.ShowNewData(Data: PtrInt);
begin
  FAutomation.ipHTML.Visible:=False;
  if ShowData then
    begin
      FAutomation.ipHTML.Visible:=True;
      FAutomation.ipHTML.Repaint;
      FAutomation.ipHTML.Scroll(hsaEnd);
      Application.ProcessMessages;
    end
  else
    FAutomation.ipHTML.Visible:=True;
end;

constructor TProdTreeData.Create;
begin
  PreText := TStringList.Create;
  WorkText := TStringList.Create;
  ScriptOutput := TStringList.Create;
  PrepareOutput := TStringList.Create;
  PrepDocuments:=nil;
  Documents := nil;
  Prepared:=False;
end;
destructor TProdTreeData.Destroy;
begin
  PreText.Free;
  WorkText.Free;
  ScriptOutput.Free;
  PrepareOutput.Free;
  if Assigned(Script) then Script.Free;
  if Assigned(Preparescript) then Preparescript.Free;
  if Assigned(Documents) then Documents.Free;
  if Assigned(PrepDocuments) then PrepDocuments.Free;
  inherited Destroy;
end;
function TProdTreeData.CheckContent: Boolean;
begin
  Result := False;
  if (WorkText.Text<>'')
  or (PreText.Text<>'')
  or (Assigned(Script) and (Script.Count>0)) then
    Result := True;
end;
function TProdTreeData.ShowData: Boolean;
var
  aHTML: TSimpleIpHtml;
  ss: TStringStream;
  procedure DoCompile;
  begin
  end;

begin
  result := False;
  FAutomation.FScript := nil;
  FAutomation.acExecuteStep.Enabled:=False;
  FAutomation.acPrepare.Enabled:=PreText.Text<>'';
  FAutomation.acProduce.Enabled:=True;
  FAutomation.acReady.Enabled:=True;
  if (FAutomation.acPrepare.Checked and (not FAutomation.acPrepare.Enabled)) or (Prepared) then
    begin
      FAutomation.acProduce.Checked:=True;
      DoCompile;
    end
  else if FAutomation.acPrepare.Enabled then
    FAutomation.acPrepare.Checked:=True
  else
    begin
      FAutomation.acProduce.Checked:=True;
      DoCompile;
    end;
  if FAutomation.acPrepare.Checked then
    begin
      if FAutomation.acExecutePrepareStep.Checked then
        ss := TStringStream.Create((PrepareOutput.Text))
      else
        ss := TStringStream.Create(#$EF+#$BB+#$BF+'<body>'+(PreText.Text+'<br><br>'+PrepareOutput.Text)+'</body>');
      if actualHtml<>ss.DataString then
        begin
          Result := True;
          actualHtml := ss.DataString;
          aHTML := TSimpleIPHtml.Create;
          aHTML.LoadFromStream(ss);
          ss.Free;
          FAutomation.ipHTML.SetHtml(aHTML);
          FAutomation.bExecute.Action:=FAutomation.acExecutePrepareStep;
        end;
    end
  else
    begin
      if FAutomation.acExecuteStep.Checked then
        begin
          if pos('<body',lowercase(WorkText.Text))=0 then
            ss := TStringStream.Create(#$EF+#$BB+#$BF+'<body>'+(ScriptOutput.Text)+'</body>')
          else
            ss := TStringStream.Create(UniToSys(ScriptOutput.Text));
        end
      else
        begin
          if pos('<body',lowercase(WorkText.Text))=0 then
            ss := TStringStream.Create(#$EF+#$BB+#$BF+'<body>'+(WorkText.Text+'<br><br>'+ScriptOutput.Text)+'</body>')
          else
            ss := TStringStream.Create((WorkText.Text+ScriptOutput.Text));
        end;
      if actualHtml<>ss.DataString then
        begin
          Result := True;
          aHTML := TSimpleIPHtml.Create;
          actualHtml := ss.DataString;
          aHTML.LoadFromStream(ss);
          ss.Free;
          FAutomation.ipHTML.SetHtml(aHTML);
        end;
      FAutomation.bExecute.Action:=FAutomation.acExecuteStep;
    end;
  if Assigned(Script) and (Script.Count>0) then
    begin
      FAutomation.acExecuteStep.Enabled:=(Prepared or ((PreText.Text=''))) and (Assigned(Script));
      FAutomation.FScript := Script;
    end;
  FAutomation.acExecutePrepareStep.Enabled:=Assigned(Preparescript) and (Preparescript.Count>0);
end;

procedure TFAutomation.ClearScreen;
var
  TreeData: TProdTreeData;
begin
  if Assigned(tvStep.Selected) then
    begin
      TreeData := TProdTreeData(tvStep.Selected.Data);
      TreeData.WorkText.Clear;
      TreeData.ScriptOutput.Clear;
      TreeData.PrepareOutput.Clear;
    end;
end;

procedure TProdTreeData.LoadScript(aScript: string; aVersion: Variant);
begin
  Func:='';
  if not Assigned(Script) then
    Script := TBaseScript.Create(nil);
  if aScript='' then
    begin
      Script.Close;
      exit;
    end;
  Script.SelectByName(aScript);
  Script.Open;
  if (not Script.Locate('VERSION',aVersion,[]))
  and ((aVersion='') and (not Script.Locate('VERSION',Null,[])))
  then
    Script.Close;
  if Script.Active then
    begin
      Script.Writeln:=@ScriptWriteln;
      Script.Debugln:=@ScriptDebugln;
      if Assigned(Script) then
        begin
          FAutomation.DoCompileScript:=@Self;
        end;
    end;
end;

procedure TProdTreeData.LoadPrepareScript(aScript: string; aVersion: Variant);
begin
  PrepareFunc:='';
  if not Assigned(PrepareScript) then
    PrepareScript := TBaseScript.Create(nil);
  PrepareScript.SelectByName(aScript);
  PrepareScript.Open;
  PrepareScript.Writeln:=@ScriptPrepareWriteln;
  PrepareScript.Debugln:=@ScriptDebugln;
  if (not PrepareScript.Locate('VERSION',aVersion,[]))
  and ((aVersion='') and (not PrepareScript.Locate('VERSION',Null,[])))
  then
    PrepareScript.Close;
end;

procedure TProdTreeData.LoadDocuments(aID: largeInt; aType: string;
  aTID: string; aVersion: Variant; aLanguage: Variant);
begin
  if not Assigned(Documents) then
    Documents := TDocument.Create(nil);
  Documents.Select(aID,aType,aTID,aVersion,aLanguage);
end;

procedure TProdTreeData.LoadPrepDocuments(aID: largeInt; aType: string;
  aTID: string; aVersion: Variant; aLanguage: Variant);
begin
  if not Assigned(PrepDocuments) then
    PrepDocuments := TDocument.Create(nil);
  PrepDocuments.Select(aID,aType,aTID,aVersion,aLanguage);
end;

procedure TFAutomation.DoOpen;
begin
  Screen.Cursor:=crHourGlass;
  Application.ProcessMessages;
  nComm := nil;
  while not DataSet.EOF do
    begin
      DoAddPosition;
      DataSet.Next;
    end;
  Application.ProcessMessages;
  SelectFirstStep;
  Screen.Cursor:=crDefault;
end;

procedure TFAutomation.SelectFirstStep;
var
  Result: Boolean = False;
  aHTML: TSimpleIpHtml;
  ss: TStringStream;
begin
  if tvStep.Items.Count>1 then
    begin
      tvStep.Selected:=tvStep.Items[0];
      tvStep.Items[0].Expanded:=True;
      FindNextStep;
    end
  else if tvStep.Items.Count=1 then
    begin
      tvStep.Selected:=tvStep.Items[0];
      if Assigned(tvStep.Selected) then
        begin
          Result := LoadStep;// or (tvStep.Selected.ImageIndex=49);
          if Result then acReady.Enabled:=True;
        end;
      if not Result then
        begin
          aHTML := TSimpleIPHtml.Create;
          ss := TStringStream.Create('<body>'+UniToSys(strNotmoreSteps)+'</body>');
          aHTML.LoadFromStream(ss);
          ss.Free;
          ipHTML.SetHtml(aHTML);
        end;
      if Result then
        if Assigned(FSelStep) then FSelStep(tvStep);
    end;
end;

procedure TFAutomation.DoAddPosition;
var
  nNode: TTreeNode;
  function GetParentNode : TTreeNode;
  var
    aNode: TTreeNode;
  begin
    result := nil;
    aNode := nil;
    if tvStep.Items.Count>0 then
      aNode := tvStep.Items[0];
    while Assigned(aNode) do
      begin
        if TProdTreeData(aNode.Data).Position=DataSet.FieldByName('PARENT').AsVariant then
          begin
            Result := aNode;
            break;
          end;
        aNode := aNode.GetNext;
      end;
    if tvStep.Items.Count>0 then
      begin
        case DataSet.PosTyp.FieldByName('TYPE').AsInteger of
        0,1,2:
          begin
            if not Assigned(nComm) then
              begin
                nComm := tvStep.Items.AddChildObject(GetParentNode,strDoPick,TProdTreeData.Create);
                nComm.ImageIndex:=43;
                nComm.SelectedIndex:=nComm.ImageIndex;
              end;
            Result := nComm
          end;
        else nComm := nil;
        end;
      end;
  end;
begin
  nNode := tvStep.Items.AddChildObject(GetParentNode,DataSet.FieldByName('SHORTTEXT').AsString,TProdTreeData.Create);
  case DataSet.PosTyp.FieldByName('TYPE').AsInteger of
  0,1,2:
    begin
      nNode.ImageIndex:=14;//Artikel
      nNode.Text:=nNode.Text+' ['+DataSet.FieldByName('VERSION').AsString+']';
    end;
  3:nNode.ImageIndex:=49;//Text
  9,6:nNode.ImageIndex:=22;//Montage/Argeitsgang
  end;
  nNode.SelectedIndex:=nNode.ImageIndex;
  TProdTreeData(nNode.Data).Position:=DataSet.Id.AsVariant;
end;

procedure InternalClearScreen(Sender : TObject);
begin
  if Assigned(FAutomation) then
    FAutomation.ClearScreen;
end;

function NumberSetEmpty(aSet : string) : Boolean;
var
  newSerials: String;
  Code: String;
  aStart: String;
  aStop: String;
begin
  Result := False;
  newSerials := InputBox(strNumberSetEmpty+' ('+aSet+')',strNewNumbers,'');
  if newSerials<>'' then
    begin
      try
      Code  := DecodeStringBase64(newSerials);
      if pos(':',Code)>0 then
        begin
          aStart:=copy(Code,0,pos(':',Code)-1);
          Code := copy(Code,pos(':',Code)+1,length(Code));
          aStop:=Code;
          if Data.Numbers.Locate('NAME',aSet,[]) then
            begin
              Data.Users.History.AddItemPlain('SERIALS','New Serial Range Old:'+Data.Numbers.FieldByName('ACTUAL').AsString+' New:'+aStart+'-'+aStop);
              Data.Numbers.Edit;
              Data.Numbers.FieldByName('ACTUAL').AsString:=aStart;
              Data.Numbers.FieldByName('STOP').AsString:=aStop;
              Data.Numbers.Post;
              Result := True;
            end;
        end
      else raise Exception.Create('');
      except
        Result := False;
      end;
    end;
end;

procedure InternalBringToFront(Sender : TObject);
begin
  if Assigned(FAutomation) then
    begin
      if Assigned(FAutomation.Parent) then
        begin
          Application.BringToFront;
          FAutomation.Parent.BringToFront;
          FAutomation.BringToFront;
          {$IFDEF WINDOWS}
          if IsIconic(Application.MainForm.Handle) then
           ShowWindow(Application.MainForm.Handle, SW_SHOWNOACTIVATE);
          SetWindowPos(Application.MainForm.Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOACTIVATE or SWP_NOMOVE);
          SetWindowPos(Application.MainForm.Handle, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOACTIVATE or SWP_NOMOVE);
          {$ENDIF}
        end;
    end;
end;

initialization
  {$I fautomationform.lrs}
  genscript.DoClearScreen:=@InternalClearScreen;
  genscript.DoBringToFront:=@InternalBringToFront;
  DoInputBox:=@InputBox;
  OnNumbersetEmpty := @NumberSetEmpty;
  IsCompiling := False;
end.

