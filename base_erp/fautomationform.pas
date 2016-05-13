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
  uBaseDbClasses, uBaseERPDBClasses, uprometscripts, uDocuments,
  uprometpascalscript, genpascalscript, genscript, db, simpleipc, blcksock,
  synsock, uprometscriptprinting,uImageCache;

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
    ActionList1: TActionList;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Bevel7: TBevel;
    Bevel8: TBevel;
    BitBtn1: TBitBtn;
    bExecute: TSpeedButton;
    BitBtn3: TSpeedButton;
    BitBtn5: TSpeedButton;
    ipWorkHTML: TIpHtmlPanel;
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
    sbMenue3: TSpeedButton;
    sbMenue4: TSpeedButton;
    sbMenue5: TSpeedButton;
    bNet: TToggleBox;
    ToolBar1: TPanel;
    ToolButton1: TSpeedButton;
    ToolButton2: TSpeedButton;
    tvStep: TTreeView;
    procedure acDebugLogExecute(Sender: TObject);
    procedure acEditExecute(Sender: TObject);
    procedure acExecutePrepareStepExecute(Sender: TObject);
    procedure acExecuteStepExecute(Sender: TObject);
    procedure acPrepareExecute(Sender: TObject);
    procedure acProduceExecute(Sender: TObject);
    procedure acReadyExecute(Sender: TObject);
    procedure bNetChange(Sender: TObject);
    function FCacheGetFile(URL: string; var NewPath: string): TStream;
    procedure fLogWaitFormbAbortClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FSocketTerminate(Sender: TObject);
    procedure TreeDataScriptScriptRunLine(Sender: TScript; Module: string;
      aPosition, aRow, aCol: Integer);
    procedure tvStepSelectionChanged(Sender: TObject);
    function ExecuteServerFunction(aFunc : string) : Variant;
  private
    FDataSet: TBaseDBPosition;
    FSelStep: TNotifyEvent;
    fScript : TBaseScript;
    nComm : TTreeNode;
    FSocket : TTCPCommandDaemon;
    FCache : TFileCache;
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

  TProdTreeData = class
    procedure ScriptDebugln(const s: string);
    procedure ScriptPrepareWriteln(const s: string);
    procedure ScriptWriteln(const s: string);
  public
    EditParent : Int64;
    Position : Int64;
    Script,Preparescript : TBaseScript;
    Documents,PrepDocuments : TDocument;
    Func,PrepareFunc : string;

    PreText : TStringList;
    WorkText : TStringList;
    ScriptOutput,PrepareOutput : TStringList;
    Prepared : Boolean;
    constructor Create;
    destructor Destroy; override;
    function CheckContent : Boolean;
    procedure ShowData;
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
  uautomationframe,wikitohtml
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
      if not acExecuteStep.Checked then
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
                  TreeData.ShowData;
                  FAutomation.ipWorkHTML.Repaint;
                  FAutomation.ipWorkHTML.Scroll(hsaEnd);
                  Application.ProcessMessages;
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
                TreeData.ShowData;
                FAutomation.ipWorkHTML.Repaint;
                FAutomation.ipWorkHTML.Scroll(hsaEnd);
                Application.ProcessMessages;
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
  FAutomation.lStatusProblems.Visible:=False;
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
begin
  Result := nil;
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
end;

procedure TFAutomation.FormDestroy(Sender: TObject);
begin
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
          ipWorkHTML.SetHtml(nil);
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
      while tvStep.Selected.ImageIndex=43 do
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
      ipWorkHTML.SetHtml(aHTML);
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
begin
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
  if (Assigned(DataSet.FieldByName('PRSCRIPT')) and (DataSet.FieldByName('PRSCRIPT').AsString<>'')) or (DataSet.FieldByName('SCRIPT').AsString<>'') or (DataSet.FieldByName('TEXT').AsString<>'') then
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
            TreeData.WorkText.Text:=WikiText2HTML(aTexts.FieldByName('TEXT').AsString);
          TreeData.LoadDocuments(aTexts.Id.AsVariant,'B',aTexts.FieldByName('NAME').AsString,Null,Null);
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
          //nach Artikel/Version/Sprache suchen
          if not aMasterdata.Locate('ID;VERSION;LANGUAGE',VarArrayOf([nOrder.Positions.FieldByName('IDENT').AsString,nOrder.Positions.FieldByName('VERSION').AsString,nOrder.Positions.FieldByName('LANGUAGE').AsString]),[]) then
            begin
              //nach Artikel/Version suchen (Sprache ignorieren)
              if not aMasterdata.Locate('ID;VERSION',VarArrayOf([nOrder.Positions.FieldByName('IDENT').AsString,nOrder.Positions.FieldByName('VERSION').AsString]),[]) then
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
      if not aMasterdata.Locate('ID;VERSION;LANGUAGE',VarArrayOf([DataSet.FieldByName('IDENT').AsString,DataSet.FieldByName('VERSION').AsString,DataSet.FieldByName('LANGUAGE').AsString]),[]) then
        begin
          //nach Artikel/Version suchen (Sprache ignorieren)
          if not aMasterdata.Locate('ID;VERSION',VarArrayOf([DataSet.FieldByName('IDENT').AsString,DataSet.FieldByName('VERSION').AsString]),[]) then
            if not aMasterdata.Locate('ID;VERSION',VarArrayOf([DataSet.FieldByName('IDENT').AsString,DataSet.FieldByName('VERSION').AsString]),[]) then
              if (DataSet.FieldByName('VERSION').AsString='') and (not aMasterdata.Locate('ID;VERSION',VarArrayOf([DataSet.FieldByName('IDENT').AsString,Null]),[])) then
                aMasterdata.Close
              else if (DataSet.FieldByName('VERSION').AsString<>'') then
                aMasterdata.Close;
        end;
      if aMasterdata.Active then
        begin
          TreeData.LoadScript(aMasterdata.FieldByName('SCRIPT').AsString,aMasterdata.FieldByName('SCRIPTVER').AsVariant);
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
      TreeData.ShowData;
    end;
end;

procedure TFAutomation.Clear;
begin
  fLogWaitForm.SetLanguage;
  fLogWaitForm.Caption:='Debug';
  fLogWaitForm.bAbort.Kind:=bkClose;
  fLogWaitForm.bAbort.OnClick:=@fLogWaitFormbAbortClick;
  tvStep.Items.Clear;
  ipWorkHTML.SetHtml(nil);
  nComm := nil;
  bExecute.Enabled:=False;
end;

procedure TSimpleIpHtml.SimpleIpHtmlGetImageX(Sender: TIpHtmlNode;
  const URL: string; var Picture: TPicture);
var
  aPicture: TPicture = nil;
  aFile: TMemoryStream = nil;
  NewURL : string = '';
  FActNode: TIpHtmlNode;
begin
  FActNode := Sender;
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
    PrepareOutput.Add('<img src="ICON(22)"></img><i>'+copy(s,8,length(s))+'</i><br>')
  else if copy(s,0,10)='**STEPEND ' then
    begin
      aTxt := PrepareOutput[PrepareOutput.Count-1];
      aTxt := copy(aTxt,30,length(aTxt)-29-8);
      PrepareOutput.Delete(PrepareOutput.Count-1);
      PrepareOutput.Add('<img src="ICON(74)"></img><span>'+aTxt+' -> '+copy(s,11,length(s))+'</span><br>')
    end
  else if copy(s,0,8)='**ERROR ' then
    begin
      aTxt := PrepareOutput[PrepareOutput.Count-1];
      aTxt := copy(aTxt,30,length(aTxt)-29-8);
      PrepareOutput.Delete(PrepareOutput.Count-1);
      PrepareOutput.Add('<img src="ICON(75)"></img><b>'+aTxt+' -> '+copy(s,9,length(s))+'</b><br>')
    end
  else PrepareOutput.Add(s);
  FAutomation.ipWorkHTML.Visible:=False;
  ShowData;
  FAutomation.ipWorkHTML.Visible:=True;
  FAutomation.ipWorkHTML.Repaint;
  FAutomation.ipWorkHTML.Scroll(hsaEnd);
  Application.ProcessMessages;
end;

procedure TProdTreeData.ScriptWriteln(const s: string);
var
  aTxt: String;
begin
  fLogWaitForm.ShowInfo(s);
  if copy(s,0,7)='**STEP ' then
    ScriptOutput.Add('<img src="ICON(22)"></img><i>'+copy(s,8,length(s))+'</i><br>')
  else if copy(s,0,10)='**STEPEND ' then
    begin
      aTxt := ScriptOutput[ScriptOutput.Count-1];
      aTxt := copy(aTxt,30,length(aTxt)-29-8);
      ScriptOutput.Delete(ScriptOutput.Count-1);
      ScriptOutput.Add('<img src="ICON(74)"></img><span>'+aTxt+' -> '+copy(s,11,length(s))+'</span><br>')
    end
  else if copy(s,0,8)='**ERROR ' then
    begin
      aTxt := ScriptOutput[ScriptOutput.Count-1];
      aTxt := copy(aTxt,30,length(aTxt)-29-8);
      ScriptOutput.Delete(ScriptOutput.Count-1);
      ScriptOutput.Add('<img src="ICON(75)"></img><b>'+aTxt+' -> '+copy(s,9,length(s))+'</b><br>')
    end
  else ScriptOutput.Add(s);
  ShowData;
  FAutomation.ipWorkHTML.Repaint;
  FAutomation.ipWorkHTML.Scroll(hsaEnd);
  Application.ProcessMessages;
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
procedure TProdTreeData.ShowData;
var
  aHTML: TSimpleIpHtml;
  ss: TStringStream;
  procedure DoCompile;
  begin
  end;

begin
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
      aHTML := TSimpleIPHtml.Create;
      if FAutomation.acExecutePrepareStep.Checked then
        ss := TStringStream.Create(UniToSys(PrepareOutput.Text))
      else
        ss := TStringStream.Create('<body>'+UniToSys(PreText.Text+'<br><br>'+PrepareOutput.Text)+'</body>');
      aHTML.LoadFromStream(ss);
      ss.Free;
      FAutomation.ipWorkHTML.SetHtml(aHTML);
      FAutomation.bExecute.Action:=FAutomation.acExecutePrepareStep;
    end
  else
    begin
      aHTML := TSimpleIPHtml.Create;
      if FAutomation.acExecuteStep.Checked then
        begin
          if pos('<body',lowercase(WorkText.Text))=0 then
            ss := TStringStream.Create('<body>'+UniToSys(ScriptOutput.Text)+'</body>')
          else
            ss := TStringStream.Create(UniToSys(ScriptOutput.Text));
        end
      else
        begin
          if pos('<body',lowercase(WorkText.Text))=0 then
            ss := TStringStream.Create('<body>'+UniToSys(WorkText.Text+'<br><br>'+ScriptOutput.Text)+'</body>')
          else
            ss := TStringStream.Create(UniToSys(WorkText.Text+ScriptOutput.Text));
        end;
      aHTML.LoadFromStream(ss);
      ss.Free;
      FAutomation.ipWorkHTML.SetHtml(aHTML);
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
          Script.Compile;
          if Script.StatusProblems.Text<>'' then
            begin
              FAutomation.lStatusProblems.Caption:=strPartiallyProblematic+LineEnding+trim(Script.StatusProblems.Text);
              FAutomation.lStatusProblems.Visible:=True;
            end;
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
          ipWorkHTML.SetHtml(aHTML);
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
end.

