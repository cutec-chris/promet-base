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
unit uprometscripts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uBaseDbClasses, uBaseDBInterface, db, Utils
  ,uBaseDatasetInterfaces,uBaseERPDBClasses,contnrs,genscript;

type
  { TBaseScript }

  TBaseScript = class(TBaseERPList,IBaseHistory)
    procedure aScriptCheckModule(Sender: TObject);
    procedure DataSetAfterScroll(aDataSet: TDataSet);
    procedure FDataSourceDataChange(Sender: TObject; Field: TField);
  private
    FActObject: TBaseDBDataset;
    FDWrFunc: TStrOutFunc;
    FHistory: TBaseHistory;
    FLinks: TLinks;
    FDataSource: TDataSource;
    FRlFunc: TStrInFunc;
    FWrFunc: TStrOutFunc;
    FWriFunc: TStrOutFunc;
    FScript : TScript;
    FSelectedName : variant;
    FStatus : string;
    FStateChange: TNotifyEvent;
    FStatusProblems: TStringList;
    function GetScript: TScript;
    function GetVersion: TField;
    procedure SetDWRFunc(AValue: TStrOutFunc);
    procedure SetRlFunc(AValue: TStrInFunc);
    procedure SetWRFunc(AValue: TStrOutFunc);
    procedure SetWriFunc(AValue: TStrOutFunc);
    procedure ConnectEvents;
    procedure DoSetStatus(s : string);
  protected
    function GetTextFieldName: string;override;
    function GetNumberFieldName : string;override;
    function GetHistory: TBaseHistory;
    function GetStatusFieldName: string;override;
  public
    constructor CreateEx(aOwner: TComponent; DM: TComponent;
      aConnection: TComponent=nil; aMasterdata: TDataSet=nil); override;
    procedure DefineFields(aDataSet: TDataSet); override;
    procedure FillDefaults(aDataSet: TDataSet); override;
    function SelectByName(aName: string): Boolean;
    procedure Open; override;
    property Script : TScript read GetScript;
    procedure ResetScript;
    procedure CheckStatus(Output: TStringList; Module, aStatus: string);
    function CheckScript : string;
    function Execute(Parameters : Variant;Debug : Boolean = False) : Boolean;virtual;
    property Write : TStrOutFunc read FWriFunc write SetWriFunc;
    property Writeln : TStrOutFunc read FWrFunc write SetWRFunc;
    property Debugln : TStrOutFunc read FDWrFunc write SetDWRFunc;
    property Readln : TStrInFunc read FRlFunc write SetRlFunc;
    property History : TBaseHistory read FHistory;
    property Links : TLinks read FLinks;
    property Version : TField read GetVersion;
    function Copy(aNewVersion : Variant) : Boolean;
    procedure OpenItem(AccHistory: Boolean=True); override;
    property ActualObject : TBaseDBDataset read FActObject write FActObject;
    function Versionate(aNewversion : Variant;aMakeActive : Boolean = True) : Boolean;
    function Compile : Boolean;
    destructor Destroy;override;
    property OnStateChange : TNotifyEvent read FStateChange write FStateChange;
    property StatusProblems : TStringList read FStatusProblems;
  end;

  TSQLScript = class(TScript)
  private
    aDS: TDataSet;
    procedure SQLConn;
    procedure DoSetResults(aRes : string);
  protected
    function GetTyp: string; override;
    function GetStatus: TScriptStatus; override;
  public
    function Execute(aParameters: Variant;Debug : Boolean = false): Boolean; override;
  end;

  function ProcessScripts : Boolean;//process Scripts that must be runned cyclic
  procedure RegisterScriptType(aType : TScriptClass);

var
  Historyrun : Boolean;
  ScriptTypes : TClassList;
  FStatusCache : TStringList;

implementation

uses uStatistic,uData,httpsend,variants,uPerson,uMasterdata,uProjects,uOrder,
  uBaseApplication,uSystemMessage,utask,uMessages,uDocuments, uIntfStrConsts;

procedure RegisterScriptType(aType : TScriptClass);
begin
  ScriptTypes.Add(aType);
end;

function ProcessScripts : Boolean;//process Scripts that must be runned cyclic Result shows that it should be runned faster (debug)
var
  aScript: TBaseScript;
  aHistory: TBaseHistory;
  bScript: TBaseScript;
begin
  Result:=false;
  aScript := TBaseScript.Create(nil);
  aScript.Filter(Data.QuoteField('RUNEVERY')+'>'+Data.QuoteValue('0')+' OR '+Data.QuoteField('STATUS')+'='+Data.QuoteValue('d'));
  while not aScript.EOF do
    begin
      if (aScript.FieldByName('STATUS').AsString<>'S') and ((aScript.FieldByName('RUNMASHINE').AsString='') or (pos(GetSystemName,aScript.FieldByName('RUNMASHINE').AsString)>0)) then
        if (aScript.FieldByName('LASTRUN').AsDateTime+(aScript.FieldByName('RUNEVERY').AsInteger/MinsPerDay)<Now()) or (aScript.FieldByName('STATUS').AsString='d') or (aScript.FieldByName('STATUS').AsString='r') then
          begin
            bScript := TBaseScript.CreateEx(nil,aScript.DataModule,aScript.Connection);
            bScript.Select(aScript.Id.AsVariant);
            bScript.Open;
            Result := (aScript.FieldByName('STATUS').AsString='d');
            if bScript.Count=1 then
              bScript.Execute(Null);
            bScript.Free;
          end;
      aScript.Next;
    end;
  if Historyrun then
    begin
      aScript.Filter(Data.QuoteField('RUNONHISTORY')+'='+Data.QuoteValue('Y'));
      if (not aScript.EOF) then
        begin
          if aHistory.Count>0 then
            begin
              aHistory := TBaseHistory.Create(nil);
              while not aScript.EOF do
                begin
                  if aScript.FieldByName('STATUS').AsString<>'E' then
                    if aScript.FieldByName('LASTRUN').AsDateTime+(aScript.FieldByName('RUNEVERY').AsInteger/MinsPerDay)<Now() then
                      begin
                        aHistory.Filter(Data.QuoteField('DATE')+'>'+Data.DateTimeToFilter(aScript.FieldByName('LASTRUN').AsDateTime));
                        aHistory.Last;
                        while not aHistory.DataSet.BOF do
                          begin
                            aHistory.Prior;
                            bScript := TBaseScript.CreateEx(nil,aScript.DataModule,aScript.Connection);
                            bScript.Select(aScript.Id.AsVariant);
                            bScript.Open;
                            if bScript.Count=1 then
                              bScript.Execute(VarArrayOf([aHistory.FieldByName('ACTION').AsString,aHistory.FieldByName('DATE').AsDateTime]));
                            bScript.Free;
                          end;
                      end;
                  aScript.Next;
                end;
              aHistory.Free;
            end;
        end
      else Historyrun:=False;
    end;
  aScript.Free;
end;

procedure TSQLScript.SQLConn;
var
  aSQL: String;
begin
  aSQL := ReplaceSQLFunctions(Source);
  aDS := TBaseDBModule(Data).GetNewDataSet(aSQL);
end;

procedure TSQLScript.DoSetResults(aRes: string);
begin
  Results:=aRes;
end;

function TSQLScript.GetTyp: string;
begin
  Result := 'SQL';
end;

function TSQLScript.GetStatus: TScriptStatus;
begin
  Result:=inherited GetStatus;
end;

function TSQLScript.Execute(aParameters: Variant; Debug: Boolean): Boolean;
begin
  try
    SQLConn;
    with aDS as IBaseDbFilter do
      DoExecSQL;
    with aDS as IBaseDbFilter do
      DoSetResults('Num Rows Affected: '+IntToStr(NumRowsAffected));
    Result := True;
  except
    on e : Exception do
      begin
        if Assigned(Write) then
          Write('Error:'+e.Message);
        DoSetResults(e.Message);
        Result := False;
      end;
  end;
end;

procedure TBaseScript.ConnectEvents;
begin
  if Assigned(GetScript) then
    begin
      GetScript.Write:=Write;
      GetScript.Readln:=Readln;
      GetScript.Writeln:=Writeln;
      GetScript.Debugln:=Debugln;
    end;
end;

procedure TBaseScript.DoSetStatus(s: string);
begin
  Edit;
  FieldByName('STATUS').AsString:=s;
  Post;
end;

function TBaseScript.GetVersion: TField;
begin
  Result := FieldByName('VERSION');
end;

procedure TBaseScript.SetDWRFunc(AValue: TStrOutFunc);
begin
  if FDWrFunc=AValue then Exit;
  FDWrFunc:=AValue;
  ConnectEvents;
end;

procedure TBaseScript.SetRlFunc(AValue: TStrInFunc);
begin
  if FRlFunc=AValue then Exit;
  FRlFunc:=AValue;
  ConnectEvents;
end;

procedure TBaseScript.SetWRFunc(AValue: TStrOutFunc);
begin
  if FWrFunc=AValue then Exit;
  FWrFunc:=AValue;
  ConnectEvents;
end;

procedure TBaseScript.SetWriFunc(AValue: TStrOutFunc);
begin
  if FWriFunc=AValue then Exit;
  FWriFunc:=AValue;
  ConnectEvents;
end;

procedure TBaseScript.aScriptCheckModule(Sender: TObject);
begin
  if Assigned(FStatusProblems) then
    CheckStatus(FStatusProblems,(Sender as TBaseScript).Script.Name,(Sender as TBaseScript).Status.AsString);
end;

procedure TBaseScript.DataSetAfterScroll(aDataSet: TDataSet);
begin
  ResetScript;
end;

procedure TBaseScript.FDataSourceDataChange(Sender: TObject; Field: TField);
begin
  if not Assigned(Field) then exit;
  if DataSet.ControlsDisabled then exit;
  if Field.FieldName='SCRIPT' then
    if Assigned(GetScript) then
      FScript.Source:=Field.AsString;
  if (Dataset.State <> dsInsert) and (Field.FieldName = 'STATUS') then
    begin
      if not History.DataSet.Active then
        History.Open;
      if FStatus=Field.AsString then exit;
      History.AddItem(Self.DataSet,Format(strStatusChanged,[FStatus,Field.AsString]),'','',DataSet,ACICON_STATUSCH);
      FStatus := Field.AsString;
      if Assigned(FStateChange) then
        FStateChange(Self);
      OpenItem(False);
    end;
end;

function TBaseScript.GetScript: TScript;
var
  aScript: TScript = nil;
  i: Integer;
  aType: String;
begin
  aScript:=nil;
  if (not Assigned(FScript)) and (Assigned(FieldByName('SYNTAX'))) then
    begin
      for i := 0 to ScriptTypes.Count-1 do
        begin
          aScript := TScript(ScriptTypes[i].Create);
          if Uppercase(aScript.Typ)<>Uppercase(FieldByName('SYNTAX').AsString) then
            FreeAndNil(aScript);
          if Assigned(aScript) then break;
        end;
      if Assigned(aScript) then
        begin
          aScript.Init;
          aScript.Source:=FieldByName('SCRIPT').AsString;
          aScript.Name:=FieldByName('NAME').AsString;
          aScript.OnCheckModule:=@aScriptCheckModule;
          FScript:=aScript;
          FScript.Parent:=Self;
          FScript.Id:=Id.AsVariant;
          FScript.Version:=Version.AsVariant;
          ConnectEvents;
        end;
    end;
  Result := FScript;
end;

destructor TBaseScript.Destroy;
begin
  FStatusProblems.Free;
  ResetScript;
  FLinks.Free;
  FHistory.Free;
  FDataSource.Destroy;
  inherited Destroy;
end;

function TBaseScript.GetTextFieldName: string;
begin
  Result := 'NAME';
end;

function TBaseScript.GetNumberFieldName: string;
begin
  Result := 'NAME';
end;

function TBaseScript.GetHistory: TBaseHistory;
begin
  Result := FHistory;
end;

function TBaseScript.GetStatusFieldName: string;
begin
  Result:='STATUS';
end;

constructor TBaseScript.CreateEx(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited CreateEx(aOwner, DM, aConnection, aMasterdata);
  FStatusProblems := TStringList.Create;
  FDataSource := TDataSource.Create(Self);
  FDataSource.OnDataChange:=@FDataSourceDataChange;
  FSelectedName := Null;
  FDataSource.DataSet := DataSet;
  FHistory := TBaseHistory.CreateEx(Self,DM,aConnection,DataSet);
  FLinks := TLinks.CreateEx(Self,DM,aConnection);
  DataSet.AfterScroll:=@DataSetAfterScroll;
end;

procedure TBaseScript.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'SCRIPTS';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('TYPE',ftString,1,False);
            Add('PARENT',ftLargeint,0,False);
            Add('NAME',ftString,60,True);
            Add('STATUS',ftString,4,false);
            Add('VERSION',ftString,8,False);
            Add('ACTIVE',ftString,1,True);
            Add('SYNTAX',ftString,15,True);
            Add('PRIORITY',ftInteger,0,False);
            Add('RUNEVERY',ftInteger,0,False);
            Add('RUNMASHINE',ftString,150,False);
            Add('RUNONHISTORY',ftString,1,False);
            Add('LASTRUN',ftDateTime,0,False);
            Add('SCRIPT',ftMemo,0,false);
            Add('NOTE',ftMemo,0,false);
            Add('FOLDSTATE',ftString,200,false);
            Add('LASTRESULT',ftMemo,0,false);
          end;
      if Assigned(ManagedIndexdefs) then
        with ManagedIndexDefs do
          Add('NAME','NAME;VERSION',[ixUnique]);
    end;
end;
procedure TBaseScript.FillDefaults(aDataSet: TDataSet);
begin
  inherited FillDefaults(aDataSet);
  FieldByName('SYNTAX').AsString:='Pascal';
  FieldByName('SCRIPT').AsString:='begin'+LineEnding+'  '+LineEnding+'end.';
  FieldByName('ACTIVE').AsString  := 'Y';
  if FSelectedName<>Null then
    FieldByName('NAME').AsVariant:=FSelectedName;
end;

function TBaseScript.SelectByName(aName: string): Boolean;
begin
  with BaseApplication as IBaseDBInterface do
    with DataSet as IBaseDBFilter do
      begin
        Filter := Data.QuoteField('NAME')+'='+Data.QuoteValue(aName);
        FSelectedName := aName;
      end;
end;

procedure TBaseScript.Open;
begin
  inherited Open;
  FStatus := Status.AsString;
end;

procedure TBaseScript.ResetScript;
begin
  try
    if Assigned(FScript) then
      FreeAndNil(FScript);
  except
    on e : Exception do
      Write('Internal Error:'+e.Message);
  end;
end;

procedure TBaseScript.CheckStatus(Output: TStringList; Module,aStatus: string);
begin
  if FStatusCache.Count=0 then
    begin
      Data.States.Filter(Data.QuoteField('TYPE')+'='+Data.QuoteValue('S'));
      Data.States.First;
      while not Data.States.EOF do
        begin
          if Data.States.FieldByName('ACTIVE').AsString='N' then
            FStatusCache.Values[Data.States.FieldByName('STATUS').AsString]:=Data.States.FieldByName('STATUSNAME').AsString;
          Data.States.Next;
        end;
    end;
  if FStatusCache.Values[aStatus]<>'' then
    Output.Add(Module+'='+FStatusCache.Values[aStatus]);
end;

function TBaseScript.CheckScript: string;
begin
  Result := '';
  if Assigned(GetScript) then
    if FScript is TByteCodeScript then
      with FScript as TByteCodeScript do
        begin
          Compile;
        end;
end;

function TBaseScript.Execute(Parameters: Variant; Debug: Boolean): Boolean;
var
  aStartTime: TDateTime;
begin
  Result := False;
  try
    if Assigned(GetScript) then
      Result := FScript.Execute(Parameters,Debug);
  except
    Result := False;
  end;
end;

function TBaseScript.Copy(aNewVersion: Variant): Boolean;
var
  bScript: TBaseScript;
begin
  Result := True;
  bScript := TBaseScript.CreateEx(Self,DataModule,Self.Connection);
  try
    try
      bScript.Select(Id.AsVariant);
      bScript.Append;
      bScript.DirectAssign(Self);
      if aNewVersion <> bScript.Version.AsVariant then
        bScript.Version.AsVariant:=aNewVersion;
      bScript.CascadicPost;
      Self.Select(bScript.Id.AsVariant);
      Self.Open;
    except
      Result := False;
    end;
  finally
    bScript.Free;
  end;
  DataSet.Edit;
  Change;
end;

procedure TBaseScript.OpenItem(AccHistory: Boolean);
var
  aHistory: TAccessHistory;
  aObj: TObjects;
  aID: String;
  aFilter: String;
begin
  if Self.Count=0 then exit;
  try
    try
      aHistory := TAccessHistory.Create(nil);
      aObj := TObjects.Create(nil);
      if AccHistory then
        begin
          if DataSet.State<>dsInsert then
            begin
              if not Data.TableExists(aHistory.TableName) then
                aHistory.CreateTable;
              aHistory.Free;
              aHistory := TAccessHistory.CreateEx(nil,Data,nil,DataSet);
              aHistory.AddItem(DataSet,Format(strItemOpened,[Data.GetLinkDesc(Data.BuildLink(DataSet))]),Data.BuildLink(DataSet));
            end;
        end;
      if (DataSet.State<>dsInsert) and (Self.FieldByName('ACTIVE').AsString='Y') then
        begin
          if not Data.TableExists(aObj.TableName) then
            begin
              aObj.CreateTable;
              aObj.Free;
              aObj := TObjects.CreateEx(nil,Data,nil,DataSet);
            end;
          with aObj.DataSet as IBaseDBFilter do
            begin
              aFilter :=  Data.QuoteField(aObj.TableName)+'.'+Data.QuoteField('SQL_ID')+'='+Data.QuoteValue(Self.Id.AsVariant);
              Filter := aFilter;
              Limit := 0;
            end;
          aObj.Open;
          if aObj.Count=0 then
            begin
              aObj.Insert;
              aObj.Text.AsString := Self.Text.AsString;
              aObj.FieldByName('SQL_ID').AsVariant:=Self.Id.AsVariant;
              if aObj.Number.AsString<>Self.Number.AsString then
                begin
                  aObj.Edit;
                  aObj.Number.AsString := Self.FieldByName('NAME').AsString;
                end;
              if Assigned(Self.Matchcode) then
                aObj.Matchcode.AsString := Self.Matchcode.AsString;
              if Assigned(Self.Status) then
                aObj.Status.AsString := Self.Status.AsString;
              aObj.Number.AsVariant:=Self.Number.AsVariant;
              aObj.FieldByName('LINK').AsString:=Data.BuildLink(Self.DataSet);
              aObj.FieldByName('ICON').AsInteger:=Data.GetLinkIcon(Data.BuildLink(Self.DataSet),True);
              aObj.FieldByName('VERSION').AsString:=Self.FieldByName('VERSION').AsString;
              aObj.Post;
              Self.GenerateThumbnail;
            end
          else //Modify existing
            begin
              while aObj.Count>1 do
                aObj.Delete;
              if aObj.Text.AsString<>Self.Text.AsString then
                begin
                  aObj.Edit;
                  aObj.Text.AsString := Self.Text.AsString;
                end;
              if aObj.Number.AsString<>Self.Number.AsString then
                begin
                  aObj.Edit;
                  aObj.Number.AsString := Self.FieldByName('NAME').AsString;
                end;
              if Assigned(Self.Status) and (aObj.Status.AsString<>Self.Status.AsString) then
                begin
                  aObj.Edit;
                  aObj.Status.AsString := Self.Status.AsString;
                end;
              if Assigned(Self.Matchcode) and (aObj.Matchcode.AsString<>Self.Matchcode.AsString) then
                begin
                  aObj.Edit;
                  aObj.Matchcode.AsString := Self.Matchcode.AsString;
                end;
              if aObj.FieldByName('LINK').AsString<>Data.BuildLink(Self.DataSet) then
                begin
                  aObj.Edit;
                  aObj.FieldByName('LINK').AsString:=Data.BuildLink(Self.DataSet);
                end;
              if aObj.FieldByName('VERSION').AsString<>Self.FieldByName('VERSION').AsString then
                begin
                  aObj.Edit;
                  aObj.FieldByName('VERSION').AsString:=Self.FieldByName('VERSION').AsString;
                end;
              if aObj.CanEdit then
                aObj.Post;
            end;
        end;
    finally
      aObj.Free;
      aHistory.Free;
    end;
  except
  end;
end;

function TBaseScript.Versionate(aNewversion: Variant; aMakeActive: Boolean
  ): Boolean;
var
  bScript: TBaseScript;
begin
  Result := Copy(aNewversion);
  if aMakeActive then
    begin
      bScript := TBaseScript.CreateEx(Self,DataModule,Self.Connection);
      try
        try
          bScript.SelectByName(Number.AsString);
          bScript.Open;
          while not bScript.EOF do
            begin
              bScript.Edit;
              if bScript.Id.AsVariant<>Self.Id.AsVariant then
                bScript.FieldByName('ACTIVE').AsString:='N'
              else
                bScript.FieldByName('ACTIVE').AsString:='Y';
              bScript.Post;
              bScript.Next;
            end;
        except
          Result := False;
        end;
      finally
        bScript.Free;
      end;
    end
  else
    begin
      Edit;
      FieldByName('ACTIVE').AsString:='N';
      Post;
    end;
end;

function TBaseScript.Compile: Boolean;
begin
  FStatusProblems.Clear;
  Result := True;
  if Assigned(GetScript) and (FScript is TByteCodeScript) then
    begin
      if Assigned(Script.OnCheckModule) then
        Script.OnCheckModule(Self);
      Result := TByteCodeScript(FScript).Compile;
    end;
end;

initialization
  Historyrun:=True;
  ScriptTypes:=TClassList.Create;
  RegisterScriptType(TSQLScript);
  FStatusCache := TStringList.Create;
finalization
  FStatusCache.Free;
  ScriptTypes.Free;
end.

