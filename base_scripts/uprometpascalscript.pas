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
Created 07.04.2015
*******************************************************************************}
unit uprometpascalscript;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, genpascalscript, uprometscripts,uPSRuntime,uPSCompiler,uPSUtils,
  Utils,db,uBaseDBInterface,genscript,uBaseDbClasses;

type
  TScriptInternalPrint = function (aType,Reportname,Printer : string;Copies : Integer) : Boolean;
  { TPrometPascalScript }

  TPrometPascalScript = class(TPascalScript)
    function TPascalScriptUses(Sender: TPascalScript; const aName: tbtString;
      OnlyAdditional : Boolean): Boolean;
  private
    FRlFunc: TStrInFunc;
    FScript: TScript;
    FSlFunc: TSleepFunc;
    FWrFunc: TStrOutFunc;
    FWriFunc: TStrOutFunc;

    function InternalParamStr(Param : Integer) : String;
    function InternalParamCount : Integer;

    function InternalDataSet(SQL : string) : TDataSet;
    function InternalData : TBaseDBModule;
    function ActualObject : TBaseDBDataset;
    function InternalHistory(Action: string; ParentLink: string; Icon: Integer=0;
      ObjectLink: string=''; Reference: string='';aCommission: string='';aSource : string='';Date:TDateTime = 0) : Boolean;
    function InternalUserHistory(Action: string; UserName: string; Icon: Integer;
      ObjectLink: string; Reference: string; aCommission: string;
  aSource: string; Date: TDateTime): Boolean;
    procedure InternalStorValue(aName, Value: string);
    function InternalGetValue(aName : string) : string;
    procedure InternalMemoryStorValue(aName, Value: string);
    function InternalMemoryGetValue(aName : string) : string;
    procedure InternalExecuteScript(aCommand, aClient: string);

    procedure InternalExecuteScriptFuncionPS(aScript, aFunc, aParam: string);
    function InternalExecuteScriptFuncionPSRS(aScript, aFunc, aParam: string) : string;
    function InternalExecuteScriptFuncionRS(aScript, aFunc : string) : string;

    function InternalPrint(aType,Reportname,Printer : string;Copies : Integer) : Boolean;
    procedure InternalSetReportVariable(aName,Value : string);

    function InternalGetNumberFromNumberset(Numberset : string) : string;

    function InternalSaveFilefromDocuments(Filename,OutPath : string) : Boolean;
  public
    function InternalUses(Comp: TPSPascalCompiler; aName: string): Boolean; override;
    property Sleep : TSleepFunc read FSlFunc write FSlFunc;
    constructor Create; override;
    destructor Destroy;override;
  end;

var
  FContextDataSet : TDataSet;
  FVariables : TStringList;
  OnInternalPrint : TScriptInternalPrint;
  FReportVariables : TStringList;

implementation

uses uPerson,uMasterdata,uBaseERPDBClasses,uProjects,uMessages,
  uDocuments,utask,uOrder,uData,variants,uBaseApplication,uStatistic,
  uBaseDatasetInterfaces;

procedure TBaseDbListPropertyTextR(Self: TBaseDbList; var T: TField); begin T := Self.Text; end;
procedure TBaseDbListPropertyNumberR(Self: TBaseDbList; var T: TField); begin T := Self.Number; end;
procedure TBaseDbListPropertyBookNumberR(Self: TBaseDbList; var T: TField); begin T := Self.BookNumber; end;
procedure TBaseDbListPropertyBarcodeR(Self: TBaseDbList; var T: TField); begin T := Self.Barcode; end;
procedure TBaseDbListPropertyTypR(Self: TBaseDbList; var T: string); begin T := Self.Typ; end;
procedure TBaseDbListPropertyMatchCodeR(Self: TBaseDbList; var T: TField); begin T := Self.Matchcode; end;
procedure TBaseDbListPropertyDescriptionR(Self: TBaseDbList; var T: TField); begin T := Self.Description; end;
procedure TBaseDbListPropertyComissionR(Self: TBaseDbList; var T: TField); begin T := Self.Commission; end;
procedure TBaseDbListPropertyStatusR(Self: TBaseDbList; var T: TField); begin T := Self.Status; end;
procedure TPersonPropertyContR(Self: TPerson; var T: TPersonContactData); begin T := Self.ContactData; end;
procedure TPersonPropertyAdressR(Self: TPerson; var T: TBaseDbAddress); begin T := Self.Address; end;
procedure TBaseDbListPropertyHistoryR(Self: TBaseDBList; var T: TBaseHistory); var Hist : IBaseHistory; begin if Supports(Self, IBaseHistory, Hist) then T := Hist.GetHistory; end;
procedure TUserPropertyFollowsR(Self: TUser; var T: TFollowers); begin T := Self.Follows; end;
procedure TUserPropertyOptionsR(Self: TUser; var T: TOptions); begin T := Self.Options; end;
procedure TBaseDBDatasetPropertyDataSetR(Self: TBaseDBDataset; var T: TDataSet); begin T := Self.DataSet; end;
procedure TBaseDBModulePropertyUsersR(Self: TBaseDBModule; var T: TUser); begin T := Self.Users; end;
procedure TBaseDBDatasetPropertyCountR(Self: TBaseDBDataSet; var T: Integer); begin T := Self.Count; end;
procedure TBaseDBDatasetPropertyCanEditR(Self: TBaseDBDataSet; var T: Boolean); begin T := Self.CanEdit; end;
procedure TBaseDBDatasetPropertyActiveR(Self: TBaseDBDataSet; var T: Boolean); begin T := Self.Active; end;
procedure TBaseDBDatasetPropertyActiveW(Self: TBaseDBDataSet; var T: Boolean); begin Self.Active := T; end;
procedure TStoragePropertyJournalR(Self: TStorage; var T: TStorageJournal); begin T := Self.Journal; end;
procedure TMasterdataPropertyStorageR(Self: TMasterdata; var T: TStorage); begin T := Self.Storage; end;
procedure TProjectsTasksR(Self: TProject; var T: TProjectTasks); begin T := Self.Tasks; end;
procedure TMessagePropertyContentR(Self : TMessage;var T : TMessageContent);begin T := Self.Content; end;
procedure TMessagePropertyDocumentsR(Self : TMessage;var T : TDocuments);begin T := Self.Documents; end;
procedure TOrderPropertyPositionsR(Self : TOrder;var T : TOrderPos);begin T := Self.Positions; end;
procedure TOrderPropertyAddressR(Self : TOrder;var T : TOrderAddress);begin T := Self.Address; end;
procedure TMasterdataPropertyPositionsR(Self : TMasterdata;var T : TMDPos);begin T := Self.Positions; end;
procedure TProjectPropertyPositionsR(Self : TProject;var T : TProjectPositions);begin T := Self.Positions; end;
procedure TObjectPropertyMeasurementsR(Self : TObjects;var T : TMeasurement);begin T := Self.Measurements; end;
procedure TMasterdataPropertyMeasurementsR(Self : TMasterdata;var T : TMeasurement);begin T := Self.Measurements; end;
procedure TProjectPropertyMeasurementsR(Self : TProject;var T : TMeasurement);begin T := Self.Measurements; end;
procedure TBaseDbListPropertyDependenciesR(Self : TTaskList;var T : TDependencies);begin T := Self.Dependencies; end;
procedure TOrderQMTestDetailsR(Self : TOrderQMTest;var T : TOrderQMTestDetails);begin T := Self.Details; end;
procedure TOrderRepairDetailsR(Self : TOrderRepair;var T : TOrderRepairDetail);begin T := Self.Details; end;
procedure TOrderPosRepairR(Self : TOrderPos;var T : TOrderRepair);begin T := Self.Repair; end;
procedure TOrderPosQMTestR(Self : TOrderPos;var T : TOrderQMTest);begin T := Self.QMTest; end;

function TPrometPascalScript.TPascalScriptUses(Sender: TPascalScript;
  const aName: tbtString; OnlyAdditional: Boolean): Boolean;
var
  aScript: TBaseScript;
begin
  Result:=False;
  with BaseApplication as IBaseApplication do
    Debug('Uses start:'+aName);
  if aName = 'SYSTEM' then
    begin
      Result := True;
      try
        Sender.AddMethod(Self,@TPrometPascalScript.InternalWriteln,'procedure Writeln(P1: string);');
        Sender.AddMethod(Self,@TPrometPascalScript.InternalDebugln,'procedure Debugln(P1: string);');
        Sender.AddMethod(Self,@TPrometPascalScript.InternalWrite,'procedure Write(P1: string);');
        Sender.AddMethod(Self, @TPrometPascalScript.InternalParamStr,'function ParamStr(Param : Integer) : String;');
        Sender.AddMethod(Self, @TPrometPascalScript.InternalParamCount,'function ParamCount : Integer;');
        Sender.AddFunction(@UniToSys,'function UniToSys(const s: string): string;');
        Sender.AddFunction(@SysToUni,'function SysToUni(const s: string): string;');
      except
        Result := False; // will halt compilation
      end;
    end
  else if aName = 'SCRIPT' then
    begin
      Result := True;
      Sender.AddMethod(Self,@TPrometPascalScript.InternalExecuteScriptFuncionPS,'procedure ExecuteScriptFunctionPS(aScript, aFunc, aParam: string);');
      Sender.AddMethod(Self,@TPrometPascalScript.InternalExecuteScriptFuncionPSRS,'function ExecuteScriptFunctionPSRS(aScript, aFunc, aParam: string) : string;');
      Sender.AddMethod(Self,@TPrometPascalScript.InternalExecuteScriptFuncionRS,'function ExecuteScriptFunctionRS(aScript, aFunc : string) : string;');
    end
  else if aName = 'PROMET' then
    begin
      Result := True;
      try
        Sender.InternalUses(Sender.Compiler,'DB');
        Sender.InternalUses(Sender.Compiler,'DATEUTILS');
        Sender.AddMethod(Self,@TPrometPascalScript.InternalDataSet,'function DataSet(SQL : string) : TDataSet;');
        Sender.AddMethod(Self,@TPrometPascalScript.InternalHistory,'function History(Action : string;ParentLink : string;Icon : Integer;ObjectLink : string;Reference : string;Commission: string;Source : string;Date:TDateTime) : Boolean;');
        Sender.AddMethod(Self,@TPrometPascalScript.InternalUserHistory,'function UserHistory(Action : string;User   : string;Icon : Integer;ObjectLink : string;Reference : string;Commission: string;Source : string;Date:TDateTime) : Boolean;');
        Sender.AddMethod(Self,@TPrometPascalScript.InternalStorValue,'procedure StorDBValue(Name,Value : string);');
        Sender.AddMethod(Self,@TPrometPascalScript.InternalGetValue,'function GetDBValue(Name : string) : string;');
        Sender.AddMethod(Self,@TPrometPascalScript.InternalMemoryStorValue,'procedure StorMemoryValue(Name,Value : string);');
        Sender.AddMethod(Self,@TPrometPascalScript.InternalMemoryGetValue,'function GetMemoryValue(Name : string) : string;');
        Sender.AddMethod(Self,@TPrometPascalScript.InternalExecuteScript,'procedure ExecuteScript(Name,Client : string);');
        Sender.AddMethod(Self,@TPrometPascalScript.InternalPrint,'function PrintReport(aType : string;aReportname : string;aPrinter : string;Copies : Integer) : Boolean;');
        Sender.AddMethod(Self,@TPrometPascalScript.InternalSetReportVariable,'procedure SetReportVariable(Name : string;Value : string);');
        Sender.AddMethod(Self,@TPrometPascalScript.InternalGetNumberFromNumberset,'function GetNumberFromNumberset(Numberset : string) : string;');
        Sender.AddMethod(Self,@TPrometPascalScript.InternalSaveFilefromDocuments,'function SaveFilefromDocuments(Filename,OutPath : string) : Boolean;');
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TComponent'),TBaseDBDataset) do
          begin
            RegisterMethod('procedure Open;');
            RegisterMethod('procedure Close;');
            RegisterMethod('procedure Insert;');
            RegisterMethod('procedure Append;');
            RegisterMethod('procedure Delete;');
            RegisterMethod('procedure First;');
            RegisterMethod('procedure Last;');
            RegisterMethod('procedure Next;');
            RegisterMethod('procedure Prior;');
            RegisterMethod('procedure Post;');
            RegisterMethod('procedure Edit;');
            RegisterMethod('procedure Cancel;');
            RegisterMethod('function Locate(const keyfields: string; const keyvalues: Variant; options: TLocateOptions) : boolean;');
            RegisterMethod('function EOF : Boolean;');
            RegisterMethod('function FieldByName(const aFieldName : string) : TField;');
            RegisterMethod('procedure Filter(aFilter : string;aLimit : Integer);');
            RegisterMethod('procedure Select(aID : Variant);');
            RegisterProperty('ActualFilter','String',iptRW);
            RegisterProperty('ActualLimit','Integer',iptRW);
            RegisterProperty('DataSet','TDataSet',iptRW);
            RegisterProperty('Count','Integer',iptRW);
            RegisterProperty('CanEdit','Boolean',iptRW);
            RegisterProperty('Active','Boolean',iptRW);
          end;
        with Sender.ClassImporter.Add(TBaseDBDataset) do
          begin
            RegisterVirtualMethod(@TBaseDBDataset.Open, 'OPEN');
            RegisterVirtualMethod(@TBaseDBDataset.Close, 'CLOSE');
            RegisterVirtualMethod(@TBaseDBDataset.Insert, 'INSERT');
            RegisterVirtualMethod(@TBaseDBDataset.Append, 'APPEND');
            RegisterVirtualMethod(@TBaseDBDataset.Delete, 'DELETE');
            RegisterVirtualMethod(@TBaseDBDataset.First, 'FIRST');
            RegisterVirtualMethod(@TBaseDBDataset.Last, 'LAST');
            RegisterVirtualMethod(@TBaseDBDataset.Next, 'NEXT');
            RegisterVirtualMethod(@TBaseDBDataset.Prior, 'PRIOR');
            RegisterVirtualMethod(@TBaseDBDataset.Post, 'POST');
            RegisterVirtualMethod(@TBaseDBDataset.Edit, 'EDIT');
            RegisterVirtualMethod(@TBaseDBDataset.Cancel, 'CANCEL');
            RegisterVirtualMethod(@TBaseDBDataset.Locate, 'LOCATE');
            RegisterVirtualMethod(@TBaseDBDataset.EOF, 'EOF');
            RegisterVirtualMethod(@TBaseDBDataset.FieldByName, 'FIELDBYNAME');
            RegisterVirtualMethod(@TBaseDBDataset.Filter, 'FILTER');
            RegisterVirtualMethod(@TBaseDBDataset.Select, 'SELECT');
            RegisterPropertyHelper(@TBaseDBDatasetPropertyDataSetR,nil,'DATASET');
            RegisterPropertyHelper(@TBaseDBDatasetPropertyCountR,nil,'COUNT');
            RegisterPropertyHelper(@TBaseDBDatasetPropertyCanEditR,nil,'CANEDIT');
            RegisterPropertyHelper(@TBaseDBDatasetPropertyActiveR,@TBaseDBDatasetPropertyActiveW,'ACTIVE');
          end;
        Sender.AddMethod(Self,@TPrometPascalScript.ActualObject,'function ActualObject : TBaseDBDataSet;');
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TBaseDBDataSet'),TBaseDbList) do
          begin
            RegisterProperty('Text','TField',iptR);
            RegisterProperty('Number','TField',iptR);
            RegisterProperty('BookNumber','TField',iptR);
            RegisterProperty('Barcode','TField',iptR);
            RegisterProperty('Description','TField',iptR);
            RegisterProperty('Commission','TField',iptR);
            RegisterProperty('Status','TField',iptR);
            RegisterProperty('Typ','string',iptR);
            RegisterProperty('MatchCode','TField',iptR);
            RegisterMethod('function SelectFromLink(aLink : string) : Boolean;');
            RegisterMethod('function SelectFromNumber(aNumber : string) : Boolean;');
            RegisterMethod('function  ExportToXML : string;');
            RegisterMethod('procedure ImportFromXML(XML : string;OverrideFields : Boolean);');
            RegisterMethod('function ChangeStatus(aNewStatus : string) : Boolean;');
          end;
        with Sender.ClassImporter.Add(TBaseDbList) do
          begin
            RegisterVirtualMethod(@TBaseDbList.SelectFromLink,'SELECTFROMLINK');
            RegisterVirtualMethod(@TBaseDbList.SelectFromNumber,'SELECTFROMNUMBER');
            RegisterPropertyHelper(@TBaseDbListPropertyTextR,nil,'TEXT');
            RegisterPropertyHelper(@TBaseDbListPropertyNumberR,nil,'NUMBER');
            RegisterPropertyHelper(@TBaseDbListPropertyBookNumberR,nil,'BOOKNUMBER');
            RegisterPropertyHelper(@TBaseDbListPropertyBarcodeR,nil,'BARCODE');
            RegisterPropertyHelper(@TBaseDbListPropertyDescriptionR,nil,'DESCRIPTION');
            RegisterPropertyHelper(@TBaseDbListPropertyComissionR,nil,'COMMISSION');
            RegisterPropertyHelper(@TBaseDbListPropertyStatusR,nil,'STATUS');
            RegisterPropertyHelper(@TBaseDbListPropertyTypR,nil,'TYP');
            RegisterPropertyHelper(@TBaseDbListPropertyMatchCodeR,nil,'MATCHCODE');
            RegisterVirtualMethod(@TBaseDbList.ImportFromXML,'IMPORTFROMXML');
            RegisterVirtualMethod(@TBaseDbList.ExportToXML,'EXPORTTOXML');
            RegisterVirtualMethod(@TBaseDbList.ChangeStatus,'CHANGESTATUS');
          end;
        //TBaseHistory
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TBaseDBList'),TBaseHistory) do
          begin
            RegisterMethod('function         AddItem(aObject: TDataSet; aAction: string; aLink: string; aReference: string; aRefObject: TDataSet; aIcon: Integer;aComission: string;CheckDouble: Boolean;DoPost: Boolean; DoChange: Boolean) : Boolean;');
            RegisterMethod('function AddParentedItem(aObject: TDataSet; aAction: string;aParent : Variant; aLink: string; aReference: string; aRefObject: TDataSet; aIcon: Integer; aComission: string;CheckDouble: Boolean;DoPost: Boolean; DoChange: Boolean):Boolean;');
          end;
        with Sender.ClassImporter.Add(TBaseHistory) do
          begin
            RegisterVirtualMethod(@TBaseHistory.AddItem,'ADDITEM');
            RegisterVirtualMethod(@TBaseHistory.AddParentedItem,'ADDPARENTEDITEM');
          end;
        //Object (Element)
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TBaseDBDataSet'),TMeasurement) do
          begin
            RegisterMethod('constructor Create(aOwner : TComponent);');
          end;
        with Sender.ClassImporter.Add(TMeasurement) do
          begin
            RegisterConstructor(@TMeasurement.Create,'CREATE');
          end;
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TBaseDBList'),TObjects) do
          begin
            RegisterMethod('constructor Create(aOwner : TComponent);');
            RegisterProperty('Measurements','TMeasurement',iptR);
            RegisterProperty('History','TBaseHistory',iptR);
          end;
        with Sender.ClassImporter.Add(TObjects) do
          begin
            RegisterConstructor(@TObjects.Create,'CREATE');
            RegisterPropertyHelper(@TObjectPropertyMeasurementsR,nil,'MEASUREMENTS');
            RegisterPropertyHelper(@TBaseDbListPropertyHistoryR,nil,'HISTORY');
          end;
        //Document
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TBaseDBList'),TDocuments) do
          begin
            RegisterMethod('constructor Create(aOwner : TComponent);');
          end;
        with Sender.ClassImporter.Add(TDocuments) do
          begin
            RegisterConstructor(@TDocuments.Create,'CREATE');
          end;
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TDocuments'),TDocument) do
          begin
            RegisterMethod('constructor Create(aOwner : TComponent);');
          end;
        with Sender.ClassImporter.Add(TDocument) do
          begin
            RegisterConstructor(@TDocument.Create,'CREATE');
          end;
        //Messages
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TBaseDBDataSet'),TMessageContent) do
          begin
          end;
        with Sender.ClassImporter.Add(TMessageContent) do
          begin
          end;
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TBaseDBList'),TMessageList) do
          begin
            RegisterMethod('constructor Create(aOwner : TComponent);');
          end;
        with Sender.ClassImporter.Add(TMessageList) do
          begin
            RegisterConstructor(@TMessageList.Create,'CREATE');
          end;
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TMessageList'),TMessage) do
          begin
            RegisterMethod('constructor Create(aOwner : TComponent);');
            RegisterProperty('Content','TMessageContent',iptR);
            RegisterProperty('Documents','TDocuments',iptR);
            RegisterProperty('History','TBaseHistory',iptR);
          end;
        with Sender.ClassImporter.Add(TMessage) do
          begin
            RegisterConstructor(@TMessageList.Create,'CREATE');
            RegisterPropertyHelper(@TMessagePropertyContentR,nil,'CONTENT');
            RegisterPropertyHelper(@TMessagePropertyDocumentsR,nil,'DOCUMENTS');
            RegisterPropertyHelper(@TBaseDbListPropertyHistoryR,nil,'HISTORY');
          end;
        //Person
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TBaseDBList'),TBaseDbAddress) do
          begin
            RegisterMethod('function ToString: ansistring;');
            RegisterMethod('procedure FromString(aStr : AnsiString);');
          end;
        with Sender.ClassImporter.Add(TBaseDbAddress) do
          begin
            RegisterVirtualMethod(@TBaseDbAddress.ToString,'TOSTRING');
            RegisterVirtualMethod(@TBaseDbAddress.FromString,'FROMSTRING');
          end;
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TBaseDbAddress'),TPersonAddress) do
          begin
          end;
        with Sender.ClassImporter.Add(TPersonAddress) do
          begin
          end;
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TBaseDbList'),TPersonContactData) do
          begin
          end;
        with Sender.ClassImporter.Add(TPersonContactData) do
          begin
          end;
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TBaseDBList'),TPersonList) do
          begin
            RegisterMethod('constructor Create(aOwner : TComponent);');
          end;
        with Sender.ClassImporter.Add(TPersonList) do
          begin
            RegisterConstructor(@TPersonList.Create,'CREATE');
          end;
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TPersonList'),TPerson) do
          begin
            RegisterProperty('Address','TPersonAddress',iptR);
            RegisterProperty('ContactData','TPersonContactData',iptR);
            RegisterProperty('History','TBaseHistory',iptR);
          end;
        with Sender.ClassImporter.Add(TPerson) do
          begin
            RegisterPropertyHelper(@TPersonPropertyAdressR,nil,'ADDRESS');
            RegisterPropertyHelper(@TPersonPropertyContR,nil,'CONTACTDATA');
            RegisterPropertyHelper(@TBaseDbListPropertyHistoryR,nil,'HISTORY');
          end;
        //Positions
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TBaseDbDataSet'),TBaseDBPosition) do
          begin
          end;
        with Sender.ClassImporter.Add(TBaseDBPosition) do
          begin
          end;
        //Masterdata
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TBaseDbPosition'),TMDPos) do
          begin
          end;
        with Sender.ClassImporter.Add(TMDPos) do
          begin
          end;
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TBaseDBDataSet'),TStorageJournal) do
          begin
          end;
        with Sender.ClassImporter.Add(TStorageJournal) do
          begin
          end;
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TBaseDBDataSet'),TStorage) do
          begin
            RegisterProperty('Journal','TStorageJournal',iptR);
          end;
        with Sender.ClassImporter.Add(TStorage) do
          begin
            RegisterPropertyHelper(@TStoragePropertyJournalR,nil,'JOURNAL');
          end;

        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TBaseDBList'),TMasterdataList) do
          begin
            RegisterMethod('constructor Create(aOwner : TComponent);');
          end;
        with Sender.ClassImporter.Add(TMasterdataList) do
          begin
            RegisterConstructor(@TMasterdataList.Create,'CREATE');
          end;
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TMasterdataList'),TMasterdata) do
          begin
            RegisterProperty('History','TBaseHistory',iptR);
            RegisterProperty('Storage','TStorage',iptR);
            RegisterProperty('Positions','TMDPos',iptR);
            RegisterProperty('Measurements','TMeasurement',iptR);
          end;
        with Sender.ClassImporter.Add(TMasterdata) do
          begin
            RegisterPropertyHelper(@TBaseDbListPropertyHistoryR,nil,'HISTORY');
            RegisterPropertyHelper(@TMasterdataPropertyStorageR,nil,'STORAGE');
            RegisterPropertyHelper(@TMasterdataPropertyPositionsR,nil,'POSITIONS');
            RegisterPropertyHelper(@TMasterdataPropertyMeasurementsR,nil,'MEASUREMENTS');
          end;
        //Projects
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TBaseDbPosition'),TProjectPositions) do
          begin
          end;
        with Sender.ClassImporter.Add(TProjectPositions) do
          begin
          end;
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TBaseDbDataSet'),TDependencies) do
          begin
            RegisterMethod('procedure Add(aLink : string);');
          end;
        with Sender.ClassImporter.Add(TDependencies) do
          begin
            RegisterMethod(@TDependencies.Add,'ADD');
          end;
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TBaseDBList'),TTaskList) do
          begin
            RegisterMethod('constructor Create(aOwner : TComponent);');
            RegisterProperty('History','TBaseHistory',iptR);
            RegisterProperty('Dependencies','TDependencies',iptR);
            RegisterMethod('function Terminate(aEarliest : TDateTime;var aStart,aEnd,aDuration : TDateTime;IgnoreDepend : Boolean) : Boolean;');
          end;
        with Sender.ClassImporter.Add(TTaskList) do
          begin
            RegisterConstructor(@TTaskList.Create,'CREATE');
            RegisterPropertyHelper(@TBaseDbListPropertyHistoryR,nil,'HISTORY');
            RegisterPropertyHelper(@TBaseDbListPropertyDependenciesR,nil,'DEPENDENCIES');
            RegisterMethod(@TTaskList.Terminate,'TERMINATE');
          end;
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TTaskList'),TTask) do
          begin
            RegisterMethod('constructor Create(aOwner : TComponent);');
          end;
        with Sender.ClassImporter.Add(TTask) do
          begin
            RegisterConstructor(@TTask.Create,'CREATE');
          end;
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TBaseDBList'),TProjectList) do
          begin
            RegisterMethod('constructor Create(aOwner : TComponent);');
          end;
        with Sender.ClassImporter.Add(TProjectList) do
          begin
            RegisterConstructor(@TProjectList.Create,'CREATE');
          end;
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TTaskList'),TProjectTasks) do
          begin
          end;
        with Sender.ClassImporter.Add(TProjectTasks) do
          begin
          end;
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TProjectList'),TProject) do
          begin
            RegisterMethod('constructor Create(aOwner : TComponent);');
            RegisterProperty('History','TBaseHistory',iptR);
            RegisterProperty('Tasks','TProjectTasks',iptR);
            RegisterProperty('Positions','TProjectPositions',iptR);
            RegisterProperty('Measurements','TMeasurement',iptR);
          end;
        with Sender.ClassImporter.Add(TProject) do
          begin
            RegisterConstructor(@TProject.Create,'CREATE');
            RegisterPropertyHelper(@TBaseDbListPropertyHistoryR,nil,'HISTORY');
            RegisterPropertyHelper(@TProjectsTasksR,nil,'TASKS');
            RegisterPropertyHelper(@TProjectPropertyPositionsR,nil,'POSITIONS');
            RegisterPropertyHelper(@TProjectPropertyMeasurementsR,nil,'MEASUREMENTS');
          end;
        //Orders
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TBaseDBDataSet'),TOrderQMTestDetails) do
          begin
          end;
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TBaseDBDataSet'),TOrderQMTest) do
          begin
            RegisterProperty('Details','TOrderQMTestDetails',iptR);
          end;
        with Sender.ClassImporter.Add(TOrderQMTest) do
          begin
            RegisterPropertyHelper(@TOrderQMTestDetailsR,nil,'DETAILS');
          end;
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TBaseDbDataSet'),TOrderRepairDetail) do
          begin
          end;
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TBaseDbDataSet'),TOrderRepair) do
          begin
            RegisterProperty('Details','TOrderRepairDetail',iptR);
          end;
        with Sender.ClassImporter.Add(TOrderRepair) do
          begin
            RegisterPropertyHelper(@TOrderRepairDetailsR,nil,'DETAILS');
          end;
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TBaseDbDataSet'),TOrderRepairImages) do
          begin
          end;
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TBaseDbPosition'),TOrderPos) do
          begin
            RegisterProperty('QMTest','TOrderQMTest',iptR);
            RegisterProperty('Repair','TOrderRepair',iptR);
          end;
        with Sender.ClassImporter.Add(TOrderPos) do
          begin
            RegisterPropertyHelper(@TOrderPosRepairR,nil,'REPAIR');
            RegisterPropertyHelper(@TOrderPosQMTestR,nil,'QMTEST');
          end;
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TBaseDbAddress'),TOrderAddress) do
          begin
          end;
        with Sender.ClassImporter.Add(TOrderAddress) do
          begin
          end;
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TBaseDBList'),TOrderList) do
          begin
            RegisterMethod('constructor Create(aOwner : TComponent);');
          end;
        with Sender.ClassImporter.Add(TOrderList) do
          begin
            RegisterConstructor(@TOrderList.Create,'CREATE');
          end;
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TOrderList'),TOrder) do
          begin
            RegisterProperty('History','TBaseHistory',iptR);
            RegisterProperty('Address','TOrderAddress',iptR);
            RegisterProperty('Positions','TOrderPos',iptR);
            Sender.Compiler.AddTypeS('TPostResult', '(prSuccess,prAlreadyPosted,prFailed)');
            RegisterMethod('function DoPost: TPostResult;')
          end;
        with Sender.ClassImporter.Add(TOrder) do
          begin
            RegisterPropertyHelper(@TBaseDbListPropertyHistoryR,nil,'HISTORY');
            RegisterPropertyHelper(@TOrderPropertyAddressR,nil,'ADDRESS');
            RegisterPropertyHelper(@TOrderPropertyPositionsR,nil,'POSITIONS');
            RegisterMethod(@TOrder.DoPost,'DOPOST');
          end;
        //Small Gneral Datasets
        Sender.Compiler.AddClass(Sender.Compiler.FindClass('TBaseDBDataSet'),TFollowers);
        Sender.ClassImporter.Add(TFollowers);
        Sender.Compiler.AddClass(Sender.Compiler.FindClass('TBaseDBDataSet'),TOptions);
        Sender.ClassImporter.Add(TOptions);
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TBaseDbList'),TUser) do
          begin
            RegisterMethod('constructor Create(aOwner : TComponent);');
            RegisterProperty('History','TBaseHistory',iptR);
            RegisterProperty('Follows','TFollowers',iptR);
            RegisterProperty('Options','TOptions',iptR);
          end;
        with Sender.ClassImporter.Add(TUser) do
          begin
            RegisterConstructor(@TUser.Create,'CREATE');
            RegisterPropertyHelper(@TBaseDbListPropertyHistoryR,nil,'HISTORY');
            RegisterPropertyHelper(@TUserPropertyFollowsR,nil,'FOLLOWS');
            RegisterPropertyHelper(@TUserPropertyOptionsR,nil,'OPTIONS');
          end;
        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TBaseDbDataSet'),TActiveUsers) do
          begin
            RegisterMethod('constructor Create(aOwner : TComponent);');
          end;
        with Sender.ClassImporter.Add(TActiveUsers) do
          begin
            RegisterConstructor(@TActiveUsers.Create,'CREATE');
          end;

        with Sender.Compiler.AddClass(Sender.Compiler.FindClass('TComponent'),TBaseDBModule) do
          begin
            RegisterMethod('function GetConnection: TComponent;');
            //RegisterMethod('function GetSyncOffset: Integer;');
            //RegisterMethod('procedure SetSyncOffset(const AValue: Integer);');
            //RegisterMethod('function GetLimitAfterSelect: Boolean;');
            //RegisterMethod('function GetLimitSTMT: string;');

            RegisterMethod('function BuildLink(aDataSet : TDataSet) : string;');
            RegisterMethod('function GotoLink(const aLink : string) : Boolean;');
            RegisterMethod('function GetLinkDesc(aLink : string) : string;');
            RegisterMethod('function GetLinkLongDesc(aLink : string) : string;');
            RegisterMethod('function GetLinkIcon(aLink : string) : Integer;');

            RegisterMethod('function QuoteField(aField : string) : string;');
            RegisterMethod('function QuoteValue(aValue : string) : string;');
            RegisterMethod('function EscapeString(aValue : string) : string;');
            RegisterMethod('function DateToFilter(aValue : TDateTime) : string;');
            RegisterMethod('function DateTimeToFilter(aValue : TDateTime) : string;');
            RegisterMethod('function ProcessTerm(aTerm : string) : string;');

            RegisterProperty('Users','TUser',iptR);
          end;
        with Sender.ClassImporter.Add(TBaseDBModule) do
          begin
            //RegisterVirtualMethod(@TBaseDBModule.GetConnection, 'GETCONNECTION');
            RegisterVirtualMethod(@TBaseDBModule.BuildLink, 'BUILDLINK');
            RegisterVirtualMethod(@TBaseDBModule.GotoLink, 'GOTOLINK');
            RegisterVirtualMethod(@TBaseDBModule.GetLinkDesc, 'GETLINKDESC');
            RegisterVirtualMethod(@TBaseDBModule.GetLinkLongDesc, 'GETLINKLONGDESC');
            RegisterVirtualMethod(@TBaseDBModule.GetLinkIcon, 'GETLINKICON');

            RegisterVirtualMethod(@TBaseDBModule.QuoteField, 'QUOTEFIELD');
            RegisterVirtualMethod(@TBaseDBModule.QuoteValue, 'QUOTEVALUE');
            RegisterVirtualMethod(@TBaseDBModule.EscapeString, 'ESCAPESTRING');
            RegisterVirtualMethod(@TBaseDBModule.DateToFilter, 'DATETOFILTER');
            RegisterVirtualMethod(@TBaseDBModule.DateTimeToFilter, 'DATETIMETOFILTER');
            RegisterVirtualMethod(@TBaseDBModule.ProcessTerm, 'PROCESSTERM');

            RegisterPropertyHelper(@TBaseDBModulePropertyUsersR,nil,'USERS');
          end;
        Sender.AddMethod(Self,@TPrometPascalScript.InternalData,'function Data : TBaseDBModule');
      except
        Result := False; // will halt compilation
      end;
    end
  else if not OnlyAdditional then
    begin
      try
        try
          Result:=False;
          with BaseApplication as IBaseApplication do
            Debug('Uses get Unit from Database:'+aName);
          aScript := TBaseScript.CreateEx(nil,Data);
          aScript.Filter(Data.ProcessTerm('UPPER('+Data.QuoteField('NAME')+')=UPPER('+Data.QuoteValue(aName)+') AND '+Data.QuoteField('ACTIVE')+'='+Data.QuoteValue('Y')));
          if aScript.Count>0 then
            if aScript.Locate('NAME',aName,[loCaseInsensitive]) then
              begin
                if Assigned(OnCheckModule) then
                  OnCheckModule(aScript);
                Result := Sender.Compiler.Compile(aScript.FieldByName('SCRIPT').AsString);
              end;
        except
          Result := False;
        end;
      finally
        aScript.Free;
      end;
    end;
  if not OnlyAdditional then
    begin
      with BaseApplication as IBaseApplication do
        begin
          if Result then
            Debug('Uses end:'+aName+' successfully')
          else
            Debug('Uses end:'+aName+' failed');
        end;
    end
  else Result:=True;
end;

function TPrometPascalScript.InternalParamStr(Param: Integer): String;
begin
  Result:='';
  if not VarIsArray(FScript.Parameters) then exit;
  if Param<=VarArrayHighBound(FScript.Parameters,1) then
    Result:=FScript.Parameters[Param];
end;

function TPrometPascalScript.InternalParamCount: Integer;
begin
  if not VarIsArray(FScript.Parameters) then exit;
  Result := VarArrayHighBound(FScript.Parameters,1)+1;
end;

function TPrometPascalScript.InternalDataSet(SQL: string): TDataSet;
begin
  Result := TBaseDBModule(Data).GetNewDataSet(ReplaceSQLFunctions(SQL));
end;

function TPrometPascalScript.InternalData: TBaseDBModule;
begin
  Result := uData.Data;
end;

function TPrometPascalScript.ActualObject: TBaseDBDataset;
begin
  Result := nil;
  if Assigned(Parent) and (Parent is TBaseScript) then
    Result := TBaseScript(Parent).ActualObject;
end;

function TPrometPascalScript.InternalHistory(Action: string; ParentLink: string;
  Icon: Integer; ObjectLink: string; Reference: string; aCommission: string;
  aSource: string; Date: TDateTime): Boolean;
var
  aHistory: TBaseHistory;
  aDataSetClass: TBaseDBDatasetClass;
  aDataSet: TBaseDBDataset;
begin
  Result := False;
  if TBaseDBModule(Data).DataSetFromLink(ParentLink,aDataSetClass) then
    begin
      aDataSet := aDataSetClass.CreateEx(nil,Data);
      TBaseDbList(aDataSet).SelectFromLink(ParentLink);
      aDataSet.Open;
      if aDataSet.Count>0 then
        begin
          aHistory := TBaseHistory.CreateEx(nil,Data,nil,aDataSet.DataSet);
          aHistory.AddItemSR(aDataSet.DataSet,Action,ObjectLink,Reference,ObjectLink,Icon,aCommission,True,False);
          if aSource<>'' then
            aHistory.FieldByName('SOURCE').AsString:=aSource;
          aHistory.Post;
          aHistory.Free;
          result := True;
        end;
      aDataSet.Free;
    end;
end;
function TPrometPascalScript.InternalUserHistory(Action: string; UserName: string;
  Icon: Integer; ObjectLink: string; Reference: string; aCommission: string;
  aSource: string; Date: TDateTime): Boolean;
var
  aUsers: TUser;
begin
  Result := False;
  aUsers := TUser.Create(nil);
  if aUsers.Locate('NAME',UserName,[loCaseInsensitive]) then
    begin
      Result := aUsers.History.AddItemSR(aUsers.DataSet,Action,ObjectLink,Reference,ObjectLink,Icon,aCommission,True,False);
      if aSource<>'' then
        aUsers.History.FieldByName('SOURCE').AsString:=aSource;
      aUsers.History.Post;
    end;
  aUsers.Free;
end;

procedure TPrometPascalScript.InternalStorValue(aName, Value : string);
var
  aVariable: TVariables;
begin
  aVariable := TVariables.Create(nil);
  aVariable.StringValue[aName] := Value;
  aVariable.Free;
end;

function TPrometPascalScript.InternalGetValue(aName: string): string;
var
  aVariable: TVariables;
begin
  aVariable := TVariables.Create(nil);
  result := aVariable.StringValue[aName];
  aVariable.Free;
end;

procedure TPrometPascalScript.InternalMemoryStorValue(aName, Value: string);
begin
  fVariables.Values[aName] := Value;
end;

function TPrometPascalScript.InternalMemoryGetValue(aName: string): string;
begin
  result := fVariables.Values[aName];
end;

procedure TPrometPascalScript.InternalExecuteScript(aCommand, aClient: string);
begin
  {
  if Assigned(BaseApplication) then
    with BaseApplication as IBaseApplication do
      TMessageHandler(GetMessageManager).SendCommand(aClient,aCommand);
  }
end;

procedure TPrometPascalScript.InternalExecuteScriptFuncionPS(aScript,aFunc,
  aParam: string);
var
  bScript: TBaseScript;
begin
  bScript := TBaseScript.Create(nil);
  bScript.Filter(Data.QuoteField('NAME')+'='+Data.QuoteValue(aScript));
  if bScript.Count>0 then
    begin
      try
        if TPrometPascalScript(bScript.Script).Compile then
          TPrometPascalScript(bScript.Script).Runtime.RunProcPN([aParam],aFunc);
      except
      end;
    end;
  bScript.Free;
end;

function TPrometPascalScript.InternalExecuteScriptFuncionPSRS(aScript, aFunc,
  aParam: string): string;
var
  bScript: TBaseScript;
begin
  bScript := TBaseScript.Create(nil);
  bScript.Filter(Data.QuoteField('NAME')+'='+Data.QuoteValue(aScript));
  if bScript.Count>0 then
    begin
      try
        if TPascalScript(bScript.Script).Compile then
          Result := TPascalScript(bScript.Script).Runtime.RunProcPN([aParam],aFunc);
      except
        Result := '';
      end;
    end;
  bScript.Free;
end;

function TPrometPascalScript.InternalExecuteScriptFuncionRS(aScript,
  aFunc: string): string;
var
  bScript: TBaseScript;
begin
  bScript := TBaseScript.Create(nil);
  bScript.Filter(Data.QuoteField('NAME')+'='+Data.QuoteValue(aScript));
  if bScript.Count>0 then
    begin
      try
        if TPascalScript(bScript.Script).Compile then
          Result := TPascalScript(bScript.Script).Runtime.RunProcPN([],aFunc);
      except
        Result := '';
      end;
    end;
  bScript.Free;
end;

function TPrometPascalScript.InternalPrint(aType, Reportname, Printer: string;
  Copies: Integer): Boolean;
begin
  Result := False;
  if Assigned(OnInternalPrint) then
    Result := OnInternalPrint(aType,ReportName,Printer,Copies);
end;

procedure TPrometPascalScript.InternalSetReportVariable(aName, Value: string);
begin
  if not Assigned(FReportVariables) then
    FReportVariables := TStringList.Create;
  FReportVariables.Values[aName] := Value;
end;

function TPrometPascalScript.InternalGetNumberFromNumberset(Numberset: string
  ): string;
begin
  Result := Data.Numbers.GetNewNumber(Numberset);
end;

function TPrometPascalScript.InternalSaveFilefromDocuments(Filename,
  OutPath: string): Boolean;
var
  aDocuments: TDocument;
  aStream: TFileStream;
  aName: String;
  aExt: String;
  aDocument: TDocument;
  aScript: TBaseScript;
begin
  Result := False;
  aDocuments := TDocument.Create(nil);
  aDocuments.Select(Id,'S',Id,Version,Null);
  aDocuments.Open;
  aName := ExtractFileName(Filename);
  if rpos('.',aName)>0 then
    aName := copy(aName,0,rpos('.',aName)-1);
  aExt := ExtractFileExt(Filename);
  if pos('.',aExt)>0 then
    aExt := copy(aExt,pos('.',aExt)+1,length(aExt));
  if aDocuments.Locate('NAME;EXTENSION',VarArrayOf([aName,aExt]),[loCaseInsensitive]) then
    begin
      aDocument := TDocument.Create(nil);
      aDocument.SelectByNumber(aDocuments.FieldByName('NUMBER').AsVariant);
      aDocument.Open;
      aStream := TFileStream.Create(OutPath,fmCreate);
      aDocument.CheckoutToStream(aStream);
      aStream.Free;
      aDocument.Free;
      Result := True;
    end;
  aDocuments.Free;
end;

function TPrometPascalScript.InternalUses(Comp: TPSPascalCompiler; aName: string
  ): Boolean;
begin
  Result:=inherited InternalUses(Comp, aName);
  Result := TPascalScriptUses(Self,aName,Result);
end;

constructor TPrometPascalScript.Create;
begin
  inherited Create;
  FReportVariables := nil
end;


destructor TPrometPascalScript.Destroy;
begin
  if Assigned(FReportVariables) then
    FReportVariables.Free;
  inherited Destroy;
end;

initialization
  RegisterScriptType(TPrometPascalScript);
  FVariables := TStringList.Create;
  OnInternalPrint := nil;
finalization
  FVariables.Free;
end.

