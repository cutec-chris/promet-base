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
  ,uBaseDatasetInterfaces,uBaseERPDBClasses;

type
  { TBaseScript }

  TBaseScript = class(TBaseERPList,IBaseHistory)
  private
    aDS: TDataSet;
    FHistory: TBaseHistory;
    FLinks: TLinks;
    FDataSource: TDataSource;
    FRlFunc: TStrInFunc;
    FWrFunc: TStrOutFunc;
    FWriFunc: TStrOutFunc;
    FSelectedName : variant;
    function GetVersion: TField;
    procedure SQLConn;
  protected
    procedure DoSetResults(aRes : string);
    procedure DoSetStatus(s : string);
    function GetTextFieldName: string;override;
    function GetNumberFieldName : string;override;
    procedure InternalWrite(const s: string);
    procedure InternalWriteln(const s: string);
    procedure InternalReadln(var s: string);
    function GetHistory: TBaseHistory;
  public
    constructor CreateEx(aOwner: TComponent; DM: TComponent;
      aConnection: TComponent=nil; aMasterdata: TDataSet=nil); override;
    procedure DefineFields(aDataSet: TDataSet); override;
    procedure FillDefaults(aDataSet: TDataSet); override;
    function SelectByName(aName: string): Boolean;
    function Execute(Parameters : Variant) : Boolean;virtual;
    property Write : TStrOutFunc read FWriFunc write FWriFunc;
    property Writeln : TStrOutFunc read FWrFunc write FWRFunc;
    property Readln : TStrInFunc read FRlFunc write FRlFunc;
    property History : TBaseHistory read FHistory;
    property Links : TLinks read FLinks;
    property Version : TField read GetVersion;
    function Copy(aNewVersion : Variant) : Boolean;
    procedure OpenItem(AccHistory: Boolean=True); override;
    function Versionate(aNewversion : Variant;aMakeActive : Boolean = True) : Boolean;
    destructor Destroy;override;
  end;

  function ProcessScripts : Boolean;//process Scripts that must be runned cyclic

var
  Historyrun : Boolean;

implementation
uses uStatistic,uData,httpsend,variants,uPerson,uMasterdata,uProjects,uOrder,
  uBaseApplication,uSystemMessage,utask,uMessages,uDocuments, uIntfStrConsts;
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

procedure TBaseScript.SQLConn;
var
  aSQL: String;
begin
  aSQL := ReplaceSQLFunctions(FieldByName('SCRIPT').AsString);
  aDS := TBaseDBModule(DataModule).GetNewDataSet(aSQL,Connection);
end;

function TBaseScript.GetVersion: TField;
begin
  Result := FieldByName('VERSION');
end;

destructor TBaseScript.Destroy;
begin
  FLinks.Free;
  FHistory.Free;
  FDataSource.Destroy;
  inherited Destroy;
end;

procedure TBaseScript.DoSetResults(aRes: string);
begin
  Edit;
  FieldByName('LASTRESULT').AsString:=aRes;
  Post;
end;

procedure TBaseScript.DoSetStatus(s: string);
begin
  Edit;
  FieldByName('STATUS').AsString:=s;
  Post;
end;

function TBaseScript.GetTextFieldName: string;
begin
  Result := 'NAME';
end;

function TBaseScript.GetNumberFieldName: string;
begin
  Result := 'NAME';
end;

procedure TBaseScript.InternalWrite(const s: string);
begin
  if Assigned(FWriFunc) then FWriFunc(s);
end;

procedure TBaseScript.InternalWriteln(const s: string);
begin
  if Assigned(FWrFunc) then FWrFunc(s);
end;

procedure TBaseScript.InternalReadln(var s: string);
begin
  if Assigned(FRlFunc) then FRlFunc(s);
end;

function TBaseScript.GetHistory: TBaseHistory;
begin
  Result := FHistory;
end;

constructor TBaseScript.CreateEx(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited CreateEx(aOwner, DM, aConnection, aMasterdata);
  FDataSource := TDataSource.Create(Self);
  FSelectedName := Null;
  FDataSource.DataSet := DataSet;
  FHistory := TBaseHistory.CreateEx(Self,DM,aConnection,DataSet);
  FLinks := TLinks.CreateEx(Self,DM,aConnection);
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

function TBaseScript.Execute(Parameters: Variant): Boolean;
var
  aStartTime: TDateTime;
begin
  Result := False;
  if Count=0 then exit;
  OpenItem(False);
  if (pos(GetSystemName,FieldByName('RUNMASHINE').AsString)>0) or (trim(FieldByName('RUNMASHINE').AsString)='') then
    begin
      DoSetStatus('R');
      Edit;
      FieldByName('LASTRESULT').Clear;
      aStartTime:=Now();
      Post;
      Result := False;
      try
        if lowercase(FieldByName('SYNTAX').AsString) = 'sql' then
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
                  InternalWriteln('Error:'+e.Message);
                  DoSetResults(e.Message);
                  Result := False;
                end;
            end;
          end;
        if Result then
          begin
            DoSetStatus('N');
            Edit;
            FieldByName('LASTRUN').AsDateTime:=aStartTime;
            Post;
          end
        else
          begin
            DoSetStatus('E');
          end;
      except
      end;
    end
  else
    begin
      DoSetStatus('r');
      while FieldByName('STATUS').AsString='r' do
        begin
          sleep(500);
          DataSet.Refresh;
        end;
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
              aID := FieldByName('NAME').AsString;
              aFilter :=  Data.QuoteField('NUMBER')+'='+Data.QuoteValue(aID);
              Filter := aFilter;
              Limit := 0;
            end;
          aObj.Open;
          if aObj.Count=0 then
            begin
              aObj.Insert;
              aObj.Text.AsString := Self.Text.AsString;
              aObj.FieldByName('SQL_ID').AsVariant:=Self.Id.AsVariant;
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

initialization
  Historyrun:=True;
end.

