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
Created 15.07.2015
*******************************************************************************}
unit usqldbdm;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, db, uModifiedDS,sqldb,Utils,
  uBaseDatasetInterfaces,syncobjs,uBaseDBInterface,uBaseDbClasses;
type
  TUnprotectedDataSet = class(TDataSet);

  { TZeosDBDM }

  TSqlDBDM = class(TBaseDBModule)
  private
    FMainConnection : TSQLConnection;
    FLimitAfterSelect : Boolean;
    FLimitSTMT : string;
    FDBTyp : string;
    FProperties : string;
    function GetConnection: TComponent;override;
    function DBExists : Boolean;
  protected
    function GetSyncOffset: Integer;override;
    procedure SetSyncOffset(const AValue: Integer);override;
    function GetLimitAfterSelect: Boolean;override;
    function GetLimitSTMT: string;override;
  public
    constructor Create(AOwner : TComponent);override;
    destructor Destroy;override;
    function SetProperties(aProp : string;Connection : TComponent = nil) : Boolean;override;
    function CreateDBFromProperties(aProp: string): Boolean; override;
    function IsSQLDB : Boolean;override;
    function GetNewDataSet(aTable : TBaseDBDataSet;aConnection : TComponent = nil;MasterData : TDataSet = nil;aTables : string = '') : TDataSet;override;
    function GetNewDataSet(aSQL : string;aConnection : TComponent = nil;MasterData : TDataSet = nil;aOrigtable : TBaseDBDataSet = nil) : TDataSet;override;
    procedure DestroyDataSet(DataSet : TDataSet);override;
    function Ping(aConnection : TComponent) : Boolean;override;
    function DateToFilter(aValue : TDateTime) : string;override;
    function DateTimeToFilter(aValue : TDateTime) : string;override;
    function GetUniID(aConnection : TComponent = nil;Generator : string = 'GEN_SQL_ID';AutoInc : Boolean = True) : Variant;override;
    procedure StreamToBlobField(Stream : TStream;DataSet : TDataSet;Fieldname : string);override;
    procedure BlobFieldToStream(DataSet: TDataSet; Fieldname: string;
      dStream: TStream); override;
    function GetErrorNum(e: EDatabaseError): Integer; override;
    procedure DeleteExpiredSessions;override;
    function GetNewConnection: TComponent;override;
    function QuoteField(aField: string): string; override;
    procedure Disconnect(aConnection : TComponent);override;
    function StartTransaction(aConnection : TComponent;ForceTransaction : Boolean = False): Boolean;override;
    function CommitTransaction(aConnection : TComponent): Boolean;override;
    function RollbackTransaction(aConnection : TComponent): Boolean;override;
    function TableExists(aTableName : string;aConnection : TComponent = nil;AllowLowercase: Boolean = False) : Boolean;override;
    function TriggerExists(aTriggerName: string; aConnection: TComponent=nil;
       AllowLowercase: Boolean=False): Boolean; override;
    function GetDBType: string; override;
    function GetDBLayerType : string;override;
    function CreateTrigger(aTriggerName: string; aTableName: string;
      aUpdateOn: string; aSQL: string;aField : string = ''; aConnection: TComponent=nil): Boolean;
      override;
    function DropTable(aTableName : string) : Boolean;override;
    function FieldToSQL(aName : string;aType : TFieldType;aSize : Integer;aRequired : Boolean) : string;
    function GetColumns(TableName : string) : TStrings;override;
  end;

  { TSqlDBDataSet }

  TSqlDBDataSet = class(TSQLQuery,IBaseDBFilter,IBaseManageDB,IBaseSubDatasets,IBaseModifiedDS)
  private
    FSubDataSets : Tlist;
    FFields : string;
    FFilter,FBaseFilter : string;
    FLimit : Integer;
    FMDS: TDataSource;
    FSortDirection : TSortDirection;
    FSortFields : string;
    FTableNames : string;
    FDefaultTableName : string;
    FManagedFieldDefs : TFieldDefs;
    FManagedIndexDefs : TIndexDefs;
    FOrigTable : TBaseDBDataSet;
    FUsePermissions : Boolean;
    FTableCaption : string;
    FDistinct : Boolean;
    DoCheck: Boolean;
    FUpStdFields : Boolean;
    FUpChangedBy : Boolean;
    FBaseSortFields : string;
    FBaseSorting : string;
    FBaseSortDirection : TSortDirection;
    FUseBaseSorting : Boolean;
    FUseIntegrity : Boolean;
    FChangeUni : Boolean;
    FSQL : string;
    FHasNewID : Boolean;
    procedure SetNewIDIfNull;
    function BuildSQL : string;
    function IndexExists(aIndexName : string) : Boolean;
    procedure WaitForLostConnection;
  protected
    //Internal DataSet Methods that needs to be changed
    procedure InternalOpen; override;
    procedure InternalRefresh; override;
    procedure InternalPost; override;
    procedure DoAfterInsert; override;
    procedure DoBeforePost; override;
    procedure DoBeforeInsert; override;
    procedure DoBeforeEdit; override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
    procedure DoBeforeDelete; override;
    procedure DoAfterDelete; override;
    procedure DoAfterScroll; override;
    procedure DoBeforeCancel; override;
    //IBaseDBFilter
    function GetFields: string;
    function GetBaseFilter: string;
    function GetLimit: Integer;
    function GetSortDirection: TSortDirection;
    function GetSortFields: string;
    function GetLocalSortFields : string;
    function GetBaseSortFields: string;
    function GetSortLocal: Boolean;
    procedure SetFields(const AValue: string);
    function GetFilter: string;
    procedure SetFilter(const AValue: string);
    procedure SetBaseFilter(const AValue: string);
    function GetSQL: string;
    procedure SetSQL(const AValue: string);
    procedure Setlimit(const AValue: Integer);
    procedure SetSortDirection(const AValue: TSortDirection);
    procedure SetSortFields(const AValue: string);
    procedure SetLocalSortFields(const AValue : string);
    procedure SetBaseSortFields(const AValue: string);
    procedure SetSortLocal(const AValue: Boolean);
    function GetFilterTables: string;
    procedure SetFilterTables(const AValue: string);
    function GetUsePermissions: Boolean;
    procedure SetUsePermisions(const AValue: Boolean);
    function GetDistinct: Boolean;
    procedure SetDistinct(const AValue: Boolean);
    function GetBaseSorting: string;
    procedure SetBaseSorting(AValue: string);
    function GetBaseSortDirection: TSortDirection;
    procedure SetBaseSortDirection(AValue: TSortDirection);
    function GetUseBaseSorting: Boolean;
    procedure SetUseBaseSorting(AValue: Boolean);
    function GetfetchRows: Integer;
    procedure SetfetchRows(AValue: Integer);
    //IBaseManageDB
    function GetManagedFieldDefs: TFieldDefs;
    function GetManagedIndexDefs: TIndexDefs;
    function GetTableName: string;
    procedure SetTableName(const AValue: string);
    function CreateTable : Boolean;
    function CheckTable : Boolean;
    function AlterTable : Boolean;
    function GetConnection: TComponent;
    function GetTableCaption: string;
    procedure SetTableCaption(const AValue: string);
    function GetUpStdFields: Boolean;
    procedure SetUpStdFields(AValue: Boolean);
    function GetUpChangedBy: Boolean;
    procedure SetUpChangedBy(AValue: Boolean);
    function GetUseIntegrity: Boolean;
    procedure SetUseIntegrity(AValue: Boolean);
    //IBaseSubDataSets
    function GetSubDataSet(aName : string): TComponent;
    procedure RegisterSubDataSet(aDataSet : TComponent);
    function GetCount : Integer;
    function GetSubDataSetIdx(aIdx : Integer): TComponent;
    //IBaseModifiedDS
    function IsChanged: Boolean;
  public
    constructor Create(AOwner : TComponent);override;
    destructor Destroy;override;
    property MasterDataSource : TDataSource read FMDS write FMDS;
    property DefaultTableName : string read FDefaultTableName;
    procedure DoExecSQL;
    function NumRowsAffected: Integer;
  end;
implementation
uses uBaseApplication,uEncrypt;
resourcestring
  strUnknownDbType                = 'Unbekannter Datenbanktyp';
  strDatabaseConnectionLost       = 'Die Datenbankverbindung wurde verlohren !';

procedure TSqlDBDataSet.SetNewIDIfNull;
begin
  if (FieldDefs.IndexOf('AUTO_ID') = -1) and (FieldDefs.IndexOf('SQL_ID') > -1) and  FieldByName('SQL_ID').IsNull then
    begin
      FieldByName('SQL_ID').AsVariant:=TBaseDBModule(Self.Owner).GetUniID(Transaction);
      FHasNewID:=True;
    end
  else if (FieldDefs.IndexOf('SQL_ID') = -1) and (FieldDefs.IndexOf('AUTO_ID') > -1) and FieldByName('AUTO_ID').IsNull then
    begin
      FieldByName('AUTO_ID').AsVariant:=TBaseDBModule(Self.Owner).GetUniID(Transaction,'GEN_AUTO_ID');
      FHasNewID:=True;
    end;
end;

function TSqlDBDataSet.BuildSQL : string;
function BuildJoins : string;
var
  aDS : string;
  tmp: String;
begin
  if not (pos(',',FTableNames) > 0) then
    begin
      Result := FTableNames;
      if Result = '' then
        Result := FDefaultTableName;
      Result := TBaseDBModule(Owner).QuoteField(Result);
      exit;
    end;
  tmp := FTableNames+',';
  Result := copy(FTableNames,0,pos(',',FTableNames)-1);
  aDS := Result;
  tmp := copy(tmp,pos(',',tmp)+1,length(tmp));
  while pos(',',tmp) > 0 do
    begin
      Result := Result+ ' inner join '+TBaseDBModule(Owner).QuoteField(copy(tmp,0,pos(',',tmp)-1))+' on '+TBaseDBModule(Owner).QuoteField(copy(tmp,0,pos(',',tmp)-1))+'.REF_ID='+aDS+'.SQL_ID';
      aDS := TBaseDBModule(Owner).QuoteField(copy(tmp,0,pos(',',tmp)-1));
      tmp := copy(tmp,pos(',',tmp)+1,length(tmp));
    end;
end;

var
  aFilter: String;
  aRefField: String;
  tmp: String;
  SResult: String;
  PJ: String = '';
  PW: String = '';

  procedure BuildSResult;
  begin
    SResult := '';
    if pos(',',TSqlDBDM(Owner).QuoteField(FSortFields)) = 0 then
      begin
        sResult += TSqlDBDM(Owner).QuoteField(FDefaultTableName)+'.'+TSqlDBDM(Owner).QuoteField(FSortFields);
        if FSortDirection = sdAscending then
          sResult += ' ASC'
        else if FSortDirection = sdDescending then
          sResult += ' DESC'
        else
          begin
            if FBaseSortDirection = sdAscending then
              sResult += ' ASC'
            else if FBaseSortDirection = sdDescending then
              sResult += ' DESC'
          end;
      end
    else
      begin
        tmp := FSortFields;
        while pos(',',tmp) > 0 do
          begin
            sResult += TSqlDBDM(Owner).QuoteField(FDefaultTableName)+'.'+TSqlDBDM(Owner).QuoteField(copy(tmp,0,pos(',',tmp)-1));
            tmp := copy(tmp,pos(',',tmp)+1,length(tmp));
            if FSortDirection = sdAscending then
              sResult += ' ASC'
            else
              sResult += ' DESC';
            if trim(tmp) > '' then
              sResult+=',';
          end;
        if tmp <> '' then
          begin
            sResult += TSqlDBDM(Owner).QuoteField(FDefaultTableName)+'.'+TSqlDBDM(Owner).QuoteField(tmp);
            if FSortDirection = sdAscending then
              sResult += ' ASC'
            else
              sResult += ' DESC';
          end;
      end;
  end;

begin
  if FSQL <> '' then
    begin
      BuildSResult;
      if (FManagedFieldDefs.IndexOf('AUTO_ID') = -1) and (TSqlDBDM(Owner).UsersFilter <> '') and FUsePermissions then
        begin
          PJ := ' LEFT JOIN '+TSqlDBDM(Owner).QuoteField('PERMISSIONS')+' ON ('+TSqlDBDM(Owner).QuoteField('PERMISSIONS')+'.'+TSqlDBDM(Owner).QuoteField('REF_ID_ID')+'='+TSqlDBDM(Owner).QuoteField(FDefaultTableName)+'.'+TSqlDBDM(Owner).QuoteField('SQL_ID')+')';
          PW := ' AND ('+aFilter+') AND (('+TSqlDBDM(Owner).UsersFilter+') OR '+TSqlDBDM(Owner).QuoteField('PERMISSIONS')+'.'+TSqlDBDM(Owner).QuoteField('USER')+' is NULL)';
        end
      else if (FManagedFieldDefs.IndexOf('AUTO_ID') = -1) and FUsePermissions then
        begin
          PJ := ' LEFT JOIN '+TSqlDBDM(Owner).QuoteField('PERMISSIONS')+' ON ('+TSqlDBDM(Owner).QuoteField('PERMISSIONS')+'.'+TSqlDBDM(Owner).QuoteField('REF_ID_ID')+'='+TSqlDBDM(Owner).QuoteField(FDefaultTableName)+'.'+TSqlDBDM(Owner).QuoteField('SQL_ID')+')';
          PW := ' AND ('+TSqlDBDM(Owner).QuoteField('PERMISSIONS')+'.'+TSqlDBDM(Owner).QuoteField('USER')+' is NULL)'
        end;
      PW := StringReplace(PW,'AND ()','',[rfReplaceAll]);
      Result := StringReplace(StringReplace(StringReplace(FSQL,'@PERMISSIONJOIN@',PJ,[]),'@PERMISSIONWHERE@',PW,[]),'@DEFAULTORDER@',SResult,[]);
    end
  else if Assigned(FOrigTable) then
    begin
      Result := 'SELECT ';
      if FDistinct then
        Result := Result+'DISTINCT ';
      if TSqlDBDM(Owner).LimitAfterSelect and ((FLimit > 0)) then
        Result += Format(TSqlDBDM(Owner).LimitSTMT,[FLimit])+' ';
      if FFields = '' then
        Result += TSqlDBDM(Owner).QuoteField(FDefaultTableName)+'.'+'* '
      else
        Result += FFields+' ';
      aFilter := FFilter;
      if (FBaseFilter <> '') and (aFilter <> '') then
        aFilter := '('+fBaseFilter+') and ('+aFilter+')'
      else if (FBaseFilter <> '') then
        aFilter := '('+fBaseFilter+')';
      if Assigned(DataSource) then
        begin
          with Self as IBaseManageDb do
            begin
              if ManagedFieldDefs.IndexOf('AUTO_ID') > -1 then
                aRefField := 'AUTO_ID'
              else
                aRefField := 'SQL_ID';
            end;
          if aFilter <> '' then
            aFilter := '('+aFilter+') and ('+TSqlDBDM(Owner).QuoteField('REF_ID')+'=:'+TSqlDBDM(Owner).QuoteField(aRefField)+')'
          else
            aFilter := TSqlDBDM(Owner).QuoteField('REF_ID')+'=:'+TSqlDBDM(Owner).QuoteField(aRefField);
        end;
      if (FManagedFieldDefs.IndexOf('AUTO_ID') = -1) and (TSqlDBDM(Owner).UsersFilter <> '') and FUsePermissions then
        Result += 'FROM '+BuildJoins+' LEFT JOIN '+TSqlDBDM(Owner).QuoteField('PERMISSIONS')+' ON ('+TSqlDBDM(Owner).QuoteField('PERMISSIONS')+'.'+TSqlDBDM(Owner).QuoteField('REF_ID_ID')+'='+TSqlDBDM(Owner).QuoteField(FDefaultTableName)+'.'+TSqlDBDM(Owner).QuoteField('SQL_ID')+') WHERE ('+aFilter+') AND (('+TSqlDBDM(Owner).UsersFilter+') OR '+TSqlDBDM(Owner).QuoteField('PERMISSIONS')+'.'+TSqlDBDM(Owner).QuoteField('USER')+' is NULL)'
      else if (FManagedFieldDefs.IndexOf('AUTO_ID') = -1) and FUsePermissions then
        Result += 'FROM '+BuildJoins+' LEFT JOIN '+TSqlDBDM(Owner).QuoteField('PERMISSIONS')+' ON ('+TSqlDBDM(Owner).QuoteField('PERMISSIONS')+'.'+TSqlDBDM(Owner).QuoteField('REF_ID_ID')+'='+TSqlDBDM(Owner).QuoteField(FDefaultTableName)+'.'+TSqlDBDM(Owner).QuoteField('SQL_ID')+') WHERE ('+aFilter+') AND ('+TSqlDBDM(Owner).QuoteField('PERMISSIONS')+'.'+TSqlDBDM(Owner).QuoteField('USER')+' is NULL)'
      else
        Result += 'FROM '+BuildJoins+' WHERE ('+aFilter+')';
      Result := StringReplace(Result,' WHERE () AND ','WHERE ',[]);
      Result := StringReplace(Result,' WHERE ()','',[]);
      if (FSortFields <> '') and ((FSortDirection <> sdIgnored) or (FBaseSortDirection <> sdIgnored)) then
        begin
          BuildSResult;
          if FUseBaseSorting then
            Result += ' ORDER BY '+Format(FBaseSorting,[sResult])
          else
            Result += ' ORDER BY '+sResult;
        end;
      if (FLimit > 0) and (not TSqlDBDM(Owner).LimitAfterSelect) then
        Result += ' '+Format(TSqlDBDM(Owner).LimitSTMT,[FLimit]);
    end
  else
    Result := SQL.text;
  if Assigned(FOrigTable) then TBaseDBModule(ForigTable.DataModule).LastStatement := Result;
end;
function TSqlDBDataSet.IndexExists(aIndexName: string): Boolean;
var
  CustomQuery: TSQLQuery;
begin
  CustomQuery := TSQLQuery.Create(Self);
  CustomQuery.Transaction := Transaction;
  if (copy(TSQLConnection(TBaseDBModule(Owner).MainConnection).Protocol,0,8) = 'firebird')
  or (copy(TSQLConnection(TBaseDBModule(Owner).MainConnection).Protocol,0,9) = 'interbase') then
    begin
      CustomQuery.SQL.Text := 'select rdb$index_name from rdb$indices where rdb$index_name='+TSqlDBDM(Owner).QuoteValue(indexname);
      CustomQuery.Open;
      Result := CustomQuery.RecordCount > 0;
      CustomQuery.Close;
    end
  else if (copy(TSQLConnection(TBaseDBModule(Owner).MainConnection).Protocol,0,6) = 'sqlite') then
    begin
      CustomQuery.SQL.Text := 'select name from SQLITE_MASTER where "TYPE"=''index'' and NAME='+TSqlDBDM(Owner).QuoteValue(indexname);
      CustomQuery.Open;
      Result := CustomQuery.RecordCount > 0;
      CustomQuery.Close;
    end
  else if (copy(TSQLConnection(TBaseDBModule(Owner).MainConnection).Protocol,0,5) = 'mssql') then
    begin
      CustomQuery.SQL.Text := 'select name from dbo.sysindexes where NAME='+TSqlDBDM(Owner).QuoteValue(indexname);
      CustomQuery.Open;
      Result := CustomQuery.RecordCount > 0;
      CustomQuery.Close;
    end
  else if (copy(TSQLConnection(TBaseDBModule(Owner).MainConnection).Protocol,0,8) = 'postgres') then
    begin
      CustomQuery.SQL.Text := 'select * from pg_class where relname='+TSqlDBDM(Owner).QuoteValue(indexname);
      CustomQuery.Open;
      Result := CustomQuery.RecordCount > 0;
      CustomQuery.Close;
    end
  ;
  CustomQuery.Free;
end;

procedure TSqlDBDataSet.WaitForLostConnection;
var
  aConnThere: Boolean;
begin
  if not TSqlDBDM(Owner).Ping(Connection) then
    begin
      if Assigned(TSqlDBDM(Owner).OnConnectionLost) then
        TSqlDBDM(Owner).OnConnectionLost(TSqlDBDM(Owner));
      aConnThere := False;
      while not aConnThere do
        begin
          if GetCurrentThreadID=MainThreadID then
            begin
              if Assigned(TSqlDBDM(Owner).OnDisconnectKeepAlive) then
                TSqlDBDM(Owner).OnDisconnectKeepAlive(TSqlDBDM(Owner));
            end;
          try
            if TSqlDBDM(Owner).Ping(Connection) then aConnThere := True
            else sleep(200);
          except
            sleep(200);
          end;
        end;
      if Assigned(TSqlDBDM(Owner).OnConnect) then
        TSqlDBDM(Owner).OnConnect(TSqlDBDM(Owner));
    end;
end;

function TSqlDBDataSet.CreateTable : Boolean;
var
  aSQL: String;
  i: Integer;
  bConnection: TZAbstractConnection = nil;
//  bConnection: TSQLConnection = nil;
  GeneralQuery: TSQLQuery;
  RestartTransaction: Boolean = False;
begin
  Result := False;
  with TBaseDBModule(Owner) do
    begin
      if Assigned(FOrigTable) and (FFields = '') then
        begin
          if FFields = '' then
            DoCheck := True;
          bConnection := Connection;
          Result := True;
          aSQL := 'CREATE TABLE '+QuoteField(Uppercase(Self.FDefaultTableName))+' ('+lineending;
          if FManagedFieldDefs.IndexOf('AUTO_ID') = -1 then
            aSQL += TSqlDBDM(Self.Owner).FieldToSQL('SQL_ID',ftLargeInt,0,True)+' PRIMARY KEY,'+lineending
          else
            begin
              aSQL += TSqlDBDM(Self.Owner).FieldToSQL('AUTO_ID',ftLargeInt,0,True)+' PRIMARY KEY,'+lineending;
            end;
          if Assigned(MasterSource) then
            begin
              aSQL += TSqlDBDM(Self.Owner).FieldToSQL('REF_ID',ftLargeInt,0,True);
              if FUseIntegrity then
                begin
                  with MasterSource.DataSet as IBaseManageDB do
                    begin
                      if ManagedFieldDefs.IndexOf('AUTO_ID') = -1 then
                        aSQL += ' REFERENCES '+QuoteField(TSqlDBDataSet(MasterSource.DataSet).DefaultTableName)+'('+QuoteField('SQL_ID')+') ON DELETE CASCADE'
                      else
                        aSQL += ' REFERENCES '+QuoteField(TSqlDBDataSet(MasterSource.DataSet).DefaultTableName)+'('+QuoteField('AUTO_ID')+') ON DELETE CASCADE';
                    end;
                  if (copy(TSQLConnection(TBaseDBModule(Self.Owner).MainConnection).Protocol,0,6) = 'sqlite') then
                    aSQL += ' DEFERRABLE INITIALLY DEFERRED';
                end;
              aSQL+=','+lineending;
            end;
          for i := 0 to FManagedFieldDefs.Count-1 do
            if FManagedFieldDefs[i].Name <> 'AUTO_ID' then
              aSQL += TSqlDBDM(Self.Owner).FieldToSQL(FManagedFieldDefs[i].Name,FManagedFieldDefs[i].DataType,FManagedFieldDefs[i].Size,FManagedFieldDefs[i].Required)+','+lineending;
          aSQL += TSqlDBDM(Self.Owner).FieldToSQL('TIMESTAMPD',ftDateTime,0,True)+');';
          try
            try
              GeneralQuery := TSQLQuery.Create(Self);
              GeneralQuery.Connection := bConnection;
              GeneralQuery.SQL.Text := aSQL;
              GeneralQuery.ExecSQL;
              if bConnection.InTransaction then
                begin
                  TSqlDBDM(Self.Owner).CommitTransaction(bConnection);
                  TSqlDBDM(Self.Owner).StartTransaction(bConnection);
                end;
            except
            end;
          finally
            GeneralQuery.Destroy;
          end;
        end;
    end;
  Close;
end;
function TSqlDBDataSet.CheckTable: Boolean;
var
  i: Integer;
begin
  Result := False;
  with TBaseDBModule(Owner) do
    begin
      if DoCheck or (FFields = '') then
          begin
            for i := 0 to FManagedFieldDefs.Count-1 do
              if (FieldDefs.IndexOf(FManagedFieldDefs[i].Name) = -1) and (FManagedFieldDefs[i].Name <> 'AUTO_ID') then
                begin
                  Result := True;
                end;
            if Assigned(FManagedIndexDefs) then
              for i := 0 to FManagedIndexDefs.Count-1 do                                           //Primary key
                if (not IndexExists(Uppercase(Self.DefaultTableName+'_'+FManagedIndexDefs.Items[i].Name))) and (FManagedIndexDefs.Items[i].Name <>'SQL_ID') then
                  begin
                    Result := True;
                  end;
          end;
      if not Result then
        begin
          TBaseDBModule(Self.Owner).UpdateTableVersion(Self.FDefaultTableName);
        end;
    end;
end;
function TSqlDBDataSet.AlterTable: Boolean;
var
  i: Integer;
  aSQL: String;
  GeneralQuery: TSQLQuery;
  Changed: Boolean;
  aConnection : TZAbstractConnection;
begin
  Result := True;
  try
    if FFields <> '' then exit;
    with TBaseDBModule(Owner) do
      begin
        for i := 0 to FManagedFieldDefs.Count-1 do
          if (FieldDefs.IndexOf(FManagedFieldDefs[i].Name) = -1) and (FManagedFieldDefs[i].Name <> 'AUTO_ID') then
            begin
              aSQL := 'ALTER TABLE '+QuoteField(FDefaultTableName)+' ADD '+TSqlDBDM(Self.Owner).FieldToSQL(FManagedFieldDefs[i].Name,FManagedFieldDefs[i].DataType,FManagedFieldDefs[i].Size,False)+';';
              aConnection := Connection;
              GeneralQuery := TSQLQuery.Create(Self);
              try
                GeneralQuery.Connection := aConnection;
                GeneralQuery.SQL.Text := aSQL;
                GeneralQuery.ExecSQL;
              finally
                GeneralQuery.Free;
              end;
              Changed := True;
              Result := True;
            end;
        aSQL := '';
        if Assigned(FManagedIndexDefs) then
          for i := 0 to FManagedIndexDefs.Count-1 do                                           //Primary key
            if (not IndexExists(Uppercase(Self.DefaultTableName+'_'+FManagedIndexDefs.Items[i].Name))) and (FManagedIndexDefs.Items[i].Name <>'SQL_ID') then
              begin
                aSQL := aSQL+'CREATE ';
                if ixUnique in FManagedIndexDefs.Items[i].Options then
                  aSQL := aSQL+'UNIQUE ';
                aSQL := aSQL+'INDEX '+QuoteField(Uppercase(Self.DefaultTableName+'_'+FManagedIndexDefs.Items[i].Name))+' ON '+QuoteField(Self.DefaultTableName)+' ('+QuoteField(StringReplace(FManagedIndexDefs.Items[i].Fields,';',QuoteField(','),[rfReplaceAll]))+');'+lineending;
                if aSQL <> '' then
                  begin
                    try
                      GeneralQuery := TSQLQuery.Create(Self);
                      GeneralQuery.Connection := Connection;
                      GeneralQuery.SQL.Text := aSQL;
                      GeneralQuery.ExecSQL;
                    finally
                      GeneralQuery.Free;
                      aSQL := '';
                    end;
                  end;
                Result := True;
              end;
      end;
  except
    Result := False;
  end;
  TBaseDBModule(Self.Owner).UpdateTableVersion(Self.FDefaultTableName);
end;
procedure TSqlDBDataSet.InternalOpen;
var
  a: Integer;
begin
  if Assigned(FOrigTable) then
    TBaseDBModule(ForigTable.DataModule).LastTime := GetTicks;
  if TSqlDBDM(Owner).IgnoreOpenRequests then exit;
  try
      try
        inherited InternalOpen;
      except
        InternalClose;
        if TSqlDBDM(Owner).Ping(Connection) then
          inherited InternalOpen
        else
          begin
            WaitForLostConnection;
            inherited InternalOpen;
          end;
      end;
      try
      if Assigned(FOrigTable) then
        begin
          FOrigTable.SetDisplayLabels(Self);
          if FOrigTable.UpdateFloatFields then
            begin
              DisableControls;
              for a := 0 to Fields.Count -1 do
                begin
                  if Fields[a] is TFloatField then
                    begin
                      if Fields[a].Name = 'WEIGHT' then
                        begin
                          TFloatField(Fields[a]).DisplayFormat := '#,##0.000##';
                          TFloatField(Fields[a]).EditFormat := '0.000##';
                          TFloatField(Fields[a]).Precision:=5;
                        end
                      else
                        begin
                          TFloatField(Fields[a]).DisplayFormat := '#,##0.00##';
                          TFloatField(Fields[a]).EditFormat := '0.00##';
                          TFloatField(Fields[a]).Precision:=5;
                        end;
                    end;
                  if (Fields[a] is TDateTimeField)
                  or (Fields[a] is TDateField)
                  then
                    TDateTimeField(Fields[a]).DisplayFormat := ShortDateFormat+' '+ShortTimeFormat;
                end;
              EnableControls;
            end;
        end;
      except
        begin
          FOrigTable:=nil;
          raise;
        end;
      end;
  finally
  end;
end;

procedure TSqlDBDataSet.InternalRefresh;
begin
  if TSqlDBDM(Owner).IgnoreOpenRequests then exit;
  try
    inherited InternalRefresh;
  except
    InternalClose;
    if not Active then
      begin
        if TSqlDBDM(Owner).Ping(Connection) then
          InternalOpen
        else
          begin
            WaitForLostConnection;
            InternalOpen;
          end;
      end;
  end;
end;

procedure TSqlDBDataSet.InternalPost;
var
  ok : boolean = false;
  rc : Integer = 0;

  function CheckID : Boolean;
  var
    aDs: TDataSet;
  begin
    if (FieldDefs.IndexOf('AUTO_ID') = -1) and (FieldDefs.IndexOf('SQL_ID') > -1)  then
      begin
        aDs := TBaseDBModule(FOrigTable.DataModule).GetNewDataSet('select '+TBaseDBModule(FOrigTable.DataModule).QuoteField('SQL_ID')+' from '+TBaseDBModule(FOrigTable.DataModule).QuoteField(DefaultTableName)+' where '+TBaseDBModule(FOrigTable.DataModule).QuoteField('SQL_ID')+'='+TBaseDBModule(FOrigTable.DataModule).QuoteValue(FieldByName('SQL_ID').AsVariant));
      end
    else if (FieldDefs.IndexOf('SQL_ID') = -1) and (FieldDefs.IndexOf('AUTO_ID') > -1) then
      begin
        aDs := TBaseDBModule(FOrigTable.DataModule).GetNewDataSet('select '+TBaseDBModule(FOrigTable.DataModule).QuoteField('AUTO_ID')+' from '+TBaseDBModule(FOrigTable.DataModule).QuoteField(DefaultTableName)+' where '+TBaseDBModule(FOrigTable.DataModule).QuoteField('AUTO_ID')+'='+TBaseDBModule(FOrigTable.DataModule).QuoteValue(FieldByName('AUTO_ID').AsVariant));
      end;
    aDs.Open;
    Result := aDs.RecordCount>0;
    aDs.Free;
  end;

  procedure CleanID;
  begin
    if (FieldDefs.IndexOf('AUTO_ID') = -1) and (FieldDefs.IndexOf('SQL_ID') > -1)  then
      begin
        FieldByName('SQL_ID').AsVariant:=Null
      end
    else if (FieldDefs.IndexOf('SQL_ID') = -1) and (FieldDefs.IndexOf('AUTO_ID') > -1) then
      begin
        FieldByName('AUTO_ID').AsVariant:=Null;
      end;
  end;

begin
  try
    while not ok do
      begin
        ok := True;
        try
          inherited InternalPost;
        except
          begin
            inc(rc);
            ok := false;
            if (FHasNewID and (rc<3)) then
              begin
                CleanID;
                SetNewIDIfNull;
                while CheckID do
                  begin
                    CleanID;
                    SetNewIDIfNull;
                  end;
              end
            else
              begin
                raise;
                exit;
              end;
          end;
        end;
      end;
  finally
  end;
end;

procedure TSqlDBDataSet.DoAfterInsert;
begin
  inherited DoAfterInsert;
  if Assigned(FOrigTable) then
    begin
      FOrigTable.DisableChanges;
      FOrigTable.FillDefaults(Self);
      FOrigTable.EnableChanges;
    end;
end;
procedure TSqlDBDataSet.DoBeforePost;
var
  UserCode: String;
begin
  inherited DoBeforePost;
  if Assigned(Self.FOrigTable) then
    Self.FOrigTable.DisableChanges;
  FHasNewID:=False;
  try
  SetNewIDIfNull;
  if FUpStdFields and Assigned(FOrigTable) {and (FOrigTable.Changed)} then
    begin
      if (FieldDefs.IndexOf('TIMESTAMPD') > -1) then
        FieldByName('TIMESTAMPD').AsDateTime:=Now();
      with BaseApplication as IBaseDBInterface do
        begin
          if Data.Users.DataSet.Active then
            UserCode := Data.Users.IDCode.AsString
          else UserCode := 'SYS';
          if (FieldDefs.IndexOf('CREATEDBY') > -1) and (FieldByName('CREATEDBY').IsNull) then
            FieldByName('CREATEDBY').AsString:=UserCode;
          if FUpChangedBy and (FieldDefs.IndexOf('CHANGEDBY') > -1) then
            FieldByName('CHANGEDBY').AsString:=UserCode;
        end;
    end;
  if Assigned(DataSource) and (FieldDefs.IndexOf('REF_ID')>-1) and  Assigned(FieldByName('REF_ID')) and FieldbyName('REF_ID').IsNull then
    begin
      if DataSource.DataSet.FieldDefs.IndexOf('AUTO_ID') > -1 then
        FieldbyName('REF_ID').AsVariant:=DataSource.DataSet.FieldByName('AUTO_ID').AsVariant
      else
        FieldbyName('REF_ID').AsVariant:=DataSource.DataSet.FieldByName('SQL_ID').AsVariant;
    end;
  finally
    if Assigned(Self.FOrigTable) then
      Self.FOrigTable.EnableChanges;
  end;
end;
procedure TSqlDBDataSet.DoBeforeInsert;
begin
  if Assigned(DataSource) then
    begin
      if (DataSource.State <> dsInsert) and (DataSource.DataSet.RecordCount = 0) then
        begin
          DataSource.DataSet.Append;
        end;
      if (DataSource.DataSet.State = dsInsert) then
        begin
          DataSource.DataSet.Post;
          DataSource.DataSet.Edit;
        end;
    end;
  inherited DoBeforeInsert;
end;

procedure TSqlDBDataSet.DoBeforeEdit;
begin
  inherited DoBeforeEdit;
end;

procedure TSqlDBDataSet.DoBeforeDelete;
begin
  inherited DoBeforeDelete;
  if (GetTableName='DELETEDITEMS')
  or (GetTableName='TABLEVERSIONS')
  then exit;
  try
    if Assigned(FOrigTable) and Assigned(FOrigTable.OnRemove) then FOrigTable.OnRemove(FOrigTable);
    if GetUpStdFields = True then
      TSqlDBDM(Owner).DeleteItem(FOrigTable);
  except
  end;
end;
procedure TSqlDBDataSet.DoAfterDelete;
begin
  inherited DoAfterDelete;
  if Assigned(FOrigTable) then
    FOrigTable.Change;
end;
procedure TSqlDBDataSet.DoAfterScroll;
begin
  inherited DoAfterScroll;
  if Assigned(ForigTable) then
    FOrigTable.UnChange;
end;

procedure TSqlDBDataSet.DoBeforeCancel;
begin
  inherited DoBeforeCancel;
  if State = dsInsert then
    begin
      if Assigned(FOrigTable) and Assigned(FOrigTable.OnRemove) then FOrigTable.OnRemove(FOrigTable);
    end;
end;

function TSqlDBDataSet.GetFields: string;
begin
  Result := FFields;
end;
function TSqlDBDataSet.GetFilter: string;
begin
  Result := FFilter;
end;
function TSqlDBDataSet.GetBaseFilter: string;
begin
  Result := FBaseFilter;
end;
function TSqlDBDataSet.GetLimit: Integer;
begin
  Result := FLimit;
end;
function TSqlDBDataSet.GetSortDirection: TSortDirection;
begin
  Result := FSortDirection;
end;
function TSqlDBDataSet.GetSortFields: string;
begin
  Result := FSortFields;
end;

function TSqlDBDataSet.GetLocalSortFields: string;
begin
  Result := SortedFields;
end;

function TSqlDBDataSet.GetBaseSortFields: string;
begin
  Result := FBaseSortFields;
end;
function TSqlDBDataSet.GetSortLocal: Boolean;
begin
  Result := SortType <> stIgnored;
end;
procedure TSqlDBDataSet.SetFields(const AValue: string);
begin
  FFields := AValue;
  Close;
  SQL.text := BuildSQL;
end;
procedure TSqlDBDataSet.SetFilter(const AValue: string);
begin
  if (FFilter=AValue) and (SQL.text<>'') then
    begin
      if (AValue<>'') or (pos('where',SQL.Text)=0) then
        exit;
    end;
  if TSqlDBDM(Owner).CheckForInjection(AValue) then exit;
  FFilter := AValue;
  FSQL := '';
  Close;
  SQL.text := BuildSQL;
end;
procedure TSqlDBDataSet.SetBaseFilter(const AValue: string);
begin
  FBaseFilter := AValue;
  Close;
  SQL.text := BuildSQL;
end;
function TSqlDBDataSet.GetSQL: string;
begin
  Result := FSQL;
end;
procedure TSqlDBDataSet.SetSQL(const AValue: string);
begin
  FSQL := AValue;
  Close;
  SQL.text := BuildSQL;
end;
procedure TSqlDBDataSet.Setlimit(const AValue: Integer);
begin
  if FLimit = AValue then exit;
  FLimit := AValue;
  Close;
  SQL.text := BuildSQL;
end;
procedure TSqlDBDataSet.SetSortDirection(const AValue: TSortDirection);
begin
  if (FSortDirection=AValue) and Active then exit;
  FSortDirection := AValue;
  if not GetSortLocal then
    begin
      Close;
      SQL.text := BuildSQL;
    end;
end;
procedure TSqlDBDataSet.SetSortFields(const AValue: string);
begin
  FSortFields := AValue;
end;

procedure TSqlDBDataSet.SetLocalSortFields(const AValue: string);
begin
  SortedFields:=AValue;
end;

procedure TSqlDBDataSet.SetBaseSortFields(const AValue: string);
begin
  FBaseSortFields := AValue;
end;
procedure TSqlDBDataSet.SetSortLocal(const AValue: Boolean);
begin
  if AValue then
    begin
      if FSortDirection = sdAscending then
        SortType := stAscending
      else if FSortDirection = sdDescending then
        SortType := stDescending
      else
        SortType := stIgnored;
    end
  else
    SortType := stIgnored;
end;
function TSqlDBDataSet.GetFilterTables: string;
begin
  Result := FTableNames;
end;
procedure TSqlDBDataSet.SetFilterTables(const AValue: string);
begin
  if AValue = FTableNames then exit;
  FTableNames := AValue;
  Close;
  SQL.text := BuildSQL;
end;
function TSqlDBDataSet.GetUsePermissions: Boolean;
begin
  Result := FUsePermissions;
end;
procedure TSqlDBDataSet.SetUsePermisions(const AValue: Boolean);
begin
  if AValue = FUsePermissions then exit;
  FUsePermissions := AValue;
  Close;
  SQL.text := BuildSQL;
end;
function TSqlDBDataSet.GetDistinct: Boolean;
begin
  Result := FDistinct;
end;
procedure TSqlDBDataSet.SetDistinct(const AValue: Boolean);
begin
  if AValue = FDistinct then exit;
  FDistinct := AValue;
  Close;
  SQL.text := BuildSQL;
end;
function TSqlDBDataSet.GetBaseSorting: string;
begin
  Result := FBaseSorting;
end;
procedure TSqlDBDataSet.SetBaseSorting(AValue: string);
begin
  FBaseSorting := AValue;
end;

function TSqlDBDataSet.GetBaseSortDirection: TSortDirection;
begin
  Result := FBaseSortDirection;
end;
procedure TSqlDBDataSet.SetBaseSortDirection(AValue: TSortDirection);
begin
  FBaseSortDirection := AValue;
end;
function TSqlDBDataSet.GetUseBaseSorting: Boolean;
begin
  Result := FUseBaseSorting;
end;
procedure TSqlDBDataSet.SetUseBaseSorting(AValue: Boolean);
begin
  FUseBaseSorting := AValue;
  SQL.text := BuildSQL;
end;
function TSqlDBDataSet.GetfetchRows: Integer;
begin
  result := FetchRow;
end;
procedure TSqlDBDataSet.SetfetchRows(AValue: Integer);
begin
  FetchRow:=AValue;
end;
function TSqlDBDataSet.GetManagedFieldDefs: TFieldDefs;
begin
  Result := FManagedFieldDefs;
end;
function TSqlDBDataSet.GetManagedIndexDefs: TIndexDefs;
begin
  Result := FManagedIndexDefs;
end;
function TSqlDBDataSet.GetTableName: string;
begin
  Result := FDefaultTableName;
end;
procedure TSqlDBDataSet.SetTableName(const AValue: string);
begin
  FDefaultTableName := AValue;
end;
function TSqlDBDataSet.GetConnection: TComponent;
begin
  Result := Connection;
end;
function TSqlDBDataSet.GetTableCaption: string;
begin
  Result := FTableCaption;
end;
procedure TSqlDBDataSet.SetTableCaption(const AValue: string);
begin
  FTableCaption := AValue;
end;
function TSqlDBDataSet.GetUpStdFields: Boolean;
begin
  Result := FUpStdFields;
end;

procedure TSqlDBDataSet.SetUpStdFields(AValue: Boolean);
begin
  FUpStdFields := AValue;
end;

function TSqlDBDataSet.GetUpChangedBy: Boolean;
begin
  Result := FUpChangedBy;
end;

procedure TSqlDBDataSet.SetUpChangedBy(AValue: Boolean);
begin
  FUpChangedBy:=AValue;
end;

function TSqlDBDataSet.GetUseIntegrity: Boolean;
begin
  Result := FUseIntegrity;
end;
procedure TSqlDBDataSet.SetUseIntegrity(AValue: Boolean);
begin
  FUseIntegrity:=AValue;
end;
procedure TSqlDBDataSet.SetFieldData(Field: TField; Buffer: Pointer);
var
  tmp: String;
begin
  inherited;
  {
  try
    if ((Field.DataType=ftString)
    or (Field.DataType=ftWideString)
    ) and (not FChangeUni)
    then
      begin
        tmp := SysToUni(Field.AsString);
        if tmp <> Field.AsString then
          begin
            FChangeUni := True;
            Field.AsString:=tmp;
            FChangeUni := False;
          end;
      end;
  except
  end;
  }
  if Assigned(FOrigTable) then
    FOrigTable.Change;
end;
function TSqlDBDataSet.GetSubDataSet(aName: string): TComponent;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FSubDataSets.Count-1 do
    with TBaseDBDataSet(FSubDataSets[i]) as IBaseManageDB do
      if TableName = aName then
        Result := TBaseDBDataSet(FSubDataSets[i]);
end;
procedure TSqlDBDataSet.RegisterSubDataSet(aDataSet: TComponent);
begin
  FSubDataSets.Add(aDataSet);
end;
function TSqlDBDataSet.GetCount: Integer;
begin
  Result := FSubDataSets.Count;
end;
function TSqlDBDataSet.GetSubDataSetIdx(aIdx: Integer): TComponent;
begin
  Result := nil;
  if aIdx < FSubDataSets.Count then
    Result := TBaseDbDataSet(FSubDataSets[aIdx]);
end;
function TSqlDBDataSet.IsChanged: Boolean;
begin
  Result := Modified;
  if Assigned(FOrigTable) then
    Result := ForigTable.Changed;
end;
constructor TSqlDBDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DoCheck := False;
  fBaseSorting := '%s';
  FChangeUni:=False;
  FUseBaseSorting:=False;
  FBaseSortDirection:=sdIgnored;
  FManagedFieldDefs := TFieldDefs.Create(Self);
  FManagedIndexDefs := TIndexDefs.Create(Self);
  FSubDataSets := TList.Create;
  FUsePermissions := False;
  Options:= [doCalcDefaults, doAlwaysDetailResync, doDontSortOnPost, doPreferPrepared{, doPreferPreparedResolver}];
  FOrigTable := nil;
  SortType := stIgnored;
  FUpStdFields := True;
  FUpChangedBy := True;
  FUseIntegrity:=False;//disable for sync
end;
destructor TSqlDBDataSet.Destroy;
begin
  //TODO: Free Subdatasets ??
  FManagedFieldDefs.Free;
  FManagedIndexDefs.Free;
  FSubDataSets.Free;
  try
    inherited Destroy;
  except
  end;
end;

procedure TSqlDBDataSet.DoExecSQL;
begin
  ExecSQL;
end;

function TSqlDBDataSet.NumRowsAffected: Integer;
begin
  Result := RowsAffected;
end;

procedure TSqlDBDM.MonitorTrace(Sender: TObject; Event: TZLoggingEvent;
  var LogTrace: Boolean);
begin
  if LastTime>0 then
    LastTime := GetTicks-LastTime;
  if LastTime<0 then LastTime := 0;
  if Assigned(BaseApplication) then
    with BaseApplication as IBaseApplication do
      begin
        if Event.Error<>'' then
          Error(Event.AsString+'('+LastStatement+')')
        else if BaseApplication.HasOption('debug-sql') then
          Debug(Event.AsString)
        else if (LastTime)>50 then
          Debug('Long running Query:'+IntToStr(round(LastTime))+' '+Event.AsString);
        LastTime:=0;
        LastStatement:='';
      end;
end;
function TSqlDBDM.GetConnection: TComponent;
begin
  Result := TComponent(FMainConnection);
end;
function TSqlDBDM.DBExists: Boolean;
begin
  Result := TableExists('USERS') and TableExists('GEN_SQL_ID') and TableExists('GEN_AUTO_ID');
end;

function TSqlDBDM.GetLimitAfterSelect: Boolean;
begin
  Result := FLimitAfterSelect;
end;

function TSqlDBDM.GetLimitSTMT: string;
begin
  Result := FLimitSTMT;
end;

function TSqlDBDM.GetSyncOffset: Integer;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
  bConnection: TComponent;
begin
  if Assigned(Sequence) then
    begin
      bConnection := MainConnection;
      Sequence.Connection := TSQLConnection(bConnection);
      Result := Sequence.GetCurrentValue shr 56;
      Sequence.Connection := nil;
    end
  else
    begin
      Statement := TSQLConnection(MainConnection).DbcConnection.CreateStatement;
      ResultSet := Statement.ExecuteQuery('SELECT "ID" FROM "GEN_SQL_ID"');
      if ResultSet.Next then
        Result := ResultSet.GetLong(1) shr 56
      else Result := 0;
      ResultSet.Close;
      Statement.Close;
    end;
end;
procedure TSqlDBDM.SetSyncOffset(const AValue: Integer);
var
  Statement: IZStatement;
  aVal: Int64;
begin
  aVal := AValue;
  aVal := aVal shl 56;
  if Assigned(Sequence) then
    begin
      raise Exception.Create('Not implemented !!!');
    end
  else
    begin
      Statement := TSQLConnection(MainConnection).DbcConnection.CreateStatement;
      Statement.Execute('update "GEN_SQL_ID" set "ID"='+IntToStr(aVal));
      Statement.Close;
    end;
end;
constructor TSqlDBDM.Create(AOwner: TComponent);
begin
  FDataSetClass := TSqlDBDataSet;
  FMainConnection := TSQLConnection.Create(AOwner);
  Monitor := TZSQLMonitor.Create(FMainConnection);
  Monitor.Active:=True;
  Monitor.OnTrace:=@MonitorTrace;
  Sequence := nil;
  inherited Create(AOwner);
end;
destructor TSqlDBDM.Destroy;
begin
  if FMainconnection.Connected then
    FMainConnection.Disconnect;
  if Assigned(Sequence) then
    begin
      Sequence.Connection := nil;
      FreeAndNil(Sequence);
    end;
  Monitor.Free;
  FMainConnection.Free;
  try
    inherited Destroy;
  except
  end;
end;
function TSqlDBDM.SetProperties(aProp: string;Connection : TComponent = nil): Boolean;
var
  tmp: String;
  FConnection : TSQLConnection;
begin
  inherited;
  if Assigned(BaseApplication) then
    with BaseApplication as IBaseDBInterface do
      LastError := '';
  FProperties := aProp;
  FConnection := TSQLConnection(Connection);
  if not Assigned(FConnection) then
    begin
      FConnection := FMainConnection;
      if FConnection.Connected then
        FConnection.Disconnect;
    end;
  Result := True;
  tmp := aProp;
  try
    if FConnection.Connected then
      FConnection.Disconnect;
    FConnection.Port:=0;
    FConnection.Properties.Clear;
    FConnection.Properties.Add('timeout=2');
    FConnection.ClientCodepage:='UTF8';
    FConnection.Protocol:='';
    FConnection.User:='';
    FConnection.Password:='';
    FConnection.HostName:='';
    FConnection.Database:='';
    FConnection.Properties.Clear;
    FConnection.Protocol:=copy(tmp,0,pos(';',tmp)-1);
    Assert(FConnection.Protocol<>'',strUnknownDbType);
    tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
    FConnection.HostName := copy(tmp,0,pos(';',tmp)-1);
    if pos(':',FConnection.HostName) > 0 then
      begin
        FConnection.Port:=StrToInt(copy(FConnection.HostName,pos(':',FConnection.HostName)+1,length(FConnection.HostName)));
        FConnection.HostName:=copy(FConnection.HostName,0,pos(':',FConnection.HostName)-1);
      end
    else if pos('/',FConnection.HostName) > 0 then
      begin
        FConnection.Port:=StrToInt(copy(FConnection.HostName,pos('/',FConnection.HostName)+1,length(FConnection.HostName)));
        FConnection.HostName:=copy(FConnection.HostName,0,pos('/',FConnection.HostName)-1);
      end;
    tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
    FConnection.Database:=copy(tmp,0,pos(';',tmp)-1);
    tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
    FConnection.User := copy(tmp,0,pos(';',tmp)-1);
    tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
    if copy(tmp,0,1) = 'x' then
      FConnection.Password := Decrypt(copy(tmp,2,length(tmp)),99998)
    else
      FConnection.Password := tmp;
    if (copy(FConnection.Protocol,0,6) = 'sqlite')
    or (copy(FConnection.Protocol,0,8) = 'postgres')
    then
      begin
        FConnection.TransactIsolationLevel:=tiNone;
        if (copy(FConnection.Protocol,0,6) = 'sqlite') then
          if not FileExists(FConnection.Database) then
            raise Exception.Create('Databasefile dosend exists');
      end
    else if (copy(FConnection.Protocol,0,5) = 'mssql') then
      FConnection.TransactIsolationLevel:=tiReadUnCommitted
    else if (copy(FConnection.Protocol,0,8) = 'firebird')
    or (copy(FConnection.Protocol,0,9) = 'interbase')
    or (copy(FConnection.Protocol,0,5) = 'mysql')
    then
      begin
        FConnection.TransactIsolationLevel:=tiReadCommitted;
      end;
    FConnection.Connected:=True;
    FLimitAfterSelect := False;
    FLimitSTMT := 'LIMIT %d';
    FDBTyp := FConnection.Protocol;
    if FConnection.Protocol = 'sqlite-3' then
      begin
//        FConnection.ExecuteDirect('PRAGMA synchronous = NORMAL;');
//        FConnection.ExecuteDirect('PRAGMA cache_size = 5120;');
//        FConnection.ExecuteDirect('PRAGMA auto_vacuum = FULL;');
        FConnection.ExecuteDirect('PRAGMA journal_mode = MEMORY;');
        FConnection.ExecuteDirect('PRAGMA recursive_triggers = ON;');
        FConnection.ExecuteDirect('PRAGMA foreign_keys = ON;');
        FConnection.ExecuteDirect('PRAGMA case_sensitive_like = ON;');
      end
    else if (copy(FConnection.Protocol,0,8) = 'firebird')
         or (copy(FConnection.Protocol,0,9) = 'interbase') then
      begin
        FDBTyp := 'firebird';
        FLimitSTMT := 'ROWS 1 TO %d';
        if not Assigned(Sequence) then
          begin
            Sequence := TZSequence.Create(Owner);
          end;
      end
    else if FConnection.Protocol = 'mssql' then
      begin
        FLimitAfterSelect := True;
        FLimitSTMT := 'TOP %d';
      end;
  except on e : Exception do
    begin
      if Assigned(BaseApplication) then
        with BaseApplication as IBaseDBInterface do
          LastError := e.Message;
      Result := False;
    end;
  end;
  if Result then
    begin
      if not DBExists then //Create generators
        begin
          try
            if (copy(FConnection.Protocol,0,8) = 'firebird')
            or (copy(FConnection.Protocol,0,9) = 'interbase') then
              begin
                FConnection.ExecuteDirect('EXECUTE BLOCK AS BEGIN'+lineending
                                         +'if (not exists(select 1 from rdb$generators where rdb$generator_name = ''GEN_SQL_ID'')) then'+lineending
                                         +'execute statement ''CREATE SEQUENCE GEN_SQL_ID;'';'+lineending
                                         +'END;');
                FConnection.ExecuteDirect('EXECUTE BLOCK AS BEGIN'+lineending
                                         +'if (not exists(select 1 from rdb$generators where rdb$generator_name = ''GEN_AUTO_ID'')) then'+lineending
                                         +'execute statement ''CREATE SEQUENCE GEN_AUTO_ID;'';'+lineending
                                         +'END;');
              end
            else if copy(FConnection.Protocol,0,6) = 'sqlite' then
              begin
                FConnection.ExecuteDirect('CREATE TABLE IF NOT EXISTS "GEN_SQL_ID"("SQL_ID" BIGINT NOT NULL PRIMARY KEY,ID BIGINT);');
                FConnection.ExecuteDirect('CREATE TABLE IF NOT EXISTS "GEN_AUTO_ID"("SQL_ID" BIGINT NOT NULL PRIMARY KEY,ID BIGINT);');
              end
            else
              begin
                if not TableExists('GEN_SQL_ID') then
                  FConnection.ExecuteDirect('CREATE TABLE '+QuoteField('GEN_SQL_ID')+'('+QuoteField('SQL_ID')+' BIGINT NOT NULL PRIMARY KEY,'+QuoteField('ID')+' BIGINT);');
                if not TableExists('GEN_AUTO_ID') then
                  FConnection.ExecuteDirect('CREATE TABLE '+QuoteField('GEN_AUTO_ID')+'('+QuoteField('SQL_ID')+' BIGINT NOT NULL PRIMARY KEY,'+QuoteField('ID')+' BIGINT);');
              end
          except on e : Exception do
            begin
              if Assigned(BaseApplication) then
                with BaseApplication as IBaseDBInterface do
                  LastError := e.Message;
              Result := False;
            end;
          end;
        end;
    end;
end;
function TSqlDBDM.CreateDBFromProperties(aProp: string): Boolean;
var
  FConnection: TSQLConnection;
  tmp: String;
  aPassword: String;
  aUser: String;
  aDatabase: String;
begin
  FConnection := TSQLConnection.Create(Self);
  if Assigned(BaseApplication) then
    with BaseApplication as IBaseDBInterface do
      LastError := '';
  tmp := aProp;
  FConnection.Protocol:=copy(tmp,0,pos(';',tmp)-1);
  Assert(FConnection.Protocol<>'',strUnknownDbType);
  tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
  FConnection.HostName := copy(tmp,0,pos(';',tmp)-1);
  if pos(':',FConnection.HostName) > 0 then
    begin
      FConnection.Port:=StrToInt(copy(FConnection.HostName,pos(':',FConnection.HostName)+1,length(FConnection.HostName)));
      FConnection.HostName:=copy(FConnection.HostName,0,pos(':',FConnection.HostName)-1);
    end
  else if pos('/',FConnection.HostName) > 0 then
    begin
      FConnection.Port:=StrToInt(copy(FConnection.HostName,pos('/',FConnection.HostName)+1,length(FConnection.HostName)));
      FConnection.HostName:=copy(FConnection.HostName,0,pos('/',FConnection.HostName)-1);
    end;
  tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
  aDatabase:=copy(tmp,0,pos(';',tmp)-1);
  tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
  aUser := copy(tmp,0,pos(';',tmp)-1);
  FConnection.User:=aUser;
  tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
  FConnection.Database:=aDatabase;
  if copy(tmp,0,1) = 'x' then
    aPassword := Decrypt(copy(tmp,2,length(tmp)),99998)
  else
    aPassword := tmp;
  FConnection.Password:=aPassword;
  if (copy(FConnection.Protocol,0,8) = 'postgres')
  then
    begin
      FConnection.Database:='postgres';
    end
    else if (copy(FConnection.Protocol,0,5) = 'mssql') then
      FConnection.Properties.Add('CreateNewDatabase=CREATE DATABASE "'+aDatabase+'"')
    else if (copy(FConnection.Protocol,0,8) = 'firebird')
    or (copy(FConnection.Protocol,0,9) = 'interbase')
    then
      begin
        if FConnection.HostName <> '' then
          FConnection.Properties.Add('CreateNewDatabase=CREATE DATABASE '''+FConnection.HostName+':'+aDatabase+''' USER '''+aUser+''' PASSWORD '''+aPassword+''' PAGE_SIZE = 4096 DEFAULT CHARACTER SET UTF8')
        else
          FConnection.Properties.Add('CreateNewDatabase=CREATE DATABASE '''+aDatabase+''' USER '''+aUser+''' PASSWORD '''+aPassword+''' PAGE_SIZE = 4096 DEFAULT CHARACTER SET UTF8');
      end
    else if (copy(FConnection.Protocol,0,6) = 'sqlite') then
      begin
        ForceDirectories(ExtractFileDir(FConnection.Database));
      end;
  try
    FConnection.Connected:=True;
  except
    on e : Exception do
    if Assigned(BaseApplication) then
      with BaseApplication as IBaseDBInterface do
        begin
          LastError := e.Message;
          //debugln(LastError);
        end;
  end;
  if (copy(FConnection.Protocol,0,8) = 'postgres')
  then
    begin
      Result := FConnection.ExecuteDirect('CREATE DATABASE "'+aDatabase+'" WITH OWNER = "'+aUser+'" ENCODING = ''UTF8'' CONNECTION LIMIT = -1;');
      FConnection.Disconnect;
      FConnection.Database:=aDatabase;
    end;
  FConnection.Connected:=True;
  Result := FConnection.Connected;
  FConnection.Free;
end;
function TSqlDBDM.IsSQLDB: Boolean;
begin
  Result:=True;
end;
function TSqlDBDM.GetNewDataSet(aTable: TBaseDBDataSet;
  aConnection: TComponent; MasterData: TDataSet; aTables: string): TDataSet;
begin
  if IgnoreOpenrequests then exit;
  Result := FDataSetClass.Create(Self);
  if not Assigned(aConnection) then
    aConnection := MainConnection;
  with TSqlDBDataSet(Result) do
    begin
      Connection := TSQLConnection(aConnection);
      FTableNames := aTables;
      aTable.DefineFields(Result);
      aTable.DefineDefaultFields(Result,Assigned(Masterdata));
      FOrigTable := aTable;
      if Assigned(Masterdata) then
        begin
          if not Assigned(TSqlDBDataSet(MasterData).MasterDataSource) then
            begin
              TSqlDBDataSet(MasterData).MasterDataSource := TDataSource.Create(Self);
              TSqlDBDataSet(MasterData).MasterDataSource.DataSet := MasterData;
            end;
          DataSource := TSqlDBDataSet(MasterData).MasterDataSource;
          MasterSource := TSqlDBDataSet(MasterData).MasterDataSource;
          with Masterdata as IBaseSubDataSets do
            RegisterSubDataSet(aTable);
        end;
    end;
end;
function TSqlDBDM.GetNewDataSet(aSQL: string; aConnection: TComponent;
  MasterData : TDataSet = nil;aOrigtable : TBaseDBDataSet = nil): TDataSet;
begin
  Result := FDataSetClass.Create(Self);
  if not Assigned(aConnection) then
    aConnection := MainConnection;
  with TSqlDBDataSet(Result) do
    begin
      FOrigTable := aOrigtable;
      Connection := TSQLConnection(aConnection);
      SQL.Text := aSQL;
      if Assigned(Masterdata) then
        begin
          if not Assigned(TSqlDBDataSet(MasterData).MasterDataSource) then
            begin
              TSqlDBDataSet(MasterData).MasterDataSource := TDataSource.Create(Self);
              TSqlDBDataSet(MasterData).MasterDataSource.DataSet := MasterData;
            end;
          DataSource := TSqlDBDataSet(MasterData).MasterDataSource;
          MasterSource := TSqlDBDataSet(MasterData).MasterDataSource;
        end;
    end;
end;

procedure TSqlDBDM.DestroyDataSet(DataSet: TDataSet);
begin
  try
    if Assigned(DataSet) and Assigned(TSqlDBDataSet(DataSet).MasterSource) then
      begin
        TSqlDBDataSet(DataSet).MasterSource.DataSet.DataSource.Free;
        TSqlDBDataSet(DataSet).MasterSource := nil;
        TSqlDBDataSet(DataSet).DataSource := nil;
      end;
  except
    with BaseApplication as IBaseApplication do
     Debug(Self.ClassName+' has Masterdata that is destroyed before itself !!');
  end;
end;

function TSqlDBDM.Ping(aConnection: TComponent): Boolean;
var
  atime: Integer;
begin
  Result := True;
  exit;
  try
    Result := TSQLConnection(aConnection).Ping;
  except
    Result := False;
  end;
end;
function TSqlDBDM.DateToFilter(aValue: TDateTime): string;
begin
  if FMainConnection.Protocol = 'mssql' then
    Result := QuoteValue(FormatDateTime('YYYYMMDD',aValue))
  else
    Result:=inherited DateToFilter(aValue);
end;
function TSqlDBDM.DateTimeToFilter(aValue: TDateTime): string;
begin
  if FMainConnection.Protocol = 'mssql' then
    Result := QuoteValue(FormatDateTime('YYYYMMDD HH:MM:SS.ZZZZ',aValue))
  else
    Result:=inherited DateTimeToFilter(aValue);
end;
function TSqlDBDM.GetUniID(aConnection : TComponent = nil;Generator : string = 'GEN_SQL_ID';AutoInc : Boolean = True): Variant;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
  bConnection: TComponent;
begin
  if Assigned(Sequence) then
    begin
      bConnection := MainConnection;
      if Assigned(aConnection) then
        bConnection := aConnection;
      Sequence.SequenceName:=Generator;
      Sequence.Connection := TSQLConnection(bConnection);
      Result := Sequence.GetNextValue;
      Sequence.Connection := nil;
    end
  else
    begin
      try
        if (copy(FMainConnection.Protocol,0,6) = 'sqlite') and (Assigned(aConnection)) then
          Statement := TSQLConnection(aConnection).DbcConnection.CreateStatement //we have global locking in sqlite so we must use the actual connection
        else
          Statement := TSQLConnection(MainConnection).DbcConnection.CreateStatement;
        if AutoInc then
          begin
            if (copy(FMainConnection.Protocol,0,5) = 'mysql') then
              begin
                Statement.Execute('update '+QuoteField(Generator)+' set '+QuoteField('ID')+'='+QuoteField('ID')+'+1;')
              end
            else
              begin
                if LimitAfterSelect then
                  Statement.Execute('update '+QuoteField(Generator)+' set '+QuoteField('ID')+'=(select '+Format(LimitSTMT,[1])+' '+QuoteField('ID')+' from '+QuoteField(Generator)+')+1;')
                else
                  Statement.Execute('update '+QuoteField(Generator)+' set '+QuoteField('ID')+'=(select '+QuoteField('ID')+' from '+QuoteField(Generator)+' '+Format(LimitSTMT,[1])+')+1;');
              end;
          end;
        except
        end;
        try
          ResultSet := Statement.ExecuteQuery('SELECT '+QuoteField('ID')+' FROM '+QuoteField(Generator));
          if ResultSet.Next then
            Result := ResultSet.GetLong(1)
          else
            begin
              Statement.Execute('insert into '+QuoteField(GENERATOR)+' ('+QuoteField('SQL_ID')+','+QuoteField('ID')+') VALUES (1,1000);');
              Result := 1000;
            end;
          ResultSet.Close;
          Statement.Close;
        except
        end;
    end;
end;
const
  ChunkSize: Longint = 16384; { copy in 8K chunks }
procedure TSqlDBDM.StreamToBlobField(Stream: TStream; DataSet: TDataSet;
  Fieldname: string);
var
  Posted: Boolean;
  GeneralQuery: TSQLQuery;
  pBuf    : Pointer;
  cnt: LongInt;
  dStream: TStream;
  totCnt: LongInt;
begin
  totCnt := 0;
  if DataSet.Fielddefs.IndexOf(FieldName) = -1 then
    begin
      if DataSet.State = dsInsert then
        begin
          Posted := True;
          DataSet.Post;
        end;
      GeneralQuery := TSQLQuery.Create(Self);
      GeneralQuery.Connection := TSQLQuery(DataSet).Connection;
      GeneralQuery.SQL.Text := 'select * from '+QuoteField(TSqlDBDataSet(DataSet).DefaultTableName)+' where "SQL_ID"='+QuoteValue(DataSet.FieldByName('SQL_ID').AsString)+';';
      GeneralQuery.Open;
      GeneralQuery.Edit;
      dStream := GeneralQuery.CreateBlobStream(GeneralQuery.FieldByName(Fieldname),bmWrite);
      try
        GetMem(pBuf, ChunkSize);
        try
          cnt := Stream.Read(pBuf^, ChunkSize);
          cnt := dStream.Write(pBuf^, cnt);
          totCnt := totCnt + cnt;
          {Loop the process of reading and writing}
          while (cnt > 0) do
            begin
              {Read bufSize bytes from source into the buffer}
              cnt := Stream.Read(pBuf^, ChunkSize);
              {Now write those bytes into destination}
              cnt := dStream.Write(pBuf^, cnt);
              {Increment totCnt for progress and do arithmetic to update the gauge}
              totcnt := totcnt + cnt;
            end;
        finally
          FreeMem(pBuf, ChunkSize);
        end;
      finally
        dStream.Free;
      end;
      GeneralQuery.Post;
      GeneralQuery.Free;
      if Posted then DataSet.Edit;
    end
  else inherited;
end;
procedure TSqlDBDM.BlobFieldToStream(DataSet: TDataSet; Fieldname: string;
  dStream: TStream);
var
  GeneralQuery: TSQLQuery;
  aSQL : string;
  pBuf    : Pointer;
  cnt: LongInt;
  totCnt: LongInt=0;
  Stream: TStream;
begin
  if DataSet.Fielddefs.IndexOf(FieldName) = -1 then
    begin
      GeneralQuery := TSQLQuery.Create(Self);
      GeneralQuery.Connection := TSQLQuery(DataSet).Connection;
      aSql := 'select '+QuoteField(Fieldname)+' from '+QuoteField(TSqlDBDataSet(DataSet).DefaultTableName)+' where "SQL_ID"='+QuoteValue(DataSet.FieldByName('SQL_ID').AsString)+';';
      GeneralQuery.SQL.Text := aSql;
      GeneralQuery.Open;
      Stream := GeneralQuery.CreateBlobStream(GeneralQuery.FieldByName(Fieldname),bmRead);
      try
        GetMem(pBuf, ChunkSize);
        try
          cnt := Stream.Read(pBuf^, ChunkSize);
          cnt := dStream.Write(pBuf^, cnt);
          totCnt := totCnt + cnt;
          {Loop the process of reading and writing}
          while (cnt > 0) do
            begin
              {Read bufSize bytes from source into the buffer}
              cnt := Stream.Read(pBuf^, ChunkSize);
              {Now write those bytes into destination}
              cnt := dStream.Write(pBuf^, cnt);
              {Increment totCnt for progress and do arithmetic to update the gauge}
              totcnt := totcnt + cnt;
            end;
        finally
          FreeMem(pBuf, ChunkSize);
        end;
      finally
        Stream.Free;
      end;
      GeneralQuery.Free;
    end
  else inherited;
end;

function TSqlDBDM.GetErrorNum(e: EDatabaseError): Integer;
begin
  Result:=inherited GetErrorNum(e);
  if e is EZDatabaseError then
    Result := EZDatabaseError(e).ErrorCode;
end;

procedure TSqlDBDM.DeleteExpiredSessions;
var
  GeneralQuery: TSQLQuery;
begin
  GeneralQuery := TSQLQuery.Create(Self);
  GeneralQuery.Connection := FMainConnection;
  GeneralQuery.SQL.Text := 'DELETE FROM '+QuoteField('ACTIVEUSERS')+' WHERE ('+QuoteField('EXPIRES')+' < '+Self.DateTimeToFilter(Now)+');';
  GeneralQuery.ExecSQL;
  GeneralQuery.Free;
end;
function TSqlDBDM.GetNewConnection: TComponent;
begin
  Result := TSQLConnection.Create(Self);
  with Result as TSQLConnection do
    begin
      Setproperties(FProperties,Result);
    end;
end;

function TSqlDBDM.QuoteField(aField: string): string;
begin
  Result:=inherited QuoteField(aField);
  if (copy(TSQLConnection(MainConnection).Protocol,0,5) = 'mysql') then
    Result := '`'+aField+'`';
end;

procedure TSqlDBDM.Disconnect(aConnection: TComponent);
begin
  TSQLConnection(aConnection).Disconnect;
end;
function TSqlDBDM.StartTransaction(aConnection: TComponent;ForceTransaction : Boolean = False): Boolean;
begin
  TSQLConnection(aConnection).Tag := Integer(TSQLConnection(aConnection).TransactIsolationLevel);
  if ForceTransaction and (copy(TSQLConnection(aConnection).Protocol,0,6) = 'sqlite') then
    TSQLConnection(aConnection).TransactIsolationLevel:=tiReadCommitted
  else if (copy(TSQLConnection(aConnection).Protocol,0,8) = 'postgres') then
    TSQLConnection(aConnection).TransactIsolationLevel:=tiReadCommitted
  else if (copy(TSQLConnection(aConnection).Protocol,0,5) = 'mssql') then
    TSQLConnection(aConnection).TransactIsolationLevel:=tiReadUnCommitted;
  TSQLConnection(aConnection).StartTransaction;
end;
function TSqlDBDM.CommitTransaction(aConnection: TComponent): Boolean;
begin
  if not TSQLConnection(aConnection).AutoCommit then
    TSQLConnection(aConnection).Commit;
  if TZTransactIsolationLevel(TSQLConnection(aConnection).Tag) <> TSQLConnection(aConnection).TransactIsolationLevel then
    TSQLConnection(aConnection).TransactIsolationLevel := TZTransactIsolationLevel(TSQLConnection(aConnection).Tag);
end;
function TSqlDBDM.RollbackTransaction(aConnection: TComponent): Boolean;
begin
  if not TSQLConnection(aConnection).AutoCommit then
    TSQLConnection(aConnection).Rollback;
  if TZTransactIsolationLevel(TSQLConnection(aConnection).Tag) <> TSQLConnection(aConnection).TransactIsolationLevel then
    TSQLConnection(aConnection).TransactIsolationLevel := TZTransactIsolationLevel(TSQLConnection(aConnection).Tag);
end;
function TSqlDBDM.TableExists(aTableName: string;aConnection : TComponent = nil;AllowLowercase: Boolean = False): Boolean;
var
  aIndex: longint;
  i: Integer;
  tmp: String;
begin
  Result := False;
  try
    if Tables.Count = 0 then
      begin
        //Get uncached
        if not Assigned(aConnection) then
          begin
            FMainConnection.DbcConnection.GetMetadata.ClearCache;
            FMainConnection.GetTableNames('','',Tables);
            FMainConnection.GetTriggerNames('','',Triggers);
          end
        else
          begin
            TSQLConnection(aConnection).DbcConnection.GetMetadata.ClearCache;
            TSQLConnection(aConnection).GetTableNames('','',Tables);
            FMainConnection.GetTriggerNames('','',Triggers);
          end;
      end;
  except
  end;
  if Tables.IndexOf(aTableName) > 0 then
    begin
      Result := True;
      exit;
    end;
  for i := 0 to Tables.Count-1 do
    begin
      tmp := Tables[i];
      if (Uppercase(tmp) = aTableName)
      then
        begin
          Result := True;
          break;
        end;
    end;
end;
function TSqlDBDM.TriggerExists(aTriggerName: string; aConnection: TComponent;
  AllowLowercase: Boolean): Boolean;
var
  i: Integer;
  tmp: String;
  GeneralQuery: TSQLQuery;
begin
  if Triggers.Count= 0 then
    begin
      GeneralQuery := TSQLQuery.Create(Self);
      GeneralQuery.Connection:=TSQLConnection(MainConnection);
      if (copy(FMainConnection.Protocol,0,10) = 'postgresql') then
        begin
          GeneralQuery.SQL.Text:='select tgname from pg_trigger;';
          GeneralQuery.Open;
        end
      else if (copy(FMainConnection.Protocol,0,6) = 'sqlite') then
        begin
          GeneralQuery.SQL.Text:='select name from sqlite_master where type=''trigger'';';
          GeneralQuery.Open;
        end
      else if (FMainConnection.Protocol = 'mssql') then
        begin
          GeneralQuery.SQL.Text:='SELECT trigger_name = name FROM sysobjects WHERE type = ''TR''';
          GeneralQuery.Open;
        end;
      if GeneralQuery.Active then
        with GeneralQuery do
          begin
            First;
            while not EOF do
              begin
                Triggers.Add(Fields[0].AsString);
                Next;
              end;
          end;
      GeneralQuery.Destroy;
    end;
  Result := False;
  if Triggers.IndexOf(aTriggerName) > 0 then
    begin
      Result := True;
      exit;
    end;
  for i := 0 to Triggers.Count-1 do
    begin
      tmp := Triggers[i];
      if (Uppercase(tmp) = aTriggerName) then
        begin
          Result := True;
          break;
        end;
    end;
end;

function TSqlDBDM.GetDBType: string;
begin
  Result:=TSQLConnection(MainConnection).Protocol;
  if copy(Result,0,8)='postgres' then Result := 'postgres';
  if Result='interbase' then Result := 'firebird';
  if Result='sqlite-3' then Result := 'sqlite';
end;

function TSqlDBDM.GetDBLayerType: string;
begin
  Result := 'SQL';
end;

function TSqlDBDM.CreateTrigger(aTriggerName: string; aTableName: string;
  aUpdateOn: string; aSQL: string;aField : string = ''; aConnection: TComponent = nil): Boolean;
var
  GeneralQuery: TSQLQuery;
begin
  if TriggerExists(aTableName+'_'+aTriggerName) then exit;
  GeneralQuery := TSQLQuery.Create(Self);
  GeneralQuery.Connection := TSQLConnection(MainConnection);
  if Assigned(aConnection) then GeneralQuery.Connection:=TSQLConnection(aConnection);
  if (copy(FMainConnection.Protocol,0,10) = 'postgresql') then
    begin
      if (aField <> '') and (aUpdateOn='UPDATE') then
        begin
          aSQL := 'IF $NEW$.'+QuoteField(aField)+'!=$OLD$.'+QuoteField(aField)+' THEN '+LineEnding+aSQL+' END IF;';
        end;
      GeneralQuery.SQL.Text :=
       'DROP TRIGGER IF EXISTS '+QuoteField(aTableName+'_'+aTriggerName)+' ON '+QuoteField(aTableName)+';'+LineEnding
      +'CREATE OR REPLACE FUNCTION '+aTableName+'_'+aTriggerName+'_TRIGGER() RETURNS TRIGGER AS $BASE$'+LineEnding
      +'BEGIN'+LineEnding
      +StringReplace(StringReplace(StringReplace(aSQL,'$NEW$','new',[rfReplaceAll]),'$OLD$','old',[rfReplaceAll]),'$UPDATED$','new',[rfReplaceAll])+LineEnding
      +'RETURN NEW;'+LineEnding
      +'END;'+LineEnding
      +'$BASE$ LANGUAGE plpgsql;'+LineEnding
      +'CREATE TRIGGER '+QuoteField(aTableName+'_'+aTriggerName)+' AFTER '+aUpdateOn+' ON '+QuoteField(aTableName)+' FOR EACH ROW EXECUTE PROCEDURE '+aTableName+'_'+aTriggerName+'_TRIGGER();'+LineEnding;
      //DebugLn(GeneralQuery.SQL.Text);
      GeneralQuery.ExecSQL;
    end
  else if (FMainConnection.Protocol = 'mssql') then
    begin
      if (aField <> '') and (aUpdateOn='UPDATE') then
        begin
          aSQL := 'IF INSERTED.'+QuoteField(aField)+'!=DELETED.'+QuoteField(aField)+' THEN '+LineEnding+aSQL+' END IF;';
        end;
      GeneralQuery.SQL.Text :=
      'CREATE OR ALTER TRIGGER '+QuoteField(aTableName+'_'+aTriggerName)+' FOR '+QuoteField(aTableName)+' AFTER '+StringReplace(aUpdateOn,'or',',',[rfReplaceAll])
     +' AS'+LineEnding
     +'BEGIN'+LineEnding
     +StringReplace(StringReplace(StringReplace(aSQL,'$NEW$','INSERTED',[rfReplaceAll]),'$OLD$','DELETED',[rfReplaceAll]),'$UPDATED$','new',[rfReplaceAll])+LineEnding
     +'END;';
      //DebugLn(GeneralQuery.SQL.Text);
      GeneralQuery.ExecSQL;
    end
{  else if (copy(FMainConnection.Protocol,0,6) = 'sqlite') then
    begin
      GeneralQuery.SQL.Text :=
      'CREATE TRIGGER IF NOT EXISTS '+QuoteField(aTableName+'_'+aTriggerName)+' AFTER '+StringReplace(aUpdateOn,'or',',',[rfReplaceAll]);
      if aField <> '' then
        GeneralQuery.SQL.Text := GeneralQuery.SQL.Text+' OF '+QuoteField(aField);
      GeneralQuery.SQL.Text := GeneralQuery.SQL.Text+' ON '+QuoteField(aTableName)+' FOR EACH ROW'+LineEnding
     +'BEGIN'+LineEnding
     +StringReplace(StringReplace(StringReplace(aSQL,'$NEW$','new',[rfReplaceAll]),'$OLD$','old',[rfReplaceAll]),'$UPDATED$','new',[rfReplaceAll])+LineEnding
     +'END'+LineEnding;
      DebugLn(GeneralQuery.SQL.Text);
      GeneralQuery.ExecSQL;
    end  }
  else
    Result:=inherited CreateTrigger(aTriggerName, aTableName, aUpdateOn, aSQL,aField, aConnection);
  GeneralQuery.Destroy;
end;
function TSqlDBDM.DropTable(aTableName: string): Boolean;
var
  GeneralQuery: TSQLQuery;
begin
  GeneralQuery := TSQLQuery.Create(Self);
  GeneralQuery.Connection := TSQLConnection(MainConnection);
  GeneralQuery.SQL.Text := 'drop table '+QuoteField(aTableName);
  GeneralQuery.ExecSQL;
  GeneralQuery.Destroy;
  Result := True;
  RemoveCheckTable(aTableName);
end;
function TSqlDBDM.FieldToSQL(aName: string; aType: TFieldType;aSize : Integer;
  aRequired: Boolean): string;
begin
  Result := QuoteField(aName);
  case aType of
  ftString:
    begin
      if (copy(FMainConnection.Protocol,0,8) = 'firebird')
      or (copy(FMainConnection.Protocol,0,9) = 'interbase')
      or (copy(FMainConnection.Protocol,0,10) = 'postgresql') then
        Result := Result+' VARCHAR('+IntToStr(aSize)+')'
      else
        Result := Result+' NVARCHAR('+IntToStr(aSize)+')';
    end;
  ftSmallint,
  ftInteger:Result := Result+' INTEGER';
  ftLargeInt:
    begin
      Result := Result+' BIGINT';
    end;
  ftAutoInc:
    begin
      if (FMainConnection.Protocol = 'mssql') then
        Result := Result+' INTEGER PRIMARY KEY IDENTITY'
      else if (copy(FMainConnection.Protocol,0,6) = 'sqlite') then
        Result := Result+' INTEGER PRIMARY KEY AUTOINCREMENT'
      else Result := Result+' INTEGER PRIMARY KEY';
    end;
  ftFloat:
    begin
      if (copy(FMainConnection.Protocol,0,8) = 'firebird')
      or (copy(FMainConnection.Protocol,0,9) = 'interbase') then
        Result := Result+' DOUBLE PRECISION'
      else
        Result := Result+' FLOAT';
    end;
  ftDate:
    begin
      if (FMainConnection.Protocol = 'mssql') then
        Result := Result+' DATETIME'
      else
        Result := Result+' DATE';
    end;
  ftDateTime:
    begin
      if (FMainConnection.Protocol = 'mssql')
      or (copy(FMainConnection.Protocol,0,6) = 'sqlite')
      then
        Result := Result+' DATETIME'
      else
        Result := Result+' TIMESTAMP'
    end;
  ftTime:
    begin
      if (FMainConnection.Protocol = 'mssql') then
        Result := Result+' DATETIME'
      else
        Result := Result+' TIME';
    end;
  ftBlob:
    begin
      if (FMainConnection.Protocol = 'mssql') then
        Result := Result+' IMAGE'
      else if (copy(FMainConnection.Protocol,0,10) = 'postgresql') then
        Result := Result+' BYTEA'
      else
        Result := Result+' BLOB';
    end;
  ftMemo:
    begin;
      if (copy(FMainConnection.Protocol,0,8) = 'firebird')
      or (copy(FMainConnection.Protocol,0,9) = 'interbase') then
        Result := Result+' BLOB SUB_TYPE 1'
      else
        Result := Result+' TEXT';
    end;
  end;
  if aRequired then
    Result := Result+' NOT NULL'
  else
    begin
      if (FMainConnection.Protocol = 'mssql') then
        Result := Result+' NULL'
    end;
end;
function TSqlDBDM.GetColumns(TableName: string): TStrings;
var
  Metadata: IZDatabaseMetadata;
begin
  Metadata := FMainConnection.DbcConnection.GetMetadata;
  Result := TStringList.Create;
  with Metadata.GetColumns(FMainConnection.Catalog,'',TableNAme,'') do
   try
     while Next do
       Result.Add(GetStringByName('COLUMN_NAME'));
   finally
     Close;
   end;
end;

initialization
  uBaseDBInterface.DatabaseLayers.Add(TSqlDBDM);
end.


