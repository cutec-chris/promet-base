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
Created 01.06.2006
*******************************************************************************}
unit uZeosDBDM;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, db, ZConnection, ZSqlMetadata,
  ZAbstractRODataset, ZDataset, ZSequence,ZAbstractConnection,
  uModifiedDS,ZSqlMonitor,Utils,uBaseDatasetInterfaces,syncobjs,
  uBaseDBInterface,uBaseDbClasses,ZCompatibility,dateutils,pingsend;
type
  TUnprotectedDataSet = class(TDataSet);

  { TZeosDBDM }

  TZeosDBDM = class(TBaseDBModule)
    procedure FConnectionAfterConnect(Sender: TObject);
    procedure FConnectionBeforeConnect(Sender: TObject);
    procedure MonitorTrace(Sender: TObject; Event: TZLoggingEvent;
      var LogTrace: Boolean);
  private
    FMainConnection : TZConnection;
    FLimitAfterSelect : Boolean;
    FLimitSTMT : string;
    FDBTyp : string;
    FProperties : string;
    FPassword : string;
    Monitor : TZSQLMonitor;
    function GetConnection: TComponent;override;
    function DBExists : Boolean;
  protected
    Sequence : TZSequence;
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
    function BlobFieldStream(DataSet: TDataSet; Fieldname: string): TStream;
      override;
    function GetErrorNum(e: EDatabaseError): Integer; override;
    procedure DeleteExpiredSessions;override;
    function GetNewConnection: TComponent;override;
    function QuoteField(aField: string): string; override;
    function QuoteValue(aField: string): string; override;
    procedure Disconnect(aConnection : TComponent);override;
    procedure Connect(aConnection: TComponent); override;
    function StartTransaction(aConnection : TComponent;ForceTransaction : Boolean = False): Boolean;override;
    function CommitTransaction(aConnection : TComponent): Boolean;override;
    function RollbackTransaction(aConnection : TComponent): Boolean;override;
    function IsTransactionActive(aConnection : TComponent): Boolean;override;
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

  { TZeosDBDataSet }

  TZeosDBDataSet = class(TZQuery,IBaseDBFilter,IBaseManageDB,IBaseSubDatasets,IBaseModifiedDS)
    procedure TDateTimeFieldGetText(Sender: TField; var aText: string;
      DisplayText: Boolean);
  private
    FFirstOpen : Boolean;
    FSubDataSets : Tlist;
    FFields : string;
    FFilter,FBaseFilter,FIntFilter : string;
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
    FSQL,FIntSQL : string;
    FParams : TStringList;
    FInBeforePost : Boolean;
    FHasNewID : Boolean;
    procedure SetNewIDIfNull;
    function BuildSQL : string;
    function IndexExists(IndexName : string) : Boolean;
    procedure WaitForLostConnection;
    procedure DoUpdateSQL;
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
    function GetAsReadonly: Boolean;
    procedure SetAsReadonly(AValue: Boolean);
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
uses ZDbcIntfs,uBaseApplication,uEncrypt;
resourcestring
  strUnknownDbType                = 'Unbekannter Datenbanktyp';
  strDatabaseConnectionLost       = 'Die Datenbankverbindung wurde verlohren !';

procedure TZeosDBDataSet.TDateTimeFieldGetText(Sender: TField;
  var aText: string; DisplayText: Boolean);
begin
  if not Sender.IsNull then
    begin
      if trunc(Sender.AsDateTime)=Sender.AsDateTime then
        aText := FormatDateTime(ShortDateFormat,Sender.AsDateTime)
      else
        aText := FormatDateTime(ShortDateFormat+' '+ShortTimeFormat,Sender.AsDateTime);
    end;
end;

procedure TZeosDBDataSet.SetNewIDIfNull;
begin
  if (FieldDefs.IndexOf('AUTO_ID') = -1) and (FieldDefs.IndexOf('SQL_ID') > -1) and  FieldByName('SQL_ID').IsNull then
    begin
      FieldByName('SQL_ID').AsVariant:=TBaseDBModule(Self.Owner).GetUniID(Connection);
      FHasNewID:=True;
    end
  else if (FieldDefs.IndexOf('SQL_ID') = -1) and (FieldDefs.IndexOf('AUTO_ID') > -1) and FieldByName('AUTO_ID').IsNull then
    begin
      FieldByName('AUTO_ID').AsVariant:=TBaseDBModule(Self.Owner).GetUniID(Connection,'GEN_AUTO_ID');
      FHasNewID:=True;
    end;
end;

function TZeosDBDataSet.BuildSQL : string;
var
  DoQuote : Boolean = False;

function BuildJoins : string;
var
  aDS : string;
  tmp: String;
begin
  if not (pos(',',FTableNames) > 0) then
    begin
      Result := FTableNames;
      if Result = '' then
        begin
          Result := TBaseDBModule(Owner).GetFullTableName(GetTableName);
          DoQuote:=(pos('.',Result)>0) or DoQuote;
        end
      else Result := TBaseDBModule(Owner).QuoteField(Result);
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
    if pos(',',TZeosDBDM(Owner).QuoteField(FSortFields)) = 0 then
      begin
        sResult += TZeosDBDM(Owner).QuoteField(FDefaultTableName)+'.'+TZeosDBDM(Owner).QuoteField(FSortFields);
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
            sResult += TZeosDBDM(Owner).QuoteField(FDefaultTableName)+'.'+TZeosDBDM(Owner).QuoteField(copy(tmp,0,pos(',',tmp)-1));
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
            sResult += TZeosDBDM(Owner).QuoteField(FDefaultTableName)+'.'+TZeosDBDM(Owner).QuoteField(tmp);
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
      if (FManagedFieldDefs.IndexOf('AUTO_ID') = -1) and (TZeosDBDM(Owner).UsersFilter <> '') and FUsePermissions and TZeosDBDM(Owner).TableExists('PERMISSIONS') then
        begin
          PJ := ' LEFT JOIN '+TZeosDBDM(Owner).QuoteField('PERMISSIONS')+' ON ('+TZeosDBDM(Owner).QuoteField('PERMISSIONS')+'.'+TZeosDBDM(Owner).QuoteField('REF_ID_ID')+'='+TZeosDBDM(Owner).QuoteField(FDefaultTableName)+'.'+TZeosDBDM(Owner).QuoteField('SQL_ID')+')';
          PW := ' AND ('+aFilter+') AND (('+TZeosDBDM(Owner).UsersFilter+') OR '+TZeosDBDM(Owner).QuoteField('PERMISSIONS')+'.'+TZeosDBDM(Owner).QuoteField('USER')+' is NULL)';
        end
      else if (FManagedFieldDefs.IndexOf('AUTO_ID') = -1) and FUsePermissions and TZeosDBDM(Owner).TableExists('PERMISSIONS') then
        begin
          PJ := ' LEFT JOIN '+TZeosDBDM(Owner).QuoteField('PERMISSIONS')+' ON ('+TZeosDBDM(Owner).QuoteField('PERMISSIONS')+'.'+TZeosDBDM(Owner).QuoteField('REF_ID_ID')+'='+TZeosDBDM(Owner).QuoteField(FDefaultTableName)+'.'+TZeosDBDM(Owner).QuoteField('SQL_ID')+')';
          PW := ' AND ('+TZeosDBDM(Owner).QuoteField('PERMISSIONS')+'.'+TZeosDBDM(Owner).QuoteField('USER')+' is NULL)'
        end;
      PW := StringReplace(PW,'AND ()','',[rfReplaceAll]);
      Result := StringReplace(StringReplace(StringReplace(FSQL,'@PERMISSIONJOIN@',PJ,[]),'@PERMISSIONWHERE@',PW,[]),'@DEFAULTORDER@',SResult,[]);
    end
  else if Assigned(FOrigTable) then
    begin
      Result := 'SELECT ';
      if FDistinct then
        Result := Result+'DISTINCT ';
      if TZeosDBDM(Owner).LimitAfterSelect and ((FLimit > 0)) then
        Result += Format(TZeosDBDM(Owner).LimitSTMT,[':Limit'])+' ';
      if FFields = '' then
        Result += TZeosDBDM(Owner).QuoteField(FDefaultTableName)+'.'+'* '
      else
        Result += FFields+' ';
      aFilter := FIntFilter;
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
            aFilter := '('+aFilter+') and ('+TZeosDBDM(Owner).QuoteField('REF_ID')+'=:'+TZeosDBDM(Owner).QuoteField(aRefField)+')'
          else
            aFilter := TZeosDBDM(Owner).QuoteField('REF_ID')+'=:'+TZeosDBDM(Owner).QuoteField(aRefField);
        end;
      if (FManagedFieldDefs.IndexOf('AUTO_ID') = -1) and (TZeosDBDM(Owner).UsersFilter <> '') and FUsePermissions and TZeosDBDM(Owner).TableExists('PERMISSIONS') then
        Result += 'FROM '+BuildJoins+' LEFT JOIN '+TZeosDBDM(Owner).QuoteField('PERMISSIONS')+' ON ('+TZeosDBDM(Owner).QuoteField('PERMISSIONS')+'.'+TZeosDBDM(Owner).QuoteField('REF_ID_ID')+'='+TZeosDBDM(Owner).QuoteField(FDefaultTableName)+'.'+TZeosDBDM(Owner).QuoteField('SQL_ID')+') WHERE ('+aFilter+') AND (('+TZeosDBDM(Owner).UsersFilter+') OR '+TZeosDBDM(Owner).QuoteField('PERMISSIONS')+'.'+TZeosDBDM(Owner).QuoteField('USER')+' is NULL)'
      else if (FManagedFieldDefs.IndexOf('AUTO_ID') = -1) and FUsePermissions and TZeosDBDM(Owner).TableExists('PERMISSIONS') then
        Result += 'FROM '+BuildJoins+' LEFT JOIN '+TZeosDBDM(Owner).QuoteField('PERMISSIONS')+' ON ('+TZeosDBDM(Owner).QuoteField('PERMISSIONS')+'.'+TZeosDBDM(Owner).QuoteField('REF_ID_ID')+'='+TZeosDBDM(Owner).QuoteField(FDefaultTableName)+'.'+TZeosDBDM(Owner).QuoteField('SQL_ID')+') WHERE ('+aFilter+') AND ('+TZeosDBDM(Owner).QuoteField('PERMISSIONS')+'.'+TZeosDBDM(Owner).QuoteField('USER')+' is NULL)'
      else
        Result += 'FROM '+BuildJoins+' WHERE ('+aFilter+')';
      Result := StringReplace(Result,' WHERE () AND ','WHERE ',[]);
      Result := StringReplace(Result,' WHERE ()','',[]);
      //if (copy(TZConnection(TBaseDBModule(Owner).MainConnection).Protocol,0,5) = 'mssql') and DoQuote then
      //  Result := '('+Result+')';
      if (FSortFields <> '') and ((FSortDirection <> sdIgnored) or (FBaseSortDirection <> sdIgnored)) then
        begin
          BuildSResult;
          if FUseBaseSorting then
            Result += ' ORDER BY '+Format(FBaseSorting,[sResult])
          else
            Result += ' ORDER BY '+sResult;
        end;
      if (FLimit > 0) and (not TZeosDBDM(Owner).LimitAfterSelect) then
        Result += ' '+Format(TZeosDBDM(Owner).LimitSTMT,[':Limit']);
    end
  else
    Result := SQL.text;
  if Assigned(FOrigTable) then TBaseDBModule(ForigTable.DataModule).LastStatement := Result;
end;
function TZeosDBDataSet.IndexExists(IndexName: string): Boolean;
var
  Metadata: TZSQLMetadata;
  CustomQuery: TZQuery;
begin
  CustomQuery := TZQuery.Create(Self);
  CustomQuery.Connection := Connection;
  if (copy(TZConnection(TBaseDBModule(Owner).MainConnection).Protocol,0,8) = 'firebird')
  or (copy(TZConnection(TBaseDBModule(Owner).MainConnection).Protocol,0,9) = 'interbase') then
    begin
      CustomQuery.SQL.Text := 'select rdb$index_name from rdb$indices where rdb$index_name='+TZeosDBDM(Owner).QuoteValue(indexname);
      CustomQuery.Open;
      Result := CustomQuery.RecordCount > 0;
      CustomQuery.Close;
    end
  else if (copy(TZConnection(TBaseDBModule(Owner).MainConnection).Protocol,0,6) = 'sqlite') then
    begin
      CustomQuery.SQL.Text := 'select name from SQLITE_MASTER where "TYPE"=''index'' and NAME='+TZeosDBDM(Owner).QuoteValue(indexname);
      CustomQuery.Open;
      Result := CustomQuery.RecordCount > 0;
      CustomQuery.Close;
    end
  else if (copy(TZConnection(TBaseDBModule(Owner).MainConnection).Protocol,0,5) = 'mssql') then
    begin
      CustomQuery.SQL.Text := 'select name from dbo.sysindexes where NAME='+TZeosDBDM(Owner).QuoteValue(indexname);
      CustomQuery.Open;
      Result := CustomQuery.RecordCount > 0;
      CustomQuery.Close;
    end
  else if (copy(TZConnection(TBaseDBModule(Owner).MainConnection).Protocol,0,8) = 'postgres') then
    begin
      CustomQuery.SQL.Text := 'select * from pg_class where relname='+TZeosDBDM(Owner).QuoteValue(indexname);
      CustomQuery.Open;
      Result := CustomQuery.RecordCount > 0;
      CustomQuery.Close;
    end
  else
    begin
      Metadata := TZSQLMetaData.Create(TZConnection(TBaseDBModule(Owner).MainConnection));
      MetaData.Connection := Connection;
      MetaData.MetadataType:=mdIndexInfo;
      Metadata.Catalog:=TZConnection(TBaseDBModule(Owner).MainConnection).Catalog;
      Metadata.TableName:=copy(indexname,0,pos('_',indexname)-1);
      MetaData.Filter:='INDEX_NAME='+TZeosDBDM(Owner).QuoteValue(indexname);
      MetaData.Filtered:=True;
      MetaData.Active:=True;
      Result := MetaData.RecordCount > 0;
      MetaData.Free;
    end;
  CustomQuery.Free;
end;

procedure TZeosDBDataSet.WaitForLostConnection;
var
  aConnThere: Boolean;
begin
  if not TZeosDBDM(Owner).Ping(Connection) then
    begin
      if Assigned(TZeosDBDM(Owner).OnConnectionLost) then
        TZeosDBDM(Owner).OnConnectionLost(TZeosDBDM(Owner));
      aConnThere := False;
      while not aConnThere do
        begin
          if GetCurrentThreadID=MainThreadID then
            begin
              if Assigned(TZeosDBDM(Owner).OnDisconnectKeepAlive) then
                TZeosDBDM(Owner).OnDisconnectKeepAlive(TZeosDBDM(Owner));
            end;
          try
            if TZeosDBDM(Owner).Ping(Connection) then
              aConnThere := True;
            sleep(2000);
          except
            sleep(200);
          end;
        end;
      if Assigned(TZeosDBDM(Owner).OnConnect) then
        TZeosDBDM(Owner).OnConnect(TZeosDBDM(Owner));
    end;
end;

procedure TZeosDBDataSet.DoUpdateSQL;
begin
  Close;
  if FSQL<>'' then
    SetSQL(FSQL)
  else
    begin
      SQL.Text:='';
      FIntSQL := '';
      SetFilter(FFilter);
    end;
end;

function TZeosDBDataSet.CreateTable : Boolean;
var
  aSQL: String;
  i: Integer;
  bConnection: TZAbstractConnection = nil;
//  bConnection: TZConnection = nil;
  GeneralQuery: TZQuery;
  RestartTransaction: Boolean = False;
  NewTableName: String;
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
          NewTableName := GetFullTableName(GetTableName);
          aSQL := 'CREATE TABLE '+NewTableName+' ('+lineending;
          if FUpStdFields then
            begin
              if FManagedFieldDefs.IndexOf('AUTO_ID') = -1 then
                aSQL += TZeosDBDM(Self.Owner).FieldToSQL('SQL_ID',ftLargeInt,0,True)+' PRIMARY KEY,'+lineending
              else
                begin
                  aSQL += TZeosDBDM(Self.Owner).FieldToSQL('AUTO_ID',ftLargeInt,0,True)+' PRIMARY KEY,'+lineending;
                end;
            end;
          if Assigned(MasterSource) and (FManagedFieldDefs.IndexOf('REF_ID')=-1) then
            begin
              aSQL += TZeosDBDM(Self.Owner).FieldToSQL('REF_ID',ftLargeInt,0,True);
              if FUseIntegrity
              and (pos('.',NewTableName)=-1) //Wenn eigene Tabelle in externer Datenbank, keine Ref. Intigrität
              and (pos('.',GetFullTableName(TZeosDBDataSet(MasterSource.DataSet).DefaultTableName))=-1) then //Wenn übergeordnete Tabelle in externer Datenbank, keine Ref. Intigrität
                begin
                  with MasterSource.DataSet as IBaseManageDB do
                    begin
                      if ManagedFieldDefs.IndexOf('AUTO_ID') = -1 then
                        aSQL += ' REFERENCES '+QuoteField(TZeosDBDataSet(MasterSource.DataSet).DefaultTableName)+'('+QuoteField('SQL_ID')+') ON DELETE CASCADE'
                      else
                        aSQL += ' REFERENCES '+QuoteField(TZeosDBDataSet(MasterSource.DataSet).DefaultTableName)+'('+QuoteField('AUTO_ID')+') ON DELETE CASCADE';
                    end;
                  if (copy(TZConnection(TBaseDBModule(Self.Owner).MainConnection).Protocol,0,6) = 'sqlite') then
                    aSQL += ' DEFERRABLE INITIALLY DEFERRED';
                end;
              aSQL+=','+lineending;
            end;
          for i := 0 to FManagedFieldDefs.Count-1 do
            if FManagedFieldDefs[i].Name <> 'AUTO_ID' then
              aSQL += TZeosDBDM(Self.Owner).FieldToSQL(FManagedFieldDefs[i].Name,FManagedFieldDefs[i].DataType,FManagedFieldDefs[i].Size,FManagedFieldDefs[i].Required)+','+lineending;
          if FUpStdFields then
            aSQL += TZeosDBDM(Self.Owner).FieldToSQL('TIMESTAMPD',ftDateTime,0,True)+');'
          else
            aSql := copy(aSQL,0,length(aSQL)-2)+');';
          with BaseApplication as IBaseApplication do
            Debug(aSQL);
          TZConnection(bConnection).ExecuteDirect(aSQL);
          //TODO:reconnect to DB and reopen all tables that WAS open
          //TZConnection(bConnection).Disconnect;
          //TZConnection(bConnection).Connect;
        end;
    end;
  Close;
end;
function TZeosDBDataSet.CheckTable: Boolean;
var
  i: Integer;
begin
  Result := False;
  with TBaseDBModule(Owner) do
    begin
      if DoCheck or (FFields = '') then
          begin
            for i := 0 to FManagedFieldDefs.Count-1 do
              begin
                if (FieldDefs.IndexOf(FManagedFieldDefs[i].Name) = -1) and (FManagedFieldDefs[i].Name <> 'AUTO_ID') then
                  begin
                    Result := True;
                  end
                else if FieldDefs.IndexOf(FManagedFieldDefs[i].Name)>-1 then
                  begin
                    if FieldByName(FManagedFieldDefs[i].Name).Size<FManagedFieldDefs[i].Size then
                      Result := True;
                  end;
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
function TZeosDBDataSet.AlterTable: Boolean;
var
  i: Integer;
  aSQL: String;
  GeneralQuery: TZQuery;
  Changed: Boolean = False;
  aConnection : TZAbstractConnection;
begin
  Result := True;
  with BaseApplication as IBaseApplication do
    Debug('AlterTable:'+FDefaultTableName);
  try
    if FFields <> '' then exit;
    with TBaseDBModule(Owner) do
      begin
        for i := 0 to FManagedFieldDefs.Count-1 do
          if (FieldDefs.IndexOf(FManagedFieldDefs[i].Name) = -1) and (FManagedFieldDefs[i].Name <> 'AUTO_ID') then
            begin
              aSQL := 'ALTER TABLE '+GetFullTableName(FDefaultTableName)+' ADD '+TZeosDBDM(Self.Owner).FieldToSQL(FManagedFieldDefs[i].Name,FManagedFieldDefs[i].DataType,FManagedFieldDefs[i].Size,False)+';';
              with BaseApplication as IBaseApplication do
                Debug(aSQL);
              aConnection := Connection;
              TZConnection(aConnection).ExecuteDirect(aSQL);
              Changed := True;
              Result := True;
            end
        else if FieldDefs.IndexOf(FManagedFieldDefs[i].Name)>-1 then
          begin
            if FieldByName(FManagedFieldDefs[i].Name).Size<FManagedFieldDefs[i].Size then
              begin
                if (copy(Connection.Protocol,0,8) = 'postgres') then
                  aSQL := 'ALTER TABLE '+GetFullTableName(FDefaultTableName)+' ALTER COLUMN '+QuoteField(FManagedFieldDefs[i].Name)+' TYPE '+TZeosDBDM(Self.Owner).FieldToSQL('',FManagedFieldDefs[i].DataType,FManagedFieldDefs[i].Size,False)+';'
                else if (copy(Connection.Protocol,0,6) = 'sqlite') then
                else
                  aSQL := 'ALTER TABLE '+GetFullTableName(FDefaultTableName)+' ALTER COLUMN '+QuoteField(FManagedFieldDefs[i].Name)+' '+TZeosDBDM(Self.Owner).FieldToSQL('',FManagedFieldDefs[i].DataType,FManagedFieldDefs[i].Size,False)+';';
                with BaseApplication as IBaseApplication do
                  Debug(aSQL);
                aConnection := Connection;
                if aSQL<>'' then
                  begin
                    with BaseApplication as IBaseApplication do
                      Debug(aSQL);
                    TZConnection(aConnection).ExecuteDirect(aSQL);
                    Changed := True;
                    Result := True;
                  end;
              end;
          end;
        aSQL := '';
        if Assigned(FManagedIndexDefs) then
          for i := 0 to FManagedIndexDefs.Count-1 do                                           //Primary key
            begin
              try
                if (not IndexExists(Uppercase(Self.DefaultTableName+'_'+FManagedIndexDefs.Items[i].Name))) and (FManagedIndexDefs.Items[i].Name <>'SQL_ID') and (pos('.',GetFullTableName(Self.DefaultTableName))=0) then
                  begin
                    aSQL := 'CREATE ';
                    if ixUnique in FManagedIndexDefs.Items[i].Options then
                      aSQL := aSQL+'UNIQUE ';
                    aSQL := aSQL+'INDEX '+QuoteField(Uppercase(Self.DefaultTableName+'_'+FManagedIndexDefs.Items[i].Name))+' ON '+GetFullTableName(Self.DefaultTableName)+' ('+QuoteField(StringReplace(FManagedIndexDefs.Items[i].Fields,';',QuoteField(','),[rfReplaceAll]))+');'+lineending;
                    if aSQL <> '' then
                      begin
                        with BaseApplication as IBaseApplication do
                          Debug(aSQL);
                        TZConnection(Connection).ExecuteDirect(aSQL);
                      end;
                    Result := True;
                  end;
              except
              end;
            end;
      end;
  except
    on e : Exception do
      begin
        with BaseApplication as IBaseApplication do
          Error('Altering failed:'+e.Message);
        Result := False;
      end;
  end;
  if Result and Changed and (Self.FDefaultTableName<>'TABLEVERSIONS') then
    begin
      with BaseApplication as IBaseApplication do
        Info('Table '+Self.FDefaultTableName+' resetting Metadata Infos...');
      try
        Connection.DbcConnection.GetMetadata.ClearCache;
      except
      end;
    end;
  if Changed then
    TBaseDBModule(Self.Owner).UpdateTableVersion(Self.FDefaultTableName);
end;

procedure TZeosDBDataSet.InternalOpen;
var
  a: Integer;
begin
  if (not Assigned(Connection)) or (not Connection.Connected) then exit;
  if Connection.Protocol='mysql' then
    Properties.Values['ValidateUpdateCount'] := 'False';
  if Assigned(FOrigTable) and Assigned(ForigTable.DataModule) then
    TBaseDBModule(ForigTable.DataModule).LastTime := GetTicks;
  if TZeosDBDM(Owner).IgnoreOpenRequests then exit;
  if FFirstOpen then
    begin
      FIntSQL := BuildSQL;
      SQL.Text := FIntSQL;
      if (FLimit>0) and Assigned(Params.FindParam('Limit')) and ((copy(Connection.Protocol,0,8)<>'postgres') or (FLimit>90))  then
        ParamByName('Limit').AsInteger:=FLimit;
      FFirstOpen:=False;
    end;
  if Assigned(FOrigTable) and Assigned(ForigTable.DataModule) then
    TBaseDBModule(ForigTable.DataModule).CriticalSection.Enter;
  try
      try
        inherited InternalOpen;
      except
        on e : Exception do
          begin
            if (TZeosDBDM(Owner).CheckedTables.IndexOf(Self.GetTableName)>-1) and TZeosDBDM(Owner).Ping(Connection) then
              begin
                TZeosDBDM(Owner).CheckedTables.Delete(TZeosDBDM(Owner).CheckedTables.IndexOf(Self.GetTableName));
                try
                  if TZeosDBDM(Owner).Ping(Connection) then
                    CreateTable;
                  inherited InternalOpen;
                except
                  if TZeosDBDM(Owner).Ping(Connection) then
                    raise
                  else
                    begin
                      WaitForLostConnection;
                      inherited InternalOpen;
                    end;
                end;
              end
            else
              begin
                if TZeosDBDM(Owner).Ping(Connection) then
                  raise
                else
                  begin
                    WaitForLostConnection;
                    inherited InternalOpen;
                  end;
              end;
          end;
      end;
      try
      if Assigned(FOrigTable) and Assigned(ForigTable.DataModule) then
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
                  then
                    begin
                      TDateTimeField(Fields[a]).DisplayFormat := ShortDateFormat+' '+ShortTimeFormat;
                      TDateTimeField(Fields[a]).OnGetText:=@TDateTimeFieldGetText;
                    end;
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
      if ReadOnly then
        with BaseApplication as IBaseApplication do
          if Assigned(FOrigTable) then
            begin
              Warning(FOrigTable.TableName+' Dataset is read Only !');
              ReadOnly:=False;
            end;
  finally
    if Assigned(FOrigTable) and Assigned(ForigTable.DataModule) then
      TBaseDBModule(ForigTable.DataModule).CriticalSection.Leave;
  end;
end;

procedure TZeosDBDataSet.InternalRefresh;
begin
  if TZeosDBDM(Owner).IgnoreOpenRequests then exit;
  if Assigned(FOrigTable) and Assigned(ForigTable.DataModule) then
    TBaseDBModule(ForigTable.DataModule).CriticalSection.Enter;
  try
  try
    inherited InternalRefresh;
  except
    InternalClose;
    if not Active then
      begin
        if TZeosDBDM(Owner).Ping(Connection) then
          InternalOpen
        else
          begin
            WaitForLostConnection;
            InternalOpen;
          end;
      end;
  end;
  finally
    if Assigned(FOrigTable) and Assigned(ForigTable.DataModule) then
      TBaseDBModule(ForigTable.DataModule).CriticalSection.Leave;
  end;
end;

procedure TZeosDBDataSet.InternalPost;
begin
  inherited InternalPost;
end;

procedure TZeosDBDataSet.DoAfterInsert;
begin
  inherited DoAfterInsert;
  if Assigned(FOrigTable) then
    begin
      FOrigTable.DisableChanges;
      FOrigTable.FillDefaults(Self);
      FOrigTable.EnableChanges;
    end;
end;
procedure TZeosDBDataSet.DoBeforePost;
var
  UserCode: String;
begin
  inherited DoBeforePost;
  if FInBeforePost then exit;
  try
  FInBeforePost := True;
  if Assigned(Self.FOrigTable) then
    Self.FOrigTable.DisableChanges;
  FHasNewID:=False;
  SetNewIDIfNull;
  if FUpStdFields and Assigned(FOrigTable) {and (FOrigTable.Changed)} then
    begin
      if (FieldDefs.IndexOf('TIMESTAMPD') > -1) then
        FieldByName('TIMESTAMPD').AsDateTime:=LocalTimeToUniversal(Now());
      with BaseApplication as IBaseDBInterface do
        begin
          if TBaseDBModule(ForigTable.DataModule).Users.DataSet.Active and ((FieldDefs.IndexOf('CREATEDBY') > -1) or (FieldDefs.IndexOf('CHANGEDBY') > -1)) then
            UserCode := TBaseDBModule(ForigTable.DataModule).Users.IDCode.AsString
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
    FInBeforePost:= False;
    if Assigned(Self.FOrigTable) then
      Self.FOrigTable.EnableChanges;
  end;
end;
procedure TZeosDBDataSet.DoBeforeInsert;
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

procedure TZeosDBDataSet.DoBeforeEdit;
begin
  inherited DoBeforeEdit;
end;

procedure TZeosDBDataSet.DoBeforeDelete;
begin
  inherited DoBeforeDelete;
  if (GetTableName='DELETEDITEMS')
  or (GetTableName='DBTABLES')
  then exit;
  try
    if Assigned(FOrigTable) and Assigned(FOrigTable.OnRemove) then FOrigTable.OnRemove(FOrigTable);
    if GetUpStdFields = True then
      TZeosDBDM(Owner).DeleteItem(FOrigTable);
  except
  end;
end;
procedure TZeosDBDataSet.DoAfterDelete;
begin
  inherited DoAfterDelete;
  if Assigned(FOrigTable) then
    FOrigTable.Change;
end;
procedure TZeosDBDataSet.DoAfterScroll;
begin
  inherited DoAfterScroll;
  if Assigned(ForigTable) then
    FOrigTable.UnChange;
end;

procedure TZeosDBDataSet.DoBeforeCancel;
begin
  inherited DoBeforeCancel;
  if State = dsInsert then
    begin
      if Assigned(FOrigTable) and Assigned(FOrigTable.OnRemove) then FOrigTable.OnRemove(FOrigTable);
    end;
end;

function TZeosDBDataSet.GetFields: string;
begin
  Result := FFields;
end;
function TZeosDBDataSet.GetFilter: string;
begin
  Result := FFilter;
end;
function TZeosDBDataSet.GetBaseFilter: string;
begin
  Result := FBaseFilter;
end;
function TZeosDBDataSet.GetLimit: Integer;
begin
  Result := FLimit;
end;
function TZeosDBDataSet.GetSortDirection: TSortDirection;
begin
  Result := FSortDirection;
end;
function TZeosDBDataSet.GetSortFields: string;
begin
  Result := FSortFields;
end;

function TZeosDBDataSet.GetLocalSortFields: string;
begin
  Result := SortedFields;
end;

function TZeosDBDataSet.GetBaseSortFields: string;
begin
  Result := FBaseSortFields;
end;
function TZeosDBDataSet.GetSortLocal: Boolean;
begin
  Result := SortType <> stIgnored;
end;
procedure TZeosDBDataSet.SetFields(const AValue: string);
begin
  if FFields<>AValue then
    begin
      FFields := AValue;
      DoUpdateSQL;
    end;
end;
procedure TZeosDBDataSet.SetFilter(const AValue: string);
var
  NewSQL: string;
  i: Integer;
  aPar: TParam;
begin
  if (FFilter=AValue) and (SQL.text<>'')  then
    begin
      if (AValue<>'') or (pos('where',lowercase(SQL.Text))=0) then
        exit;
    end;
  TZeosDBDM(Owner).DecodeFilter(AValue,FParams,NewSQL);
  Close;
  FFilter := AValue;
  if (FIntFilter<>NewSQL) or (SQL.Text='')  then //Params and SQL has changed
    begin
      FSQL := '';
      if TZeosDBDM(Owner).CheckForInjection(AValue) then exit;
      FIntFilter:=NewSQL;
      FIntSQL := '';
      FIntSQL := BuildSQL;
      Params.Clear;
      SQL.text := FIntSQL;
    end;
  for i := 0 to FParams.Count-1 do
    begin
      if Assigned(Params.FindParam(FParams.Names[i])) then
        begin
          aPar := ParamByName(FParams.Names[i]);
          aPar.AsString:=FParams.ValueFromIndex[i];
        end;
    end;
  if (FLimit>0) and Assigned(Params.FindParam('Limit')) then
    ParamByName('Limit').AsInteger:=FLimit;
end;
procedure TZeosDBDataSet.SetBaseFilter(const AValue: string);
begin
  FBaseFilter := AValue;
  DoUpdateSQL;
end;
function TZeosDBDataSet.GetSQL: string;
begin
  Result := FSQL;
end;
procedure TZeosDBDataSet.SetSQL(const AValue: string);
var
  NewSQL: string;
  i: Integer;
  aPar: TParam;
begin
  if TZeosDBDM(Owner).CheckForInjection(AValue) then exit;
  if AValue=FSQL then exit;
  Close;
  Params.Clear;
  FParams.Clear;
  FSQL := AValue;
  FIntSQL := BuildSQL;
  FFilter := '';
  FIntFilter:='';
  SQL.Text := FIntSQL;
  {
  TZeosDBDM(Owner).DecodeFilter(AValue,FParams,NewSQL);
  Params.Clear;
  FSQL := NewSQL;
  FIntSQL := BuildSQL;
  FFilter := '';
  SQL.Text := FIntSQL;
  for i := 0 to FParams.Count-1 do
    begin
      aPar := ParamByName(FParams.Names[i]);
      aPar.AsString:=FParams.ValueFromIndex[i];
    end;
  }
  if (FLimit>0) and Assigned(Params.FindParam('Limit')) then
    ParamByName('Limit').AsInteger:=FLimit;
end;
procedure TZeosDBDataSet.Setlimit(const AValue: Integer);
begin
  if FLimit = AValue then exit;
  FLimit := AValue;
  DoUpdateSQL;
end;
procedure TZeosDBDataSet.SetSortDirection(const AValue: TSortDirection);
begin
  if (FSortDirection=AValue) and Active then exit;
  FSortDirection := AValue;
  if not GetSortLocal then
    begin
      DoUpdateSQL;
    end;
end;
procedure TZeosDBDataSet.SetSortFields(const AValue: string);
begin
  FSortFields := AValue;
end;

procedure TZeosDBDataSet.SetLocalSortFields(const AValue: string);
begin
  SortedFields:=AValue;
end;

procedure TZeosDBDataSet.SetBaseSortFields(const AValue: string);
begin
  FBaseSortFields := AValue;
end;
procedure TZeosDBDataSet.SetSortLocal(const AValue: Boolean);
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
function TZeosDBDataSet.GetFilterTables: string;
begin
  Result := FTableNames;
end;
procedure TZeosDBDataSet.SetFilterTables(const AValue: string);
begin
  if AValue = FTableNames then exit;
  FTableNames := AValue;
  DoUpdateSQL;
end;
function TZeosDBDataSet.GetUsePermissions: Boolean;
begin
  Result := FUsePermissions;
end;
procedure TZeosDBDataSet.SetUsePermisions(const AValue: Boolean);
begin
  if AValue = FUsePermissions then exit;
  FUsePermissions := AValue;
  DoUpdateSQL;
end;
function TZeosDBDataSet.GetDistinct: Boolean;
begin
  Result := FDistinct;
end;
procedure TZeosDBDataSet.SetDistinct(const AValue: Boolean);
begin
  if AValue = FDistinct then exit;
  FDistinct := AValue;
  DoUpdateSQL;
end;
function TZeosDBDataSet.GetBaseSorting: string;
begin
  Result := FBaseSorting;
end;
procedure TZeosDBDataSet.SetBaseSorting(AValue: string);
begin
  FBaseSorting := AValue;
end;

function TZeosDBDataSet.GetBaseSortDirection: TSortDirection;
begin
  Result := FBaseSortDirection;
end;
procedure TZeosDBDataSet.SetBaseSortDirection(AValue: TSortDirection);
begin
  FBaseSortDirection := AValue;
end;
function TZeosDBDataSet.GetUseBaseSorting: Boolean;
begin
  Result := FUseBaseSorting;
end;
procedure TZeosDBDataSet.SetUseBaseSorting(AValue: Boolean);
begin
  FUseBaseSorting := AValue;
  DoUpdateSQL;
end;
function TZeosDBDataSet.GetfetchRows: Integer;
begin
  result := FetchRow;
end;
procedure TZeosDBDataSet.SetfetchRows(AValue: Integer);
begin
  FetchRow:=AValue;
end;
function TZeosDBDataSet.GetManagedFieldDefs: TFieldDefs;
begin
  Result := FManagedFieldDefs;
end;
function TZeosDBDataSet.GetManagedIndexDefs: TIndexDefs;
begin
  Result := FManagedIndexDefs;
end;
function TZeosDBDataSet.GetTableName: string;
begin
  Result := FDefaultTableName;
end;
procedure TZeosDBDataSet.SetTableName(const AValue: string);
begin
  FDefaultTableName := AValue;
end;
function TZeosDBDataSet.GetConnection: TComponent;
begin
  Result := Connection;
end;
function TZeosDBDataSet.GetTableCaption: string;
begin
  Result := FTableCaption;
end;
procedure TZeosDBDataSet.SetTableCaption(const AValue: string);
begin
  FTableCaption := AValue;
end;
function TZeosDBDataSet.GetUpStdFields: Boolean;
begin
  Result := FUpStdFields;
end;

procedure TZeosDBDataSet.SetUpStdFields(AValue: Boolean);
begin
  FUpStdFields := AValue;
end;

function TZeosDBDataSet.GetUpChangedBy: Boolean;
begin
  Result := FUpChangedBy;
end;

procedure TZeosDBDataSet.SetUpChangedBy(AValue: Boolean);
begin
  FUpChangedBy:=AValue;
end;

function TZeosDBDataSet.GetUseIntegrity: Boolean;
begin
  Result := FUseIntegrity;
end;
procedure TZeosDBDataSet.SetUseIntegrity(AValue: Boolean);
begin
  FUseIntegrity:=AValue;
end;

function TZeosDBDataSet.GetAsReadonly: Boolean;
begin
  result := Self.ReadOnly;
end;

procedure TZeosDBDataSet.SetAsReadonly(AValue: Boolean);
begin
  Self.ReadOnly:=AValue;
end;

procedure TZeosDBDataSet.SetFieldData(Field: TField; Buffer: Pointer);
var
  tmp: String;
begin
  inherited;
  if Assigned(FOrigTable) then
    FOrigTable.Change;
end;
function TZeosDBDataSet.GetSubDataSet(aName: string): TComponent;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FSubDataSets.Count-1 do
    with TBaseDBDataSet(FSubDataSets[i]) as IBaseManageDB do
      if TableName = aName then
        Result := TBaseDBDataSet(FSubDataSets[i]);
end;
procedure TZeosDBDataSet.RegisterSubDataSet(aDataSet: TComponent);
begin
  FSubDataSets.Add(aDataSet);
end;
function TZeosDBDataSet.GetCount: Integer;
begin
  Result := FSubDataSets.Count;
end;
function TZeosDBDataSet.GetSubDataSetIdx(aIdx: Integer): TComponent;
begin
  Result := nil;
  if aIdx < FSubDataSets.Count then
    Result := TBaseDbDataSet(FSubDataSets[aIdx]);
end;
function TZeosDBDataSet.IsChanged: Boolean;
begin
  Result := Modified;
  if Assigned(FOrigTable) then
    Result := ForigTable.Changed;
end;
constructor TZeosDBDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFirstOpen:=True;
  FSQL := '';
  DoCheck := False;
  fBaseSorting := '%s';
  FChangeUni:=False;
  FUseBaseSorting:=False;
  FBaseSortDirection:=sdIgnored;
  FManagedFieldDefs := TFieldDefs.Create(Self);
  FManagedIndexDefs := TIndexDefs.Create(Self);
  FSubDataSets := TList.Create;
  FUsePermissions := False;
  FOrigTable := nil;
  SortType := stIgnored;
  FUpStdFields := True;
  FUpChangedBy := True;
  FUseIntegrity:=False;//disable for sync
  FParams := TStringList.Create;
  FInBeforePost := False;
end;
destructor TZeosDBDataSet.Destroy;
begin
  //TODO: Free Subdatasets ??
  FParams.Free;
  FManagedFieldDefs.Free;
  FManagedIndexDefs.Free;
  FSubDataSets.Free;
  try
    inherited Destroy;
  except
  end;
end;

procedure TZeosDBDataSet.DoExecSQL;
begin
  ExecSQL;
end;

function TZeosDBDataSet.NumRowsAffected: Integer;
begin
  Result := RowsAffected;
end;

procedure TZeosDBDM.FConnectionAfterConnect(Sender: TObject);
begin
  TZConnection(Sender).Password:=Encrypt(TZConnection(Sender).Password,9997);
end;

procedure TZeosDBDM.FConnectionBeforeConnect(Sender: TObject);
begin
  TZConnection(Sender).Password:=Decrypt(TZConnection(Sender).Password,9997);
end;

procedure TZeosDBDM.MonitorTrace(Sender: TObject; Event: TZLoggingEvent;
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
function TZeosDBDM.GetConnection: TComponent;
begin
  Result := TComponent(FMainConnection);
end;
function TZeosDBDM.DBExists: Boolean;
begin
  Result := TableExists('USERS') and TableExists('GEN_SQL_ID') and TableExists('GEN_AUTO_ID');
end;

function TZeosDBDM.GetLimitAfterSelect: Boolean;
begin
  Result := FLimitAfterSelect;
end;

function TZeosDBDM.GetLimitSTMT: string;
begin
  Result := FLimitSTMT;
end;

function TZeosDBDM.GetSyncOffset: Integer;
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
  bConnection: TComponent;
begin
  if Assigned(Sequence) then
    begin
      bConnection := MainConnection;
      Sequence.Connection := TZConnection(bConnection);
      Result := Sequence.GetCurrentValue shr 56;
      Sequence.Connection := nil;
    end
  else
    begin
      Statement := TZConnection(MainConnection).DbcConnection.CreateStatement;
      ResultSet := Statement.ExecuteQuery('SELECT '+QuoteField('ID')+' FROM '+QuoteField('GEN_SQL_ID'));
      if ResultSet.Next then
        Result := ResultSet.GetLong(1) shr 56
      else Result := 0;
      ResultSet.Close;
      Statement.Close;
    end;
end;
procedure TZeosDBDM.SetSyncOffset(const AValue: Integer);
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
      Statement := TZConnection(MainConnection).DbcConnection.CreateStatement;
      Statement.Execute('update '+QuoteField('GEN_SQL_ID')+' set '+QuoteField('ID')+'='+IntToStr(aVal));
      Statement.Close;
    end;
end;
constructor TZeosDBDM.Create(AOwner: TComponent);
begin
  FDataSetClass := TZeosDBDataSet;
  FMainConnection := TZConnection.Create(AOwner);
  if {BaseApplication.HasOption('debug') or} BaseApplication.HasOption('debug-sql') then
    begin
      Monitor := TZSQLMonitor.Create(FMainConnection);
      Monitor.Active:=True;
      Monitor.OnTrace:=@MonitorTrace;
    end
  else Monitor:=nil;
  Sequence := nil;
  inherited Create(AOwner);
end;
destructor TZeosDBDM.Destroy;
begin
  try
    if FMainconnection.Connected then
      FMainConnection.Disconnect;
    if Assigned(Sequence) then
      begin
        Sequence.Connection := nil;
        FreeAndNil(Sequence);
      end;
    try
      //FMainConnection.Destroy;
    except
    end;
    inherited Destroy;
  except
  end;
end;
function TZeosDBDM.SetProperties(aProp: string;Connection : TComponent = nil): Boolean;
var
  tmp: String;
  FConnection : TZConnection;
  actDir: String;
begin
  if Assigned(BaseApplication) then
    with BaseApplication as IBaseDBInterface do
      LastError := '';
  FProperties := aProp;
  FConnection := TZConnection(Connection);
  if not Assigned(FConnection) then
    FConnection := FMainConnection;
  Result := True;
  tmp := aProp;
  try
    CleanupSession;
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
    FConnection.Password:=Encrypt(FConnection.Password,9997);
    FConnection.BeforeConnect:=@FConnectionBeforeConnect;
    FConnection.AfterConnect:=@FConnectionAfterConnect;
    if (copy(FConnection.Protocol,0,6) = 'sqlite')
    then
      begin
        if Connection=MainConnection then //Dont check this on attatched db´s (we want to create them on the fly)
          if not FileExists(FConnection.Database) then
            raise Exception.Create('Databasefile dosend exists');
        FConnection.TransactIsolationLevel:=tiNone;
      end;
    if (copy(FConnection.Protocol,0,8) = 'postgres')
    then
      begin
        {$IFDEF CPUARM}
        FConnection.Properties.Add('sslmode=disable');
        {$ENDIF}
        FConnection.TransactIsolationLevel:=tiNone;
        FConnection.AutoEncodeStrings:=true;
      end
    else if (copy(FConnection.Protocol,0,8) = 'firebird')
    or (copy(FConnection.Protocol,0,9) = 'interbase')
    or (copy(FConnection.Protocol,0,5) = 'mysql')
    then
      begin
        FConnection.TransactIsolationLevel:=tiReadCommitted;
      end
    else if (copy(FConnection.Protocol,0,5) = 'mssql') then
      begin
        FConnection.TransactIsolationLevel:=tiReadUnCommitted;
        FConnection.ClientCodepage:='utf8';
        FConnection.AutoEncodeStrings:=true;
      end;
    if (copy(FConnection.Protocol,0,5) = 'mysql') then
      begin
        FConnection.Properties.Add('ValidateUpdateCount=-1');
        FConnection.Properties.Add('MYSQL_OPT_RECONNECT=TRUE');
      end;
    FConnection.Properties.Add('Undefined_Varchar_AsString_Length= 255');

    inherited;

    //*********Connect***********

    FConnection.Connected:=True;

    FLimitAfterSelect := False;
    FLimitSTMT := 'LIMIT %s';
    FDBTyp := FConnection.Protocol;
    if FConnection.Protocol = 'sqlite-3' then
      begin
        //FConnection.ExecuteDirect('PRAGMA synchronous = NORMAL;');
        FConnection.ExecuteDirect('PRAGMA cache_size = 5120;');
        //FConnection.ExecuteDirect('PRAGMA auto_vacuum = INCREMENTAL;');
        //FConnection.ExecuteDirect('PRAGMA journal_mode = TRUNCATE;');
        FConnection.ExecuteDirect('PRAGMA recursive_triggers = ON;');
        FConnection.ExecuteDirect('PRAGMA foreign_keys = ON;');
        FConnection.ExecuteDirect('PRAGMA case_sensitive_like = ON;');
        //FConnection.ExecuteDirect('PRAGMA secure_delete = ON;');
        //FConnection.ExecuteDirect('PRAGMA incremental_vacuum(50);');
      end
    else if (copy(FConnection.Protocol,0,8) = 'firebird')
         or (copy(FConnection.Protocol,0,9) = 'interbase') then
      begin
        FDBTyp := 'firebird';
        FLimitSTMT := 'ROWS 1 TO %s';
        if not Assigned(Sequence) then
          begin
            Sequence := TZSequence.Create(Owner);
          end;
      end
    else if FConnection.Protocol = 'mssql' then
      begin
        FLimitAfterSelect := True;
        FLimitSTMT := 'TOP %s';
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
  if Assigned(MandantDetails) then
    begin
      try
        MandantDetails.Open;
        DBTables.Open;
        actDir := GetCurrentDir;
        SetCurrentDir(ExtractFileDir(FConnection.Database));
        if Assigned(MandantDetails.FieldByName('DBSTATEMENTS')) and (MandantDetails.FieldByName('DBSTATEMENTS').AsString<>'') then
          FConnection.ExecuteDirect(MandantDetails.FieldByName('DBSTATEMENTS').AsString);
        SetCurrentDir(actDir);
      except

      end;
    end;
end;
function TZeosDBDM.CreateDBFromProperties(aProp: string): Boolean;
var
  FConnection: TZConnection;
  tmp: String;
  aPassword: String;
  aUser: String;
  aDatabase: String;
begin
  FConnection := TZConnection.Create(Self);
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
function TZeosDBDM.IsSQLDB: Boolean;
begin
  Result:=True;
end;
function TZeosDBDM.GetNewDataSet(aTable: TBaseDBDataSet;
  aConnection: TComponent; MasterData: TDataSet; aTables: string): TDataSet;
begin
  if IgnoreOpenrequests then exit;
  Result := FDataSetClass.Create(Self);
  if not Assigned(aConnection) then
    aConnection := MainConnection;
  with TZeosDBDataSet(Result) do
    begin
      Connection := TZConnection(aConnection);
      FTableNames := aTables;
      aTable.DefineFields(Result);
      aTable.DefineDefaultFields(Result,Assigned(Masterdata));
      FOrigTable := aTable;
      if Assigned(Masterdata) then
        begin
          if not Assigned(TZeosDBDataSet(MasterData).MasterDataSource) then
            begin
              TZeosDBDataSet(MasterData).MasterDataSource := TDataSource.Create(Self);
              TZeosDBDataSet(MasterData).MasterDataSource.DataSet := MasterData;
            end;
          DataSource := TZeosDBDataSet(MasterData).MasterDataSource;
          MasterSource := TZeosDBDataSet(MasterData).MasterDataSource;
          with Masterdata as IBaseSubDataSets do
            RegisterSubDataSet(aTable);
        end;
    end;
end;
function TZeosDBDM.GetNewDataSet(aSQL: string; aConnection: TComponent;
  MasterData : TDataSet = nil;aOrigtable : TBaseDBDataSet = nil): TDataSet;
begin
  Result := FDataSetClass.Create(Self);
  if not Assigned(aConnection) then
    aConnection := MainConnection;
  with TZeosDBDataSet(Result) do
    begin
      FOrigTable := aOrigtable;
      Connection := TZConnection(aConnection);
      SQL.Text := aSQL;
      FSQL:=aSQL;
      if Assigned(Masterdata) then
        begin
          if not Assigned(TZeosDBDataSet(MasterData).MasterDataSource) then
            begin
              TZeosDBDataSet(MasterData).MasterDataSource := TDataSource.Create(Self);
              TZeosDBDataSet(MasterData).MasterDataSource.DataSet := MasterData;
            end;
          DataSource := TZeosDBDataSet(MasterData).MasterDataSource;
          MasterSource := TZeosDBDataSet(MasterData).MasterDataSource;
        end;
    end;
end;

procedure TZeosDBDM.DestroyDataSet(DataSet: TDataSet);
begin
  try
    if Assigned(DataSet) and Assigned(TZeosDBDataSet(DataSet).MasterSource) then
      begin
        TZeosDBDataSet(DataSet).MasterSource.DataSet.DataSource.Free;
        TZeosDBDataSet(DataSet).MasterSource := nil;
        TZeosDBDataSet(DataSet).DataSource := nil;
      end;
  except
    with BaseApplication as IBaseApplication do
     Debug(Self.ClassName+' has Masterdata that is destroyed before itself !!');
  end;
end;

function TZeosDBDM.Ping(aConnection: TComponent): Boolean;
var
  atime: Integer;
begin
  Result := True;
  try
    Result := TZConnection(aConnection).Ping;
  except
    Result := PingHost(TZConnection(aConnection).HostName)>-1;//Unsupported
  end;
end;
function TZeosDBDM.DateToFilter(aValue: TDateTime): string;
begin
  if FMainConnection.Protocol = 'mssql' then
    Result := QuoteValue(FormatDateTime('YYYYMMDD',aValue))
  else
    Result:=inherited DateToFilter(aValue);
end;
function TZeosDBDM.DateTimeToFilter(aValue: TDateTime): string;
begin
  if FMainConnection.Protocol = 'mssql' then
    Result := QuoteValue(FormatDateTime('YYYYMMDD HH:MM:SS.ZZZZ',aValue))
  else
    Result:=inherited DateTimeToFilter(aValue);
end;
function TZeosDBDM.GetUniID(aConnection : TComponent = nil;Generator : string = 'GEN_SQL_ID';AutoInc : Boolean = True): Variant;
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
      Sequence.Connection := TZConnection(bConnection);
      Result := Sequence.GetNextValue;
      Sequence.Connection := nil;
    end
  else
    begin
      try
        if (copy(FMainConnection.Protocol,0,6) = 'sqlite') and (Assigned(aConnection)) then
          Statement := TZConnection(aConnection).DbcConnection.CreateStatement //we have global locking in sqlite so we must use the actual connection
        else
          Statement := TZConnection(MainConnection).DbcConnection.CreateStatement;
        if AutoInc then
          begin
            if (copy(FMainConnection.Protocol,0,5) = 'mysql') then
              begin
                Statement.Execute('update '+QuoteField(Generator)+' set '+QuoteField('ID')+'='+QuoteField('ID')+'+1;')
              end
            else
              begin
                if LimitAfterSelect then
                  Statement.Execute('update '+QuoteField(Generator)+' set '+QuoteField('ID')+'=(select '+Format(LimitSTMT,['1'])+' '+QuoteField('ID')+' from '+QuoteField(Generator)+')+1;')
                else
                  Statement.Execute('update '+QuoteField(Generator)+' set '+QuoteField('ID')+'=(select '+QuoteField('ID')+' from '+QuoteField(Generator)+' '+Format(LimitSTMT,['1'])+')+1;');
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
procedure TZeosDBDM.StreamToBlobField(Stream: TStream; DataSet: TDataSet;
  Fieldname: string);
var
  Posted: Boolean;
  GeneralQuery: TZQuery;
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
      GeneralQuery := TZQuery.Create(Self);
      GeneralQuery.Connection := TZQuery(DataSet).Connection;
      GeneralQuery.SQL.Text := 'select * from '+GetFullTableName(TZeosDBDataSet(DataSet).DefaultTableName)+' where '+QuoteField('SQL_ID')+'='+QuoteValue(DataSet.FieldByName('SQL_ID').AsString)+';';
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

function TZeosDBDM.BlobFieldStream(DataSet: TDataSet; Fieldname: string
  ): TStream;
var
  GeneralQuery: TZQuery;
  aSql: String;
begin
  Result := nil;
  if DataSet.Fielddefs.IndexOf(FieldName) = -1 then
    begin
      GeneralQuery := TZQuery.Create(Self);
      GeneralQuery.Connection := TZQuery(DataSet).Connection;
      aSql := 'select '+QuoteField(Fieldname)+' from '+GetFullTableName(TZeosDBDataSet(DataSet).DefaultTableName)+' where '+QuoteField('SQL_ID')+'='+QuoteValue(DataSet.FieldByName('SQL_ID').AsString)+';';
      GeneralQuery.SQL.Text := aSql;
      GeneralQuery.Open;
      result := GeneralQuery.CreateBlobStream(GeneralQuery.FieldByName(Fieldname),bmRead);
      GeneralQuery.Free;
    end
  else Result := inherited;
end;

function TZeosDBDM.GetErrorNum(e: EDatabaseError): Integer;
begin
  Result:=inherited GetErrorNum(e);
  if e is EZDatabaseError then
    Result := EZDatabaseError(e).ErrorCode;
end;

procedure TZeosDBDM.DeleteExpiredSessions;
var
  GeneralQuery: TZQuery;
begin
  GeneralQuery := TZQuery.Create(Self);
  GeneralQuery.Connection := FMainConnection;
  GeneralQuery.SQL.Text := 'DELETE FROM '+QuoteField('ACTIVEUSERS')+' WHERE ('+QuoteField('EXPIRES')+' < '+Self.DateTimeToFilter(Now)+');';
  GeneralQuery.ExecSQL;
  GeneralQuery.Free;
end;
function TZeosDBDM.GetNewConnection: TComponent;
begin
  Result := TZConnection.Create(Self);
  with Result as TZConnection do
    begin
      Setproperties(FProperties,Result);
    end;
end;

function TZeosDBDM.QuoteField(aField: string): string;
begin
  Result:=inherited QuoteField(aField);
  if (copy(TZConnection(MainConnection).Protocol,0,5) = 'mysql') then
    Result := '`'+aField+'`';
end;

function TZeosDBDM.QuoteValue(aField: string): string;
begin
  Result:=inherited QuoteValue(aField);
  if (copy(TZConnection(MainConnection).Protocol,0,5) = 'mysql') then
    Result := ''''+aField+'''';
end;

procedure TZeosDBDM.Disconnect(aConnection: TComponent);
begin
  TZConnection(aConnection).Disconnect;
end;

procedure TZeosDBDM.Connect(aConnection: TComponent);
begin
  TZConnection(aConnection).Connect;
end;

function TZeosDBDM.StartTransaction(aConnection: TComponent;ForceTransaction : Boolean = False): Boolean;
begin
  TZConnection(aConnection).Tag := Integer(TZConnection(aConnection).TransactIsolationLevel);
  if ForceTransaction and (copy(TZConnection(aConnection).Protocol,0,6) = 'sqlite') then
    TZConnection(aConnection).TransactIsolationLevel:=tiReadCommitted
  else if (copy(TZConnection(aConnection).Protocol,0,8) = 'postgres') then
    TZConnection(aConnection).TransactIsolationLevel:=tiReadCommitted
  else if (copy(TZConnection(aConnection).Protocol,0,5) = 'mssql') then
    TZConnection(aConnection).TransactIsolationLevel:=tiReadUnCommitted;
  TZConnection(aConnection).StartTransaction;
end;
function TZeosDBDM.CommitTransaction(aConnection: TComponent): Boolean;
begin
  if not TZConnection(aConnection).AutoCommit then
    TZConnection(aConnection).Commit;
  if TZTransactIsolationLevel(TZConnection(aConnection).Tag) <> TZConnection(aConnection).TransactIsolationLevel then
    TZConnection(aConnection).TransactIsolationLevel := TZTransactIsolationLevel(TZConnection(aConnection).Tag);
end;
function TZeosDBDM.RollbackTransaction(aConnection: TComponent): Boolean;
begin
  if not TZConnection(aConnection).AutoCommit then
    TZConnection(aConnection).Rollback;
  if TZTransactIsolationLevel(TZConnection(aConnection).Tag) <> TZConnection(aConnection).TransactIsolationLevel then
    TZConnection(aConnection).TransactIsolationLevel := TZTransactIsolationLevel(TZConnection(aConnection).Tag);
end;

function TZeosDBDM.IsTransactionActive(aConnection: TComponent): Boolean;
begin
  Result := TZConnection(aConnection).InTransaction;
end;

function TZeosDBDM.TableExists(aTableName: string;aConnection : TComponent = nil;AllowLowercase: Boolean = False): Boolean;
var
  aIndex: longint;
  i: Integer;
  tmp: String;
  aQuerry: TZReadOnlyQuery;
begin
  Result := False;
  if not FMainConnection.Connected then exit;
  aTableName:=GetFullTableName(aTableName);
  aTableName:=StringReplace(aTableName,copy(QuoteField(''),0,1),'',[rfReplaceAll]);
  try
    if Tables.Count = 0 then
      begin
        //Get uncached
        if not Assigned(aConnection) then
          aConnection := FMainConnection;
        if FMainConnection.DbcConnection<>nil then
          TZConnection(aConnection).DbcConnection.GetMetadata.ClearCache;
        TZConnection(aConnection).GetTableNames('','',Tables);
        TZConnection(aConnection).GetTriggerNames('','',Triggers);
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
  //Try to open the non existent Table since we dont know if its in another database
  if not Result then
    begin
      aTableName:=GetFullTableName(aTableName);
      aQuerry := TZReadOnlyQuery.Create(Self);
      aQuerry.Connection:=TZConnection(MainConnection);
      aQuerry.SQL.Text := 'select count(*) from '+aTableName;
      if (FMainConnection.Protocol = 'mssql') then
        aQuerry.SQL.Text := '('+aQuerry.SQL.Text+')';
      try
        aQuerry.Open;
      except
      end;
      Result := aQuerry.Active;
      if Result then
        begin
          aTableName:=StringReplace(aTableName,copy(QuoteField(''),0,1),'',[rfReplaceAll]);
          if Tables.Count>0 then
            Tables.Add(aTableName);
        end;
      aQuerry.Free;
    end;
end;

function TZeosDBDM.TriggerExists(aTriggerName: string; aConnection: TComponent;
  AllowLowercase: Boolean): Boolean;
var
  i: Integer;
  tmp: String;
  GeneralQuery: TZQuery;
begin
  if Triggers.Count= 0 then
    begin
      GeneralQuery := TZQuery.Create(Self);
      GeneralQuery.Connection:=TZConnection(MainConnection);
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

function TZeosDBDM.GetDBType: string;
begin
  Result:=TZConnection(MainConnection).Protocol;
  if copy(Result,0,8)='postgres' then Result := 'postgres';
  if Result='interbase' then Result := 'firebird';
  if Result='sqlite-3' then Result := 'sqlite';
end;

function TZeosDBDM.GetDBLayerType: string;
begin
  Result := 'SQL';
end;

function TZeosDBDM.CreateTrigger(aTriggerName: string; aTableName: string;
  aUpdateOn: string; aSQL: string;aField : string = ''; aConnection: TComponent = nil): Boolean;
var
  GeneralQuery: TZQuery;
begin
  if TriggerExists(aTableName+'_'+aTriggerName) then exit;
  GeneralQuery := TZQuery.Create(Self);
  GeneralQuery.Connection := TZConnection(MainConnection);
  if Assigned(aConnection) then GeneralQuery.Connection:=TZConnection(aConnection);
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
function TZeosDBDM.DropTable(aTableName: string): Boolean;
var
  GeneralQuery: TZQuery;
begin
  GeneralQuery := TZQuery.Create(Self);
  GeneralQuery.Connection := TZConnection(MainConnection);
  GeneralQuery.SQL.Text := 'drop table '+QuoteField(aTableName);
  GeneralQuery.ExecSQL;
  GeneralQuery.Destroy;
  Result := True;
  RemoveCheckTable(aTableName);
end;
function TZeosDBDM.FieldToSQL(aName: string; aType: TFieldType; aSize: Integer;
  aRequired: Boolean): string;
begin
  if aName <> '' then
    Result := QuoteField(aName)
  else Result:='';
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
function TZeosDBDM.GetColumns(TableName: string): TStrings;
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
  uBaseDBInterface.DatabaseLayers.Add(TZeosDBDM);
end.


