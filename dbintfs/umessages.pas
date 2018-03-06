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
*******************************************************************************}
unit uMessages;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, uBaseDbClasses, db, uBaseDBInterface, uDocuments,
  uBaseApplication, uBaseSearch, uIntfStrConsts,uBaseDatasetInterfaces,fpjson;
type

  { TMessageList }

  TMessageList = class(TBaseDBList)
    procedure FDSDataChange(Sender: TObject; Field: TField);
  private
    FDS: TDataSource;
    FUsedFields : string;
    function GetMsgID: TField;
    function GetSubject: TField;
  protected
    function GetUsedFields : string;virtual;
    procedure PrepareDataSet;
  public
    constructor CreateEx(aOwner: TComponent; DM: TComponent=nil; aConnection: TComponent=
      nil; aMasterdata: TDataSet=nil); override;
    procedure Open; override;
    destructor Destroy; override;
    procedure DefineFields(aDataSet : TDataSet);override;
    procedure SelectByID(aID : string);overload; //Select by ID
    procedure SelectByDir(aDir : Variant);
    procedure SelectByMsgID(aID : Int64);
    procedure SelectByGrpID(aID : Int64;aTreeentry : Variant);
    procedure SelectByParent(aParent : Variant);
    function GetTextFieldName: string;override;
    function GetNumberFieldName : string;override;
    function GetCommissionFieldName: string;override;
    procedure Delete;virtual;
    procedure Archive;
    procedure MarkAsRead;
    property MsgID : TField read GetMsgID;
    property Subject : TField read GetSubject;
  end;

  TMessage = class(TMessageList,IBaseHistory)
  private
    FDocuments: TDocuments;
    FHistory: TBaseHistory;
    FSubMessages : TMessageList;
    function GetSubMessages: TMessageList;
    function GetHistory: TBaseHistory;
  public
    constructor CreateEx(aOwner : TComponent;DM : TComponent=nil;aConnection : TComponent = nil;aMasterdata : TDataSet = nil);override;
    destructor Destroy;override;
    procedure Select(aID : Variant);override;
    procedure Open;override;
    procedure Delete;override;
    function CreateTable : Boolean;override;
    procedure FillDefaults(aDataSet : TDataSet);override;
    function BuildMessageID(aID : Variant) : string;
    function GetText: string;
    procedure ObjectToJSON(AObject: TBaseDBDataSet; AJSON: TJSONObject;
  const ADateAsString: Boolean); override;
    function AsString : string;
    property Documents : TDocuments read FDocuments;
    property SubMessages : TMessageList read GetSubMessages;
    function SelectFromLink(aLink: string) : Boolean; override;
    property History : TBaseHistory read FHistory;
    procedure Next; override;
    procedure Prior; override;
    function ToString: ansistring; override;
  end;
  TSpecialMessage = class(TMessage)
  public
    destructor Destroy;override;
  end;

  { TArchivedMessage }

  TArchivedMessage = class(TBaseDBDataSet)
  public
    constructor CreateEx(aOwner: TComponent; DM: TComponent; aConnection: TComponent
  =nil; aMasterdata: TDataSet=nil); override;
    procedure DefineFields(aDataSet : TDataSet);override;
  end;
implementation
uses uData,md5,Variants,Utils,htmltowiki;

constructor TArchivedMessage.CreateEx(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited CreateEx(aOwner, DM, aConnection, aMasterdata);
  with DataSet as IBaseManageDB do
    UseIntegrity:=False;
end;

procedure TArchivedMessage.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'ARCHIVESTORE';
      TableCaption := strArchive;
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('ID',ftString,120,True);
            Add('DATA',ftBlob,0,False);
          end;
    end;
end;
destructor TSpecialMessage.Destroy;
begin
  inherited Destroy;
end;

function TMessage.GetText: string;
var
  sl: TStringList;
  ss: TStringStream;
  tmp: String;
  i: Integer;
begin
  sl := TStringList.Create;
  if UpperCase(FieldByName('DATATYP').AsString) = 'PLAIN' then
    begin
      ss := TStringStream.Create('');
      Data.BlobFieldToStream(DataSet,'DATA',ss);
      sl.Text:=HTMLDecode(ss.DataString{ConvertEncoding(ss.DataString,GuessEncoding(ss.DataString),EncodingUTF8)});
      sl.TextLineBreakStyle := tlbsCRLF;
      ss.Free;
    end
  else if UpperCase(FieldByName('DATATYP').AsString) = 'HTML' then
    begin
      ss:=TStringStream.Create('');
      Data.BlobFieldToStream(DataSet,'DATA',ss);
      ss.Position:=0;
      tmp := ss.DataString;
      tmp := StripHTML(tmp);
      //tmp := ConvertEncoding(tmp,GuessEncoding(tmp),EncodingUTF8);
      tmp := HTMLDecode(tmp);
      sl.Text:=tmp;
      ss.Free;
    end;
  i := 0;
  while i<sl.Count do
    begin
      if trim(sl[i])='' then
        sl.Delete(i)
      else inc(i);
    end;
  Result := sl.Text;
  sl.Free;
end;

function TMessage.GetSubMessages: TMessageList;
begin
  if not Assigned(FSubMessages) then
    begin
      FSubMessages := TMessageList.CreateEx(Owner,DataModule,Connection);
      FSubmessages.SelectByParent(Self.Id.AsVariant);
    end;
  Result := FSubMessages;
end;

function TMessage.GetHistory: TBaseHistory;
begin
  Result := FHistory;
end;

constructor TMessage.CreateEx(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited CreateEx(aOwner, DM, aConnection, aMasterdata);
  FHistory := TBaseHistory.CreateEx(Self,DataModule,aConnection,DataSet);
  FDocuments := TDocuments.CreateEx(Owner,DataModule,aConnection);
  FSubMessages := nil;
end;
destructor TMessage.Destroy;
begin
  FreeAndNil(FHistory);
  FreeAndNil(FSubMessages);
  FDocuments.Free;
  inherited Destroy;
end;
procedure TMessage.Select(aID: Variant);
begin
  inherited Select(aID);
  if aID <> Null then
    Documents.Select(aID);
end;
procedure TMessage.Open;
begin
  inherited Open;
end;
procedure TMessage.Delete;
var
  aDocument: TDocument;
  Found: Boolean;
begin
  if Count = 0 then exit;
  try
    Documents.Open;
    while Documents.Count > 0 do
      begin
        aDocument := TDocument.CreateEx(Self,Data);
        aDocument.SelectByNumber(Documents.FieldByName('NUMBER').AsVariant);
        aDocument.Open;
        Found := False;
        while aDocument.Count > 0 do
          begin
            aDocument.DataSet.Delete;
            Found := True;
          end;
        aDocument.Free;
        Documents.DataSet.Refresh;
        if not Found then break;
      end;
  except
  end;
  DataSet.Delete;
end;
function TMessage.CreateTable : Boolean;
begin
  Result := inherited CreateTable;
end;
procedure TMessage.FillDefaults(aDataSet: TDataSet);
var
  tmpID: String;
  aGUID: TGUID;
begin
  with aDataSet,BaseApplication as IBaseDBInterface do
    begin
      FieldByName('TREEENTRY').AsVariant:=TREE_ID_MESSAGES;
      tmpID := '';
      CreateGUID(aGUID);
      tmpID := StringReplace(StringReplace(StringReplace(GUIDToString(aGUID),'-','',[rfReplaceAll]),'{','',[rfReplaceAll]),'}','',[rfReplaceAll]);
      FieldByName('ID').AsString:=tmpID+'@inv.local';
      DataSet.FieldByName('DATATYP').AsString:='PLAIN';
    end;
end;
function TMessage.BuildMessageID(aID: Variant): string;
begin

end;

procedure TMessage.ObjectToJSON(AObject: TBaseDBDataSet; AJSON: TJSONObject;
  const ADateAsString: Boolean);
var
  aArray: TJSONArray;
  aNewObj: TJSONObject;
begin
  inherited ObjectToJSON(AObject, AJSON, ADateAsString);
{
  Content.Select(DataSet.FieldbyName('ID').AsString);
  if Content.Count>0 then
    begin
      aArray := TJSONArray.Create;
      while not Content.EOF do
        begin
          aNewObj := TJSONObject.Create;
          Content.ObjectToJSON(Content,aNewObj,ADateAsString);
          aArray.Add(aNewObj);
          Content.Next;
        end;
      AJSON.Add(Content.TableName,aArray);
    end;
}
end;

function TMessage.AsString: string;
begin
  Result := GetText;
end;

function TMessage.SelectFromLink(aLink: string): Boolean;
begin
  Result := inherited SelectFromLink(aLink);
  if not Result then
    begin
      Select(0);
      if rpos('{',aLink) > 0 then
        aLink := copy(aLink,0,rpos('{',aLink)-1)
      else if rpos('(',aLink) > 0 then
        aLink := copy(aLink,0,rpos('(',aLink)-1);
      with DataSet as IBaseManageDB do
        if copy(aLink,0,pos('@',aLink)-1) = TableName then
          begin
            SelectByID(copy(aLink,pos('@',aLink)+1,length(aLink)));
            Result := True;
          end;
    end;
end;
procedure TMessage.Next;
begin
  inherited Next;
end;
procedure TMessage.Prior;
begin
  inherited Prior;
end;

function TMessage.ToString: ansistring;
begin
  Result := ToString;
end;

procedure TMessageList.FDSDataChange(Sender: TObject; Field: TField);
begin
  if not Assigned(Field) then exit;
  if DataSet.ControlsDisabled then exit;
  if (Dataset.State <> dsInsert)
  then
    begin
      if (Field.FieldName = 'TREEENTRY') then
        begin
          FieldByName('GRP_ID').Clear;
        end;
    end;
end;

function TMessageList.GetMsgID: TField;
begin
  result := FieldByName('MSG_ID');
end;

function TMessageList.GetSubject: TField;
begin
  result := FieldByName('SUBJECT');
end;

function TMessageList.GetUsedFields: string;
var
  tmpFields : string = '';
  i: Integer;
  aOldLimit: Integer;
  OldUseP: Boolean;
  oldFilter: String;
begin
  if FUsedFields = '' then
    begin
      with BaseApplication as IBaseDbInterface do
        begin
          with Self.DataSet as IBaseDBFilter,Self.DataSet as IBaseManageDB do
            begin
              oldFilter := Filter;
              Filter := TBaseDBModule(DataModule).ProcessTerm(TBaseDBModule(DataModule).QuoteField(TableName)+'.'+TBaseDBModule(DataModule).QuoteField('SQL_ID')+'='+TBaseDBModule(DataModule).QuoteValue(''));
              Fields := '';
              aOldLimit := Limit;
              Limit := 1;
              OldUseP := UsePermissions;
              UsePermissions:=False;
              DataSet.Open;
              for i := 0 to DataSet.FieldDefs.Count-1 do
                if  (DataSet.FieldDefs[i].Name <> 'DATA')
                and  (DataSet.FieldDefs[i].Name <> 'HEADER')
                then
                  tmpfields := tmpfields+','+TBaseDBModule(DataModule).QuoteField(TableName)+'.'+TBaseDBModule(DataModule).QuoteField(DataSet.FieldDefs[i].Name);
              tmpFields := copy(tmpFields,2,length(tmpFields));
              FUsedFields := tmpFields;
              Limit := aOldLimit;
              UsePermissions:=OldUseP;
              Filter := oldFilter;
            end;
        end;
    end;
  Result := FUsedFields;
end;

procedure TMessageList.PrepareDataSet;
begin
  if FUsedFields = '' then
    GetUsedFields;
  with DataSet as IBaseDBFilter do
    begin
      Fields:=FUsedFields;
    end;
end;

constructor TMessageList.CreateEx(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited CreateEx(aOwner, DM, aConnection, aMasterdata);
  FDS := TDataSource.Create(nil);
  FDS.DataSet := DataSet;
  FDS.OnDataChange:=@FDSDataChange;
end;

procedure TMessageList.Open;
begin
  if (FUsedFields = '') and TBaseDBModule(DataModule).TableExists(TableName) then
    PrepareDataSet;
  inherited Open;
end;

destructor TMessageList.Destroy;
begin
  FDS.Free;
  inherited Destroy;
end;

procedure TMessageList.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'MESSAGES';
      TableCaption := strMessages;
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('USER',ftString,20,True);
            Add('ID',ftString,220,True);
            Add('TREEENTRY',ftLargeint,0,True);
            Add('MSG_ID',ftLargeint,0,True);
            Add('GRP_ID',ftLargeint,0,False); //ID Per Group must be changed on Move (NNTP,IMAP needs an LongInt id out SQL_ID is too Big)
            Add('TYPE',ftString,5,True);
            Add('READ',ftString,1,True);
            Add('DRAFT',ftString,1,False);
            Add('FLAGGED',ftString,1,False);
            Add('SENDER',ftString,800,True);
            Add('REPLYTO',ftMemo,0,False);
            Add('SENDDATE',ftDateTime,0,True);
            Add('ANSWERED',ftDateTime,0,False);
            Add('SUBJECT',ftString,220,false);
            Add('RECEIVERS',ftString,800,False);
            Add('CC',ftString,800,False);
            Add('BCC',ftString,800,False);
            Add('CONTENTTYP',ftString,50,False);
            Add('PRIORITY',ftInteger,0,False);
            Add('PARENT',ftLargeint,0,False);
            Add('LINES',ftInteger,0,False);
            Add('GRP_FLAGS',ftInteger,0,False); //Compiled Flags for faster access
            Add('SIZE',ftLargeInt,0,False);
            Add('DATATYP',ftString,6,False);
            Add('HEADER',ftMemo,0,False);
            Add('DATA',ftBlob,0,False);
          end;
      if Assigned(ManagedIndexdefs) then
        with ManagedIndexDefs do
          begin
            Add('ID','ID',[]);
            Add('MSG_ID','MSG_ID',[]);
            Add('GRP_ID','GRP_ID',[]);
            Add('USER','USER',[]);
            Add('PARENT','PARENT',[]);
            Add('TREEENTRY','TREEENTRY',[]);
          end;
    end;
end;
procedure TMessageList.SelectByID(aID: string);
begin
  with BaseApplication as IBaseDBInterface do
    with DataSet as IBaseDBFilter do
      begin
        Filter := Data.ProcessTerm(Data.QuoteField('ID')+'='+Data.QuoteValue(aID));
      end;
end;
procedure TMessageList.SelectByDir(aDir: Variant);
begin
  with BaseApplication as IBaseDBInterface do
    with DataSet as IBaseDBFilter do
      begin
        Filter := Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(VarToStr(aDir))+' AND '+Data.ProcessTerm(Data.QuoteField('PARENT')+'='+Data.QuoteValue(''));
      end;
end;

procedure TMessageList.SelectByMsgID(aID: Int64);
begin
  with BaseApplication as IBaseDBInterface do
    with DataSet as IBaseDBFilter do
      begin
        Filter := Data.QuoteField('MSG_ID')+'='+Data.QuoteValue(IntToStr(aID));
      end;
end;

procedure TMessageList.SelectByGrpID(aID: Int64; aTreeentry: Variant);
begin
  with BaseApplication as IBaseDBInterface do
    with DataSet as IBaseDBFilter do
      begin
        Filter := Data.QuoteField('GRP_ID')+'='+Data.QuoteValue(IntToStr(aID))+' AND '+Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(aTreeentry);
      end;
end;

procedure TMessageList.SelectByParent(aParent: Variant);
begin
  with BaseApplication as IBaseDBInterface do
    with DataSet as IBaseDBFilter do
      begin
        Filter := Data.QuoteField('PARENT')+'='+Data.QuoteValue(IntToStr(aParent));
      end;
end;
function TMessageList.GetTextFieldName: string;
begin
  Result:='SUBJECT';
end;
function TMessageList.GetNumberFieldName: string;
begin
  Result:='MSG_ID';
end;

function TMessageList.GetCommissionFieldName: string;
begin
  Result:='SENDER';
end;

procedure TMessageList.Delete;
begin
  if Count = 0 then exit;
  DataSet.Edit;
  DataSet.FieldByName('TREEENTRY').AsVariant := TREE_ID_DELETED_MESSAGES;
  DataSet.FieldByName('GRP_ID').Clear;
  DataSet.FieldByName('READ').AsString := 'Y';
  DataSet.Post;
end;
procedure TMessageList.Archive;
begin
  if Count = 0 then exit;
  DataSet.Edit;
  DataSet.FieldByName('TREEENTRY').AsVariant := TREE_ID_ARCHIVE_MESSAGES;
  DataSet.FieldByName('GRP_ID').Clear;
  DataSet.FieldByName('READ').AsString := 'Y';
  DataSet.Post;
end;
procedure TMessageList.MarkAsRead;
begin
  if Count = 0 then exit;
  DataSet.Edit;
  DataSet.FieldByName('READ').AsString := 'Y';
  DataSet.Post;
end;
initialization
  RegisterdataSetClass('MESSAGE',TMessage);
end.
