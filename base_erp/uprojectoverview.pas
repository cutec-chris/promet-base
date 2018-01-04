unit uprojectoverview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TreeFilterEdit, Forms, Controls, ComCtrls, Menus,
  ActnList, ExtCtrls, uprometframesinplace, uMainTreeFrame, uIntfStrConsts,
  uProjects, uBaseDBInterface, uBaseDbClasses, uBaseDatasetInterfaces, db,
  EditBtn, StdCtrls;

type

  { TfObjectStructureFrame }

  TfObjectStructureFrame = class(TPrometInplaceFrame)
    acAddLevel: TAction;
    ActionList1: TActionList;
    eFilter: TEdit;
    MenuItem1: TMenuItem;
    Panel1: TPanel;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    procedure acAddLevelExecute(Sender: TObject);
    procedure eFilterChange(Sender: TObject);
    procedure FrameEnter(Sender: TObject);
    function FTreeOpen(aEntry: TTreeEntry): Boolean;
    procedure FTreetvMainExpanded(Sender: TObject; Node: TTreeNode);
  private
    FObject: TBaseDbList;
    FrootNode : TTreeNode;
    { private declarations }
    FTree : TfMainTree;
    procedure SetObject(AValue: TBaseDbList);
    procedure DoFilterItems(aNode : TTreeNode);
  public
    { public declarations }
    property ParentObject : TBaseDbList read FObject write SetObject;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation
{$R *.lfm}
uses uData,uMasterdata,Dialogs;
procedure TfObjectStructureFrame.FrameEnter(Sender: TObject);
begin
  FrootNode.Expanded:=True;
end;

procedure TfObjectStructureFrame.acAddLevelExecute(Sender: TObject);
var
  aEntry: TTreeEntry;
  aDataSet: TBaseDBDataset;
  aParent: TField;
  bDataSet: TBaseDBDataset;
  Node1: TTreeNode;
  Node2: TTreeNode;
  Node3: TTreeNode;
  Node4: TTreeNode;
  aPos: TMDPos;
  aMS: TMasterdata;
  DoMove: Boolean = True;
  Found: Boolean;
begin
  with FTree do
    begin
      if tvMain.Items.Count=0 then exit;
      aEntry := TTreeEntry(tvMain.Items[0].Data);
      aDataSet := aEntry.DataSourceType.CreateEx(Self,Data);
      with aDataSet.DataSet as IBaseDBFilter do
        Filter := aEntry.Filter;
      aDataSet.Open;
      if aDataSet.Count>0 then
        begin
          aParent := aDataSet.FieldByName('PARENT');
          if Assigned(aParent) and (aParent.AsVariant<>Null) then //Aus Tabellenparent generieren
            begin
              bDataSet := aEntry.DataSourceType.CreateEx(Self,Data);
              bDataSet.Filter(Data.QuoteField('SQL_ID')+'='+Data.QuoteValue(aParent.AsString));
              bDataSet.Open;
              if bDataSet.Count>0 then
                begin
                  FObject:=TBaseDbList(bDataSet);
                  Node1 := tvMain.Items.AddChildObject(nil,'',TTreeEntry.Create);
                  TTreeEntry(Node1.Data).Rec := FObject.GetBookmark;
                  Node1.HasChildren:=True;
                  with FObject.DataSet as IBaseManageDB do
                    TTreeEntry(Node1.Data).Filter:=Data.QuoteField(TableName)+'.'+Data.QuoteField('SQL_ID')+'='+Data.QuoteValue(FObject.Id.AsString);
                  TTreeEntry(Node1.Data).DataSourceType := TBaseDBDataSetClass(FObject.ClassType);
                  TTreeEntry(Node1.Data).Text[0] := FObject.Text.AsString+' ('+FObject.Number.AsString+')'+' ['+FObject.Status.AsString+']';
                  case FObject.ClassName of
                  'TProject':TTreeEntry(Node1.Data).Typ := etProject;
                  end;
                  Node1.HasChildren:=True;
                  Node1.Expanded:=True;
                  Node2 := tvMain.Items[0];
                  while Assigned(Node2) do
                    begin
                      Node3 := Node2;
                      Node2 := Node2.GetNextSibling;
                      if Node3<>Node1 then
                        begin
                          Node4 := nil;
                          if Node1.Count>0 then
                            Node4 := Node1.Items[0];
                          while Assigned(Node4) do
                            begin
                              if (TTreeEntry(Node4.data).Rec=TTreeEntry(Node3.Data).Rec)
                              or (TTreeEntry(Node4.data).Text[0]=TTreeEntry(Node3.Data).Text[0])
                              then
                                begin
                                  Node3.MoveTo(Node4,naInsertBehind);
                                  Node4.Free;
                                  break;
                                end;
                              Node4 := Node4.GetNextSibling;
                            end;
                        end;
                    end;
                end;
              bDataSet.Free;
            end
          else if (aEntry.Typ=etArticle) then //Aus Stücklisten generieren
            begin
              aPos := TMDPos.Create(nil);
              Data.SetFilter(aPos,Data.QuoteField('IDENT')+'='+Data.QuoteValue(aDataSet.FieldByName('ID').AsString));
              with aPos do
                begin
                  First;
                  while not EOF do
                    begin
                      bDataSet := TMasterdata.Create(nil);
                      bDataSet.Select(aPos.FieldByName('REF_ID').AsVariant);
                      bDataSet.Open;
                      if (bDataSet.Count>0) and (bDataSet.FieldByName('ACTIVE').AsString='Y') then
                        begin
                          FObject:=TBaseDbList(bDataSet);
                          Node1 := tvMain.Items.AddChildObject(nil,'',TTreeEntry.Create);
                          TTreeEntry(Node1.Data).Rec := FObject.GetBookmark;
                          Node1.HasChildren:=True;
                          with FObject.DataSet as IBaseManageDB do
                            TTreeEntry(Node1.Data).Filter:=Data.QuoteField(TableName)+'.'+Data.QuoteField('SQL_ID')+'='+Data.QuoteValue(FObject.Id.AsString);
                          TTreeEntry(Node1.Data).DataSourceType := TBaseDBDataSetClass(FObject.ClassType);
                          TTreeEntry(Node1.Data).Text[0] := FObject.Text.AsString+' ('+FObject.Number.AsString+')'+' ['+FObject.Status.AsString+']';
                          case FObject.ClassName of
                          'TMasterdata':TTreeEntry(Node1.Data).Typ := etArticle;
                          end;
                          Node1.HasChildren:=True;
                          if DoMove then
                            begin
                              Found := False;
                              Node1.Expanded:=True;
                              Node2 := tvMain.Items.GetFirstNode;
                              while Assigned(Node2) do
                                begin
                                  Node3 := Node2;
                                  Node2 := Node2.GetNextSibling;
                                  if Node3<>Node1 then
                                    begin
                                      Node4 := nil;
                                      Node4 := Node1.GetFirstChild;
                                      while Assigned(Node4) do
                                        begin
                                          if (TTreeEntry(Node4.data).Rec=TTreeEntry(Node3.Data).Rec)
                                          or (copy(TTreeEntry(Node3.data).Text[0],0,length(TTreeEntry(Node4.Data).Text[0]))=TTreeEntry(Node4.Data).Text[0])
                                          then
                                            begin
                                              Node3.MoveTo(Node4,naInsertBehind);
                                              Node3.Selected:=True;
                                              Found := True;
                                              Node4.Free;
                                              break;
                                            end;
                                          Node4 := Node4.GetNextSibling;
                                        end;
                                    end;
                                end;
                              DoMove := False;
                              if not Found then
                                begin
                                  Node1.Visible:=False;
                                  Application.RemoveAsyncCalls(Node1);
                                  fMainTreeFrame.FNode := Node1;
                                  Application.QueueAsyncCall(@fMainTreeFrame.DeleteNodeCall,PtrInt(@fMainTreeFrame.FNode));
                                  eFilterChange(eFilter);
                                end;
                            end;
                        end;
                      Next;
                    end;
                end;
              aPos.Free;
            end;
        end;
      aDataSet.Free;
    end;
end;

procedure TfObjectStructureFrame.eFilterChange(Sender: TObject);
var
  aItem: TTreeNode;
begin
  aItem := FTree.tvMain.Items.GetFirstNode;
  while Assigned(aItem) do
    begin
      DoFilterItems(FTree.tvMain.TopItem);
      aItem := aItem.GetNextSibling;
    end;
end;

function TfObjectStructureFrame.FTreeOpen(aEntry: TTreeEntry): Boolean;
var
  aDataSet: TBaseDBDataset;
begin
  case aEntry.Typ of
  etCustomer,etEmployee,etArticle,etArticleText,etArticleActivity,etProject,etProcess:
    begin
      if (aEntry.Link<>'') then
        fMainTreeFrame.OpenLink(aEntry.Link,Self)
      else
        begin
          aDataSet := aEntry.DataSourceType.CreateEx(Self,Data);
          with aDataSet.DataSet as IBaseDBFilter do
            Filter := aEntry.Filter;
          aDataSet.Open;
          if aDataSet.Count > 0 then
            fMainTreeFrame.OpenLink(Data.BuildLink(aDataSet.DataSet),Self);
          aDataSet.Free;
        end;
    end;
  end;
end;

procedure TfObjectStructureFrame.FTreetvMainExpanded(Sender: TObject;
  Node: TTreeNode);
begin
  eFilterChange(eFilter);
end;

procedure TfObjectStructureFrame.SetObject(AValue: TBaseDbList);
var
  Node1: TTreeNode;
begin
  if FObject=AValue then Exit;
  FObject:=AValue;
  FTree.tvMain.Items.Clear;
  with FTree do
    begin
      Node1 := tvMain.Items.AddChildObject(nil,'',TTreeEntry.Create);
      TTreeEntry(Node1.Data).Rec := FObject.GetBookmark;
      Node1.HasChildren:=True;
      with FObject.DataSet as IBaseManageDB do
        TTreeEntry(Node1.Data).Filter:=Data.QuoteField(TableName)+'.'+Data.QuoteField('SQL_ID')+'='+Data.QuoteValue(FObject.Id.AsString);
      TTreeEntry(Node1.Data).DataSourceType := TBaseDBDataSetClass(FObject.ClassType);
      case FObject.ClassName of
      'TProject':
        begin
          TTreeEntry(Node1.Data).Typ := etProject;
          TTreeEntry(Node1.Data).Text[0] := FObject.Text.AsString+' ('+FObject.Number.AsString+')'+' ['+FObject.Status.AsString+']';
        end;
      'TMasterdata':
        begin
          TTreeEntry(Node1.Data).Typ := etArticle;
          TTreeEntry(Node1.Data).Text[0] := FObject.Text.AsString+' ('+FObject.Number.AsString+')';
          if FObject.FieldByName('VERSION').AsString<>'' then
            TTreeEntry(Node1.Data).Text[0] := TTreeEntry(Node1.Data).Text[0] + ' ['+FObject.FieldByName('VERSION').AsString+']';
          TTreeEntry(Node1.Data).Rec:=FObject.Id.AsVariant;
        end;
      end;
      Node1.HasChildren:=True;
      FrootNode := Node1;
    end;
end;

procedure TfObjectStructureFrame.DoFilterItems(aNode: TTreeNode);
var
  aSubNode: TTreeNode;
begin
  try
    if not Assigned(aNode) then exit;
    aNode.Visible := aNode.HasChildren;
    if not aNode.Visible then
      begin
        aNode.Visible:=not Assigned(aNode.Data);
        if not aNode.Visible then
          aNode.Visible:=(trim(eFilter.Text)='') or (pos(lowercase(eFilter.Text),lowercase(TTreeEntry(aNode.Data).Text[0]))>0);
      end;
    aSubNode := aNode.GetFirstChild;
    while Assigned(aSubNode) do
      begin
        DoFilterItems(aSubNode);
        aSubNode := aSubNode.GetNextSibling;
      end;
  except
  end;
end;

constructor TfObjectStructureFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTree :=TfMainTree.Create(Self);
  Ftree.Parent := Self;
  FTree.Align := alClient;
  FTree.OnOpen:=@FTreeOpen;
  Caption:=strStructure;
  {$ifdef WINDOWS}
  FTree.tvMain.Font.Height := 18;
  {$ENDIF}
  FTree.tvMain.OnExpanded:=@FTreetvMainExpanded;
end;

destructor TfObjectStructureFrame.Destroy;
begin
  FTree.Free;
  inherited Destroy;
end;

end.

