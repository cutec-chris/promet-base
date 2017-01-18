unit uImageCache;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CacheCls;
type
  TGetFileEvent = function(Path : string;var NewPath : string;var ExpireDate : TDateTime = 0) : TStream of object;

  { TFileCache }

  TFileCache = class(TCache)
    procedure FileCacheFreeSlot(ACache: TCache; SlotIndex: Integer);
  private
    FGetFile: TGetFileEvent;
    FURLList: TStringList;
    FNewURLList: TStringList;
    FExpires : TStringList;
    function GetStream(Path: string; var NewPath: string; var aExpireDate: TDateTime
      ): TStream;
  public
    constructor Create(ASlotCount: Integer);
    destructor Destroy; override;
    function GetFile(Path : string;var NewPath : string) : TMemoryStream;
    property OnGetFile : TGetFileEvent read FGetFile write FGetFile;
  end;
implementation

procedure TFileCache.FileCacheFreeSlot(ACache: TCache; SlotIndex: Integer);
begin
//  TMemoryStream(ACache.Slots[SlotIndex]).Free;
end;
function TFileCache.GetStream(Path: string; var NewPath: string; var aExpireDate : TDateTime): TStream;
begin
  Result := nil;
  if Assigned(FGetFile) then
    Result := FGetFile(path,newpath,aExpireDate);
end;
constructor TFileCache.Create(ASlotCount: Integer);
begin
  inherited Create(ASlotCount);
  FURLList := TStringList.Create;
  FNewURLList := TStringList.Create;
  FExpires := TStringList.Create;
  Self.OnFreeSlot:=@FileCacheFreeSlot;
end;
destructor TFileCache.Destroy;
var
  i: Integer;
begin
  for i := 0 to FURLList.Count-1 do
    FURLList.Objects[i].Free;
  FURLList.Destroy;
  FNewURLList.Destroy;
  FExpires.Destroy;
  inherited Destroy;
end;
function TFileCache.GetFile(Path: string;var NewPath : string): TMemoryStream;
var
  aMem: TMemoryStream;
  aFile: TStream;
  aIdx: Integer;
  aExpireDate: TDateTime;
begin
  NewPath:=Path;
  Result := nil;
  aIdx := FURLList.IndexOf(Path);
  if (aIdx = -1) or (StrToDateTimeDef(FExpires[FExpires.IndexOfObject(FURLList.Objects[aIdx])],0)>Now()) then
    begin
      aFile := GetStream(Path,NewPath,aExpireDate);
      if Assigned(aFile) then
        begin
          aMem := TMemoryStream.Create;
          try
            aMem.CopyFrom(aFile,aFile.Size);
            FUrlList.AddObject(Path,aMem);
            FNewURLList.Add(NewPath);
            FExpires.AddObject(DateTimeToStr(aExpireDate),aMem);
//            Add(aMem);
            Result := aMem;
          except
            Result := nil;
          end;
          aFile.Free;
        end;
      while FUrlList.Count > Self.SlotCount do
        FUrlList.Delete(FUrlList.Count-1);
    end
  else
    begin
      NewPath:=FNewURLList[aIdx];
      FNewUrlList.Move(aIdx,0);
      Result := TmemoryStream(FURLList.Objects[aIdx]);//TMemoryStream(Self.Data[Self.FindSlot()^.Index]);
      FUrlList.Move(aIdx,0);
    end;
end;
end.

