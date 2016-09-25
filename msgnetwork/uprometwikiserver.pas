unit uprometwikiserver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uappserverhttp, uWiki, syncobjs,uAppServer;

implementation

uses uData;

type

  { TWikiSession }

  TWikiSession = class
  private
    FSocket: TAppNetworkThrd;
    procedure CreateWikiList;
    procedure DestroyWikiList;
    procedure SetSocket(AValue: TAppNetworkThrd);
  public
    Url : string;
    Code : Integer;
    Result : string;
    WikiList: TWikiList;
    property Socket : TAppNetworkThrd read FSocket write SetSocket;

    constructor Create;
    destructor Destroy; override;
    procedure ProcessWikiRequest;
  end;

function HandleWikiRequest(Sender : TAppNetworkThrd;Method, URL: string;Headers : TStringList;Input,Output : TMemoryStream): Integer;
var
  lOut, aParameters: TStringList;
  i: Integer;
  aSock: TWikiSession = nil;
begin
  {
  http://www.odata.org/
  /wiki/folder1/page2
  }
  for i := 0 to Sender.Objects.Count-1 do
    if TObject(Sender.Objects[i]) is TWikiSession then
      aSock := TWikiSession(Sender.Objects[i]);
  if not Assigned(aSock) then
    begin
      aParameters := TStringList.Create;
      aParameters.NameValueSeparator:=':';
      aParameters.CaseSensitive:=False;
      aSock := TWikiSession.Create;
      aSock.Socket := Sender;
      Sender.Objects.Add(aSock);
    end;
  Result := 500;
  if copy(lowercase(url),0,6)='/wiki/' then
    begin
      aSock.Url := copy(url,7,length(url));
      Sender.Synchronize(Sender,@aSock.ProcessWikiRequest);
      Result := aSock.Code;
      lOut := TStringList.Create;
      lOut.Text:=aSock.Result;
      lOut.SaveToStream(Output);
      lOut.Free;
      Headers.Clear;
      Headers.Add('Content-Type: '+ 'text/html');
    end;
end;

{ TWikiSession }

procedure TWikiSession.CreateWikiList;
begin
  WikiList := TWikiList.Create(nil);
end;

procedure TWikiSession.DestroyWikiList;
begin
  WikiList.Free;
end;

procedure TWikiSession.SetSocket(AValue: TAppNetworkThrd);
begin
  if FSocket=AValue then Exit;
  FSocket:=AValue;
  FSocket.Synchronize(FSocket,@CreateWikiList);
end;

constructor TWikiSession.Create;
begin
  Code := 500;
end;

destructor TWikiSession.Destroy;
begin
  FSocket.Synchronize(FSocket,@DestroyWikiList);
  inherited Destroy;
end;

procedure TWikiSession.ProcessWikiRequest;
begin
  Code:=404;
  if WikiList.FindWikiPage(Url) then
    begin
      Result := WikiList.PageAsHtml;
      Code := 200;
    end;
end;

initialization
  uappserverhttp.RegisterHTTPHandler(@HandleWikiRequest);
finalization
end.

