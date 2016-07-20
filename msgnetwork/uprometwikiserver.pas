unit uprometwikiserver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uappserverhttp, uWiki, syncobjs,uAppServer;

implementation

uses uData;

var
  WikiList : TWikiList;
  CS : TCriticalSection;

function HandleWikiRequest(Sender : TAppNetworkThrd;Method, URL: string;Headers : TStringList;Input,Output : TMemoryStream): Integer;
var
  lOut: TStringList;
begin
  {
  http://www.odata.org/
  /wiki/folder1/page2
  }
  if (not Assigned(WikiList)) and Assigned(Data) then
    WikiList := TWikiList.Create(nil);
  Result := 500;
  if copy(lowercase(url),0,6)='/wiki/' then
    begin
      url := copy(url,7,length(url));
      CS.Enter;
      lOut := TStringList.Create;
      try
        if WikiList.FindWikiPage(url) then
          begin
            lOut.Text := WikiList.PageAsText;
            lOut.SaveToStream(Output);
            Result := 200;
          end;
        lOut.Free;
      finally
        CS.Leave;
      end;
    end;
end;

initialization
  uappserverhttp.RegisterHTTPHandler(@HandleWikiRequest);
  WikiList := nil;
  CS := TCriticalSection.Create;
finalization
  CS.Free;
end.

