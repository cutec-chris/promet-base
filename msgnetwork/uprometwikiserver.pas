unit uprometwikiserver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uappserverhttp, uWiki;

implementation

function HandleWikiRequest(Method, URL: string;Headers : TStringList;Input,Output : TStream): Integer;
begin
  {
  http://www.odata.org/
  /wiki/folder1/page2
  }
  Result := 500;
  if copy(lowercase(url),0,6)='/wiki/' then
    begin
      url := copy(url,7,length(url));

    end;
end;

initialization
  uappserverhttp.RegisterHTTPHandler(@HandleWikiRequest);
end.

