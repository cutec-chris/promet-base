unit uprometluascript;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, genluascript,uprometscripts;

type
  TPrometLuascript = class(TLuaScript)
  end;

implementation

initialization
  RegisterScriptType(TPrometLuascript);
end.

