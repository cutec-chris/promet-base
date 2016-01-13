unit uprometvispascalscript;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uprometpascalscript,uPSCompiler,LR_Class;

type
  TPrometVisPascalScript = class(TPrometPascalScript)
  private
    FReport: TfrReport;
  public
    constructor Create; override;
    destructor Destroy; override;
    function InternalUses(Comp: TPSPascalCompiler; Name: string): Boolean; override;
  end;

implementation

uses db,uData,uBaseDBInterface,uBaseDatasetInterfaces,Printers,uBaseApplication,
  uprometscripts;


constructor TPrometVisPascalScript.Create;
begin
  inherited Create;
  FReport := TfrReport.Create(nil);
end;

destructor TPrometVisPascalScript.Destroy;
begin
  FReport.Free;
  inherited Destroy;
end;

function TPrometVisPascalScript.InternalUses(Comp: TPSPascalCompiler;
  Name: string): Boolean;
begin
  Result:=inherited InternalUses(Comp, Name);
  if not Result then
    begin
      if lowercase(Name) = 'prometvisual' then
        begin
          Result:=True;
        end;
    end;
end;

initialization
end.

