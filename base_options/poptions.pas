{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pOptions;

interface

uses
  utableoptions, uOptions, uOptionsFrame, uProcessOptions, uuseroptions, 
  uSyncOptions, uvisualoptions, uimportoptions, upaygroups, uScriptOptions, 
  uUserfieldDefOptions, uMandantOptions, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('pOptions', @Register);
end.
