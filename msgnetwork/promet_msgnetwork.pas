{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit promet_msgnetwork;

{$warn 5023 off : no warning about unused units}
interface

uses
  uprometmsgnetwork, uprometdataserver, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('promet_msgnetwork', @Register);
end.
