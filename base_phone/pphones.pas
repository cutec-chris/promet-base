{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pPhones;

{$warn 5023 off : no warning about unused units}
interface

uses
  uPhone, uPhones, uSkypePhone, uTAPIPhone, uSkype, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('pPhones', @Register);
end.
