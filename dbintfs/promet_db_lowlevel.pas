{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit promet_db_lowlevel;

interface

uses
  uBaseDBInterface, uBaseSearch, uBaseDbClasses, uBaseERPDBClasses, uWiki, 
  uStatistic, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('promet_db_lowlevel', @Register);
end.
