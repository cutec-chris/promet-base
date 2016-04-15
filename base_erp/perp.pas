{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit perp;

{$warn 5023 off : no warning about unused units}
interface

uses
  utextpositionframe, uPositionFrame, ufinance, uautomationframe, 
  uDetailPositionFrame, ubookfibuaccount, ucalcframe, fautomationform, 
  uprometscriptprinting, uPosGotoArticle, uArticleFrame, 
  uArticlePositionFrame, uArticleStorageFrame, uArticleText, uCopyArticleData, 
  uarticlesupplierframe, uprojectoverview, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('perp', @Register);
end.
