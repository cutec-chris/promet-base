{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit perp;

interface

uses
  fautomationform, uArticleFrame, uArticlePositionFrame, uArticleStorageFrame, 
  uarticlesupplierframe, uArticleText, uautomationframe, ubookfibuaccount, 
  ucalcframe, uCopyArticleData, uDetailPositionFrame, ufinance, 
  uPosGotoArticle, uPositionFrame, uprojectoverview, uprometscriptprinting, 
  utextpositionframe, unumbersetempty, uCreateProductionOrder, uChangeStatus, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('perp', @Register);
end.
