unit uCreateProductionOrder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ButtonPanel,uOrder,uMasterdata,variants;

type

  { TfCreateProductionOrder }

  TfCreateProductionOrder = class(TForm)
    ButtonPanel1: TButtonPanel;
    eVersion: TComboBox;
    eMasterdata: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
  private
    { private declarations }
  public
    { public declarations }
    function Execute(FOrder: TOrder; aID: string; aVersion: Variant): Boolean;
  end;

var
  fCreateProductionOrder: TfCreateProductionOrder;

implementation

{ TfCreateProductionOrder }

function TfCreateProductionOrder.Execute(FOrder: TOrder; aID: string;
  aVersion: Variant): Boolean;
var
  aMasterdata: TMasterdata;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfCreateProductionOrder,fCreateProductionOrder);
      Self := fCreateProductionOrder;
    end;
  Result := ShowModal=mrOK;
  if Result then
    begin
      //Find Article and Create Order
      aMasterdata := TMasterdata.Create(nil);
      aMasterdata.SelectFromNumber(aID);
      aMasterdata.Open;
      aMasterdata.Locate('VERSION',aVersion,[]);
      if aMasterdata.Count>0 then
        begin
          FOrder.OrderType.Open;
          if FOrder.OrderType.Locate('SI_PROD;TYPE',VarArrayOf(['Y',7]),[]) then
            begin
              FOrder.Insert;
              FOrder.Positions.Insert;
              //FOrder.Status.AsString:=FOrder.OrderType.FieldByName('STATUS').AsString;
              FOrder.Positions.Assign(aMasterdata);
              FOrder.Positions.Post;
              FOrder.Post;
            end;
        end;
      aMasterdata.Free;
    end;
end;

initialization
  {$I uCreateProductionOrder.lrs}

end.

