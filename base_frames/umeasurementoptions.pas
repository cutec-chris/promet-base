{*******************************************************************************
  Copyright (C) Christian Ulrich info@cu-tec.de

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or commercial alternative
  contact us for more information

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
*******************************************************************************}
unit uMeasurementOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, FileUtil, Forms, Controls, Graphics, Dialogs, DbCtrls,
  ColorBox, StdCtrls, Spin, ButtonPanel;

type

  { TfMeasurementOptions }

  TfMeasurementOptions = class(TForm)
    ButtonPanel1: TButtonPanel;
    ColorButton1: TColorButton;
    FloatSpinEdit1: TFloatSpinEdit;
    Label4: TLabel;
    Measurement: TDatasource;
    DBCheckBox1: TDBCheckBox;
    DBCheckBox2: TDBCheckBox;
    DBComboBox1: TDBComboBox;
    DBEdit1: TDBEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure ColorButton1ColorChanged(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    function Execute: Boolean;
    procedure SetLanguage;
  end;

var
  fMeasurementOptions: TfMeasurementOptions;

implementation

{$R *.lfm}

{ TfMeasurementOptions }

procedure TfMeasurementOptions.ColorButton1ColorChanged(Sender: TObject);
begin
  if not (Measurement.DataSet.State=dsEdit) then
    Measurement.DataSet.Edit;
  Measurement.DataSet.FieldByName('COLOR').AsString:=ColorToString(ColorButton1.Color);
end;

function TfMeasurementOptions.Execute: Boolean;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfMeasurementOptions,fMeasurementOptions);
      Self := fMeasurementOptions;
    end;
  FloatSpinEdit1.Value:=Measurement.DataSet.FieldByName('TOLLERANCE').AsFloat;
  Result := Showmodal=mrOK;
  if Result then
    begin
      if (Measurement.DataSet.State=dsEdit)
      or (Measurement.DataSet.State=dsInsert) then else
        Measurement.DataSet.Edit;
      Measurement.DataSet.FieldByName('TOLLERANCE').AsFloat:=FloatSpinEdit1.Value;
    end;
end;

procedure TfMeasurementOptions.SetLanguage;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfMeasurementOptions,fMeasurementOptions);
      Self := fMeasurementOptions;
    end;
end;

end.

