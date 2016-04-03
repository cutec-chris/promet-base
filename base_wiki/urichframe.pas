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
Created 27.02.2014
*******************************************************************************}
unit urichframe;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Dialogs, Buttons, Spin,
  ActnList, FontComboBox,Graphics, ComCtrls, StdCtrls, ColorBox, kmemo;

type

  { TfRichFrame }

  TfRichFrame = class(TFrame)
    acBold: TAction;
    AcBorderHCenter: TAction;
    AcBorderLeft: TAction;
    AcBorderNone: TAction;
    AcBorderRight: TAction;
    AcBorderTop: TAction;
    AcFont: TAction;
    AcFontBold: TAction;
    AcFontItalic: TAction;
    AcFontStrikeout: TAction;
    AcFontUnderline: TAction;
    AcHorCenterAlign: TAction;
    AcHorDefaultAlign: TAction;
    acItalic: TAction;
    AcLeftAlign: TAction;
    AcNew: TAction;
    AcOpen: TAction;
    AcQuit: TAction;
    AcRightAlign: TAction;
    AcSaveAs: TAction;
    acStrikeOut: TAction;
    ActionList: TActionList;
    ActionList1: TActionList;
    acUnderline: TAction;
    AcVAlignBottom: TAction;
    AcVAlignCenter: TAction;
    AcVAlignDefault: TAction;
    AcVAlignTop: TAction;
    FontComboBox: TComboBox;
    FontDialog: TFontDialog;
    FontSizeComboBox: TComboBox;
    FormatToolBar: TToolBar;
    ImageList: TImageList;
    rmText: TKMemo;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton19: TToolButton;
    ToolButton21: TToolButton;
    ToolButton26: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    procedure rmTextExit(Sender: TObject);
    function GetPlainText: string;
    function GetReadOnly: Boolean;
    function GetRichText: string;
    procedure rmTextClick(Sender: TObject);
    procedure rmTextKeyPress(Sender: TObject; var Key: char);
  private
    OldSelStart : Integer;
    FRichText : string;
    procedure SetPlainText(const AValue: string);
    procedure SetReadOnly(const AValue: Boolean);
    procedure SetRichText(const AValue: string);
    { private declarations }
  public
    { public declarations }
    constructor Create(AOwner : TComponent);override;
    property AsText : string read GetPlainText write SetPlainText;
    property AsRichText : string read GetRichText write SetRichText;
    property ReadOnly : Boolean read GetReadOnly write SetReadOnly;
    procedure LoadFromStream(Stream : TStream);
    procedure SaveToStream(Stream : TStream);
    procedure Clear;
  end;

implementation

{$R *.lfm}

procedure TfRichFrame.rmTextClick(Sender: TObject);
begin
//  if rmText.SelStart <> OldSelStart then
//    PositionChanged;
end;

procedure TfRichFrame.rmTextExit(Sender: TObject);
var
  ss: TStringStream;
begin
  ss := TStringStream.Create('');
  rmText.SaveToRTFStream(ss);
  ss.Position:=0;
  FRichText := ss.DataString;
  ss.Free;
  {$IFDEF WINDOWS}
  FRichText := copy(FRichText,0,length(FRichText)-2);
  {$ENDIF}
end;

procedure TfRichFrame.rmTextKeyPress(Sender: TObject; var Key: char);
begin
//  if rmText.SelStart <> OldSelStart then
//    PositionChanged;
  rmText.Invalidate;
end;

function TfRichFrame.GetPlainText: string;
var
  tmp: String;
begin
  tmp := rmText.Blocks.Text;
  Result := tmp;
end;

procedure TfRichFrame.SetReadOnly(const AValue: Boolean);
begin
  rmText.ReadOnly:=AValue;
  acItalic.Enabled:=not AValue;
  acUnderline.Enabled:=not AValue;
  acBold.Enabled:=not AValue;
  acStrikeOut.Enabled:=not AValue;
end;

function TfRichFrame.GetReadOnly: Boolean;
begin
  Result := rmText.ReadOnly;
end;

function TfRichFrame.GetRichText: string;
begin
  Result := FRichText;
end;

procedure TfRichFrame.SetPlainText(const AValue: string);
begin
  rmText.Blocks.Text := AValue;
  rmText.SelStart:=0;
  rmText.SelLength:=0;
  //PositionChanged;
  Application.ProcessMessages;
end;

procedure TfRichFrame.SetRichText(const AValue: string);
var
  ss: TStringStream;
begin
  ss := TStringStream.Create(AValue);
  rmText.Clear;
  rmText.LoadFromRTFStream(ss);
  ss.Free;
  rmTextExit(rmText);
end;

constructor TfRichFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRichText := '';
  FontCombobox.Items.Assign(Screen.Fonts);
end;

procedure TfRichFrame.LoadFromStream(Stream: TStream);
begin
  rmText.LoadFromRTFStream(Stream);
end;

procedure TfRichFrame.SaveToStream(Stream: TStream);
begin
  rmText.SaveToRTFStream(Stream);
end;

procedure TfRichFrame.Clear;
begin
  rmText.Clear;
end;

end.

