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
  ActnList, RichMemo,FontComboBox,Graphics, ComCtrls, StdCtrls, ColorBox;

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
    rmText: TRichMemo;
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
    procedure AcFontBoldExecute(Sender: TObject);
    procedure AcFontExecute(Sender: TObject);
    procedure FontComboBoxSelect(Sender: TObject);
    procedure FontSizeComboBoxSelect(Sender: TObject);
    procedure rmTextExit(Sender: TObject);
    procedure cbFontChange(Sender: TObject);
    function GetPlainText: string;
    function GetReadOnly: Boolean;
    function GetRichText: string;
    procedure PositionChanged;
    procedure rmTextClick(Sender: TObject);
    procedure rmTextKeyPress(Sender: TObject; var Key: char);
  private
    actAttributes: TFontParams;
    actAlignment: TParaAlignment;
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
  if rmText.SelStart <> OldSelStart then
    PositionChanged;
end;

procedure TfRichFrame.rmTextExit(Sender: TObject);
var
  ss: TStringStream;
begin
  ss := TStringStream.Create('');
  rmText.SaveRichText(ss);
  ss.Position:=0;
  FRichText := ss.DataString;
  ss.Free;
  {$IFDEF WINDOWS}
  FRichText := copy(FRichText,0,length(FRichText)-2);
  {$ENDIF}
end;

procedure TfRichFrame.AcFontBoldExecute(Sender: TObject);
begin
  if AcFontBold.Checked then Include(actAttributes.Style, fsBold) else Exclude(actAttributes.Style, fsBold);
  if AcFontItalic.Checked then Include(actAttributes.Style, fsItalic) else Exclude(actAttributes.Style, fsItalic);
  if AcFontStrikeout.Checked then Include(actAttributes.Style, fsStrikeout) else Exclude(actAttributes.Style, fsStrikeOut);
  if AcFontUnderline.Checked then Include(actAttributes.Style, fsUnderline) else Exclude(actAttributes.Style, fsUnderline);
  if AcLeftAlign.Checked then actAlignment := paLeft;
  if AcRightAlign.Checked then actAlignment := paRight;
  if AcHorCenterAlign.Checked then actAlignment := paCenter;
  rmText.SetParaAlignment(rmText.SelStart,rmText.SelLength,actAlignment);
  rmText.SetTextAttributes(rmText.SelStart,rmText.SelLength,actAttributes);
end;

procedure TfRichFrame.AcFontExecute(Sender: TObject);
begin
  FontDialog.Font.Name:=actAttributes.Name;
  FontDialog.Font.Style:=actAttributes.Style;
  FontDialog.Font.Size:=actAttributes.Size;
  if FontDialog.Execute then
    begin
      actAttributes.Name:=FontDialog.Font.Name;
      actAttributes.Size:=FontDialog.Font.Size;
      actAttributes.Style:=FontDialog.Font.Style;
      rmText.SetTextAttributes(rmText.SelStart,rmText.SelLength,actAttributes);
    end;
end;

procedure TfRichFrame.FontComboBoxSelect(Sender: TObject);
var
  aname: String;
begin
  aname := FontCombobox.Items[FontCombobox.ItemIndex];
  if aname <> '' then
    begin
      actAttributes.Name:=aname;
      rmText.SetTextAttributes(rmText.SelStart,rmText.SelLength,actAttributes);
    end;
end;

procedure TfRichFrame.FontSizeComboBoxSelect(Sender: TObject);
var
  aname: String;
begin
  aname := FontSizeComboBox.Items[FontSizeComboBox.ItemIndex];
  if aname <> '' then
    begin
      actAttributes.Size:=StrToInt(aname);
      rmText.SetTextAttributes(rmText.SelStart,rmText.SelLength,actAttributes);
    end;
end;

procedure TfRichFrame.cbFontChange(Sender: TObject);
begin
  actAttributes.Name := FontComboBox.Text;
  rmText.SetTextAttributes(rmText.SelStart,rmText.SelLength,actAttributes);
  rmText.SetFocus;
end;

procedure TfRichFrame.rmTextKeyPress(Sender: TObject; var Key: char);
begin
  if rmText.SelStart <> OldSelStart then
    PositionChanged;
  rmText.Invalidate;
end;

function TfRichFrame.GetPlainText: string;
var
  tmp: String;
begin
  tmp := rmText.Lines.Text;
  Result := tmp;
end;

procedure TfRichFrame.PositionChanged;
begin
  rmText.GetTextAttributes(rmText.SelStart, actAttributes);
  actAlignment := rmText.GetParaAlignment(rmText.SelStart);
  acFontBold.Checked := fsBold in actAttributes.Style;
  AcFontItalic.Checked := fsItalic in actAttributes.Style;
  AcFontUnderline.Checked := fsUnderline in actAttributes.Style;
  AcFontStrikeout.Checked := fsStrikeOut in actAttributes.Style;
  AcLeftAlign.Checked:=actAlignment=paLeft;
  AcRightAlign.Checked:=actAlignment=paRight;
  AcHorCenterAlign.Checked:=actAlignment=paCenter;
  OldSelStart := rmText.SelStart;
  FontSizeComboBox.Text := IntToStr(actAttributes.Size);
  FontComboBox.Text:=ActAttributes.Name;
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
  rmText.Lines.Text := AValue;
  rmText.SelStart:=0;
  rmText.SelLength:=0;
  PositionChanged;
  Application.ProcessMessages;
end;

procedure TfRichFrame.SetRichText(const AValue: string);
var
  ss: TStringStream;
begin
  ss := TStringStream.Create(AValue);
  rmText.Clear;
  rmText.LoadRichText(ss);
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
  rmText.LoadRichText(Stream);
end;

procedure TfRichFrame.SaveToStream(Stream: TStream);
begin
  rmText.SaveRichText(Stream);
end;

procedure TfRichFrame.Clear;
begin
  rmText.Clear;
end;

end.

