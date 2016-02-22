unit uEditorToolbar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, ActnList, kmemo,
  Graphics, Dialogs, Menus, ExtDlgs, kgraphics, keditcommon, kfunctions,
  kmessagebox, kres, KDialogs, KMemoDlgParaStyle, KMemoDlgTextStyle,
  KMemoDlgHyperlink, KMemoDlgNumbering;

type
  { TfEditorToolbar }

  TfEditorToolbar = class(TFrame)
    ACEditHyperlink: TAction;
    ACEditSelectAll: TKMemoEditSelectAllAction;
    ACFontBold: TAction;
    ACFontItalic: TAction;
    ACFontStrikeout: TAction;
    ACFontStyle: TAction;
    ACFontSubscript: TAction;
    ACFontSuperscript: TAction;
    ACFontUnderline: TAction;
    ACFormatCopy: TAction;
    ACInsertHyperlink: TAction;
    ACParaCenter: TAction;
    ACParaDecIndent: TAction;
    ACParaIncIndent: TAction;
    ACParaLeft: TAction;
    ACParaNumbering: TAction;
    ACParaRight: TAction;
    ACParaStyle: TAction;
    ACShowFormatting: TAction;
    ACInsertImage: TAction;
    acCopy: TAction;
    acPaste: TAction;
    ALMain: TActionList;
    ILMain: TImageList;
    MIEditCopy: TMenuItem;
    MIEditHyperlink: TMenuItem;
    MIEditPaste: TMenuItem;
    MIFontStyle: TMenuItem;
    MIParaStyle: TMenuItem;
    N1: TMenuItem;
    N3: TMenuItem;
    ODMain: TOpenDialog;
    OpenPictureDialog1: TOpenPictureDialog;
    PMMain: TPopupMenu;
    SDMain: TSaveDialog;
    ToBCopy: TToolButton;
    ToBFont: TToolButton;
    ToBFontBold: TToolButton;
    ToBFontItalic: TToolButton;
    ToBFontSubscript: TToolButton;
    ToBFontSuperscript: TToolButton;
    ToBFontUnderline: TToolButton;
    ToBFormatCopy: TToolButton;
    ToBPara: TToolButton;
    ToBParaCenter: TToolButton;
    ToBParaDecIndent: TToolButton;
    ToBParaIncIndent: TToolButton;
    ToBParaLeft: TToolButton;
    ToBParaNumbering: TToolButton;
    ToBParaRight: TToolButton;
    ToBPaste: TToolButton;
    ToBSecond: TToolBar;
    ToBShowFormatting: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    procedure acCopyExecute(Sender: TObject);
    procedure ACFontBoldExecute(Sender: TObject);
    procedure ACFontItalicExecute(Sender: TObject);
    procedure ACFontUnderlineExecute(Sender: TObject);
    procedure ACFontStrikeoutExecute(Sender: TObject);
    procedure ACFontStyleExecute(Sender: TObject);
    procedure ACInsertImageExecute(Sender: TObject);
    procedure ACParaLeftExecute(Sender: TObject);
    procedure ACParaCenterExecute(Sender: TObject);
    procedure ACParaRightExecute(Sender: TObject);
    procedure ACParaIncIndentExecute(Sender: TObject);
    procedure ACParaDecIndentExecute(Sender: TObject);
    procedure ACParaStyleExecute(Sender: TObject);
    procedure ACFormatCopyExecute(Sender: TObject);
    procedure acPasteExecute(Sender: TObject);
    procedure EditorMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure EditorDropFiles(Sender: TObject; X, Y: Integer; Files: TStrings);
    procedure ACShowFormattingExecute(Sender: TObject);
    procedure ACInsertHyperlinkExecute(Sender: TObject);
    procedure EditorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ACParaNumberingExecute(Sender: TObject);
    procedure ACFontSuperscriptExecute(Sender: TObject);
    procedure ACFontSubscriptExecute(Sender: TObject);
    procedure ACFontStyleUpdate(Sender: TObject);
  private
    FNewFile: Boolean;
    FEditor: TKMemo;
    FLastFileName: TKString;
    { private declarations }
    FFormatCopyParaStyle: TKMemoParaStyle;
    FFormatCopyTextStyle: TKMemoTextStyle;
    FHyperlinkForm: TKMemoHyperlinkForm;
    FNumberingForm: TKMemoNumberingForm;
    FParaStyle: TKMemoParaStyle;
    FParaStyleForm: TKMemoParaStyleForm;
    FTextStyle: TKMemoTextStyle;
    FTextStyleForm: TKMemoTextStyleForm;
    procedure UpdatellActions;
    procedure OpenFile(FileName: TKString);
    procedure OpenNewFile;
    procedure CloseFile;
    procedure ParaStyleChanged(Sender: TObject; AReasons: TKMemoUpdateReasons);
    function SaveFile(SaveAs, NeedAnotherOp: Boolean): Boolean;
    procedure SetEditor(AValue: TKMemo);
    procedure TextStyleChanged(Sender: TObject);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property Editor : TKMemo read FEditor write SetEditor;
  end;

implementation

uses Math;

{$R *.lfm}

{ TfEditorToolbar }

constructor TfEditorToolbar.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FEditor:=nil;
  FFormatCopyParaStyle := TKMemoParaStyle.Create;
  FFormatCopyTextStyle := TKMemoTextStyle.Create;
  FHyperlinkForm := TKMemoHyperlinkForm.Create(Self);
  FNumberingForm := TKMemoNumberingForm.Create(Self);
  FParaStyle := TKMemoParaStyle.Create;
  FParaStyle.OnChanged:=@ParaStyleChanged;
  FParaStyleForm := TKMemoParaStyleForm.Create(Self);
  FTextStyle := TKMemoTextStyle.Create;
  FTextStyle.OnChanged:=@TextStyleChanged;
  FTextStyleForm := TKMemoTextStyleForm.Create(Self);
end;

destructor TfEditorToolbar.Destroy;
begin
  SetEditor(nil);
  FParaStyle.OnChanged:=nil;
  FTextStyle.OnChanged:=nil;
  inherited Destroy;
end;

procedure TfEditorToolbar.ACFontBoldExecute(Sender: TObject);
begin
  if not Assigned(FEditor) then exit;
  if TAction(Sender).Checked then
    FTextStyle.Font.Style := FTextStyle.Font.Style - [fsBold]
  else
    FTextStyle.Font.Style := FTextStyle.Font.Style + [fsBold];
end;

procedure TfEditorToolbar.acCopyExecute(Sender: TObject);
begin
  if not Assigned(FEditor) then exit;
  FEditor.ExecuteCommand(ecCopy);
end;

procedure TfEditorToolbar.ACFontItalicExecute(Sender: TObject);
begin
  if not Assigned(FEditor) then exit;
  if TAction(Sender).Checked then
    FTextStyle.Font.Style := FTextStyle.Font.Style - [fsItalic]
  else
    FTextStyle.Font.Style := FTextStyle.Font.Style + [fsItalic];
end;

procedure TfEditorToolbar.ACFontStrikeoutExecute(Sender: TObject);
begin
  if not Assigned(FEditor) then exit;
  if TAction(Sender).Checked then
    FTextStyle.Font.Style := FTextStyle.Font.Style - [fsStrikeout]
  else
    FTextStyle.Font.Style := FTextStyle.Font.Style + [fsStrikeout];
end;

procedure TfEditorToolbar.ACFontStyleExecute(Sender: TObject);
begin
  if not Assigned(FEditor) then exit;
  FTextStyleForm.Load(FTextStyle);
  if FTextStyleForm.ShowModal = mrOk then
    FTextStyleForm.Save(FTextStyle);
end;

procedure TfEditorToolbar.ACInsertImageExecute(Sender: TObject);
begin
  if not Assigned(FEditor) then exit;
  if OpenPictureDialog1.Execute then
    begin
      Editor.Blocks.AddImageBlock(OpenPictureDialog1.FileName);
    end;
end;

procedure TfEditorToolbar.ACFontStyleUpdate(Sender: TObject);
begin
  if not Assigned(FEditor) then exit;
  FTextStyle.OnChanged := nil;
  if not Assigned(FEditor) then exit;
  try
    if Editor.NewTextStyleValid then
      FTextStyle.Assign(Editor.NewTextStyle)
    else
      FTextStyle.Assign(Editor.SelectionTextStyle);
    UpdatellActions;
  finally
    FTextStyle.OnChanged:=@TextStyleChanged;
  end;
end;

procedure TfEditorToolbar.ACFontSubscriptExecute(Sender: TObject);
begin
  if not Assigned(FEditor) then exit;
  if TAction(Sender).Checked then
    FTextStyle.ScriptPosition := tpoNormal
  else
    FTextStyle.ScriptPosition := tpoSubscript;
end;

procedure TfEditorToolbar.UpdatellActions;
begin
  if not Assigned(FEditor) then exit;
  ACFontBold.Checked := fsBold in FTextStyle.Font.Style;
  ACFontItalic.Checked := fsItalic in FTextStyle.Font.Style;
  ACFontStrikeout.Checked := fsStrikeout in FTextStyle.Font.Style;
  ACFontSuperscript.Checked := FTextStyle.ScriptPosition = tpoSuperScript;
  ACFontSubscript.Checked := FTextStyle.ScriptPosition = tpoSubScript;
  ACFontUnderline.Checked := fsUnderline in FTextStyle.Font.Style;
  ACShowFormatting.Checked := eoShowFormatting in Editor.Options;
  ACParaCenter.Checked := FParaStyle.HAlign = halCenter;
  //TAction(Sender).Visible := Editor.ActiveInnerBlock is TKMemoHyperlink;
  ACParaLeft.Checked := FParaStyle.HAlign = halLeft;
  //TAction(Sender).Enabled := Editor.NearestParagraph <> nil;
  ACParaRight.Checked := FParaStyle.HAlign = halRight;
  FParaStyle.OnChanged := nil;
  try
    FParaStyle.Assign(Editor.SelectionParaStyle);
  finally
    FParaStyle.OnChanged := @ParaStyleChanged;
  end;
  //TAction(Sender).Enabled := FParaStyle.LeftPadding > 0;
  //TAction(Sender).Enabled := FParaStyle.LeftPadding < Editor.RequiredContentWidth - FParaStyle.RightPadding - 20;
end;

procedure TfEditorToolbar.ACFontSuperscriptExecute(Sender: TObject);
begin
  if not Assigned(FEditor) then exit;
  if TAction(Sender).Checked then
    FTextStyle.ScriptPosition := tpoNormal
  else
    FTextStyle.ScriptPosition := tpoSuperscript;
end;

procedure TfEditorToolbar.ACFontUnderlineExecute(Sender: TObject);
begin
  if not Assigned(FEditor) then exit;
  if TAction(Sender).Checked then
    FTextStyle.Font.Style := FTextStyle.Font.Style - [fsUnderline]
  else
    FTextStyle.Font.Style := FTextStyle.Font.Style + [fsUnderline];
end;

procedure TfEditorToolbar.ACFormatCopyExecute(Sender: TObject);
begin
  if not Assigned(FEditor) then exit;
  FFormatCopyParaStyle.Assign(FParaStyle);
  FFormatCopyTextStyle.Assign(FTextStyle);
  TAction(Sender).Checked := True;
end;

procedure TfEditorToolbar.acPasteExecute(Sender: TObject);
begin
  if not Assigned(FEditor) then exit;
  FEditor.ExecuteCommand(ecPaste);
end;

procedure TfEditorToolbar.ACInsertHyperlinkExecute(Sender: TObject);
var
  Item: TKMemoBlock;
  Hyperlink: TKMemoHyperlink;
  Created: Boolean;
begin
  if not Assigned(FEditor) then exit;
  Created := False;
  if Editor.SelAvail then
  begin
    Hyperlink := TKMemoHyperlink.Create;
    Hyperlink.Text := Editor.SelText;
    Item := Editor.ActiveInnerBlock;
    if Item is TKMemoHyperlink then
      Hyperlink.URL := TKMemoHyperlink(Item).URL;
    Created := True;
  end else
  begin
    Item := Editor.ActiveInnerBlock;
    if Item is TKMemoHyperlink then
      Hyperlink := TKMemoHyperlink(Item)
    else
    begin
      Hyperlink := TKMemoHyperlink.Create;
      Created := True;
    end;
  end;
  FHyperlinkForm.Load(Hyperlink);
  if FHyperlinkForm.ShowModal = mrOk then
  begin
    FHyperlinkForm.Save(Hyperlink);
    if Created then
    begin
      if Editor.SelAvail then
        Editor.ClearSelection;
      Editor.ActiveInnerBlocks.AddHyperlink(Hyperlink, Editor.SplitAt(Editor.SelEnd));
    end;
    Editor.Modified := True;
  end
  else if Created then
    Hyperlink.Free;
end;

procedure TfEditorToolbar.ACParaCenterExecute(Sender: TObject);
begin
  if not Assigned(FEditor) then exit;
  FParaStyle.HAlign := halCenter;
end;

procedure TfEditorToolbar.ACParaDecIndentExecute(Sender: TObject);
begin
  if not Assigned(FEditor) then exit;
  FParaStyle.LeftPadding := Max(FParaStyle.LeftPadding - 20, 0);
end;

procedure TfEditorToolbar.ACParaIncIndentExecute(Sender: TObject);
begin
  if not Assigned(FEditor) then exit;
  FParaStyle.LeftPadding := Min(FParaStyle.LeftPadding + 20, Editor.RequiredContentWidth - FParaStyle.RightPadding - 20);
end;

procedure TfEditorToolbar.ACParaLeftExecute(Sender: TObject);
begin
  if not Assigned(FEditor) then exit;
  FParaStyle.HAlign := halLeft;
end;

procedure TfEditorToolbar.ACParaNumberingExecute(Sender: TObject);
begin
  if not Assigned(FEditor) then exit;
  FNumberingForm.Load(Editor.ListTable, Editor.NearestParagraph);
  if FNumberingForm.ShowModal = mrOk then
    FNumberingForm.Save;
end;

procedure TfEditorToolbar.ACParaRightExecute(Sender: TObject);
begin
  if not Assigned(FEditor) then exit;
  FParaStyle.HAlign := halRight;
end;

procedure TfEditorToolbar.ACParaStyleExecute(Sender: TObject);
begin
  if not Assigned(FEditor) then exit;
  FParaStyleForm.Load(FParaStyle);
  if FParaStyleForm.ShowModal = mrOk then
    FParaStyleForm.Save(FParaStyle);
end;

procedure TfEditorToolbar.ACShowFormattingExecute(Sender: TObject);
begin
  if not Assigned(FEditor) then exit;
  if ACShowFormatting.Checked then
    Editor.Options := Editor.Options - [eoShowFormatting]
  else
    Editor.Options := Editor.Options + [eoShowFormatting];
end;

procedure TfEditorToolbar.EditorDropFiles(Sender: TObject; X, Y: Integer;
  Files: TStrings);
begin
  if not Assigned(FEditor) then exit;
  if Files.Count > 0 then
  begin
    Application.BringToFront;
    OpenFile(Files[0]);
  end;
end;

procedure TfEditorToolbar.EditorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not Assigned(FEditor) then exit;
  if Button = mbRight then
    Editor.MoveCaretToMouseCursor(True);
end;

procedure TfEditorToolbar.EditorMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if not Assigned(FEditor) then exit;
  if ACFormatCopy.Checked then
  begin
    if Editor.SelAvail then
    begin
      Editor.SelectionTextStyle := FFormatCopyTextStyle;
      if Editor.SelectionHasPara then
        Editor.SelectionParaStyle := FFormatCopyParaStyle;
    end;
    ACFormatCopy.Checked := False;
  end;
end;

procedure TfEditorToolbar.OpenNewFile;
begin
  if not Assigned(FEditor) then exit;
  if SaveFile(False, True) then
  begin
    CloseFile;
    FNewFile := True;
  end;
end;

procedure TfEditorToolbar.CloseFile;
begin

end;

procedure TfEditorToolbar.OpenFile(FileName: TKString);
begin
  if not Assigned(FEditor) then exit;
  if SaveFile(False, True) then
  begin
    if FileName = '' then
      if ODMain.Execute then
        FileName := ODMain.FileName;
    if FileName <> '' then
    begin
      CloseFile;
      if ExtractFileDir(FileName) = '' then
        FileName := Format('%s\%s', [GetCurrentDir, FileName]);
      try
        Editor.LoadFromFile(FileName);
        FLastFileName := FileName;
        FNewFile := False;
      except
        //KMsgBox(sAppError, Format(sErrMemoLoadFromFile, [FileName]), [mbOk], miStop);
      end;
      //DeleteFromMRUFs(FileName);
    end;
  end;
end;

procedure TfEditorToolbar.ParaStyleChanged(Sender: TObject; AReasons: TKMemoUpdateReasons);
begin
  if not Assigned(FEditor) then exit;
  Editor.SelectionParaStyle := FParaStyle;
end;

function TfEditorToolbar.SaveFile(SaveAs, NeedAnotherOp: Boolean): Boolean;
var
  NeedDlg: Boolean;
  FileName: string;
begin
  if not Assigned(FEditor) then exit;
  Result := False;
  if FNewFile then
    FileName := 'memo.rtf'
  else
    FileName := ExtractFileName(FLastFileName);
  if NeedAnotherOp then
  begin
    if Editor.Modified then
    begin
      case KMsgBox(sAppQuery, Format(sQueryFileSave, [FileName]), [mbYes, mbNo, mbCancel], miQuestion) of
        2: Result := True;
        3: Exit;
      end
    end else
      Result := True;
  end;
  if not Result then
  begin
    NeedDlg := FNewFile or SaveAs;
    SDMain.FileName := FileName;
    if not NeedDlg or SDMain.Execute then
    begin
      if NeedDlg then FLastFileName := SDMain.Filename;
      try
        Editor.SaveToFile(FLastFileName);
        Editor.Modified := False;
        FNewFile := False;
        Result := True;
      except
        KMsgBox(sAppError, Format(sErrMemoSaveToFile, [FileName]), [mbOk], miStop);
      end;
    end;
  end;
  if Result then
    Editor.Modified := False;
end;

procedure TfEditorToolbar.SetEditor(AValue: TKMemo);
begin
  if FEditor=AValue then Exit;
  if Assigned(FEditor) then
    begin
      FEditor.OnMouseDown:=nil;
      FEditor.OnMouseUp:=nil;
      FEditor.OnDropFiles:=nil;
      FEditor.PopupMenu:=nil;
    end;
  FEditor:=AValue;
  if Assigned(FEditor) then
    begin
      FEditor.OnMouseDown:=@EditorMouseDown;
      FEditor.OnMouseUp:=@EditorMouseUp;
      FEditor.OnDropFiles:=@EditorDropFiles;
      FEditor.PopupMenu:=PMMain;
    end;
end;

procedure TfEditorToolbar.TextStyleChanged(Sender: TObject);
var
  SelAvail, DoSelect: Boolean;
  SelEnd, StartIndex, EndIndex: Integer;
begin
  if not Assigned(FEditor) then exit;
  // if there is no selection then simulate one word selection or set style for new text
  DoSelect := False;
  SelAvail := Editor.SelAvail;
  SelEnd := Editor.SelEnd;
  try
    if not SelAvail then
    begin
      // simulate MS Word behavior here, SelEnd is caret position
      // do not select the word if we are at the beginning or end of the word
      // and allow set another text style for newly added text
      DoSelect := Editor.GetNearestWordIndexes(SelEnd, False, StartIndex, EndIndex) and (StartIndex < SelEnd) and (SelEnd < EndIndex);
      if DoSelect then
        Editor.Select(StartIndex, EndIndex - StartIndex, False);
    end;
    if Editor.SelAvail then
      Editor.SelectionTextStyle := FTextStyle
    else
      Editor.NewTextStyle := FTextStyle;
  finally
    if DoSelect then
      Editor.Select(SelEnd, 0, False);
  end;
end;


end.

