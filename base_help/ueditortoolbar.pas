unit uEditorToolbar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, ActnList, kmemo;

type

  { TFrame2 }

  TFrame2 = class(TFrame)
    ACEditCopy: TKMemoEditCopyAction;
    ACEditCut: TKMemoEditCutAction;
    ACEditHyperlink: TAction;
    ACEditPaste: TKMemoEditPasteAction;
    ACEditSelectAll: TKMemoEditSelectAllAction;
    ACFileNew: TAction;
    ACFileOpen: TAction;
    ACFilePreview: TAction;
    ACFilePrint: TAction;
    ACFileSave: TAction;
    ACFileSaveAs: TAction;
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
    ALMain: TActionList;
    ILMain: TImageList;
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
    ToBSecond: TToolBar;
  private
    { private declarations }
  public
    { public declarations }
  end;

implementation

{$R *.lfm}

end.

