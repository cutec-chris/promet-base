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
Created 01.12.2015
*******************************************************************************}
unit uWikiEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterHTML, IpHtml, Forms,
  Controls, ComCtrls, ExtCtrls, DbCtrls, Buttons, DBGrids, StdCtrls, ActnList,
  uExtControls,uImageCache,uDocuments,Graphics,Dialogs;

type
  TSimpleIpHtml = class(TIpHtml)
  protected
  public
    property OnGetImageX;
    constructor Create;
  end;

  { TfWikiEditor }

  TfWikiEditor = class(TForm)
    acPaste: TAction;
    acScreenshot: TAction;
    acItalic: TAction;
    acBold: TAction;
    acLink: TAction;
    acInternetLink: TAction;
    acLinkFromLink: TAction;
    acImage: TAction;
    acSpellcheck: TAction;
    ActionList1: TActionList;
    bItalic: TSpeedButton;
    ipHTML: TIpHtmlPanel;
    Panel1: TPanel;
    pcPages: TExtMenuPageControl;
    SpeedButton2: TSpeedButton;
    mEdit: TSynEdit;
    SynHTMLSyn1: TSynHTMLSyn;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    tsPreview: TTabSheet;
    tsEdit: TTabSheet;
    procedure acBoldExecute(Sender: TObject);
    procedure acImageExecute(Sender: TObject);
    procedure acInternetLinkExecute(Sender: TObject);
    procedure acItalicExecute(Sender: TObject);
    procedure acLinkExecute(Sender: TObject);
    procedure acLinkFromLinkExecute(Sender: TObject);
    procedure acPasteExecute(Sender: TObject);
    procedure acScreenshotExecute(Sender: TObject);
    procedure acSpellcheckExecute(Sender: TObject);
    procedure AddDocuments(Sender: TObject);
    function FCacheGetFile(Path: string; var NewPath: string;var ExpireDate : TDateTime): TStream;
    procedure mEditChange(Sender: TObject);
    procedure TSimpleIpHtmlGetImageX(Sender: TIpHtmlNode; const URL: string;
      var Picture: TPicture);
  private
    FActNode: TIpHtmlNode;
    FCache: TFileCache;
    FCaption: string;
    FDocuments: TDocuments;
    FDocTyp: String;
    FDocId : Variant;
    FDocName: String;
    FIdent: Variant;
    FOnChange: TNotifyEvent;
    FOnNeedId: TNotifyEvent;
    function GetChanged: Boolean;
    function GetText: string;
    procedure SetDocuments(AValue: TDocuments);
    procedure Settext(AValue: string);
    { private declarations }
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function GetHTML(aHTML: String): TSimpleIpHtml;
    procedure Open(aText: string; aId: Variant; aTyp, aName: string;
      Refreshpages: Boolean=True);
    property Id : Variant read FIdent;
    property Text : string read GetText;
    property Caption : string read FCaption write FCaption;
    property Documents : TDocuments read FDocuments write SetDocuments;
    property Changed : Boolean read GetChanged;
    property OnChange : TNotifyEvent read FOnChange write FOnChange;
    property OnNeedId : TNotifyEvent read FOnNeedId write FOnNeedId;
  end;

resourcestring
  strWikiLoadingPage = 'Lade...';
  strYoumustSaveFirst= 'Sie müssen Speichern bevor Sie Dokumente Anhängen dürfen';
implementation
uses Clipbrd,uBaseVisualApplication,uData,uspelling,uDocumentFrame,Utils,
  wikitohtml,uBaseVisualControls,uBaseDBInterface,uscreenshotmain,uBaseApplication,
  uIntfStrConsts,uGeneralStrConsts,LCLType,rtf2html;
{$R *.lfm}

constructor TSimpleIpHtml.Create;
begin
  inherited;
end;

function TfWikiEditor.FCacheGetFile(Path: string; var NewPath: string;
  var ExpireDate: TDateTime): TStream;
var
  aPicture: TPicture;
  ms: TMemoryStream;
  Picture: TPicture;
  aDocument: TDocument;
  Aspect: real;
  aNumber: integer;
  tmp: String;
begin
  Result := nil;
  ExpireDate:=0;
  NewPath := Path;
  if copy(uppercase(Path),0,5)='ICON(' then
    begin
      if TryStrToInt(copy(Path,6,length(Path)-6),aNumber) then
        begin
          ms := TMemoryStream.Create;
          Picture := TPicture.Create;
          fVisualControls.Images.GetBitmap(aNumber,Picture.Bitmap);
          Picture.SaveToStreamWithFileExt(ms,'png');
          NewPath := Copy(Path,0,length(path)-length(ExtractFileExt(Path)))+'.png';
          ms.Position:=0;
          Result := ms;
          Picture.Free;
        end;
    end
  else if copy(uppercase(Path),0,12)='HISTORYICON(' then
    begin
      tmp := copy(Path,13,length(Path)-13);
      if TryStrToInt(tmp,aNumber) then
        begin
          ms := TMemoryStream.Create;
          Picture := TPicture.Create;
          fVisualControls.HistoryImages.GetBitmap(aNumber,Picture.Bitmap);
          Picture.SaveToStreamWithFileExt(ms,'png');
          NewPath := Copy(Path,0,length(path)-length(ExtractFileExt(Path)))+'.png';
          ms.Position:=0;
          Result := ms;
          Picture.Free;
        end;
    end
  else
    begin
      aDocument := TDocument.CreateEx(Self,Data);
      Data.SetFilter(aDocument,Data.QuoteField('TYPE')+'=''W'' and '+Data.QuoteField('NAME')+'='+Data.QuoteValue(copy(ExtractFileName(Path),0,rpos('.',ExtractFileName(Path))-1)),1);
      if aDocument.DataSet.RecordCount > 0 then
        begin
          ms := TMemoryStream.Create;
          aDocument.CheckoutToStream(ms);
          ms.Position:=0;
          if TIpHtmlNodeIMG(FActNode).Width.LengthType = hlAbsolute then
            begin
              try
                aPicture := TPicture.Create;
                aPicture.LoadFromStreamWithFileExt(ms,aDocument.FieldByName('EXTENSION').AsString);
                Picture := TPicture.Create;
                Picture.Bitmap.Width := TIpHtmlNodeIMG(FActNode).Width.LengthValue;
                Aspect := aPicture.Height/aPicture.Width;
                Picture.Bitmap.Height := round(TIpHtmlNodeIMG(FActNode).Width.LengthValue*Aspect);
                Picture.Bitmap.Canvas.AntialiasingMode:= amOn;
                Picture.Bitmap.Canvas.StretchDraw(Rect(0,0,Picture.Width,Picture.Height),aPicture.Graphic);
                aPicture.Free;
                ms.Free;
                ms := TMemoryStream.Create;
                Picture.SaveToStreamWithFileExt(ms,'png');
                NewPath := Copy(Path,0,length(path)-length(ExtractFileExt(Path)))+'.png';
                ms.Position:=0;
                Picture.Free;
              except
                on e : exception do
                  begin
                    ms.Free;
                    ms := TMemoryStream.Create;
                    Data.BlobFieldToStream(aDocument.DataSet,'DOCUMENT',ms);
                    ms.Position:=0;
                  end;
              end;
            end;
          Result := ms;
        end;
      aDocument.Free;
    end;
end;

procedure TfWikiEditor.mEditChange(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TfWikiEditor.TSimpleIpHtmlGetImageX(Sender: TIpHtmlNode;
  const URL: string; var Picture: TPicture);
var
  aPicture: TPicture = nil;
  aFile: TMemoryStream = nil;
  NewURL : string = '';
begin
  FActNode := Sender;
  aFile := FCache.GetFile(URL,NewURL);
  if Assigned(aFile) then
    begin
      Picture := TPicture.Create;
      aFile.Position := 0;
      try
        Picture.LoadFromStreamWithFileExt(aFile,ExtractFileExt(NewURL));
      except
        FreeAndNil(Picture);
      end;
    end;
end;

function TfWikiEditor.GetHTML(aHTML : String): TSimpleIpHtml;
var
  ss: TStringStream;
begin
  ss:=TStringStream.Create(#$EF+#$BB+#$BF+'<html><head><title>'+FCaption+'</title><style>p { margin-top: 0em; margin-bottom: 0em; margin: 0em; }</style></head><body>'+aHTML+'<br><font color="#ffffff">_</body></html>');
  ss.Position := 0;
  try
    Result:=TSimpleIPHtml.Create;
    TSimpleIPHtml(Result).OnGetImageX:=@TSimpleIpHtmlGetImageX;
    Result.LoadFromStream(ss);
  finally
    ss.Free;
  end;
end;

procedure TfWikiEditor.acBoldExecute(Sender: TObject);
begin
  mEdit.SelText:=''''''''+mEdit.SelText+'''''''';
  mEdit.SelStart:=mEdit.SelEnd;
end;

procedure TfWikiEditor.acImageExecute(Sender: TObject);
begin
  mEdit.SelText := '[[Bild:]]';
  mEdit.SelStart:=mEdit.SelStart+7;
end;

procedure TfWikiEditor.acInternetLinkExecute(Sender: TObject);
begin
  mEdit.SelText := '[http://]';
  mEdit.SelStart:=mEdit.SelStart+8;
end;

procedure TfWikiEditor.acItalicExecute(Sender: TObject);
begin
  mEdit.SelText:=''''''+mEdit.SelText+'''''';
  mEdit.SelStart:=mEdit.SelEnd;
end;

procedure TfWikiEditor.acLinkExecute(Sender: TObject);
begin
  mEdit.SelText := '[[]]';
  mEdit.SelStart:=mEdit.SelStart+2;
end;

procedure TfWikiEditor.acLinkFromLinkExecute(Sender: TObject);
var
  Stream: TStringStream;
begin
  if Clipboard.HasFormat(LinkClipboardFormat) then
    begin
      Stream := TStringstream.Create('');
      if Clipboard.GetFormat(LinkClipboardFormat,Stream) then
        begin
          if pos('{',Stream.DataString)>0 then
            mEdit.SelText := '[['+copy(Stream.DataString,0,pos('{',Stream.DataString)-1)+'|'+Data.GetLinkDesc(Stream.DataString)+']]'
          else
            mEdit.SelText := '[['+Stream.DataString+']]';
          mEdit.SelStart:=mEdit.SelStart+2;
        end;
      Stream.Free;
    end;
end;

procedure TfWikiEditor.acPasteExecute(Sender: TObject);
var
  Fid: TClipboardFormat;
  aStr: WideString;
  Stream: TStream;
  ToInsert: UTF8String;
begin
  Fid := Clipboard.FindFormatID('text/html');
  if Fid <> 0 then
    begin
      try
        Stream := TMemoryStream.Create;
        if Clipboard.GetFormat(Fid, Stream) then
        begin
          Stream.Write(#0#0, Length(#0#0));
          Stream.Position := 0;
          aStr := PWideChar(TMemorystream(Stream).Memory);
          ToInsert := UTF8Encode(aStr);
        end;
      finally
        Stream.Free;
      end;
    end
  else
    begin
      Fid := Clipboard.FindFormatID('Rich Text Format');
      if Fid <> 0 then
        begin
          try
            Stream := TStringStream.Create('');
            if Clipboard.GetFormat(Fid, Stream) then
              begin
                ToInsert := RtfToHtml(TStringStream(Stream).DataString);
              end
            else ToInsert := Clipboard.AsText;
          finally
            Stream.Free;
          end;
        end
      else ToInsert := Clipboard.AsText;
    end;
  mEdit.SelText:=ToInsert;
end;

procedure TfWikiEditor.acScreenshotExecute(Sender: TObject);
var
  aDocuments: TDocuments;
  aDocument: TDocument;
  aDocPage: TTabSheet;
  aName : string = 'screenshot.jpg';
  aDocFrame: TfDocumentFrame;
  aPageIndex: Integer;
begin
  if FDocId = Null then
    begin
      ShowMessage(strYoumustSaveFirst);
    end;
  Application.ProcessMessages;
  Application.MainForm.Hide;
  Application.ProcessMessages;
  aName := InputBox(strScreenshotName, strEnterAnName, aName);
  Application.ProcessMessages;
  Application.CreateForm(TfScreenshot,fScreenshot);
  with BaseApplication as IBaseApplication do
    fScreenshot.SaveTo:=AppendPathDelim(GetInternalTempDir)+aName;
  fScreenshot.Show;
  while fScreenshot.Visible do Application.ProcessMessages;
  fScreenshot.Destroy;
  fScreenshot := nil;
  aDocument := TDocument.CreateEx(Self,Data);
  aDocument.Select(FDocId ,FDocTyp,FDocName,Null,Null);
  with BaseApplication as IBaseApplication do
    aDocument.AddFromFile(AppendPathDelim(GetInternalTempDir)+aName);
  aDocument.Free;
  aDocuments := TDocuments.CreateEx(Self,Data);
  aDocuments.CreateTable;
  aDocuments.Select(FDocId ,FDocTyp,FDocName,Null,Null);
  aDocuments.Open;
  if aDocuments.Count = 0 then
    aDocuments.Free
  else
    begin
      aDocPage := pcPages.GetTab(TfDocumentFrame);
      if Assigned(aDocPage) then
        begin
          aDocFrame := TfDocumentFrame(aDocPage.Controls[0]);
          aDocFrame.DataSet := aDocuments;
        end
      else
        begin
          aDocFrame := TfDocumentFrame.Create(Self);
          aDocFrame.DataSet := aDocuments;
          aPageIndex := pcPages.AddTab(aDocFrame,False,strFiles);
        end;
    end;
  mEdit.SelText := '[[Bild:'+aName+']]';
  Application.MainForm.Show;
end;

procedure TfWikiEditor.acSpellcheckExecute(Sender: TObject);
begin
  fSpellCheck.Execute(mEdit,mEdit.SelStart);
end;

procedure TfWikiEditor.AddDocuments(Sender: TObject);
begin
  if not Assigned(FDocuments) then
    if Assigned(FOnNeedId) then
      FOnNeedId(Self);
  if not Assigned(FDocuments) then exit;
  TfDocumentFrame(Sender).DataSet := FDocuments;
  TfDocumentFrame(Sender).Refresh(FDocId,FDocTyp,FDocName,Null,Null,0);
end;

function TfWikiEditor.GetText: string;
begin
  Result := mEdit.Text;
end;

function TfWikiEditor.GetChanged: Boolean;
begin
  Result := mEdit.Modified;
end;

procedure TfWikiEditor.SetDocuments(AValue: TDocuments);
begin
  if FDocuments=AValue then Exit;
  FDocuments:=AValue;
end;

procedure TfWikiEditor.Settext(AValue: string);
begin
  mEdit.Text := AValue;
end;

procedure TfWikiEditor.Open(aText: string; aId: Variant; aTyp, aName: string;Refreshpages : Boolean = True);
var
  aDocPage: TTabSheet;
  aDocFrame: TfDocumentFrame;
  aPageIndex: Integer;
  ss: TStringStream;
  i: Integer;
begin
  i := 0;
  if Refreshpages then
    begin
      while i<pcPages.PageCount-1 do
        begin
          if (pcPages.Pages[i].ControlCount > 0) and (pcPages.Pages[i].Controls[0] is TfDocumentFrame) then
            pcPages.Pages[i].Destroy
          else inc(i);
        end;
      pcPages.ClearTabClasses;
    end;
  FIdent := aID;
  pcPages.AddTabClass(TfDocumentFrame,strFiles,@AddDocuments);
  if not Assigned(FDocuments) then
    FDocuments := TDocuments.Create(nil);
  if aId = Null then
    FreeAndNil(FDocuments)
  else
    begin
      FDocuments.Select(aId,aTyp,aName,Null,Null);
      FDocTyp := aTyp;
      FDocId := aId;
      FDocName := aName;
      FDocuments.Open;
      if FDocuments.Count > 0 then
        begin
          aDocPage := pcPages.GetTab(TfDocumentFrame);
          if Assigned(aDocPage) then
            begin
              aDocFrame := TfDocumentFrame(aDocPage.Controls[0]);
              aDocFrame.DataSet := FDocuments;
              aDocFrame.Refresh(FDocId,FDocTyp,FDocName,Null,Null,0);
            end
          else
            begin
              aDocFrame := TfDocumentFrame.Create(Self);
              aDocFrame.DataSet := FDocuments;
              aPageIndex := pcPages.AddTab(aDocFrame,False,strFiles);
              aDocFrame.Refresh(FDocId,FDocTyp,FDocName,Null,Null);
            end;
        end;
    end;
  mEdit.Text:=aText;
  tsEdit.TabVisible:=True;
  tsEdit.PageIndex:=0;
  pcPages.ActivePage:=tsEdit;
  pcPages.PageIndex:=0;
end;

constructor TfWikiEditor.Create(TheOwner: TComponent);
var
  ss: TStringStream;
begin
  inherited Create(TheOwner);
  FCache := TFileCache.Create(30);
  FCache.OnGetFile:=@FCacheGetFile;
  FDocuments := nil;
end;

destructor TfWikiEditor.Destroy;
begin
  FCache.Free;
  inherited Destroy;
end;

end.

