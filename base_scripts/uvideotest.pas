unit uVideoTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls,uvideofunctions, types, GraphUtil, Menus,Clipbrd;

type

  { TfVideoTest }

  TfVideoTest = class(TForm)
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    PopupMenu1: TPopupMenu;
    ScrollBar1: TScrollBar;
    ScrollBar2: TScrollBar;
    StatusBar1: TStatusBar;
    ScaleTimer: TTimer;
    PaintBox1: TPaintBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBox1Paint(Sender: TObject);
    procedure ScaleTimerTimer(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
  private
    { private declarations }
    FImage : TBitmap;
    FScaledImage : TBitmap;
    FScale: Double;
    FScaled : Boolean;
    procedure DoScalePreview;
  public
    { public declarations }
  end;

var
  fVideoTest: TfVideoTest;

function CaptureImageVT(dev: PChar;Width,Height : Integer): Boolean;stdcall;
procedure RefreshImageVT;stdcall;
function LoadImageVT(aFile : PChar) : Boolean;stdcall;

implementation

uses
  ucapture;

procedure TfVideoTest.DoScalePreview;
var
  amax: Integer;
begin
  FScaled := False;
  ScaleTimer.Enabled:=True;
  amax := abs(round(FImage.Width-1-PaintBox1.Width*FScale));
  if aMax >0 then
    ScrollBar1.Max:=aMax;
  amax := abs(round(FImage.Height-1-PaintBox1.Height*FScale));
  if aMax > 0 then
    ScrollBar2.Max:=aMax;
  PaintBox1.Invalidate;
end;

function CaptureImageVT(dev: PChar; Width, Height: Integer): Boolean;stdcall;
begin
  if not Assigned(fVideoTest) then
    begin
      Application.CreateForm(TfVideoTest,fVideoTest);
    end;
  fVideoTest.Show;
  Application.ProcessMessages;
  Result := DoCaptureImage(dev,Width,Height);
  if Result then
    begin
      fVideoTest.fImage.LoadFromIntfImage(BaseImage);
      fVideoTest.DoScalePreview;
      fVideoTest.BringToFront;
    end;
end;

procedure RefreshImageVT;stdcall;
begin
  RefreshImage;
  if not Assigned(Image) then exit;
  fVideoTest.fImage.LoadFromIntfImage(Image);
  fVideoTest.DoScalePreview;
  fVideoTest.Invalidate;
end;

function LoadImageVT(aFile: PChar): Boolean;stdcall;
begin
  if not Assigned(fVideoTest) then
    begin
      Application.CreateForm(TfVideoTest,fVideoTest);
    end;
  fVideoTest.Show;
  Result := LoadImage(aFile);
  fVideoTest.fImage.LoadFromIntfImage(Image);
  fVideoTest.DoScalePreview;
  fVideoTest.BringToFront;
end;

{ TfVideoTest }

procedure TfVideoTest.FormCreate(Sender: TObject);
begin
  FScale:=1;
  FImage := TBitmap.Create;
  FScaledImage := TBitmap.Create;
end;

procedure TfVideoTest.FormDestroy(Sender: TObject);
begin
  FImage.Free;
  FScaledImage.Free;
end;

procedure TfVideoTest.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  Handled := False;
  if ssCtrl in Shift then
    begin
      if WheelDelta<0 then
        FScale := FScale/1.1
      else
        FScale := FScale*1.1;
      DoScalepreview;
      Handled := True;
    end
  else
    ScrollBar2.Position:=ScrollBar2.Position+15;
  PaintBox1.Invalidate;
end;

procedure TfVideoTest.MenuItem1Click(Sender: TObject);
begin
  Clipboard.AsText:=StringReplace(StringReplace(StatusBar1.Panels.Items[0].Text+','+StatusBar1.Panels.Items[1].Text,'X:','',[]),'Y:','',[]);
end;

procedure TfVideoTest.MenuItem2Click(Sender: TObject);
begin
  Clipboard.AsText:=StringReplace(StringReplace(StringReplace(StatusBar1.Panels.Items[2].Text,' R:','',[]),' G:',',',[]),' B:',',',[]);
end;

procedure TfVideoTest.MenuItem3Click(Sender: TObject);
begin
  Clipboard.AsText:=StringReplace(StringReplace(StringReplace(StatusBar1.Panels.Items[3].Text,' H:','',[]),' L:',',',[]),' S:',',',[]);
end;

procedure TfVideoTest.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  aRGB: LongInt;
  h: Byte;
  l: Byte;
  s: Byte;
begin
  if not Assigned(Image) then exit;
  X := X+ScrollBar1.Position;
  Y := Y+ScrollBar2.Position;

  StatusBar1.Panels.Items[0].Text:='X:'+IntToStr(round(X*FScale));
  StatusBar1.Panels.Items[1].Text:='Y:'+IntToStr(round(Y*FScale));

  if (y<Image.Height) and (x<Image.Width) then
    begin
      aRGB := ColorToRGB(FImage.Canvas.Pixels[round(X*FScale),round(Y*FScale)]);
      StatusBar1.Panels.Items[2].Text:=' R:'+IntToStr(((aRGB) and $FF)*255)+
                                       ' G:'+IntToStr(((aRGB shr 8)*255) and $FF)+
                                       ' B:'+IntToStr(((aRGB shr 16)*255) and $FF);
      ColorToHLS(FPColorToTColor(Image.Colors[round(X*FScale),round(Y*FScale)]),h,l,s);
      StatusBar1.Panels.Items[3].Text:=' H:'+IntToStr(h*255)+
                                       ' L:'+IntToStr(l*255)+
                                       ' S:'+IntToStr(s*255);
    end;
end;

procedure TfVideoTest.PaintBox1Paint(Sender: TObject);
var
  RectDest, RectSource: TRect;
begin
  RectDest:=Rect(0, 0, PaintBox1.Width, PaintBox1.Height);
  if FScaled then
    begin
      RectSource:=Rect(
        ScrollBar1.Position,
        ScrollBar2.Position,
        Scrollbar1.Position+round(PaintBox1.Width),
        ScrollBar2.Position+round(PaintBox1.Height));
      PaintBox1.Canvas.CopyRect(RectDest, FScaledImage.Canvas, RectSource);
    end
  else
    begin
      RectSource:=Rect(
        ScrollBar1.Position,
        ScrollBar2.Position,
        Scrollbar1.Position+round(PaintBox1.Width/FScale),
        ScrollBar2.Position+round(PaintBox1.Height/FScale));
      PaintBox1.Canvas.CopyRect(RectDest, FImage.Canvas, RectSource);
    end;
end;

procedure TfVideoTest.ScaleTimerTimer(Sender: TObject);
begin
  ScaleTimer.Enabled:=False;
  FScaledImage.Width:=round(FImage.Width*FScale);
  FScaledImage.Height:=round(FImage.Height*FScale);
  FScaledImage.Canvas.StretchDraw(Rect(0,0,FScaledImage.Width,FScaledImage.Height),FImage);
  FScaled:=True;
end;

procedure TfVideoTest.ScrollBar1Change(Sender: TObject);
begin
  PaintBox1.Invalidate;
end;

{ TfVideoTest }

initialization
  {$I uVideoTest.lrs}

end.

