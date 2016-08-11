unit uVideoTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls,uvideofunctions;

type

  { TfVideoTest }

  TfVideoTest = class(TForm)
    eB: TEdit;
    eR: TEdit;
    eL: TEdit;
    eT: TEdit;
    WorkImage: TImage;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  fVideoTest: TfVideoTest;

function CaptureImageVT(dev: PChar): Boolean;
procedure RefreshImageVT;
function LoadImageVT(aFile : PChar) : Boolean;

implementation

uses
  {$ifdef WINDOWS}
  ucapture
  {$ELSE}
  ucapture_unix
  {$ENDIF}
  ;

function CaptureImageVT(dev: PChar): Boolean;
begin
  if not Assigned(fVideoTest) then
    begin
      Application.CreateForm(TfVideoTest,fVideoTest);
    end;
  fVideoTest.Show;
  Application.ProcessMessages;
  Result := CaptureImage(dev);
  if Result then
    fVideoTest.Workimage.Picture.Bitmap.LoadFromIntfImage(Image);
end;

procedure RefreshImageVT;
begin
  RefreshImage;
  fVideoTest.Workimage.Picture.Bitmap.LoadFromIntfImage(Image);
end;

function LoadImageVT(aFile: PChar): Boolean;
begin
  if not Assigned(fVideoTest) then
    begin
      Application.CreateForm(TfVideoTest,fVideoTest);
    end;
  fVideoTest.Show;
  Result := LoadImage(aFile);
  fVideoTest.Workimage.Picture.Bitmap.LoadFromIntfImage(Image);
end;

{ TfVideoTest }

initialization
  {$I uVideoTest.lrs}

end.

