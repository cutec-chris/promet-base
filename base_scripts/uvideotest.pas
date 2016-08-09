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

function CaptureImageVT(dev: PChar): Boolean;
begin
  if not Assigned(fVideoTest) then
    begin
      Application.CreateForm(TfVideoTest,fVideoTest);
    end;
  fVideoTest.Show;
  Result := CaptureImage(dev);
  fVideoTest.Workimage.Picture.Assign(uvideofunctions.FBitmap);
end;

procedure RefreshImageVT;
begin
  RefreshImage;
  fVideoTest.Workimage.Picture.Assign(uvideofunctions.FBitmap);
end;

function LoadImageVT(aFile: PChar): Boolean;
begin
  if not Assigned(fVideoTest) then
    begin
      Application.CreateForm(TfVideoTest,fVideoTest);
    end;
  fVideoTest.Show;
  Result := LoadImage(aFile);
  fVideoTest.Workimage.Picture.Assign(uvideofunctions.FBitmap);
end;

initialization
  {$I uVideoTest.lrs}

end.

