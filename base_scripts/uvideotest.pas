unit uVideoTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls,uvideofunctions;

type

  { TfVideoTest }

  TfVideoTest = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Image1: TImage;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  fVideoTest: TfVideoTest;

function CaptureImageVT(dev: PChar): Boolean;

implementation

function CaptureImageVT(dev: PChar): Boolean;
begin
  if not Assigned(fVideoTest) then
    begin
      Application.CreateForm(TfVideoTest,fVideoTest);
    end;
  fVideoTest.Show;
  Result := CaptureImage(dev);
end;

initialization
  {$I uVideoTest.lrs}

end.

