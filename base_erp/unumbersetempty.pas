unit unumbersetempty;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ButtonPanel, StdCtrls;

type

  { TfNumbersetEmpty }

  TfNumbersetEmpty = class(TForm)
    ButtonPanel1: TButtonPanel;
    eNumber: TEdit;
    lPasteNumbersetSetting: TLabel;
  private
    { private declarations }
  public
    { public declarations }
    function Execute(Numberset : string) : Boolean;
  end;

var
  fNumbersetEmpty: TfNumbersetEmpty;

implementation

{ TfNumbersetEmpty }

function TfNumbersetEmpty.Execute(Numberset: string): Boolean;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfNumbersetEmpty,fNumbersetEmpty);
      Self := fNumbersetEmpty;
    end;
  Result := Showmodal = mrOK;
  if Result then
    begin

    end;
end;

initialization
  {$I unumbersetempty.lrs}

end.

