unit uPosGotoArticle;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel;

type

  { TfGotoArticle }

  TfGotoArticle = class(TForm)
    ButtonPanel1: TButtonPanel;
    rbOpenOther: TRadioButton;
    rbOpenInVersion: TRadioButton;
    rbVersionate: TRadioButton;
    rbCreate: TRadioButton;
  private
    { private declarations }
  public
    { public declarations }
    function Execute : Boolean;
    procedure SetLanguage;
  end;

var
  fGotoArticle: TfGotoArticle;

implementation

{$R *.lfm}

{ TfGotoArticle }

function TfGotoArticle.Execute: Boolean;
begin
  SetLanguage;
  Result := ShowModal = mrOK;
end;

procedure TfGotoArticle.SetLanguage;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfGotoArticle,fGotoArticle);
      Self := fGotoArticle;
    end;
end;

end.

