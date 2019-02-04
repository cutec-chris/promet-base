unit utageditorform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  TagEditor;

type

  { TfTagEditor }

  TfTagEditor = class(TForm)
    ButtonPanel1: TButtonPanel;
    FEditor: TTagEditor;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    function Execute : Boolean;
  end;

var
  fTagEditor: TfTagEditor;

implementation

{$R *.lfm}

{ TfTagEditor }

procedure TfTagEditor.FormCreate(Sender: TObject);
begin
  FEditor := TTagEditor.Create(Self);
  FEditor.Parent := Self;
  FEditor.Align:=alTop;
  FEditor.AutoHeight:=True;
  FEditor.Show;
end;

function TfTagEditor.Execute: Boolean;
begin
  if not Assigned(fTagEditor) then
    begin
      fTagEditor := TfTagEditor.Create(Application);
      Self := fTagEditor;
    end;
  Result := ShowModal = mrOK;
end;

end.

