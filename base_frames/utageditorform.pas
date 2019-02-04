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
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    function Execute(aTags : string = '') : Boolean;
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
  FEditor.Align:=alClient;
  FEditor.Visible:=True;
  FEditor.MultiLine:=True;
  FEditor.BorderStyle:=bsNone;
  FEditor.Show;
end;

procedure TfTagEditor.FormShow(Sender: TObject);
begin
  FEditor.SetFocus;
end;

function TfTagEditor.Execute(aTags: string): Boolean;
begin
  if not Assigned(fTagEditor) then
    begin
      fTagEditor := TfTagEditor.Create(Application);
      Self := fTagEditor;
    end;
  FEditor.Tags.Text:=aTags;
  Result := ShowModal = mrOK;
end;

end.

