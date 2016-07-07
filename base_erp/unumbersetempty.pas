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
  end;

var
  fNumbersetEmpty: TfNumbersetEmpty;

implementation

initialization
  {$I unumbersetempty.lrs}

end.

