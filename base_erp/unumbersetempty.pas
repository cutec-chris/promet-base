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
Created 07.07.2016
*******************************************************************************}
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

