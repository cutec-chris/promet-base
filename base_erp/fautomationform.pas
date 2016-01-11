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
Created 08.10.2015
*******************************************************************************}
unit fautomationform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, IpHtml, LResources, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, StdCtrls, Buttons, ActnList,uBaseDbClasses,
  uBaseERPDBClasses;

type
  TFAutomation = class(TForm)
    acAbort: TAction;
    acExecuteStep: TAction;
    acPrepare: TAction;
    acProduce: TAction;
    acReady: TAction;
    acSave: TAction;
    ActionList1: TActionList;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Bevel7: TBevel;
    BitBtn1: TBitBtn;
    BitBtn2: TSpeedButton;
    BitBtn3: TSpeedButton;
    BitBtn5: TSpeedButton;
    ipWorkHTML: TIpHtmlPanel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lStep: TLabel;
    Panel3: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    pNav1: TPanel;
    rbArticle: TRadioButton;
    rbList: TRadioButton;
    rbNoData: TRadioButton;
    rbOrder: TRadioButton;
    sbMenue: TSpeedButton;
    ToolBar1: TPanel;
    ToolButton1: TSpeedButton;
    ToolButton2: TSpeedButton;
  private
    { private declarations }
  public
    { public declarations }
    property DataSet : TBaseDBPosition;
  end;

var
  FAutomation: TFAutomation;

implementation

initialization
  {$I unit1.lrs}

end.

