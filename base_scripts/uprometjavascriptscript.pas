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
Created 15.10.2015
*******************************************************************************}
unit uprometjavascriptscript;

//TODO:Promet classes should be nice introduceable with RTTI

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, genjavascriptscript, uprometscripts;

type
  TPrometJavaScriptScript = class(TJavascriptScript)
  end;

implementation

initialization
  RegisterScriptType(TPrometJavaScriptScript);
end.

