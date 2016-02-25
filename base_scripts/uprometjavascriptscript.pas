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
  Classes, SysUtils, genscript, uprometscripts,BESEN;

type

  { TPrometJavaScriptScript }

  TPrometJavaScriptScript = class(TScript)
  private
    FBesen : TBESEN;
    FLines : TStringList;
    FRunning: Boolean;
    FStopping : Boolean;
  protected
    function GetTyp: string; override;
  public
    procedure Init; override;
    constructor Create;
    destructor Destroy; override;
    function IsRunning: Boolean; override;
    function Stop: Boolean; override;
    function GetStatus: TScriptStatus; override;
    function Execute(aParameters: Variant; Debug: Boolean=false): Boolean; override;
  end;

implementation

{ TPrometJavaScriptScript }

function TPrometJavaScriptScript.GetTyp: string;
begin
  Result := 'JavaScript'
end;

procedure TPrometJavaScriptScript.Init;
begin
  FLines:=nil;
  FRunning:=False;
  FStopping:=False;
end;

constructor TPrometJavaScriptScript.Create;
begin
  FBesen := TBESEN.Create;
end;

destructor TPrometJavaScriptScript.Destroy;
begin
  FreeAndNil(FBesen);
  inherited Destroy;
end;

function TPrometJavaScriptScript.IsRunning: Boolean;
begin
  Result:=FRunning;
end;

function TPrometJavaScriptScript.Stop: Boolean;
begin
  if IsRunning then
    begin
      FStopping := True;
      Result:=True;
    end;
end;

function TPrometJavaScriptScript.GetStatus: TScriptStatus;
begin
  if IsRunning then
    Result := ssRunning
  else Result := ssNone;
end;

function TPrometJavaScriptScript.Execute(aParameters: Variant; Debug: Boolean
  ): Boolean;
begin
  Result := False;
  try
    FRunning := True;
    //Exec
    FBesen.Execute(Source);
    FRunning:=False;
    Result := True;
  except
    on e : Exception do
      begin
        if Assigned(Writeln) then
          Writeln(e.Message);
        FRunning:=False;
        FreeAndNil(FLines);
        Init;
        FStopping:=False;
      end;
  end;
end;

initialization
  RegisterScriptType(TPrometJavaScriptScript);
end.

