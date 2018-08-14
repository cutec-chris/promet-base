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
Created 01.06.2006
*******************************************************************************}
unit uData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uBaseDbInterface,Utils;

var
  Data : TBaseDBModule = nil;
  MultiData : array of TBaseDBModule;

procedure InitMultiData(const Cnt : Integer);
function GetData(Timeout : Integer = 6000) : TBaseDBModule;

implementation

uses uBaseApplication;

function GetData(Timeout : Integer) : TBaseDBModule;
var
  i: Integer;
  aTime: Int64;
begin
  Result := nil;
  aTime := GetTicks;
  while (not Assigned(Result))
    and (GetTicks-aTime < Timeout) do
    begin
      for i := 0 to length(MultiData)-1 do
        begin
          if MultiData[i].CriticalSection.TryEnter then
            begin
              Result := MultiData[i];
              break;
            end;
        end;
    end;
  if not Assigned(Result) then
    with BaseApplication as IBaseApplication do
      Warning('MultiData not Assigned !!');
end;

procedure InitMultiData(const Cnt : Integer);
var
  actLenght: Integer;
begin
  actLenght := length(MultiData);
  while length(MultiData)>Cnt do
    begin
      MultiData[length(MultiData)-1].Free;
      SetLength(MultiData,length(MultiData)-1);
    end;
  while length(MultiData)<Cnt do
    begin
      SetLength(MultiData,Length(MultiData)+1);
      MultiData[length(MultiData)-1] := TBaseDBModule.Create(nil);
      MultiData[length(MultiData)-1].SetProperties(Data.Properties);
    end;
end;

end.

