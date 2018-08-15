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
  DataM : TBaseDBModule = nil;
  MultiData : array of TBaseDBModule;

function Data : TBaseDBModule;
procedure InitMultiData(const Cnt : Integer);
function GetData(aUser : Int64 = 0;Timeout : Integer = 6000) : TBaseDBModule;

implementation

uses uBaseApplication;

function GetData(aUser: Int64; Timeout: Integer): TBaseDBModule;
var
  i: Integer;
  aTime: Int64;
  Runs : Integer = 0;
begin
  Result := nil;
  aTime := GetTicks;
  while (not Assigned(Result))
    and (GetTicks-aTime < Timeout) do
    begin
      for i := 0 to length(MultiData)-1 do
        begin
          if ((aUser = 0) or (Runs>1) or (MultiData[i].LoggedInUser=aUser)) //try to use an DBModule that is logged in already on this user
          and (MultiData[i].CriticalSection.TryEnter) then
            begin
              Result := MultiData[i];
              break;
            end;
        end;
      Runs := Runs+1;
    end;
  if not Assigned(Result) then
    begin
      with BaseApplication as IBaseApplication do
        Warning('MultiData not Assigned !!');
      Result := uData.DataM;
    end;
end;

function Data: TBaseDBModule;
begin
  Result := DataM;
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
      MultiData[length(MultiData)-1].SetProperties(DataM.Properties);
    end;
end;

end.

