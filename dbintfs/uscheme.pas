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
*******************************************************************************}
unit uscheme;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uBaseDBClasses, uBaseERPDbClasses,uBaseDatasetInterfaces,
  db,uIntfStrConsts,uBaseDBInterface;

type
  { TProjectList }

  { TSchemeList }

  TSchemeList = class(TBaseERPList)
  protected
    function GetDescriptionFieldName: string;override;
    function GetTextFieldName: string;override;
    function GetNumberFieldName: string;override;
    function GetStatusFieldName: string;override;
  public
    function GetTyp: string; override;
    procedure DefineFields(aDataSet : TDataSet);override;
  published
  end;

implementation

{ TSchemeList }

function TSchemeList.GetDescriptionFieldName: string;
begin
  Result:='DESC';
end;

function TSchemeList.GetTextFieldName: string;
begin
  Result := 'NAME';
end;

function TSchemeList.GetNumberFieldName: string;
begin
  Result := 'NAME';
end;

function TSchemeList.GetStatusFieldName: string;
begin
  Result:='STATUS';
end;

function TSchemeList.GetTyp: string;
begin
  Result:='S';
end;

procedure TSchemeList.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'SCHEME';
      TableCaption := strSchemeList;
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('NAME',ftString,250,True);
            Add('STATUS',ftString,4,False);
            Add('VERSION',ftString,25,False);
            Add('DESC',ftMemo,0,false);
            Add('TREEENTRY',ftLargeint,0,False);
            Add('DATA',ftBlob,0,false);
            Add('CHANGEDBY',ftString,4,false);
            Add('CREATEDBY',ftString,4,false);
            Add('CREATEDAT',ftDate,0,false);
          end;
      if Assigned(ManagedIndexdefs) then
        with ManagedIndexDefs do
          begin
            Add('NAME','NAME',[]);
            Add('TREEENTRY','TREEENTRY',[]);
          end;
    end;
end;
initialization
  RegisterdataSetClass('SCHEME',TSchemeList);
end.

