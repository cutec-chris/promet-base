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
Created 27.03.2016
*******************************************************************************}
unit uprometpubsub;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, RegExpr, Utils;

type

  { TPubSubClient }

  TPubSubClient = class(TObject)
  private
    FPub: TStrOut2Func;
    FSubscribed : TStringList;
  protected
    function InternalCheckPub(Topic : string) : Boolean;
    function InternalPub(Topic,Value : string) : Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function Publish(Topic,Value : string) : Boolean;
    function DirectPublish(Topic,Value : string) : Boolean;
    function Subscribe(Topic : string) : Boolean;
    function UnSubscribe(Topic : string) : Boolean;
    property OnPublish : TStrOut2Func read FPub write FPub;
  end;

implementation

var
  PubSubClients : TList;

function globToReg(const glob: string ): string;
  procedure quote(var r: string; c: char);
  begin
    if not (c in ['a'..'z', 'A'..'Z', '0'..'9', '_', '-']) then
      r += '\';
    r += c;
  end;
var
  i: integer = 0;
  b: integer = 0;
begin
  result := '^';
  while i < length(glob) do
  begin
    i += 1;
    case glob[i] of
      '*': result += '.*';
      '?': result += '.';
      '[', ']': result += glob[i];
      '{':
        begin
          b += 1;
          result += '(';
        end;
      '}':
        begin
          b -= 1;
          result += ')';
        end;
      ',':
        begin
          if b > 0 then
            result += '|'
          else
            quote(result, glob[i]);
        end;
      else
        quote(result, glob[i]);
    end;
  end;
end;

{ TPubSubClient }

function TPubSubClient.InternalCheckPub(Topic: string): Boolean;
var
  glb: TRegExpr;
  i: Integer;
begin
  Result := False;
  glb := TRegExpr.Create;
  for i := 0 to FSubscribed.Count-1 do
    begin
      glb.Expression:=GlobToReg(FSubscribed[i]);
      if glb.Exec(Topic) then
        begin
          Result := True;
          break;
        end;
    end;
  glb.Free;
end;

function TPubSubClient.InternalPub(Topic, Value: string): Boolean;
begin
  Result := True;
  if Assigned(FPub) then
    FPub(Topic,Value);
end;

constructor TPubSubClient.Create;
begin
  FSubscribed := TStringList.Create;
  PubSubClients.Add(Self);
end;

destructor TPubSubClient.Destroy;
begin
  if PubSubClients.IndexOf(Self)>-1 then
    PubSubClients.Delete(PubSubClients.IndexOf(Self));
  FSubscribed.Free;
  inherited Destroy;
end;

function TPubSubClient.Publish(Topic, Value: string): Boolean;
begin
  if lowercase(copy(Topic,0,length(GetSystemName)+1))<>'/'+lowercase(GetSystemName) then
    begin
      if copy(Topic,0,1)<>'/' then
        Topic:='/'+Topic;
      Topic:='/'+GetSystemName+Topic;
    end;
  DirectPublish(Topic,Value);
end;

function TPubSubClient.DirectPublish(Topic, Value: string): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to PubSubClients.Count-1 do
    if TPubSubClient(PubSubClients[i]).InternalCheckPub(Topic) then
      if not TPubSubClient(PubSubClients[i]).InternalPub(Topic,Value) then Result := False;
end;

function TPubSubClient.Subscribe(Topic: string): Boolean;
begin
  FSubscribed.Add(Topic);
end;

function TPubSubClient.UnSubscribe(Topic: string): Boolean;
var
  glb: TRegExpr;
  i: Integer = 0;
begin
  Result := False;
  glb := TRegExpr.Create;
  glb.Expression:=GlobToReg(Topic);
  while i < FSubscribed.Count do
    if glb.Exec(FSubscribed[i]) then
      begin
        FSubscribed.Delete(i);
      end
    else inc(i);
  glb.Free;
end;

initialization
  PubSubClients := TList.Create;
finalization
  FreeAndNil(PubSubClients);
end.

