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
Created 15.04.2016
*******************************************************************************}
unit uappserverhttp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, synautil, uAppServer, blcksock;

type
  THTTPHandlerProc = function(Method,URL : string;Headers : TStringList;Input,Output : TStream) : Integer;

  procedure RegisterHTTPHandler(aHandler : THTTPHandlerProc);

implementation

var
  HTTPHandlers : array of THTTPHandlerProc;

function HandleHTTPCommand(Sender : TObject;FCommand : string) : string;
var
  aCmd, uri, protocol, s: String;
  headers: TStringList;
  size, Timeout, x, ResultCode, n, i: Integer;
  InputData, OutputData: TMemoryStream;
begin
  Result := '';
  if pos(' ',FCommand)>0 then
    aCmd := copy(FCommand,0,pos(' ',FCommand)-1)
  else aCmd := FCommand;
  Fetch(FCommand,' ');
  case Uppercase(aCmd) of
  'GET','HEAD','POST','DELETE'://HTTP Request
    begin
      Timeout := 12000;
      uri := fetch(FCommand, ' ');
      if uri = '' then
        Exit;
      protocol := fetch(FCommand, ' ');
      headers := TStringList.Create;
      size := -1;
      //read request headers
      if protocol <> '' then
      begin
        if pos('HTTP/', protocol) <> 1 then
          Exit;
        repeat
          s := TAppNetworkThrd(Sender).sock.RecvString(Timeout);
          if TAppNetworkThrd(Sender).sock.lasterror <> 0 then
            Exit;
          if s <> '' then
            Headers.add(s);
          if Pos('CONTENT-LENGTH:', Uppercase(s)) = 1 then
            Size := StrToIntDef(SeparateRight(s, ' '), -1);
        until s = '';
      end;
      //recv document...
      InputData := TMemoryStream.Create;
      if size >= 0 then
      begin
        InputData.SetSize(Size);
        x := TAppNetworkThrd(Sender).Sock.RecvBufferEx(InputData.Memory, Size, Timeout);
        InputData.SetSize(x);
        if TAppNetworkThrd(Sender).sock.lasterror <> 0 then
          Exit;
      end;
      OutputData := TMemoryStream.Create;
      ResultCode:=500;
      for i := 0 to Length(HTTPHandlers)-1 do
        begin
          ResultCode := HTTPHandlers[i](aCmd, uri, Headers, InputData, OutputData);
          if ResultCode<>500 then break;
        end;
      TAppNetworkThrd(Sender).sock.SendString('HTTP/1.0 ' + IntTostr(ResultCode) + CRLF);
      if protocol <> '' then
      begin
        headers.Add('Content-length: ' + IntTostr(OutputData.Size));
        headers.Add('Connection: close');
        headers.Add('Date: ' + Rfc822DateTime(now));
        headers.Add('Server: Avamm Internal Network');
        headers.Add('');
        for n := 0 to headers.count - 1 do
          TAppNetworkThrd(Sender).sock.sendstring(headers[n] + CRLF);
      end;
      if TAppNetworkThrd(Sender).sock.lasterror <> 0 then
        Exit;
      TAppNetworkThrd(Sender).Sock.SendBuffer(OutputData.Memory, OutputData.Size);
      headers.Free;
      Result:=' ';
    end;
  end;
end;

procedure RegisterHTTPHandler(aHandler: THTTPHandlerProc);
begin
  Setlength(HTTPHandlers,length(HTTPHandlers)+1);
  HTTPHandlers[Length(HTTPHandlers)-1] := aHandler;
end;

initialization
  uAppServer.RegisterCommandHandler(@HandleHTTPCommand);
end.

