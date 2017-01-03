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
Created 12.04.2016
*******************************************************************************}
unit uAppServer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, blcksock, synsock;

type
  TAppNetworkThrd = class;
  TCommandHandlerProc = function(Sender : TAppNetworkThrd; Command : string) : string;

  { TPrometNetworkDaemon }

  { TAppNetworkDaemon }

  TAppNetworkDaemon = class(TThread)
  private
    Socks : TList;
    Sock:TTCPBlockSocket;
    function GetConnections: Integer;
  public
    Constructor Create;
    Destructor Destroy; override;
    procedure Execute; override;
    property Connections : Integer read GetConnections;
    property Sockets : TList read Socks;
  end;

  { TPrometNetworkThrd }

  { TAppNetworkThrd }

  TAppNetworkThrd = class(TThread)
  private
    CSock: TSocket;
    FResult : string;
    FObjects : TList;
    procedure DoCommand(FCommand : string);
  protected
  public
    Sock:TTCPBlockSocket;
    Constructor Create (hsock:tSocket);
    destructor Destroy; override;
    procedure Execute; override;
    property Objects : TList read FObjects;
  end;

procedure RegisterCommandHandler(aHandler : TCommandHandlerProc);

var
  NetworkDaemon : TAppNetworkDaemon;
  CommandHandlers : array of TCommandHandlerProc;

implementation

uses synautil;

function TAppNetworkDaemon.GetConnections: Integer;
begin
  Result := Socks.Count;
end;

constructor TAppNetworkDaemon.Create;
begin
  inherited create(false);
  sock:=TTCPBlockSocket.create;
  Socks := TList.Create;
  FreeOnTerminate:=true;
end;
destructor TAppNetworkDaemon.Destroy;
begin
  Terminate;
  WaitFor;
  //Sock.free;
  Socks.Free;
end;
procedure TAppNetworkDaemon.Execute;
var
  ClientSock:TSocket;
  ListenOk: Boolean;
begin
  with sock do
    begin
      ListenOk := False;
      repeat
        CreateSocket;
        setLinger(true,1000);
        if not ListenOk then
          begin
            bind('0.0.0.0','8085');
            if LastError=0 then
              begin
                ListenOk:=True;
                listen;
                if LastError<>0 then
                  ListenOk:=False;
              end
            else
              begin
                bind('127.0.0.1','8085');
                if LastError=0 then
                  begin
                    ListenOk:=True;
                    listen;
                    if LastError<>0 then
                      ListenOk:=False
                    else
                      WriteLn('accepting connections on Port 8085')
                  end
                else
                  begin
                    WriteLn('listening failed ('+LastErrorDesc+'), retrying...');
                    CloseSocket;
                    sleep(5000);
                  end;
              end;
          end
        else
          begin
            if terminated then break;
            if canread(1000) then
              begin
                try
                  ClientSock:=accept;
                except
                  Terminate;
                end;
                if lastError=0 then
                  Socks.Add(TAppNetworkThrd.create(ClientSock));
              end;
          end;
      until false;
    end;
end;

procedure RegisterCommandHandler(aHandler: TCommandHandlerProc
  );
begin
  setlength(CommandHandlers,length(CommandHandlers)+1);
  CommandHandlers[Length(CommandHandlers)-1] := aHandler;
end;

procedure TAppNetworkThrd.DoCommand(FCommand: string);
var
  i: Integer;
begin
  for i := 0 to Length(CommandHandlers)-1 do
    begin
      FResult := CommandHandlers[i](Self,FCommand);
      if FResult <>'' then break;
    end;
  if FResult='' then
    FResult:='ERROR: failed!';
end;

constructor TAppNetworkThrd.Create(hsock: tSocket);
var
  LoggedIn: Boolean;
begin
  inherited create(false);
  FObjects := TList.Create;
  Csock := Hsock;
  FreeOnTerminate:=true;
end;

destructor TAppNetworkThrd.Destroy;
begin
  try
    while FObjects.Count>0 do
      begin
        TObject(FObjects[0]).Free;
        FObjects.Delete(0);
      end;
    FObjects.Free;
  except
  end;
  inherited Destroy;
  NetworkDaemon.Sockets.Remove(Self);
end;

procedure TAppNetworkThrd.Execute;
var
  s: string;
begin
  sock:=TTCPBlockSocket.create;
  try
    Sock.socket:=CSock;
    sock.GetSins;
    with sock do
      begin
        repeat
          if terminated then break;
          s := RecvTerminated(2000,CRLF);
          if (lastError<>0) and (LastError<>WSAETIMEDOUT) then
            break;
          if s <> '' then
            begin
              DoCommand(s);
              SendString(FResult+CRLF);
              if lastError<>0 then break;
            end;
        until false;
      end;
  finally
    Sock.Free;
  end;
end;


end.

