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
  Classes, SysUtils, blcksock, synsock, uBaseApplication;

type
  TAppNetworkThrd = class;
  TCommandHandlerProc = function(Sender : TAppNetworkThrd; Command : string) : Boolean;

  { TPrometNetworkDaemon }

  { TAppNetworkDaemon }

  TAppNetworkDaemon = class(TThread)
  private
    Socks : TList;
    Sock:TTCPBlockSocket;
    ActId : Integer;
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
    FObjects : TList;
    function DoCommand(FCommand : string) : Boolean;
  protected
  public
    Sock:TTCPBlockSocket;
    Id : Integer;
    Close : Boolean;
    Constructor Create (hsock:tSocket;aId : Integer);
    destructor Destroy; override;
    procedure Execute; override;
    property Objects : TList read FObjects;
    procedure InternalSynchronize(Sender:TThread;AMethod: TThreadMethod;DoLock : Boolean = True);
  end;

procedure RegisterCommandHandler(aHandler : TCommandHandlerProc);

var
  NetworkDaemon : TAppNetworkDaemon;
  CommandHandlers : array of TCommandHandlerProc;
  GlobalLock : TRTLCriticalSection;

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
  FreeOnTerminate:=false;
  ActId := 0;
end;
destructor TAppNetworkDaemon.Destroy;
begin
  FreeAndNil(Sock);
  while Socks.Count>0 do
    begin
      TAppNetworkThrd(Socks[0]).Terminate;
      TAppNetworkThrd(Socks[0]).WaitFor;
    end;
  Socks.Free;
  inherited;
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
        setLinger(true,500);
        if not ListenOk then
          begin
            bind('0.0.0.0','8085');
            if LastError=0 then
              begin
                ListenOk:=True;
                listen;
                if LastError<>0 then
                  ListenOk:=False
                else
                  WriteLn('accepting connections on Port 8085 (1)')
              end
            else
              begin
                bind('0.0.0.0','8085');
                if LastError=0 then
                  begin
                    ListenOk:=True;
                    listen;
                    if LastError<>0 then
                      begin
                        WriteLn('listening failed ('+LastErrorDesc+'), retrying...');
                        ListenOk:=False
                      end
                    else
                      WriteLn('accepting connections on Port 8085 (r)')
                  end
                else
                  begin
                    WriteLn('bind failed ('+LastErrorDesc+'), retrying...');
                    CloseSocket;
                    sleep(1000);
                  end;
              end;
          end
        else
          begin
            if terminated then break;
            if canread(100) then
              begin
                try
                  ClientSock:=accept;
                except
                  Terminate;
                end;
                if lastError=0 then
                  begin
                    Socks.Add(TAppNetworkThrd.create(ClientSock,ActId));
                    inc(ActId);
                  end;
              end;
          end;
      until Terminated;
    end;
end;

procedure TAppNetworkThrd.InternalSynchronize(Sender: TThread;
  AMethod: TThreadMethod; DoLock: Boolean);
begin
  {
  if Assigned(BaseApplication) then
    begin
      if DoLock then
        EnterCriticalsection(GlobalLock);
      AMethod();
      if DoLock then
        LeaveCriticalsection(GlobalLock);
    end
  else
  }  Synchronize(AMethod);
end;

procedure RegisterCommandHandler(aHandler: TCommandHandlerProc
  );
begin
  setlength(CommandHandlers,length(CommandHandlers)+1);
  CommandHandlers[Length(CommandHandlers)-1] := aHandler;
end;

function TAppNetworkThrd.DoCommand(FCommand: string): Boolean;
var
  i: Integer;
begin
  for i := 0 to Length(CommandHandlers)-1 do
    begin
      Result := CommandHandlers[i](Self,FCommand);
      if Result then break;
    end;
end;

constructor TAppNetworkThrd.Create(hsock: tSocket; aId: Integer);
var
  LoggedIn: Boolean;
begin
  inherited create(false);
  Id := aId;
  FObjects := TList.Create;
  Csock := Hsock;
  Close:=False;
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
  try
    NetworkDaemon.Sockets.Remove(Self);
  except
  end;
end;

procedure TAppNetworkThrd.Execute;
var
  s: string;
  CmdIndex : Integer = 0;
begin
  try
    sock:=TTCPBlockSocket.create;
    Sock.socket:=CSock;
    sock.GetSins;
    repeat
      if Terminated then break;
      if not Assigned(BaseApplication) then break;
      if Close then break;
      s := Sock.RecvTerminated(100,CRLF);
      if (Sock.lastError<>0) and (Sock.LastError<>WSAETIMEDOUT) then
        begin
          break;
        end;
      if s <> '' then
        begin
          DoCommand(s);
          inc(CmdIndex);
        end;
    until false;
  finally
    Sock.CloseSocket;
    Sock.Free;
  end;
end;

initialization
  InitCriticalSection(GlobalLock);
finalization
//  DoneCriticalsection(GlobalLock);
  Setlength(CommandHandlers,0);
end.

