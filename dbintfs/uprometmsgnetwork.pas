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
Created 04.03.2016
*******************************************************************************}
unit uprometmsgnetwork;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, blcksock, synsock;
type
  TPrometNetworkDaemon = class(TThread)
  private
    Sock:TTCPBlockSocket;
  public
    Constructor Create;
    Destructor Destroy; override;
    procedure Execute; override;
  end;

  TPrometNetworkThrd = class(TThread)
  private
    Sock:TTCPBlockSocket;
    CSock: TSocket;
  public
    Constructor Create (hsock:tSocket);
    procedure Execute; override;
  end;

  { TPrometDiscoveryDaemon }

  TPrometDiscoveryDaemon = class(TThread)
  private
    Sock: TUDPBlockSocket;
    LogData: string;
    OwnIPAddr: String;
    procedure AddLog;
  public
    Clients: TStringList;
    constructor Create;
    destructor Destroy; override;
    procedure SendDiscover;
    procedure Execute; override;
  end;

var
  Discovery : TPrometDiscoveryDaemon;
  NetworkDaemon : TPrometNetworkDaemon;

implementation

Constructor TPrometNetworkDaemon.Create;
begin
  inherited create(false);
  sock:=TTCPBlockSocket.create;
  FreeOnTerminate:=true;
end;

Destructor TPrometNetworkDaemon.Destroy;
begin
  Sock.free;
end;

procedure TPrometNetworkDaemon.Execute;
var
  ClientSock:TSocket;
begin
  with sock do
    begin
      CreateSocket;
      setLinger(true,10000);
      bind('0.0.0.0','8087');
      listen;
      repeat
        if LastError<>0 then Listen
        else
          begin
            if terminated then break;
            if canread(1000) then
              begin
                ClientSock:=accept;
                if lastError=0 then TPrometNetworkThrd.create(ClientSock);
              end;
          end;
      until false;
    end;
end;

Constructor TPrometNetworkThrd.Create(Hsock:TSocket);
begin
  inherited create(false);
  Csock := Hsock;
  FreeOnTerminate:=true;
end;

procedure TPrometNetworkThrd.Execute;
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
          s := RecvTerminated(60000,CRLF);
          if lastError<>0 then break;
          SendString(s);
          if lastError<>0 then break;
        until false;
      end;
  finally
    Sock.Free;
  end;
end;

{ TDiscoveryDaemon }

procedure TPrometDiscoveryDaemon.AddLog;
begin

end;

constructor TPrometDiscoveryDaemon.Create;
begin
  SendDiscover;
  Clients := TStringList.Create;
  Sock := TUDPBlockSocket.Create;
  Sock.Family := SF_IP4;
  FreeOnTerminate := False;
  Priority := tpNormal;
  inherited Create(False);
end;

destructor TPrometDiscoveryDaemon.Destroy;
begin
  inherited Destroy;
  Sock.Free;
  Clients.Free;
end;

procedure TPrometDiscoveryDaemon.SendDiscover;
var
  asock: TUDPBlockSocket;
begin
  asock := TUDPBlockSocket.Create;
  try
    asock.Family := SF_IP4;
    OwnIPAddr := asock.ResolveName(asock.LocalName);
    asock.CreateSocket();
    asock.Bind('0.0.0.0', '0');
    asock.MulticastTTL := 1;
    asock.Connect('234.5.6.7', '22402');
    if asock.LastError = 0 then
      asock.SendString(OwnIPAddr+'=');
  finally
    asock.Free;
  end;
end;

procedure TPrometDiscoveryDaemon.Execute;
begin
  try
    Sock.CreateSocket();
    Sock.EnableReuse(True);
    // better to use MyIP(not to use INADDR_ANY). Because a problem occurs in Windows7.
    Sock.Bind(OwnIPAddr, '22402');
    Sock.AddMulticast('234.5.6.7');
    while not Terminated do
      begin
        LogData := Sock.RecvPacket(1000);
        LogData := Sock.GetRemoteSinIP + ': ' + LogData;
        if Sock.LastError = 0 then
          Synchronize(@AddLog);
      end;
  finally
  end;
end;

initialization
  Discovery := TPrometDiscoveryDaemon.Create;
  NetworkDaemon := TPrometNetworkDaemon.Create;
finalization
  Discovery.Free;
  NetworkDaemon.Free;
end.

