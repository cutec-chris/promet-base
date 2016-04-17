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
  Classes, SysUtils, blcksock, synsock, ssl_openssl, synautil, uBaseDbClasses,
  uBaseDBInterface,uprometpubsub,uAppServer;
type

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
  NetworkDaemon : TAppNetworkDaemon;

implementation

uses Utils,uBaseApplication,uprometdataserver;

{ TDiscoveryDaemon }
procedure TPrometDiscoveryDaemon.AddLog;
begin
  Clients.Add(LogData);
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
      asock.SendString(IntToStr(NetworkDaemon.Connections));
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
    Sock.Free;
  end;
end;
function HandlePrometCommand(Sender : TObject;FCommand : string) : string;
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
  'EXIT','QUIT'://Quit Connection
    begin
      Result:='OK:Bye!';
      TAppNetworkThrd(Sender).Terminate;
    end;
  'LOGIN':
    begin
      {
      with DataModule as IBaseDBInterface do
        begin
          DBLogin(Data.Mandant,);
        end;
      }
    end;
  'STARTTLS'://Start SSL
    begin
      if not TAppNetworkThrd(Sender).sock.SSLAcceptConnection then
        TAppNetworkThrd(Sender).Sock.SendString('This Connection is insecure.'+CRLF)
      else
        TAppNetworkThrd(Sender).Sock.SendString('This Connection is secure now.'+CRLF);
    end;
  'PING':
    begin
      Result:='PONG';
    end;
  'SHUTDOWN':
    begin
      Result:='OK';
      BaseApplication.Terminate;
    end
  else
    begin
      if (copy(aCmd,0,1)='<') and IsNumeric(copy(aCmd,2,pos('>',aCmd)-2)) then
        begin //Syslog Message
          Result:='ERROR: Syslog at time not implemented';
        end;
    end;
  end;
end;
{
function TAppNetworkThrd.ProcessHttpRequest(Request, URI: string;Header : TStringList; Input,
  Output: TMemoryStream): integer;
var
  aSL: TStringList;
  Prot, User, Pass, Host, Port, Path, Para: string;
begin
  ParseURL(URI,Prot,User,Pass,Host,Port,Path,Para);
  if copy(Path,0,10)='/objects/' then
    Result := uprometdataserver.HandleHTTPRequest(Request,URI,Header,Input,Output);
  Result := 404;
end;
}

initialization
  NetworkDaemon := TAppNetworkDaemon.Create;
  uAppServer.RegisterCommandHandler(@HandlePrometCommand);
  uAppServer.NetworkDaemon := NetworkDaemon;
  Discovery := TPrometDiscoveryDaemon.Create;
finalization
  Discovery.Terminate;
  NetworkDaemon.Terminate;
  //Discovery.Free;
  //NetworkDaemon.Free;
end.

