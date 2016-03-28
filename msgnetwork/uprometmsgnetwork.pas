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
  uBaseDBInterface,uprometpubsub;
type
  TPrometNetworkDaemon = class(TThread)
  private
    Sock:TTCPBlockSocket;
  public
    Constructor Create;
    Destructor Destroy; override;
    procedure Execute; override;
  end;

  { TPrometNetworkThrd }

  TPrometNetworkThrd = class(TThread)
  private
    Sock:TTCPBlockSocket;
    CSock: TSocket;
    FResult : string;
    DataModule : TBaseDBInterface;
    Pubsub : TPubSubClient;
    procedure DoCommand(FCommand : string);
    function ProcessHttpRequest(Request, URI: string;Input,Output : TMemoryStream): integer;
  protected
    procedure PubsubPublish(const Topic, Value: string);
  public
    Constructor Create (hsock:tSocket);
    destructor Destroy; override;
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

uses Utils,uBaseApplication;

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
  ListenOk: Boolean;
begin
  with sock do
    begin
      CreateSocket;
      setLinger(true,10000);
      ListenOk := False;
      repeat
        if not ListenOk then
          begin
            bind('0.0.0.0','8087');
            if LastError=0 then
              begin
                ListenOk:=True;
                listen;
                if LastError<>0 then
                  ListenOk:=False;
              end;
          end
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
procedure TPrometNetworkThrd.DoCommand(FCommand: string);
var
  aCmd, uri, protocol, s: String;
  headers: TStringList;
  size, Timeout, x, ResultCode, n: Integer;
  InputData, OutputData: TMemoryStream;
begin
  FResult:='ERROR: failed!';
  if pos(' ',FCommand)>0 then
    aCmd := copy(FCommand,0,pos(' ',FCommand)-1)
  else aCmd := FCommand;
  Fetch(FCommand,' ');
  case Uppercase(aCmd) of
  'EXIT','QUIT'://Quit Connection
    begin
      FResult:='OK:Bye!';
      Terminate;
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
      if not sock.SSLAcceptConnection then
        Sock.SendString('This Connection is insecure.'+CRLF)
      else
        Sock.SendString('This Connection is secure now.'+CRLF);
    end;
  'GET','HEAD'://HTTP Request
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
          s := sock.RecvString(Timeout);
          if sock.lasterror <> 0 then
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
        x := Sock.RecvBufferEx(InputData.Memory, Size, Timeout);
        InputData.SetSize(x);
        if sock.lasterror <> 0 then
          Exit;
      end;
      OutputData := TMemoryStream.Create;
      ResultCode := ProcessHttpRequest(aCmd, uri, InputData, OutputData);
      sock.SendString('HTTP/1.0 ' + IntTostr(ResultCode) + CRLF);
      if protocol <> '' then
      begin
        headers.Add('Content-length: ' + IntTostr(OutputData.Size));
        headers.Add('Connection: close');
        headers.Add('Date: ' + Rfc822DateTime(now));
        headers.Add('Server: Avamm Internal Network');
        headers.Add('');
        for n := 0 to headers.count - 1 do
          sock.sendstring(headers[n] + CRLF);
      end;
      if sock.lasterror <> 0 then
        Exit;
      Sock.SendBuffer(OutputData.Memory, OutputData.Size);
      headers.Free;
      FResult:='';
    end;
  'PUB'://Publish Message [GUID,TOPIC,MESSAGE]
    begin
      //Check if we have someone to forward this message
      //Check if we should do something with it (Scripts,Measurements)
      if Pubsub.Publish(copy(FCommand,0,pos(' ',FCommand)-1),copy(FCommand,pos(' ',FCommand)+1,length(FCommand))) then
        FResult:='OK';
    end;
  'SUB'://Subscribe to Topic [TOPIC]
    begin
      Pubsub.Subscribe(FCommand);
      FResult:='OK';
    end;
  'UNSUB'://Unsubscribe from Topic [TOPIC]
    begin
      if Pubsub.UnSubscribe(FCommand) then
        FResult:='OK';
    end;
  'PING':
    begin
      FResult:='PONG';
    end
  else
    begin
      if (copy(aCmd,0,1)='<') and IsNumeric(copy(aCmd,2,pos('>',aCmd)-2)) then
        begin //Syslog Message
          FResult:='ERROR: Syslog at time not implemented';
        end
      else
        FResult:='ERROR: not implemented';
    end;
  end;
end;
function TPrometNetworkThrd.ProcessHttpRequest(Request, URI: string; Input,
  Output: TMemoryStream): integer;
var
  aSL: TStringList;
begin
  Result := 200;
  aSL:= TStringList.Create;
  aSL.Add('<html><body>');
  aSL.Add('Seite: '+URI);
  aSL.Add('</body></html>');
  aSl.SaveToStream(Output);
end;

procedure TPrometNetworkThrd.PubsubPublish(const Topic, Value: string);
begin
  Sock.SendString('PUB:'+Topic+' '+Value+CRLF);
end;

constructor TPrometNetworkThrd.Create(hsock: tSocket);
var
  LoggedIn: Boolean;
begin
  inherited create(false);
  Csock := Hsock;
  FreeOnTerminate:=true;
  DataModule := TBaseDBInterface.Create;
  Pubsub := TPubSubClient.Create;
  Pubsub.OnPublish:=@PubsubPublish;
  DataModule.SetOwner(BaseApplication);
  if not DataModule.LoadMandants then
    raise Exception.Create('failed to Load Mandants');
end;

destructor TPrometNetworkThrd.Destroy;
begin
  Pubsub.Free;
  inherited Destroy;
  DataModule.Free;
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
          DoCommand(s);
          SendString(FResult+CRLF);
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

