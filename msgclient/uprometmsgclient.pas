unit uprometmsgclient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, blcksock, Utils;

type
  TPublishFunction = procedure(Topic,Value : string);

  { TPrometMsgClient }

  TPrometMsgClient = class(TThread)
  private
    FPub: TPublishFunction;
    Sock : TTCPBlockSocket;
    FName : string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
    function Connected : Boolean;
    procedure Log(AppName, aType: string; aMsg: string);
    function Pub(Topic,Value : string) : Boolean;
    function Sub(Topic : string) : Boolean;
    function Receive(Timeout : Integer) : string;
    property OnPublish : TPublishFunction read FPub write FPub;
  end;

implementation

{ TPrometMsgClient }

constructor TPrometMsgClient.Create;
begin
  Sock := TTCPBlockSocket.Create;
  //Sock.ConnectionTimeout:=200;
  Sock.Connect('127.0.0.1','8085');
  FName := GetSystemName; //Sock.ResolveName(Sock.LocalName);
  inherited Create(False);
end;

destructor TPrometMsgClient.Destroy;
begin
  Terminate;
  WaitFor;
  Sock.Free;
  inherited Destroy;
end;

procedure TPrometMsgClient.Execute;
begin
  while not Terminated do
    begin
      if Sock.LastError<>0 then
        Sock.Connect('127.0.0.1','8085');
      sleep(500);
    end;
end;

function TPrometMsgClient.Connected: Boolean;
begin
  Result := Sock.LastError=0;
end;

procedure TPrometMsgClient.Log(AppName,aType: string; aMsg: string);
begin
  Pub('/'+FName+'/'+AppName+'/'+aType,aMsg);
end;

function TPrometMsgClient.Pub(Topic, Value: string): Boolean;
begin
  Sock.SendString('PUB '+Topic+' '+Value+CRLF);
  Result := True;
end;

function TPrometMsgClient.Sub(Topic: string): Boolean;
begin
  Sock.SendString('SUB '+Topic+CRLF);
  Result := Receive(100)='OK';
end;

function TPrometMsgClient.Receive(Timeout: Integer): string;
begin
  Result := Sock.RecvTerminated(Timeout,CRLF);
end;

end.

