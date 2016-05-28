unit uprometmsgclient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, blcksock, Utils;

type

  { TPrometMsgClient }

  TPrometMsgClient = class(TThread)
  private
    Sock : TTCPBlockSocket;
    FName : string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
    procedure Log(AppName, aType: string; aMsg: string);
  end;

implementation

{ TPrometMsgClient }

constructor TPrometMsgClient.Create;
begin
  Sock := TTCPBlockSocket.Create;
  Sock.ConnectionTimeout:=200;
  Sock.Connect('localhost','8087');
  FName := GetSystemName; //Sock.ResolveName(Sock.LocalName);
  inherited Create(False);
end;

destructor TPrometMsgClient.Destroy;
begin
  Terminate;
  WaitFor;
  inherited Destroy;
end;

procedure TPrometMsgClient.Execute;
begin
  while not Terminated do
    begin
      if Sock.LastError<>0 then
        Sock.Connect('localhost','8087');
      sleep(500);
    end;
end;

procedure TPrometMsgClient.Log(AppName,aType: string; aMsg: string);
begin
  Sock.SendString('PUB /'+FName+'/'+AppName+'/'+aType+' '+aMsg+CRLF);
end;

end.

