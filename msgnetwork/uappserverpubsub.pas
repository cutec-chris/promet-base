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
unit uappserverpubsub;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, synautil, uprometpubsub, uAppServer, blcksock;
type

  { TPubSubHandler }

  TPubSubHandler = class
  public
    Socket : TObject;
    Pubsub : TPubSubClient;
    procedure AfterPublished(const s1,s2: string);
  end;

var
  PubSubHandlers : array of TPubSubHandler;

implementation

function HandlePubSubCommand(Sender : TAppNetworkThrd;FCommand : string) : string;
var
  aCmd, uri, protocol, s: String;
  headers: TStringList;
  size, Timeout, x, ResultCode, n, i: Integer;
  InputData, OutputData: TMemoryStream;
  PubSub : TPubSubHandler = nil;
begin
  try
    for i := 0 to Length(PubSubHandlers)-1 do
      if PubSubHandlers[i].Socket=Sender then
        begin
          PubSub := PubSubHandlers[i];
          break;
        end;
  except
    PubSub:=nil;
  end;
  if PubSub=nil then
    begin
      Pubsub := TPubSubHandler.Create;
      PubSub.Pubsub := TPubSubClient.Create;
      Pubsub.Pubsub.OnPublish:=@PubSub.AfterPublished;
      PubSub.Socket := Sender;
      SetLength(PubSubHandlers,length(PubSubHandlers)+1);
      PubSubHandlers[length(PubSubHandlers)-1] := Pubsub;
      TAppNetworkThrd(Sender).Objects.Add(Pubsub);
    end;
  Result := '';
  if pos(' ',FCommand)>0 then
    aCmd := copy(FCommand,0,pos(' ',FCommand)-1)
  else aCmd := FCommand;
  Fetch(FCommand,' ');
  case Uppercase(aCmd) of
  'PUB'://Publish Message [GUID,TOPIC,MESSAGE]
    begin
      //Check if we have someone to forward this message
      //Check if we should do something with it (Scripts,Measurements)
      if Pubsub.Pubsub.DirectPublish(copy(FCommand,0,pos(' ',FCommand)-1),copy(FCommand,pos(' ',FCommand)+1,length(FCommand))) then
        Result:='OK';
    end;
  'SUB'://Subscribe to Topic [TOPIC]
    begin
      Pubsub.Pubsub.Subscribe(FCommand);
      Result:='OK';
    end;
  'UNSUB'://Unsubscribe from Topic [TOPIC]
    begin
      if Pubsub.Pubsub.UnSubscribe(FCommand) then
        Result:='OK';
    end;
  end;

end;

{ TPubSubHandler }

procedure TPubSubHandler.AfterPublished(const s1, s2: string);
begin
  TAppNetworkThrd(Socket).Sock.SendString('PUB '+s1+' '+s2+CRLF);
end;

initialization
  uAppServer.RegisterCommandHandler(@HandlePubSubCommand);
finalization
  Setlength(PubSubHandlers,0);
end.

