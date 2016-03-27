unit uprometpubsub;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TPubSubClient = class
  public
    procedure Publish(Topic,Value : string) : Boolean;
    procedure Subscribe()
  end;

implementation

end.

