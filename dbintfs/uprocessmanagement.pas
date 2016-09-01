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
*******************************************************************************}
unit uProcessManagement;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils,uBaseDbClasses,db,Process,uBaseDatasetInterfaces,syncobjs,
  uprometpubsub;
type

  { TProcProcess }

  TProcProcess = class(TProcess)
  private
    FId: Variant;
    FInformed: Boolean;
    FName: string;
    FOutput: string;
    FStarted: TDateTime;
    FStatus: string;
    FStopped: TDateTime;
    FTimeout: TDateTime;
    function GetOutput: string;
    procedure SetTimeout(AValue: TDateTime);
  public
    aOutput,aBuffer,aLogOutput : string;
    constructor Create(AOwner: TComponent); override;
    property Informed : Boolean read FInformed write FInformed;
    property Name : string read FName write FName;
    property Id : Variant read FId write FId;
    procedure Execute; override;
    procedure CheckStatus;
    property Status : string read FStatus;
    property Started : TDateTime read FStarted;
    property Stopped : TDateTime read FStopped;
    property ProcOutput : string read GetOutput;
  end;
  TProcessParameters = class(TBaseDBDataset)
  public
    procedure DefineFields(aDataSet: TDataSet); override;
  end;

  { TProcesses }

  TProcesses = class(TBaseDBDataset)
  private
    FProcessParameters: TProcessParameters;
    FScripts: TBaseDBDataset;
    function GetScripts: TBaseDBDataset;
  public
    constructor CreateEx(aOwner: TComponent; DM: TComponent;
      aConnection: TComponent=nil; aMasterdata: TDataSet=nil); override;
    procedure Open; override;
    destructor Destroy; override;
    function CreateTable : Boolean;override;
    procedure DefineFields(aDataSet: TDataSet); override;
    procedure FillDefaults(aDataSet: TDataSet); override;
    property Parameters : TProcessParameters read FProcessParameters;
    property Scripts : TBaseDBDataset read GetScripts;
  end;

  { TProcessClient }

  TProcessClient = class(TBaseDBDataset)
  private
    FLastRefresh: TDateTime;
    FProcesses: TProcesses;
    ProcessData : array of TProcProcess;
    Pubsub: TPubSubClient;
  public
    constructor CreateEx(aOwner: TComponent; DM: TComponent;
       aConnection: TComponent=nil; aMasterdata: TDataSet=nil); override;
    destructor Destroy; override;
    function CreateTable : Boolean;override;
    procedure DefineFields(aDataSet: TDataSet); override;
    property Processes : TProcesses read FProcesses;
    property LastRefresh : TDateTime read FLastRefresh;
    procedure RefreshList;
    procedure Startup;
    procedure ShutDown;
    function ProcessAll(aSystem : string = '') : Boolean;
    procedure RefreshStatus(aProc: TProcProcess; aLog: TStringList);
    function Process(OnlyActiveRow : Boolean = False;DoAlwasyRun : Boolean = False) : Boolean;
  end;

implementation
uses uBaseDBInterface,uData,Utils,uBaseApplication,uIntfStrConsts,math,
  uprometscripts;

procedure TProcProcess.SetTimeout(AValue: TDateTime);
begin
  if FTimeout=AValue then Exit;
  FTimeout:=AValue;
end;

constructor TProcProcess.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Options := [poUsePipes, poStdErrToOutPut, poNewConsole];
  ShowWindow:=swoHIDE;
end;

function TProcProcess.GetOutput: string;
var
  OutputLine: String;
  Buf: string;
  Count,LineStart: LongInt;
begin
  OutputLine:='';
  if (not Active) then
    begin
      SetLength(Buf,Output.NumBytesAvailable);
      repeat
        if (Output<>nil) then
          begin
            Count:=Output.Read(Buf[1],length(Buf));
          end
        else Count:=0;
        FOutput:=FOutput+copy(Buf,0,Count);
      until (Count=0);
    end;
  Result := FOutput;
end;

procedure TProcProcess.Execute;
begin
  FStopped:=0;
  FStatus:='R';
  FOutput:='';
  FStarted:=Now();
  try
    inherited Execute;
  except
    FStatus := 'E';
  end;
end;

procedure TProcProcess.CheckStatus;
begin
  if not Active then
    begin
      if ExitCode<>0 then
        FStatus:='E'
      else FStatus:='N';
      FStopped:=Now();
    end;
end;

procedure TProcessParameters.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'PROCESSPARAMETERS';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('NAME',ftString,60,True);
            Add('VALUE',ftString,60,False);
          end;
    end;
end;

function TProcesses.GetScripts: TBaseDBDataset;
begin
  if not FScripts.Active then
    FScripts.Open;
  Result := FScripts;
end;

constructor TProcesses.CreateEx(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited;
  FProcessParameters := TProcessParameters.CreateEx(Self, DM,aConnection,DataSet);
  FScripts := TBaseScript.CreateEx(Self,DM,aConnection);
end;

procedure TProcesses.Open;
begin
  inherited Open;
end;

destructor TProcesses.Destroy;
begin
  FScripts.Free;
  FProcessParameters.Destroy;
  inherited Destroy;
end;

function TProcesses.CreateTable : Boolean;
begin
  Result := inherited CreateTable;
  FProcessParameters.CreateTable;
end;

procedure TProcesses.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'PROCESS';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('ACTIVE',ftString,1,False);
            Add('NAME',ftString,250,True);
            Add('INTERVAL',ftInteger,0,False);
            Add('STATUS',ftString,4,False);
            Add('STARTED',ftDateTime,0,False);
            Add('STOPPED',ftDateTime,0,False);
            Add('CLIENT',ftString,100,False);
            Add('LOG',ftMemo,0,False);
          end;
    end;
end;

procedure TProcesses.FillDefaults(aDataSet: TDataSet);
begin
  inherited FillDefaults(aDataSet);
  aDataSet.FieldByName('STATUS').AsString := 'N';
  aDataSet.FieldByName('ACTIVE').AsString := 'Y';
end;

constructor TProcessClient.CreateEx(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited;
  FProcesses := TProcesses.CreateEx(Self, DM,aConnection,DataSet);
  Pubsub:=TPubSubClient.Create;
end;

destructor TProcessClient.Destroy;
var
  i: Integer;
begin
  Pubsub.Free;
  FProcesses.Destroy;
  inherited Destroy;
end;

function TProcessClient.CreateTable : Boolean;
begin
  Result := inherited CreateTable;
  FProcesses.CreateTable;
end;

procedure TProcessClient.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'PROCESSCLIENTS';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('NAME',ftString,60,True);
            Add('STATUS',ftString,4,True);
            Add('NOTES',ftString,200,False);
          end;
    end;
end;

procedure TProcessClient.RefreshList;
var
  aNow: TDateTime;
begin
  aNow := Now();
  if aNow>0 then
    begin
      //Refresh all Minute
      if aNow>(LastRefresh+((1/SecsPerDay)*60)) then
        begin
          DataSet.Refresh;
          Processes.DataSet.Refresh;
          FLastRefresh:=Now();
        end;
    end;
end;

procedure TProcessClient.Startup;
begin
  CreateTable;
  Open;
  if not DataSet.Locate('NAME',GetSystemName,[]) then
    begin
      Insert;
      DataSet.FieldByName('NAME').AsString:=GetSystemName;
      DataSet.FieldByName('STATUS').AsString:='R';
      DataSet.Post;
    end
  else
    begin
      DataSet.Edit;
      DataSet.FieldByName('STATUS').AsString:='R';
      DataSet.Post;
    end;
end;

procedure TProcessClient.ShutDown;
var
  i: Integer;
begin
  try
    for i := 0 to length(ProcessData)-1 do
      ProcessData[i].Free;
    SetLength(ProcessData,0);
    if DataSet.Locate('NAME',GetSystemName,[]) then
      begin
        DataSet.Edit;
        DataSet.FieldByName('STATUS').AsString:='N';
        DataSet.Post;
      end;
  except
  end;
end;

function TProcessClient.ProcessAll(aSystem: string): Boolean;
begin
  Result := True;
  if aSystem='' then
    aSystem:=GetSystemName;
  Open;
  Processes.Open;
  Processes.Parameters.Open;
  if not Active then exit;
  if Locate('NAME','*',[]) then
    Process
  else
    begin
      Insert;
      FieldByName('NAME').AsString:='*';
      FieldByName('STATUS').AsString:='N';
      FieldByName('NOTES').AsString:=strRunsOnEveryMashine;
      Post;
      Process
    end;
  if DataSet.Locate('NAME',aSystem,[]) then
    begin
      if FieldByName('STATUS').AsString = '' then
        begin
          Edit;
          FieldByName('STATUS').AsString := 'R';
          Post;
        end;
      if FieldByName('STATUS').AsString = 'N' then
        begin
          Result := False;
          exit;
        end;
      Process;
    end;
end;

procedure TProcessClient.RefreshStatus(aProc: TProcProcess;aLog : TStringList);
begin
  if Processes.Locate('SQL_ID',aProc.Id,[]) then
    begin
      try
        if Processes.FieldByName('STATUS').AsString<>aProc.Status then
          begin
            Processes.Edit;
            Processes.FieldByName('STATUS').AsString:=aProc.Status;
            Processes.FieldByName('STARTED').AsDateTime:=aProc.Started;
            if (not aProc.Active) then
              Processes.FieldByName('STOPPED').AsDateTime:=aProc.Stopped
            else
              Processes.FieldByName('STOPPED').Clear;
            if aProc.Status='R' then
              Processes.FieldByName('LOG').Clear;
            Processes.FieldByName('CLIENT').AsString:=GetSystemName;
          end;
        if (not aProc.Active) and (aProc.ProcOutput<>'') then
          begin
            aLog.Text:=Processes.FieldByName('LOG').AsString;
            aLog.Add(TimeToStr(Now()));
            aLog.Text:=aLog.Text+aProc.ProcOutput;
            while aLog.Count>30 do aLog.Delete(0);
            Processes.Edit;
            Processes.FieldByName('LOG').AsString:=aLog.Text;
            aLog.Clear;
          end;
        Processes.Post;
      finally
      end;
    end;
end;

function ExpandFileName(aDir : string) : string;
begin
  Result := aDir;
end;

function TProcessClient.Process(OnlyActiveRow: Boolean; DoAlwasyRun: Boolean
  ): Boolean;
var
  aLog: TStringList;
  aProcess: String;
  Found: Boolean;
  cmd: String;
  bProcess: TProcProcess;
  sl: TStringList;
  aNow: TDateTime;
  NewProcess: TProcProcess;
  aCount: DWord;
  tmp: String;
  bId : Variant;
  procedure DoLog(aStr: string;bLog : TStringList;SysLog : Boolean);
  begin
    with BaseApplication as IBaseApplication do
      if Syslog then
        Log(TimeToStr(Now())+':'+aStr);
    if bLog.Count>100 then
      bLog.Clear;
    bLog.Add(TimeToStr(Now())+':'+SysToUni(aStr));
  end;
  function BuildCmdLine : string;
  begin
    with Processes.Parameters.DataSet do
      begin
        First;
        while not EOF do
          begin
            cmd := cmd+' "--'+FieldByName('NAME').AsString+'='+FieldByName('VALUE').AsString+'"';
            Next;
          end;
      end;
    if pos('--mandant',lowercase(cmd)) = 0 then
      cmd := cmd+' "--mandant='+BaseApplication.GetOptionValue('m','mandant')+'"';
    if Data.Users.DataSet.Active then
      cmd := cmd+' "--user='+Data.Users.FieldByName('NAME').AsString+'"';
    if BaseApplication.HasOption('c','config-path') then
      cmd := cmd+' "--config-path='+BaseApplication.GetOptionValue('c','config-path')+'"';
  end;
  function ExecCommand(aClient : string;aNewStatus : string;aLastStopped : TDateTime;aInterval : Integer) : TProcProcess;
  var
    i: Integer;
    a: Integer;
    aStartTime: TDateTime;
    arec: LargeInt;
    aStatus: String;
  begin
    Result := nil;
    if aNewStatus='' then exit;
    Found := False;
    for i := 0 to length(ProcessData)-1 do
      if Assigned(ProcessData[i]) and (ProcessData[i].CommandLine = cmd) then
        begin
          bProcess := ProcessData[i];
          Result := bProcess;
          if bProcess.Active then
            begin
              Found := True;
              sl := TStringList.Create;
              sl.Text:=bProcess.ProcOutput;
              while sl.Count>0 do
                begin
                  DoLog(aprocess+':'+sl[0],aLog,BaseApplication.HasOption('log'));
                  Pubsub.Publish('/processes/'+aProcess,sl[0]);
                  sl.Delete(0);
                end;
              sl.Free;
              if aNewStatus='N' then
                begin
                  Pubsub.Publish('/processes/'+aProcess,'Terminating process');
                  bProcess.Terminate(1);
                  RefreshStatus(bProcess,aLog);
                  FreeAndNil(bProcess);
                end
            end
          else
            begin
              aLog.Clear;
              aStartTime := Now();
              if aStartTime=0 then
                aStartTime:=Now();
              sl := TStringList.Create;
              sl.Text:=bProcess.ProcOutput;
              while sl.Count>0 do
                begin
                  DoLog(aprocess+':'+sl[0],aLog,BaseApplication.HasOption('log'));
                  Pubsub.Publish('/processes/'+aProcess,sl[0]);
                  sl.Delete(0);
                end;
              sl.Free;
              if not bProcess.Informed then
              else if Assigned(Processes) then
                if ((aNewStatus<>'R')
                and (aNow > (aLastStopped+(aInterval/MinsPerDay))))
                 or DoAlwasyRun then
                  begin
                    DoLog(aprocess+':'+strStartingProcessTimeout+' '+DateTimeToStr((aLastStopped+(max(aInterval,2)/MinsPerDay)))+'>'+DateTimeToStr(aNow),aLog,BaseApplication.HasOption('debug'));
                    DoLog(aProcess+':'+strStartingProcess+' ('+bProcess.CommandLine+')',aLog,True);
                    Pubsub.Publish('/processes/'+aProcess,'starting ('+bProcess.CommandLine+')');
                    bProcess.Informed:=False;
                    bProcess.Execute;
                    bProcess.Informed := False;
                  end;
              Found := True;
            end;
        end;
    if not Found then
      begin
        //Process is stopped and Timeout is overdune
        if (((aNewStatus<>'R') or (Processes.TimeStamp.AsDateTime<(aNow-(1)))) and (aNow > (aLastStopped+(aInterval/MinsPerDay))))
         //Process should always run
         or DoAlwasyRun
         //Process should be running on our client but were dont run it
         or ((aNewStatus='R') and (aClient=GetSystemName))
         then
          begin
            aStartTime := Now();
            aLog.Clear;
            DoLog(aProcess+':'+strStartingProcess+' ('+cmd+')',aLog,True);
            Pubsub.Publish('/processes/'+aProcess,'starting ('+cmd+')');
            NewProcess := TProcProcess.Create(nil);
            {$if FPC_FULLVERSION<20400}
            NewProcess.InheritHandles := false;
            {$endif}
            NewProcess.Id := Processes.Id.AsVariant;
            NewProcess.Informed:=False;
            NewProcess.Name:=aProcess;
            for i := 0 to Length(ProcessData)-1 do
              if ProcessData[i]=nil then break;
            if (length(ProcessData)<=i) or (not (ProcessData[i]=nil)) then
              begin
                Setlength(ProcessData,length(ProcessData)+1);
                i := length(ProcessData)-1;
              end;
            ProcessData[i] := NewProcess;
            NewProcess.CommandLine:=cmd;
            NewProcess.Execute;
            Result := NewProcess;
          end;
      end;
  end;

  procedure ProcessRow;
  var
    aProc: TProcProcess = nil;
    i: Integer;
  begin
    aLog.Clear;
    aProcess := Processes.FieldByName('NAME').AsString;
    if Processes.FieldByName('ACTIVE').AsString<>'N' then
      begin
        if FileExists(ExpandFileName(AppendPathDelim(BaseApplication.Location)+aProcess+ExtractFileExt(BaseApplication.ExeName))) then
          begin
            Found := False;
            cmd := AppendPathDelim(BaseApplication.Location)+aProcess+ExtractFileExt(BaseApplication.ExeName);
            cmd := cmd+BuildCmdLine;
            aProc := ExecCommand(Processes.FieldByName('CLIENT').AsString,Processes.FieldByName('STATUS').AsString,Processes.FieldByName('STOPPED').AsDateTime,Processes.FieldByName('INTERVAL').AsInteger);
          end
        else if FileExists(aProcess+ExtractFileExt(BaseApplication.ExeName)) then
          begin
            Found := False;
            cmd := aProcess+ExtractFileExt(BaseApplication.ExeName);
            cmd := cmd+BuildCmdLine;
            aProc := ExecCommand(Processes.FieldByName('CLIENT').AsString,Processes.FieldByName('STATUS').AsString,Processes.FieldByName('STOPPED').AsDateTime,Processes.FieldByName('INTERVAL').AsInteger);
          end
        else if FileExists('/usr/bin/'+aProcess+ExtractFileExt(BaseApplication.ExeName)) then
          begin
            Found := False;
            cmd := '/usr/bin/'+aProcess+ExtractFileExt(BaseApplication.ExeName);
            cmd := cmd+BuildCmdLine;
            aProc := ExecCommand(Processes.FieldByName('CLIENT').AsString,Processes.FieldByName('STATUS').AsString,Processes.FieldByName('STOPPED').AsDateTime,Processes.FieldByName('INTERVAL').AsInteger);
          end
        else if FileExists('/usr/local/bin/'+aProcess+ExtractFileExt(BaseApplication.ExeName)) then
          begin
            Found := False;
            cmd := '/usr/local/bin/'+aProcess+ExtractFileExt(BaseApplication.ExeName);
            cmd := cmd+BuildCmdLine;
            aProc := ExecCommand(Processes.FieldByName('CLIENT').AsString,Processes.FieldByName('STATUS').AsString,Processes.FieldByName('STOPPED').AsDateTime,Processes.FieldByName('INTERVAL').AsInteger);
          end
        else if Processes.Scripts.Locate('NAME',aProcess,[loCaseInsensitive]) then
          begin
            cmd := AppendPathDelim(BaseApplication.Location)+'pscript'+ExtractFileExt(BaseApplication.ExeName);
            if not FileExists(UniToSys(cmd)) then
              cmd := AppendPathDelim(BaseApplication.Location)+DirectorySeparator+'pscript'+ExtractFileExt(BaseApplication.ExeName);
            if FileExists(UniToSys(cmd)) then
              begin
                cmd := cmd+BuildCmdLine;
                cmd := cmd+' '+aProcess;
                aProc := ExecCommand(Processes.FieldByName('CLIENT').AsString,Processes.FieldByName('STATUS').AsString,Processes.FieldByName('STOPPED').AsDateTime,Processes.FieldByName('INTERVAL').AsInteger);
              end
            else
              begin
                DoLog(cmd+':'+'PScript dosend exists',aLog,True);
                try
                  Processes.DataSet.Edit;
                  Processes.DataSet.FieldByName('STATUS').AsString := 'E';
                  Processes.DataSet.FieldByName('STOPPED').AsDateTime:=Processes.DataSet.FieldByName('STARTED').AsDateTime;
                  if Processes.DataSet.FieldByName('LOG').AsString<>aLog.Text then
                    Processes.DataSet.FieldByName('LOG').AsString := aLog.Text;
                  Processes.DataSet.Post;
                finally
                end;
              end;
          end
        else
          begin
            DoLog(ExpandFileName(aProcess+ExtractFileExt(BaseApplication.ExeName))+':'+'File dosend exists',aLog,True);
            try
              Processes.DataSet.Edit;
              Processes.DataSet.FieldByName('STATUS').AsString := 'E';
              Processes.DataSet.FieldByName('STOPPED').AsDateTime:=Processes.DataSet.FieldByName('STARTED').AsDateTime;
              if Processes.DataSet.FieldByName('LOG').AsString<>aLog.Text then
                Processes.DataSet.FieldByName('LOG').AsString := aLog.Text;
              Processes.DataSet.Post;
            finally
            end;
          end;
        //RefreshStatus
        if Assigned(aProc) then
          begin
            //if not aProc.Informed then
            //  DoLog(aprocess+':'+strExitted,aLog,True);
            aProc.CheckStatus;
            RefreshStatus(aProc,aLog);
            if (not aProc.Active) and (aProc.Informed) then
              begin
                for i := 0 to length(ProcessData)-1 do
                  if ProcessData[i]=aProc then
                    begin
                      ProcessData[i]:=nil;
                      FreeAndNil(aProc);
                    end;
              end;
            if Assigned(aProc) and (not aProc.Informed) then
              aProc.Informed := True;
          end;
      end;
  end;

begin
  aLog := TStringList.Create;
  try
    aNow := Now();
    if aNow>0 then
      begin
        //Check processes
        if OnlyActiveRow then
          ProcessRow
        else
          begin
            RefreshList;
            Processes.DataSet.First;
            while not Processes.DataSet.EOF do
              begin
                ProcessRow;
                Processes.DataSet.Next;
              end;
          end;
      end;
  finally
    aLog.Free;
  end;
end;

end.

