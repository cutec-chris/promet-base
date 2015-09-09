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
  Classes, SysUtils,uBaseDbClasses,db,Process,uBaseDatasetInterfaces;
type
  TProcProcess = class(TThread)
  private
    FActive: Boolean;
    FCommandline: string;
    FId: Variant;
    FInformed: Boolean;
    FName: string;
    FOutput: TStringList;
    FTimeout: TDateTime;
    FStatus : string;
    OutputLine: String;
    procedure DoSetStatus;
    procedure SetActive(AValue: Boolean);
    procedure SetTimeout(AValue: TDateTime);
    procedure DoOutputLine;
  public
    aOutput,aBuffer,aLogOutput : string;
    constructor Create;
    destructor Destroy; override;
    property Informed : Boolean read FInformed write FInformed;
    property Name : string read FName write FName;
    property Id : Variant read FId write FId;
    procedure Start;
    property Active : Boolean read FActive write SetActive;
    procedure Execute; override;
    property Commandline : string read FCommandline write FCommandline;
    property Output : TStringList read FOutput;
  end;
  TProcessParameters = class(TBaseDBDataset)
  public
    procedure DefineFields(aDataSet: TDataSet); override;
  end;
  TProcesses = class(TBaseDBDataset)
  private
    FProcessParameters: TProcessParameters;
    FScripts: TBaseDBDataset;
  public
    constructor CreateEx(aOwner: TComponent; DM: TComponent;
      aConnection: TComponent=nil; aMasterdata: TDataSet=nil); override;
    procedure Open; override;
    destructor Destroy; override;
    function CreateTable : Boolean;override;
    procedure DefineFields(aDataSet: TDataSet); override;
    property Parameters : TProcessParameters read FProcessParameters;
    property Scripts : TBaseDBDataset read FScripts;
  end;

  { TProcessClient }

  TProcessClient = class(TBaseDBDataset)
  private
    FLastRefresh: TDateTime;
    FProcesses: TProcesses;
    ProcessData : array of TProcProcess;
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
    function ProcessAll : Boolean;
    function Process(OnlyActiveRow : Boolean = False;DoAlwasyRun : Boolean = False) : Boolean;
  end;

implementation
uses uBaseDBInterface,uData,Utils,uBaseApplication,uIntfStrConsts,math,
  uprometscripts;

procedure TProcProcess.DoSetStatus;
var
  aProc: TProcesses;
begin
  aProc := uProcessManagement.TProcesses.CreateEx(nil,Data);
  aProc.Select(Id);
  aProc.Open;
  if aProc.Count > 0 then
    begin
      aProc.DataSet.Edit;
      aProc.FieldByName('STATUS').AsString:=FStatus;
      if FStatus='R' then
        begin
          aProc.FieldByName('STARTED').AsDateTime:=Now;
          aProc.FieldByName('STOPPED').Clear;
        end
      else
        begin
          aProc.FieldByName('STOPPED').AsDateTime:=Now;
        end;
      aProc.DataSet.Post;
    end;
  aProc.Free;
end;

procedure TProcProcess.SetActive(AValue: Boolean);
begin
  if FActive=AValue then Exit;
  FActive:=AValue;
  if FActive then Resume;
end;

procedure TProcProcess.SetTimeout(AValue: TDateTime);
begin
  if FTimeout=AValue then Exit;
  FTimeout:=AValue;
end;

procedure TProcProcess.DoOutputLine;
begin
  FOutput.Add(OutputLine);
end;

constructor TProcProcess.Create;
begin
  FOutput:=TStringList.Create;
  inherited Create(True);
end;

destructor TProcProcess.Destroy;
begin
  inherited Destroy;
  FOutput.Free;
end;

procedure TProcProcess.Start;
begin
  Resume;
end;

procedure TProcProcess.Execute;
var
  p: TProcess;
  Buf: string;
  Count,LineStart: LongInt;
  i: Integer;
const
  BufSize = 1024; //4096;
begin
  FStatus:='R';
  FActive:=True;
  Synchronize(@DoSetStatus);
  p := TProcess.Create(nil);
  p.Options := [poUsePipes, poStdErrToOutPut, poNoConsole];
  //p.ShowWindow := swoHIDE;
//  AddMessage('Compile command: ' + c);
  p.CommandLine := FCommandline;
  p.CurrentDirectory:= copy(BaseApplication.Location,0,rpos(DirectorySeparator,BaseApplication.Location)-1);
  try
    try
      p.Execute;

      { Now process the output }
      OutputLine:='';
      SetLength(Buf,BufSize);
      repeat
        if (p.Output<>nil) then
        begin
          Count:=p.Output.Read(Buf[1],Length(Buf));
        end
        else
          Count:=0;
        LineStart:=1;
        i:=1;
        while i<=Count do
        begin
          if Buf[i] in [#10,#13] then
          begin
            OutputLine:=OutputLine+Copy(Buf,LineStart,i-LineStart);
            Synchronize(@DoOutputLine);
            OutputLine:='';
            if (i<Count) and (Buf[i+1] in [#10,#13]) and (Buf[i]<>Buf[i+1]) then
              inc(i);
            LineStart:=i+1;
          end;
          inc(i);
        end;
        OutputLine:=Copy(Buf,LineStart,Count-LineStart+1);
      until (Count=0) or Terminated;
      if OutputLine <> '' then
        Synchronize(@DoOutputLine);
      if not  Terminated then p.WaitOnExit;
      FStatus:='N';
      Synchronize(@DoSetStatus);
    except
      FStatus:='E';
      Synchronize(@DoSetStatus);
    end;
  finally
    FreeAndNil(p);
  end;
  FActive:=False;
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
  FScripts.Open;
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
            Add('NAME',ftString,250,True);
            Add('INTERVAL',ftInteger,0,False);
            Add('STATUS',ftString,4,False);
            Add('STARTED',ftDateTime,0,False);
            Add('STOPPED',ftDateTime,0,False);
            Add('LOG',ftMemo,0,False);
          end;
    end;
end;

constructor TProcessClient.CreateEx(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited;
  FProcesses := TProcesses.CreateEx(Self, DM,aConnection,DataSet);
end;

destructor TProcessClient.Destroy;
var
  i: Integer;
begin
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
begin
  try
    if DataSet.Locate('NAME',GetSystemName,[]) then
      begin
        DataSet.Edit;
        DataSet.FieldByName('STATUS').AsString:='N';
        DataSet.Post;
      end;
  except
  end;
end;

function TProcessClient.ProcessAll: Boolean;
begin
  Result := True;
  Open;
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
  if DataSet.Locate('NAME',GetSystemName,[]) then
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
  procedure ExecCommand;
  var
    i: Integer;
    a: Integer;
    aStartTime: TDateTime;
  begin
    Found := False;
    for i := 0 to length(ProcessData)-1 do
      if ProcessData[i].CommandLine = cmd then
        begin
          bProcess := ProcessData[i];
          if bProcess.Active then
            begin
              Found := True;
              while bProcess.Output.Count>0 do
                begin
                  DoLog(aprocess+':'+bProcess.Output[0],aLog,BaseApplication.HasOption('debug'));
                  bProcess.Output.Delete(0);
                end;
            end
          else
            begin
              aStartTime := Now();
              if aStartTime=0 then
                aStartTime:=Now();
              while bProcess.Output.Count>0 do
                begin
                  DoLog(aprocess+':'+bProcess.Output[0],aLog,BaseApplication.HasOption('debug'));
                  bProcess.Output.Delete(0);
                end;
              if not bProcess.Informed then
                begin
                  DoLog(aprocess+':'+strExitted,aLog,True);
                  if Processes.DataSet.FieldByName('LOG').AsString<>aLog.Text then
                    begin
                      if not Processes.CanEdit then Processes.DataSet.Edit;
                      Processes.DataSet.FieldByName('LOG').AsString:=aLog.Text;
                      Processes.DataSet.Post;
                    end;
                  bProcess.Informed := True;
                end;
              if Assigned(Processes) then
                if (aNow > (Processes.FieldByName('STOPPED').AsDateTime+(Processes.FieldByName('INTERVAL').AsInteger/MinsPerDay))) or DoAlwasyRun then
                  begin
                    aLog.Clear;
                    DoLog(aprocess+':'+strStartingProcessTimeout+' '+DateTimeToStr((Processes.FieldByName('STOPPED').AsDateTime+(max(Processes.FieldByName('INTERVAL').AsInteger,2)/MinsPerDay)))+'>'+DateTimeToStr(aNow),aLog,BaseApplication.HasOption('debug'));
                    DoLog(aProcess+':'+strStartingProcess+' ('+bProcess.CommandLine+')',aLog,True);
                    bProcess.Informed:=False;
                    bProcess.Start;
                    bProcess.Informed := False;
                  end;
              Found := True;
            end;
        end;
    if not Found then
      begin
        if (aNow > (Processes.FieldByName('STOPPED').AsDateTime+(Processes.FieldByName('INTERVAL').AsInteger/MinsPerDay))) or DoAlwasyRun then
          begin
            aStartTime := Now();
            aLog.Clear;
            DoLog(aProcess+':'+strStartingProcess+' ('+cmd+')',aLog,True);
            NewProcess := TProcProcess.Create;
            {$if FPC_FULLVERSION<20400}
            NewProcess.InheritHandles := false;
            {$endif}
            NewProcess.Id := Processes.Id.AsVariant;
            NewProcess.Informed:=False;
            Setlength(ProcessData,length(ProcessData)+1);
            ProcessData[length(ProcessData)-1] := NewProcess;
            NewProcess.CommandLine:=cmd;
            NewProcess.Start;
          end;
      end;
  end;

  procedure ProcessRow;
  begin
    aLog.Text := Processes.DataSet.FieldByName('LOG').AsString;
    aProcess := Processes.FieldByName('NAME').AsString;
    if FileExists(ExpandFileName(AppendPathDelim(BaseApplication.Location)+aProcess+ExtractFileExt(BaseApplication.ExeName))) then
      begin
        Found := False;
        cmd := AppendPathDelim(BaseApplication.Location)+aProcess+ExtractFileExt(BaseApplication.ExeName);
        cmd := cmd+BuildCmdLine;
        ExecCommand;
      end
    else if FileExists(aProcess+ExtractFileExt(BaseApplication.ExeName)) then
      begin
        Found := False;
        cmd := aProcess+ExtractFileExt(BaseApplication.ExeName);
        cmd := cmd+BuildCmdLine;
        ExecCommand;
      end
    else if FileExists('/usr/bin/'+aProcess+ExtractFileExt(BaseApplication.ExeName)) then
      begin
        Found := False;
        cmd := '/usr/bin/'+aProcess+ExtractFileExt(BaseApplication.ExeName);
        cmd := cmd+BuildCmdLine;
        ExecCommand;
      end
    else if FileExists('/usr/local/bin/'+aProcess+ExtractFileExt(BaseApplication.ExeName)) then
      begin
        Found := False;
        cmd := '/usr/local/bin/'+aProcess+ExtractFileExt(BaseApplication.ExeName);
        cmd := cmd+BuildCmdLine;
        ExecCommand;
      end
    else if Processes.Scripts.Locate('NAME',aProcess,[loCaseInsensitive]) then
      begin
        cmd := AppendPathDelim(BaseApplication.Location)+'pscript'+ExtractFileExt(BaseApplication.ExeName);
        if not FileExists(UniToSys(cmd)) then
          cmd := AppendPathDelim(BaseApplication.Location)+'tools'+DirectorySeparator+'pscript'+ExtractFileExt(BaseApplication.ExeName);
        if FileExists(UniToSys(cmd)) then
          begin
            cmd := cmd+BuildCmdLine;
            cmd := cmd+' '+aProcess;
            ExecCommand;
          end
        else
          DoLog(cmd+':'+'File dosend exists',aLog,True);
      end
    else
      begin
        aLog.Clear;
        DoLog(ExpandFileName(aProcess+ExtractFileExt(BaseApplication.ExeName))+':'+'File dosend exists',aLog,True);
      end;
    if Processes.DataSet.FieldByName('LOG').AsString<>aLog.Text then
      begin
        Processes.Edit;
        Processes.DataSet.FieldByName('LOG').AsString := aLog.Text;
        Processes.Post;
      end;
  end;

begin
  aNow := Now();
  if aNow>0 then
    begin
      Processes.Open;
      Processes.Parameters.Open;
      aLog := TStringList.Create;
      //Check processes
      if OnlyActiveRow then
        ProcessRow
      else
        begin
          Processes.DataSet.Refresh;
          Processes.DataSet.First;
          while not Processes.DataSet.EOF do
            begin
              ProcessRow;
              Processes.DataSet.Next;
            end;
        end;
      aLog.Free;
    end;
end;

end.

