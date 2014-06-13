unit manager_globals;

{$mode objfpc}{$H+}

interface

uses
  Classes, sysutils, design_commander;

const
  DesignControlCustomDataKey = 'DesignControlCustomDataKey';

function GlobalCopyList: TFPList;
function GlobalCopyListClear: TFPList;

function GlobalCommandList: TCommander;

implementation

uses
  epicustombase;

var
  FGlobalCopyList: TFPList = nil;
  FGlobalCommandList: TCommander = nil;

function GlobalCopyList: TFPList;
begin
  if not Assigned(FGlobalCopyList) then
    FGlobalCopyList := TFPList.Create;

  result := FGlobalCopyList;
end;

procedure FreeCopyList(data,arg:pointer);
begin
  TEpiCustomBase(Data).Free;
end;

function GlobalCopyListClear: TFPList;
begin
  result := GlobalCopyList;
  Result.ForEachCall(@FreeCopyList, nil);
  Result.Clear;
end;

function GlobalCommandList: TCommander;
begin
  if not Assigned(FGlobalCommandList) then
    FGlobalCommandList := TCommander.Create;

  result := FGlobalCommandList;
end;

procedure FinishGlobals;
begin
  GlobalCopyListClear;
  FGlobalCopyList.Free;
  FGlobalCopyList := nil;

  FGlobalCommandList.Free;
end;

finalization
  FinishGlobals;

end.

