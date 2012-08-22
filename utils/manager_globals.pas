unit manager_globals;

{$mode objfpc}{$H+}

interface

uses
  Classes, sysutils;



function GlobalCopyList: TFPList;
function GlobalCopyListClear: TFPList;

implementation

uses
  epicustombase;

var
  FGlobalCopyList: TFPList = nil;

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

procedure FinishGlobals;
begin
  GlobalCopyListClear;
  FGlobalCopyList.Free;
  FGlobalCopyList := nil;
end;

finalization
  FinishGlobals;

end.

