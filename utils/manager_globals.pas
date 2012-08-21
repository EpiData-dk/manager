unit manager_globals;

{$mode objfpc}{$H+}

interface

uses
  Classes, sysutils;



function GlobalCopyList: TList;

implementation

var
  FGlobalCopyList: TList = nil;

function GlobalCopyList: TList;
begin
  if not Assigned(FGlobalCopyList) then
    FGlobalCopyList := TList.Create;

  result := FGlobalCopyList;
end;

finalization
  FGlobalCopyList.Free;

end.

