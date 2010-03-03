unit ManagerProcs;

{$mode objfpc}

interface

uses
  Classes, SysUtils, settings, epidatafile;


function NextFieldName(DF: TEpiDataFile): String;
function NextLabelName(DF: TEpiDataFile): String;

implementation

function NextFieldName(DF: TEpiDataFile): String;
var
  FieldNo: Integer;
begin
  FieldNo := DF.FieldCount + 1;
  repeat
    Result := ManagerSettings.FieldNamePrefix + IntToStr(FieldNo);
    Inc(FieldNo);
  until (not DF.FieldExists(Result));
end;

function NextLabelName(DF: TEpiDataFile): String;
var
  LabelNo: Integer;
begin
  LabelNo := DF.TextLabelCount + 1;
  repeat
    Result := ManagerSettings.LabelNamePrefix + IntToStr(LabelNo);
    Inc(LabelNo);
  until (not DF.FieldExists(Result));
end;

end.

