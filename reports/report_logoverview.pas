unit report_logoverview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, report_base, epidocument, epidatafiles, epidatafilerelations,
  epiopenfile, epiadmin, epilogger;

type

  { TReportLogOverview }

  TReportLogOverview = class(TReportFileListBase)
  protected
    function GetTitle: string; override;
    procedure DoDocumentReport(const ADocumentFile: TEpiDocumentFile;
      const Index: Integer); override;
  end;


implementation

{ TReportLogOverview }

function TReportLogOverview.GetTitle: string;
begin

end;

procedure TReportLogOverview.DoDocumentReport(
  const ADocumentFile: TEpiDocumentFile; const Index: Integer);
{var
  Logger: TEpiLogger;
  Log: TEpiLog;
  LogTypeField: TEpiEnumField;
  LogTypeCount: array[TEpiLogEntry] of integer;
  BlockedCount: Integer;
  i, j, RowCount: Integer;
  LogTypeEnum: TEpiLogEntry;     }
begin
  inherited DoDocumentReport(ADocumentFile, Index);

 { Logger := ADocumentFile.Document.Logger;
  Log := Logger.Log;

  for LogTypeEnum := Low(LogTypeCount) to High(LogTypeCount) do
    LogTypeCount[LogTypeEnum] := 0;
  BlockedCount := 0;

  // Start collecting data!
  for i := 0 to Log.Size -1 do
  begin
    LogTypeField := Log.FType;
    LogTypeEnum := LogTypeField.AsEnum[i];

    LogTypeCount[LogTypeEnum] := LogTypeCount[LogTypeEnum] + 1;

    if (LogTypeEnum = ltFailedLogin) and
       (Log.FDataContent.AsInteger[i] = 3)
    then
      // Blocked login.
      Inc(BlockedCount);

  end;

  RowCount :=
   3 +                                    // Header + Lines numbers + Start date + Blocked count
   Integer(High(TEpiLogEntry));           // Count of each log type

  if (BlockedCount > 0) then
    inc(RowCount);

  Generator.TableHeader('Log Overview', 2, RowCount);

  Generator.TableCell('Task',                      0, 0);
  Generator.TableCell('Content',                   1, 0);

  Generator.TableCell('Log Entries',               0, 1);
  Generator.TableCell(IntToStr(Log.Size),          1, 1);

  Generator.TableCell('Start Date',                0, 2);
  Generator.TableCell(DateTimeToStr(Log.FTime[0]), 1, 2);

  i := 3;
  for LogTypeEnum := Low(LogTypeCount) to High(LogTypeCount) do
  begin
    if LogTypeEnum = ltNone then continue;

    Generator.TableCell(EpiLogEntryText[LogTypeEnum],        0, i);
    Generator.TableCell(IntToStr(LogTypeCount[LogTypeEnum]), 1, i);
    Inc(i);
  end;

  Generator.TableCell('Blocked Attempts',     0, i);
  Generator.TableCell(IntToStr(BlockedCount), 1, i);

  Generator.TableFooter('');

  Generator.Line('');

  Generator.TableHeader('Blocked Login Attempts', 3, BlockedCount + 1);
  Generator.TableCell('Blocked machine', 0, 0);
  Generator.TableCell('Latest login',    1, 0);
  Generator.TableCell('Date / Time',     2, 0);

  i := 1;
  for j := 0 to Log.Size -1 do
  begin
    LogTypeField := Log.FType;
    LogTypeEnum := LogTypeField.AsEnum[j];

    if (LogTypeEnum = ltFailedLogin) and
       (Log.FDataContent.AsInteger[j] = 3)
    then
      begin
        Generator.TableCell(Log.FLogContent.AsString[j], 0, i);
        if (j > 0) then
          Generator.TableCell(Log.FUserNames.AsString[j-1], 1, i);

        Generator.TableCell(DateTimeToStr(Log.FTime.AsDateTime[j]), 2, i);

        Inc(i);
      end;
  end;
  Generator.TableFooter('');     }
end;

end.

