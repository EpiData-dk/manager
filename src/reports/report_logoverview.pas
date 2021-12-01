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

uses
  episecuritylog, epiglobals, epireport_report_userlist;

{ TReportLogOverview }

function TReportLogOverview.GetTitle: string;
begin
  result := 'Access Log Overview';
end;

procedure TReportLogOverview.DoDocumentReport(
  const ADocumentFile: TEpiDocumentFile; const Index: Integer);
var
  Document: TEpiDocument;
  LogTypeCount: array[TEpiLogEntry] of integer;
  SecurityLog: TEpiSecurityDatafile;
  DataLog: TEpiSecurityDataEventLog;
  KeyLog: TEpiSecurityKeyFieldLog;
  LogTypeEnum: TEpiLogEntry;
  RowCount, i, blockedcount, j: Integer;
  R: TEpiReportUserList;
begin
  inherited DoDocumentReport(ADocumentFile, Index);

  Document := ADocumentFile.Document;

  R := TEpiReportUserList.Create(Generator);
  R.Admin := Document.Admin;
  R.RunReport;
  R.Free;

  Generator.Line('');

  SecurityLog := TEpiSecurityDatafile(Document.DataFiles.GetDataFileByName(EpiSecurityLogDatafileName));
  DataLog     := TEpiSecurityDataEventLog(Document.Datafiles.GetDataFileByName(EpiSecurityLogDataEventName));
  KeyLog      := TEpiSecurityKeyFieldLog(Document.DataFiles.GetDataFileByName(EpiSecurityLogKeyDataName));

  blockedcount := 0;
  for LogTypeEnum := Low(LogTypeCount) to High(LogTypeCount) do
    LogTypeCount[LogTypeEnum] := 0;

  for i := 0 to SecurityLog.Size -1 do
  begin
    LogTypeEnum := TEpiLogEntry(SecurityLog.LogType.AsInteger[i]);
    LogTypeCount[LogTypeEnum] := LogTypeCount[LogTypeEnum] + 1;

    if LogTypeEnum = ltBlockedLogin then
      inc(blockedcount);
  end;

  RowCount :=
   6 +                                    // Header + Lines numbers + Start date + Blocked count
   Integer(High(TEpiLogEntry));           // Count of each log type

  Generator.TableHeader('Log Overview', 2, RowCount);

  Generator.TableCell('Task',                      0, 0);
  Generator.TableCell('Content',                   1, 0);

  Generator.TableCell('Start Date',                0, 1);
  Generator.TableCell(DateTimeToStr(SecurityLog.Date.AsDateTime[0] +
                                    SecurityLog.Time.AsDateTime[0]),
                      1, 1);

  Generator.TableCell('Log Entries',               0, 2);
  Generator.TableCell(IntToStr(SecurityLog.Size),  1, 2);

  Generator.TableCell('First id#',                 0, 3);
  Generator.TableCell(SecurityLog.ID.AsString[1],  1, 3);

  Generator.TableCell('Last id#',                  0, 4);
  Generator.TableCell(SecurityLog.ID.AsString[SecurityLog.Size - 1], 1, 4);

  Generator.TableCell('Exported entries',          0, 5);
  Generator.TableCell(IntToStr(SecurityLog.ID.AsInteger[1] - 1)    , 1, 5);

  i := 6;
  for LogTypeEnum := Low(LogTypeCount) to High(LogTypeCount) do
  begin
    if LogTypeEnum = ltNone then continue;

    Generator.TableCell(EpiLogEntryText[LogTypeEnum],        0, i);
    Generator.TableCell(IntToStr(LogTypeCount[LogTypeEnum]), 1, i);
    Inc(i);
  end;

  Generator.TableFooter('');

  Generator.Line('');

  Generator.TableHeader('Blocked Login Attempts', 3, BlockedCount + 1);
  Generator.TableCell('Blocked machine', 0, 0);
  Generator.TableCell('Latest login',    1, 0);
  Generator.TableCell('Date / Time',     2, 0);

  i := 1;
  for j := 0 to SecurityLog.Size -1 do
  begin
    LogTypeEnum := TEpiLogEntry(SecurityLog.LogType.AsInteger[j]);

    if (LogTypeEnum = ltBlockedLogin)
    then
      begin
        Generator.TableCell(SecurityLog.MachineName.AsString[j], 0, i);
        Generator.TableCell(SecurityLog.UserName.AsString[j], 1, i);
        Generator.TableCell(DateTimeToStr(SecurityLog.Date.AsDateTime[j] +
                                          SecurityLog.Time.AsDateTime[j]),
                            2, i);

        Inc(i);
      end;
  end;
  Generator.TableFooter('');

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

