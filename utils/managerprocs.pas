unit managerprocs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure ReadClipBoard(ClipBoardLine: TStrings);
procedure CopyAndBackup(Const AFileName: string);
procedure LoadIniFile;


implementation

uses
  Clipbrd, FileUtil, settings2, settings2_var, forms;

procedure ReadClipBoard(ClipBoardLine: TStrings);
var
  TmpStr: String;
begin
  if Clipboard.HasFormat(CF_Text) then
  begin
    TmpStr := Clipboard.AsText;
    TmpStr := StringReplace(TmpStr, LineEnding, #1, [rfReplaceAll]);
    ClipBoardLine.Delimiter := #1;
    ClipBoardLine.StrictDelimiter := true;
    ClipBoardLine.DelimitedText := TmpStr;
  end;
end;

procedure CopyAndBackup(const AFileName: string);
var
  TheExt: String;
  TheName: String;
  i: Integer;
  Fn: String;
begin
  if not FileExistsUTF8(AFileName) then exit;

  TheExt := ExtractFileExt(AFileName);
  TheName := LeftStr(AFileName, Length(AFileName) - Length(TheExt));
  i := 1;
  repeat
    Fn := TheName + '.' + IntToStr(i) + TheExt;
    inc(i);
  until (not FileExistsUTF8(Fn));
  RenameFileUTF8(AFileName, Fn);
end;

procedure LoadIniFile;
const
  IniName = 'epidatamanager.ini';
var
  S: String;
begin
  // TODO : Settings can be loaded from commandline?
  if LoadSettingsFromIni(GetAppConfigFileUTF8(false)) then exit;

  // Todo - this is not optimal on Non-windows OS's. Do some checks for writeability first.
  if LoadSettingsFromIni(ExtractFilePath(Application.ExeName) + IniName) then exit;

  if not DirectoryExistsUTF8(ExtractFilePath(GetAppConfigFileUTF8(false))) then
    ForceDirectoriesUTF8(ExtractFilePath(GetAppConfigFileUTF8(false)));
  ManagerSettings.IniFileName := GetAppConfigFileUTF8(false);
end;

end.

