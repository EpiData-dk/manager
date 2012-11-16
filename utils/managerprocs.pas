unit managerprocs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure ReadClipBoard(ClipBoardLine: TStrings);
procedure CopyAndBackup(Const AFileName: string);
procedure LoadIniFile;
function GetRandomComponentName: string;



implementation

uses
  Clipbrd, FileUtil, settings2, settings2_var, forms, strutils;

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

  // TODO : This is not optimal on Non-windows OS's. Do some checks for writeability first.
  if LoadSettingsFromIni(ExtractFilePath(Application.ExeName) + IniName) then exit;

  if not DirectoryExistsUTF8(ExtractFilePath(GetAppConfigFileUTF8(false))) then
    ForceDirectoriesUTF8(ExtractFilePath(GetAppConfigFileUTF8(false)));
  ManagerSettings.IniFileName := GetAppConfigFileUTF8(false);
end;

function GetRandomComponentName: string;
var
  GUID: TGUID;
begin
  // Hack: Create a GUID to use as Component name.
  //  - the comp. name is not used in other parts of the program anyway,
  //  - so using GUID is a valid way to create random components names... :)
  //  - And the chance of creating to equal component name are very-very-very unlikely.
  CreateGUID(GUID);
  Result := '_' + StringsReplace(GUIDToString(GUID), ['{','}','-'], ['','',''], [rfReplaceAll]);
end;

end.

