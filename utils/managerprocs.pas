unit managerprocs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidocument, epiopenfile;

procedure ReadClipBoard(ClipBoardLine: TStrings);
procedure CopyAndBackup(Const AFileName: string);
procedure LoadIniFiles;
function GetIniFileName: string;
function GetRecentIniFileName: string;
procedure ParseCommandLineOpts;
function GetRandomComponentName: string;



implementation

uses
  Clipbrd, FileUtil, settings2, settings2_var, forms, strutils,
  epimiscutils, LCLVersion, LazUTF8, dialogs, controls;

var
  IniFileName: string = '';
  RecentIniFileName: string = '';

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

procedure LoadIniFiles;
begin
  LoadSettingsFromIni(GetIniFileName);
  LoadRecentFilesIni(GetRecentIniFileName);
end;

function GetIniFileName: string;
var
  S: string;
begin
  // IniFileName has been set during ParCommandLineOpts if
  // it was part of the startup.
  // else set the default path! (only first time required).
  if IniFileName = '' then
  begin
    IniFileName := GetAppConfigFileUTF8(false,
      {$IFDEF windows}
      false
      {$ELSE}
      true
      {$ENDIF}
      {$IF ((lcl_major = 1) and (lcl_minor >= 1))}
      , true
      {$ENDIF}
      );

    {$IF ((lcl_major = 1) and (lcl_minor < 1))}
    S := ExtractFilePath(IniFileName);
    if not DirectoryExistsUTF8(S) then
      if not ForceDirectoriesUTF8(S) then
        Exit;
    {$ENDIF}
  end;

  Result := IniFileName;
end;

function GetRecentIniFileName: string;
begin
  // IniFileName has been set during ParCommandLineOpts if
  // it was part of the startup.
  // else set the default path! (only first time required).

  if RecentIniFileName = '' then
    RecentIniFileName := ExpandFileNameUTF8(GetAppConfigDirUTF8(False) + '..' + PathDelim + 'epidatarecentfiles.ini');

  Result := RecentIniFileName;
end;

procedure ParseCommandLineOpts;
const
  RecentFileIni =      '--recentfile';
  RecentFileIniShort = '-r';
  IniFile =            '--inifile';
  IniFileShort =       '-i';
  ShowHelp =           '--help';
  ShowHelpShort =      '-h';
  ShowVersion =        '--version';
  ShowVersionShort =   '-v';

  function ParseLine(Const Param, Option: string; var Value: string): boolean;
  begin
    Result := false;
    if LeftStr(Param, Length(Option)) = Option then
    begin
      Result := true;
      Value := Copy(Param, Length(Option) + 2, Length(Param));
    end;
  end;

  procedure DoOutputText(Const AText: string);
  begin
    if TextRec(Output).Mode = fmClosed then
      MessageDlg('Information:', AText, mtInformation, [mbOk], 0)
    else
      WriteLn(UTF8ToConsole(AText));
  end;

  procedure DoShowHelp;
  var
    HText: TStringList;
  begin
    HText := TStringList.Create;

    HText.Add('Usage:');
    HText.Add(ApplicationName + ' [OPTIONS] [FILE]');
    HText.Add('');
    HText.Add('Options:');
    HText.Add('-h or --help             Show this help and exit.');
    HText.Add('-v or --version          Show version info and exit.');
    HText.Add('');
    HText.Add('-i= or --inifile=...     Location of the configuration file storing user preferences.');
    HText.Add('                         If no location is specified, the default configuration file is used.');
    HText.Add('');
    HText.Add('-r= or --recentfile=...  Location of the configuration file storing a list of recently used files.');
    HText.Add('                         This file can be shared with EpiData EntryClient.');
    HText.Add('                         If no location is specified, the default configuration file is used.');
    HText.Add('');
    HText.Add('FILE                     If a project file is specified (either .epx or .epz), then this file is');
    HText.Add('                         is opened at startup.');
    DoOutputText(HText.Text);
    HText.Free;
  end;

  procedure DoShowVersion;
  begin
    DoOutputText(GetManagerVersion);
  end;

var
  i: Integer;
  S, P: string;
begin
  for i := 1 to Paramcount do
  begin
    P := ParamStrUTF8(i);

    if ParseLine(P, RecentFileIni, RecentIniFileName) or
       ParseLine(P, RecentFileIniShort, RecentIniFileName)
    then
    begin
      RecentIniFileName := ExpandFileNameUTF8(RecentIniFileName);
      Continue;
    end;

    if ParseLine(P, IniFile, IniFileName) or
       ParseLine(P, IniFileShort, IniFileName)
    then
    begin
      IniFileName := ExpandFileNameUTF8(IniFileName);
      Continue;
    end;

    if ParseLine(P, ShowHelp, S) or
       ParseLine(P, ShowHelpShort, S)
    then
    begin
      DoShowHelp;
      halt(0);
    end;

    if ParseLine(P, ShowVersion, S) or
       ParseLine(P, ShowVersionShort, S)
    then
    begin
      DoShowVersion;
      Halt(0);
    end;

    if not (P[1] = '-') then
    begin
      if not Assigned(StartupFiles) then
        StartupFiles := TStringList.Create;
      StartupFiles.Add(P);
    end else begin
      DoOutputText('Unrecognized option: ' + P);
      Halt(0);
    end;
  end;
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

finalization
  if Assigned(StartupFiles) then StartupFiles.Free;

end.

