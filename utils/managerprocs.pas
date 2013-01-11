unit managerprocs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dialogs, epidocument;

type

  { TOpenEpiDoc }

  TOpenEpiDoc = class
  private
    constructor Create;
    procedure EpiDocumentPassWord(Sender: TObject; var Login: string;
      var Password: string);
  public
    class function OpenDoc(const FileName, Lang: string): TEpiDocument; overload;
    class function OpenDoc(Doc: TEpiDocument; const FileName: string): TEpiDocument;
  end;




procedure ReadClipBoard(ClipBoardLine: TStrings);
procedure CopyAndBackup(Const AFileName: string);
procedure LoadIniFile;
function GetRandomComponentName: string;



implementation

uses
  Clipbrd, FileUtil, settings2, settings2_var, forms, strutils,
  epimiscutils;

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
  Fn: String;
  S: String;
begin
  Fn := GetAppConfigFileUTF8(false,
    {$IFDEF windows}
    false
    {$ELSE}
    true
    {$ENDIF}
    , true);

  // TODO : Settings can be loaded from commandline?
  if not LoadSettingsFromIni(Fn) then
  begin
    // TODO : This is not optimal on Non-windows OS'Fn. Do some checks for writeability first.
    S := ExtractFilePath(Application.ExeName) + IniName;
    if (not FileIsReadOnlyUTF8(S)) then
       LoadSettingsFromIni(S);
  end;

  ManagerSettings.IniFileName := Fn;

  FN := ExpandFileNameUTF8(GetAppConfigDirUTF8(False) + '..' + PathDelim + 'epidatarecentfiles.ini');
  LoadRecentFilesIni(Fn);
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

{ TOpenEpiDoc }

constructor TOpenEpiDoc.Create;
begin

end;

procedure TOpenEpiDoc.EpiDocumentPassWord(Sender: TObject; var Login: string;
  var Password: string);
begin
  Password :=
    PasswordBox('Project Password',
                'Project data is password protected.' + LineEnding +
                'Please enter password:');
end;

class function TOpenEpiDoc.OpenDoc(const FileName, Lang: string): TEpiDocument;
begin
  result := OpenDoc(TEpiDocument.Create(Lang), FileName);
end;

class function TOpenEpiDoc.OpenDoc(Doc: TEpiDocument; const FileName: string): TEpiDocument;
var
  Tmp: TOpenEpiDoc;
  St: TMemoryStream;
begin
  Tmp := TOpenEpiDoc.Create;

  St := TMemoryStream.Create;
  if ExtractFileExt(UTF8ToSys(FileName)) = '.epz' then
    ZipFileToStream(St, FileName)
  else
    St.LoadFromFile(UTF8ToSys(FileName));
  St.Position := 0;

  result := Doc;
  result.OnPassword := @Tmp.EpiDocumentPassWord;
  result.LoadFromStream(St);

  St.Free;
  Tmp.Free;
end;

end.

