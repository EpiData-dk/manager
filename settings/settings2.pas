unit settings2;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ComCtrls, epiversionutils;

type

  { TSettingsForm }

  TSettingsForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Panel1: TPanel;
    SettingsView: TTreeView;
    Splitter1: TSplitter;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
    procedure SettingsViewChange(Sender: TObject; Node: TTreeNode);
    procedure SettingsViewChanging(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
  private
    { private declarations }
    FActiveFrame: TFrame;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    class procedure RestoreDefaultPos;
  end;

  {$I epidatamanager.revision.inc}

function GetManagerVersion: String;

function SaveSettingToIni(Const FileName: string): boolean;
function LoadSettingsFromIni(Const FileName: string): boolean;
function SaveRecentFilesToIni(Const FileName: string): boolean;
function LoadRecentFilesIni(Const FileName: string): boolean;

procedure SaveFormPosition(Const AForm: TForm; Const SectionName: string);
procedure LoadFormPosition(AForm: TForm; Const SectionName: string);

procedure AddToRecent(Const AFilename: string);

{const
  ManagerVersion: TEpiVersionInfo = (
    {$I epidatamanager.version.inc}
  );
}

procedure InitFont(Font: TFont);

implementation

{$R *.lfm}

uses
  settings2_interface, settings2_var, epidatafilestypes,
  IniFiles, strutils, epieximtypes, epiexportsettings,
  main,

  // settings
  settings_advanced_frame, settings_fielddefinitions_frame,
  settings_general_frame, settings_visualdesign_frame,
  settings_export,

  // export
  export_csv_frame, export_stata_frame, export_sas_frame, export_spss_frame,

  // project settings
  project_settings_field_frame, project_settings_general_frame,
  project_settings_study_contentdesc_frame, project_settings_study_frame,
  project_settings_study_ownership_frame;

const
  RecentIniFileName: string = '';


function GetIniFile(Const FileName: String): TIniFile;
begin
  result := TIniFile.Create(UTF8ToSys(FileName));
end;

function GetManagerVersion: String;
begin
  Result := GetEpiVersionInfo(HINSTANCE);
end;

function SaveSettingToIni(Const FileName: string): boolean;
var
  Sec: string;
  i: Integer;
  Ini: TIniFile;
begin
  Result := false;

  try
    Ini := GetIniFile(FileName);
    with Ini, ManagerSettings do
    begin
      {  // Visual design:
        DefaultRightPostion:   Integer;
        SnapFields:            boolean;
        SnappingThresHold:     Integer;
        SpaceBtwFieldField:    Integer;
        SpaceBtwFieldLabel:    Integer;
        SpaceBtwLabelLabel:    Integer;}
      Sec := 'visual';
      WriteInteger(Sec, 'DefaultRightPosition',  DefaultRightPosition);
      WriteInteger(Sec, 'DefaultLabelPosition',  DefaultLabelPosition);
      WriteBool   (Sec, 'SnapFields',            SnapFields);
      WriteInteger(Sec, 'SnappingThresHold',     SnappingThresHold);
      WriteInteger(Sec, 'SpaceBtwFieldField',    SpaceBtwFieldField);
      WriteInteger(Sec, 'SpaceBtwFieldLabel',    SpaceBtwFieldLabel);
      WriteInteger(Sec, 'SpaceBtwLabelLabel',    SpaceBtwLabelLabel);

      {  // Field definitions:
      IntFieldLength:        Integer;
      FloatIntLength:        Integer;
      FloatDecimalLength:    Integer;
      StringFieldLength:     Integer;
      DefaultDateType:       TEpiFieldType;
      FieldNamePrefix:       string;
    //    FieldNamingStyle:      TFieldNaming;}
      Sec := 'fielddefs';
      WriteInteger(Sec, 'IntFieldLength',     IntFieldLength);
      WriteInteger(Sec, 'FloatIntLength',     FloatIntLength);
      WriteInteger(Sec, 'FloatDecimalLength', FloatDecimalLength);
      WriteInteger(Sec, 'StringFieldLength',  StringFieldLength);
      WriteInteger(Sec, 'DefaultDateType',    Word(DefaultDateType));
      WriteString(Sec,  'FieldNamePrefix',    FieldNamePrefix);

  {    // Advanced:
      WorkingDirUTF8:        string;
      PasteSpecialType:      byte}
      Sec := 'advanced';
      WriteString(Sec, 'WorkingDirectory', WorkingDirUTF8);
      WriteString(Sec, 'TutorialDirectory', TutorialDirUTF8);
      WriteString(Sec, 'TutorialURL', TutorialURLUTF8);
      WriteString(Sec, 'EntryCLientDirectory', EntryClientDirUTF8);
      WriteInteger(Sec, 'PasteAsType', PasteSpecialType);
      WriteInteger(Sec, 'SaveAsType', SaveType);
      WriteBool(Sec, 'SaveWindowPositions', SaveWindowPositions);
      WriteBool(Sec, 'ShowWelcome', ShowWelcome);
      WriteBool(Sec, 'ShowWorkToolbar', ShowWorkToolBar);
      WriteBool(Sec, 'ShowA4GuideLines', ShowA4GuideLines);
      WriteBool(Sec, 'MultipleInstances', MultipleInstances);

      Sec := 'fonts';
      WriteString(sec, 'FieldFontName', FieldFont.Name);
      WriteInteger(sec, 'FieldFontSize', FieldFont.Size);
      WriteInteger(sec, 'FieldFontStyle', Integer(FieldFont.Style));
      WriteInteger(sec, 'FieldFontColour', FieldFont.Color);
      WriteString(sec, 'HeadingFontName', HeadingFont.Name);
      WriteInteger(sec, 'HeadingFontSize', HeadingFont.Size);
      WriteInteger(sec, 'HeadingFontStyle', Integer(HeadingFont.Style));
      WriteInteger(sec, 'HeadingFontColour', HeadingFont.Color);
      WriteString(sec, 'SectionFontName', SectionFont.Name);
      WriteInteger(sec, 'SectionFontSize', SectionFont.Size);
      WriteInteger(sec, 'SectionFontStyle', Integer(SectionFont.Style));
      WriteInteger(sec, 'SectionFontColour', SectionFont.Color);

      // Export:
      Sec := 'export';
      WriteInteger(sec, 'ExportType', ExportType);
      WriteBool(sec, 'ExportDeleted', ExportDeleted);
      WriteInteger(sec, 'ExportEncoding', Integer(ExportEncoding));

      Sec := 'exportstata';
      WriteInteger(sec, 'ExportStataVersion', Integer(ExportStataVersion));
      WriteInteger(sec, 'ExportStataFieldCase', Integer(ExportStataFieldCase));
      WriteBool(sec, 'ExportStataValueLabels', ExportStataValueLabels);

      sec := 'exportcsv';
      WriteBool(sec, 'ExportCSVFieldName', ExportCSVFieldName);
      WriteInteger(sec, 'ExportCSVNewLine', ExportCSVNewLine);
      WriteString(sec, 'ExportCSVQuote', ExportCSVQuote);
      WriteString(sec, 'ExportCSVFieldSep', ExportCSVFieldSep);
      WriteString(sec, 'ExportCSVDateSep', ExportCSVDateSep);
      WriteString(sec, 'ExportCSVTimeSep', ExportCSVTimeSep);
      WriteString(sec, 'ExportCSVDecSep', ExportCSVDecSep);

      sec := 'exportsas';
      WriteBool(sec, 'ExportSASValueLabels', ExportSASValueLabels);

      sec := 'exportspss';
      WriteBool(sec, 'ExportSPSSValueLabels', ExportSPSSValueLabels);

      // Project Defaults
      // - general:
      Sec := 'ProjectGeneral';
      WriteInteger(Sec, 'TimedRecoveryInterval', TimedRecoveryInterval);
      WriteBool(Sec, 'SaveBackup', SaveBackup);
      WriteInteger(Sec, 'AutoIncStart', AutoIncStart);
      // - Fields:
      Sec := 'ProjectFields';
      WriteBool(Sec, 'ShowNames', ShowNames);
      WriteBool(Sec, 'ShowBorders', ShowBorders);
      WriteBool(Sec, 'ShowValuelabelText', ShowValuelabelText);
      // - Study:
      Sec := 'ProjectStudy';
      WriteString(Sec, 'StudyTitle', StudyTitle);
      WriteString(Sec, 'StudyIndent', StudyIndent);
      WriteString(Sec, 'StudyLang', StudyLang);
      WriteString(Sec, 'StudyVersion', StudyVersion);
      // - Content Desc:
      Sec := 'ProjectContent';
      WriteString(Sec, 'ContPurpose', ContPurpose);
      WriteString(Sec, 'ContAbstract', ContAbstract);
      WriteString(Sec, 'ContCitation', ContCitation);
      WriteString(Sec, 'ContGeoCover', ContGeoCover);
      WriteString(Sec, 'ContTimeCover', ContTimeCover);
      // - Ownership:
      Sec := 'ProjectOwnership';
      WriteString(Sec, 'OwnAuthers', OwnAuthers);
      WriteString(Sec, 'OwnRights', OwnRights);
      WriteString(Sec, 'OwnPublisher', OwnPublisher);
      WriteString(Sec, 'OwnFunding', OwnFunding);

      for i := 0 to RecentFiles.Count - 1 do
        WriteString('recent', 'file'+inttostr(i), RecentFiles[i]);
    end;

    // Read recent files.
    Result := true;
  finally
    Ini.Free;
  end;
end;

function LoadSettingsFromIni(Const FileName: string): boolean;
var
  Sec: String;
  i: Integer;
  S: String;
  Ini: TIniFile;

  procedure CorrectFont(F: TFont);
  begin
    if (F.Name = '') or
       (LowerCase(F.Name) = 'default') or
       (F.Size = 0)
    then
      InitFont(F);
  end;

begin
  Result := false;
  ManagerSettings.IniFileName := FileName;

  if not FileExistsUTF8(FileName) then exit;

  try
    Ini := GetIniFile(FileName);
    with Ini, ManagerSettings do
    begin
      {  // Visual design:
        DefaultRightPostion:   Integer;
        SnapFields:            boolean;
        SnappingThresHold:     Integer;
        SpaceBtwFieldField:    Integer;
        SpaceBtwFieldLabel:    Integer;
        SpaceBtwLabelLabel:    Integer;}
      Sec := 'visual';
      DefaultRightPosition  := ReadInteger(Sec, 'DefaultRightPosition',  DefaultRightPosition);
      DefaultLabelPosition  := ReadInteger(Sec, 'DefaultLabelPosition',  DefaultLabelPosition);
      SnapFields            := ReadBool   (Sec, 'SnapFields',            SnapFields);
      SnappingThresHold     := ReadInteger(Sec, 'SnappingThresHold',     SnappingThresHold);
      SpaceBtwFieldField    := ReadInteger(Sec, 'SpaceBtwFieldField',    SpaceBtwFieldField);
      SpaceBtwFieldLabel    := ReadInteger(Sec, 'SpaceBtwFieldLabel',    SpaceBtwFieldLabel);
      SpaceBtwLabelLabel    := ReadInteger(Sec, 'SpaceBtwLabelLabel',    SpaceBtwLabelLabel);

      {  // Field definitions:
      IntFieldLength:        Integer;
      FloatIntLength:        Integer;
      FloatDecimalLength:    Integer;
      StringFieldLength:     Integer;
      DefaultDateType:       TEpiFieldType;
      FieldNamePrefix:       string;
    //    FieldNamingStyle:      TFieldNaming;}
      Sec := 'fielddefs';
      DefaultDateType    := TEpiFieldType(ReadInteger(Sec, 'DefaultDateType', Word(DefaultDateType)));
      IntFieldLength     := ReadInteger(Sec, 'IntFieldLength',     IntFieldLength);
      FloatIntLength     := ReadInteger(Sec, 'FloatIntLength',     FloatIntLength);
      FloatDecimalLength := ReadInteger(Sec, 'FloatDecimalLength', FloatDecimalLength);
      StringFieldLength  := ReadInteger(Sec, 'StringFieldLength',  StringFieldLength);
      FieldNamePrefix    := ReadString(Sec,  'FieldNamePrefix',    FieldNamePrefix);

  {    // Advanced:
      WorkingDirUTF8:        string;
      PasteSpecialType:      byte;}
      Sec := 'advanced';
      WorkingDirUTF8      := ReadString(Sec, 'WorkingDirectory', WorkingDirUTF8);
      TutorialDirUTF8     := ReadString(Sec, 'TutorialDirectory', TutorialDirUTF8);
      TutorialURLUTF8     := ReadString(Sec, 'TutorialURL', TutorialURLUTF8);
      EntryClientDirUTF8  := ReadString(Sec, 'EntryClientDirectory', EntryClientDirUTF8);
      PasteSpecialType    := ReadInteger(Sec, 'PasteAsType', PasteSpecialType);
      SaveType            := ReadInteger(Sec, 'SaveAsType', SaveType);
      SaveWindowPositions := ReadBool(Sec, 'SaveWindowPositions', SaveWindowPositions);
      ShowWelcome         := ReadBool(Sec, 'ShowWelcome', ShowWelcome);
      ShowWorkToolBar     := ReadBool(Sec, 'ShowWorkToolBar', ShowWorkToolBar);
      ShowA4GuideLines    := ReadBool(Sec, 'ShowA4GuideLines', ShowA4GuideLines);
      MultipleInstances   := ReadBool(Sec, 'MultipleInstances', MultipleInstances);

      // Export:
      Sec := 'export';
      ExportType             := ReadInteger(sec, 'ExportType', ExportType);
      ExportDeleted          := ReadBool(sec, 'ExportDeleted', ExportDeleted);
      ExportEncoding         := TEpiEncoding(ReadInteger(sec, 'ExportEncoding', Integer(ExportEncoding)));

      Sec := 'exportstata';
      ExportStataVersion     := TEpiStataVersion(ReadInteger(sec, 'ExportStataVersion', Integer(ExportStataVersion)));
      ExportStataFieldCase   := TEpiStataFieldNamingCase(ReadInteger(sec, 'ExportStataFieldCase', Integer(ExportStataFieldCase)));
      ExportStataValueLabels := ReadBool(sec, 'ExportStataValueLabels', ExportStataValueLabels);

      sec := 'exportcsv';
      ExportCSVFieldName     := ReadBool(sec, 'ExportCSVFieldName', ExportCSVFieldName);
      ExportCSVNewLine       := ReadInteger(sec, 'ExportCSVNewLine', ExportCSVNewLine);
      ExportCSVQuote         := ReadString(sec, 'ExportCSVQuote', ExportCSVQuote);
      ExportCSVFieldSep      := ReadString(sec, 'ExportCSVFieldSep', ExportCSVFieldSep);
      ExportCSVDateSep       := ReadString(sec, 'ExportCSVDateSep', ExportCSVDateSep);
      ExportCSVTimeSep       := ReadString(sec, 'ExportCSVTimeSep', ExportCSVTimeSep);
      ExportCSVDecSep        := ReadString(sec, 'ExportCSVDecSep', ExportCSVDecSep);

      sec := 'exportsas';
      ExportSASValueLabels   := ReadBool(sec, 'ExportSASValueLabels', ExportSASValueLabels);

      sec := 'exportspss';
      ExportSPSSValueLabels  := ReadBool(sec, 'ExportSPSSValueLabels', ExportSPSSValueLabels);

      // Fonts
      Sec := 'fonts';
      FieldFont.Name   := ReadString(sec, 'FieldFontName', FieldFont.Name);
      FieldFont.Size   := ReadInteger(sec, 'FieldFontSize', FieldFont.Size);
      FieldFont.Style  := TFontStyles(ReadInteger(sec, 'FieldFontStyle', Integer(FieldFont.Style)));
      FieldFont.Color  := ReadInteger(sec, 'FieldFontColour', FieldFont.Color);
      CorrectFont(FieldFont);
      HeadingFont.Name   := ReadString(sec, 'HeadingFontName', HeadingFont.Name);
      HeadingFont.Size   := ReadInteger(sec, 'HeadingFontSize', HeadingFont.Size);
      HeadingFont.Style  := TFontStyles(ReadInteger(sec, 'HeadingFontStyle', Integer(HeadingFont.Style)));
      HeadingFont.Color  := ReadInteger(sec, 'HeadingFontColour', HeadingFont.Color);
      CorrectFont(HeadingFont);
      SectionFont.Name   := ReadString(sec, 'SectionFontName', SectionFont.Name);
      SectionFont.Size   := ReadInteger(sec, 'SectionFontSize', SectionFont.Size);
      SectionFont.Style  := TFontStyles(ReadInteger(sec, 'SectionFontStyle', Integer(SectionFont.Style)));
      SectionFont.Color  := ReadInteger(sec, 'SectionFontColour', SectionFont.Color);
      CorrectFont(SectionFont);

      // Project Defaults
      // - general:
      Sec := 'ProjectGeneral';
      TimedRecoveryInterval := ReadInteger(Sec, 'TimedRecoveryInterval', TimedRecoveryInterval);
      SaveBackup            := ReadBool(Sec, 'SaveBackup', SaveBackup);
      AutoIncStart          := ReadInteger(Sec, 'AutoIncStart', AutoIncStart);
      // - Fields:
      Sec := 'ProjectFields';
      ShowNames             := ReadBool(Sec, 'ShowNames', ShowNames);
      ShowBorders           := ReadBool(Sec, 'ShowBorders', ShowBorders);
      ShowValuelabelText    := ReadBool(Sec, 'ShowValuelabelText', ShowValuelabelText);
      // - Study:
      Sec := 'ProjectStudy';
      StudyTitle            := ReadString(Sec, 'StudyTitle', StudyTitle);
      StudyIndent           := ReadString(Sec, 'StudyIndent', StudyIndent);
      StudyLang             := ReadString(Sec, 'StudyLang', StudyLang);
      StudyVersion          := ReadString(Sec, 'StudyVersion', StudyVersion);
      // - Content Desc:
      Sec := 'ProjectContent';
      ContPurpose           := ReadString(Sec, 'ContPurpose', ContPurpose);
      ContAbstract          := ReadString(Sec, 'ContAbstract', ContAbstract);
      ContCitation          := ReadString(Sec, 'ContCitation', ContCitation);
      ContGeoCover          := ReadString(Sec, 'ContGeoCover', ContGeoCover);
      ContTimeCover         := ReadString(Sec, 'ContTimeCover', ContTimeCover);
      // - Ownership:
      Sec := 'ProjectOwnership';
      OwnAuthers            := ReadString(Sec, 'OwnAuthers', OwnAuthers);
      OwnRights             := ReadString(Sec, 'OwnRights', OwnRights);
      OwnPublisher          := ReadString(Sec, 'OwnPublisher', OwnPublisher);
      OwnFunding            := ReadString(Sec, 'OwnFunding', OwnFunding);
    end;

    // Read recent files.
    Sec := 'recent';
    for i := 0 to 9 do
    begin
      S := Ini.ReadString(sec, 'file'+inttostr(i), '');
      if S > '' then
        RecentFiles.Add(S);
    end;
  finally
    Ini.Free;
  end;
  Result := true;
end;

function SaveRecentFilesToIni(const FileName: string): boolean;
var
  Ini: TIniFile;
  Fn: String;
  i: Integer;
begin
  Result := false;

  Fn := FileName;
  if (RecentIniFileName <> '') and
     (FileName = '')
  then
    Fn := RecentIniFileName;

  try
    Ini := GetIniFile(Fn);

    for i := 0 to RecentFiles.Count - 1 do
      Ini.WriteString('Files', 'file'+inttostr(i), RecentFiles[i]);
  finally
    Ini.Free;
  end;
end;

function LoadRecentFilesIni(const FileName: string): boolean;
var
  Ini: TIniFile;
  Sec: String;
  i: Integer;
  S: String;
begin
  Result := false;

  // trick - store filename in const :)
  // and use in save recent.
  if RecentIniFileName = '' then RecentIniFileName := FileName;

  try
    Ini := GetIniFile(FileName);

    // Read recent files.
    Sec := 'Files';
    for i := 0 to 9 do
    begin
      S := Ini.ReadString(sec, 'file'+inttostr(i), '');
      if S <> '' then
        RecentFiles.Add(S);
    end;
  finally
    Ini.Free;
  end;
end;

procedure SaveFormPosition(const AForm: TForm; const SectionName: string);
var
  Ini: TIniFile;
begin
  if ManagerSettings.IniFileName = '' then exit;

  try
    Ini := GetIniFile(ManagerSettings.IniFileName);
    with Ini, AForm do
    begin
      WriteInteger(SectionName, 'Top', Top);
      WriteInteger(SectionName, 'Left', Left);
      WriteInteger(SectionName, 'Width', Width);
      WriteInteger(SectionName, 'Height', Height);
    end;
  finally
    Ini.Free;
  end;
end;

procedure LoadFormPosition(AForm: TForm; const SectionName: string);
var
  Ini: TIniFile;
begin
  if ManagerSettings.IniFileName = '' then exit;

  try
    Ini := GetIniFile(ManagerSettings.IniFileName);
    with Ini, AForm do
    begin
      Top     := ReadInteger(SectionName, 'Top', Top);
      Left    := ReadInteger(SectionName, 'Left', Left);
      Width   := ReadInteger(SectionName, 'Width', Width);
      Height  := ReadInteger(SectionName, 'Height', Height);
    end;
  finally
    Ini.Free;
  end;
end;

procedure AddToRecent(const AFilename: string);
var
  Idx: Integer;
begin
  Idx := RecentFiles.IndexOf(AFilename);
  if (Idx >= 0) then
    RecentFiles.Exchange(Idx, 0)
  else
    RecentFiles.Insert(0, AFilename);
  if RecentFiles.Count > 10 then
    RecentFiles.Delete(10);

  SaveRecentFilesToIni('');
end;

{ TSettingsForm }

procedure TSettingsForm.SettingsViewChange(Sender: TObject; Node: TTreeNode);
begin
  // Happens after the change...
  if csDestroying in ComponentState then exit;

  if Node.Text = 'Project Defaults' then
  begin
    SettingsView.Selected := SettingsView.Items.FindNodeWithText('Project Defaults').GetFirstChild;
    Exit;
  end;

  FActiveFrame := TFrame(Node.Data);
  (FActiveFrame as ISettingsFrame).SetSettings(@ManagerSettings);
  FActiveFrame.Parent := Self;
  FActiveFrame.Align := alClient;
  FActiveFrame.Show;
end;

procedure TSettingsForm.SettingsViewChanging(Sender: TObject; Node: TTreeNode;
  var AllowChange: Boolean);
begin
  // Happens before the change...
  if csDestroying in ComponentState then exit;
  if Node.Text = 'Project Defaults' then exit;

  FActiveFrame := TFrame(Node.Data);
  AllowChange := (FActiveFrame as ISettingsFrame).ApplySettings;
  if not AllowChange then exit;

  FActiveFrame.Hide;
end;

procedure TSettingsForm.FormShow(Sender: TObject);
begin
  FActiveFrame := TFrame(SettingsView.Items[0].Data);
  SettingsView.Selected := SettingsView.Items[0];
  if ManagerSettings.SaveWindowPositions then
    LoadFormPosition(Self, 'SettingsForm');

  if SettingsView.Items.Count > 0 then
    TFrame(SettingsView.Items[0].Data).Show;
  SettingsView.SetFocus;
end;

procedure TSettingsForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if ModalResult = mrCancel then
  begin
    CanClose := true;
    exit;
  end;

  CanClose := false;
  if not (FActiveFrame as ISettingsFrame).ApplySettings then exit;
  SaveSettingToIni(ManagerSettings.IniFileName);
  if ManagerSettings.SaveWindowPositions then
    SaveFormPosition(Self, 'SettingsForm');
  CanClose := true;
end;

constructor TSettingsForm.Create(TheOwner: TComponent);
var
  i: Integer;
  Frame: TCustomFrame;
  FrameClass: TCustomFrameClass;
begin
  inherited Create(TheOwner);


  with SettingsView.Items do
  begin
    FindNodeWithText('General').Data             := Pointer(TSettings_GeneralFrame.Create(Self));
    FindNodeWithText('Paths').Data               := Pointer(TSettings_PathsFrame.Create(Self));
    FindNodeWithText('Field Definitions').Data   := Pointer(TSettings_FieldDefinitionFrame.Create(Self));
    FindNodeWithText('Visual Design').Data       := Pointer(TSettings_VisualDesign.Create(Self));

    // Export
    FindNodeWithText('Export').Data              := Pointer(TSettings_ExportFrame.Create(Self));
    FindNodeWithText('Stata').Data               := Pointer(TExportStataFrame.Create(Self));
    FindNodeWithText('CSV').Data                 := Pointer(TExportCSVFrame.Create(Self));
    FindNodeWithText('SPSS').Data                := Pointer(TExportSPSSFrame.Create(Self));
    FindNodeWithText('SAS').Data                 := Pointer(TExportSASFrame.Create(Self));

    // Project options
    FindNodeWithText('Project Defaults').GetFirstChild.Data := Pointer(TProjectSettings_GeneralFrame.Create(Self));
    FindNodeWithText('Fields').Data              := Pointer(TProjectSettings_FieldFrame.Create(Self));
    FindNodeWithText('Study').Data               := Pointer(TProjectsettings_StudyFrame.Create(Self));
    FindNodeWithText('Content Description').Data := Pointer(TProjectSetting_ContentDescFrame.Create(Self));
    FindNodeWithText('Ownership').Data           := Pointer(TProjectSetting_OwnershipFrame.Create(Self));
  end;
end;

class procedure TSettingsForm.RestoreDefaultPos;
var
  Aform: TForm;
begin
  Aform := TForm.Create(nil);
  Aform.Width := 600;
  Aform.Height := 480;
  Aform.top := (Screen.Monitors[0].Height - Aform.Height) div 2;
  Aform.Left := (Screen.Monitors[0].Width - Aform.Width) div 2;
  SaveFormPosition(Aform, 'SettingsForm');
  AForm.free;
end;

{$I initfont.inc}

initialization
begin
  ManagerSettings.FieldFont := TFont.Create;
  ManagerSettings.HeadingFont := TFont.Create;
  ManagerSettings.SectionFont := TFont.Create;
//  InitFont(ManagerSettings.FieldFont);
//  InitFont(ManagerSettings.HeadingFont);
//  InitFont(ManagerSettings.SectionFont);

  ManagerSettings.WorkingDirUTF8 := GetCurrentDirUTF8 + DirectorySeparator + 'data';
  if not DirectoryExistsUTF8(ManagerSettings.WorkingDirUTF8) then
    ManagerSettings.WorkingDirUTF8 := GetCurrentDirUTF8;

  ManagerSettings.TutorialDirUTF8 := GetCurrentDirUTF8 + DirectorySeparator + 'tutorials';
  if not DirectoryExistsUTF8(ManagerSettings.TutorialDirUTF8) then
    ManagerSettings.TutorialDirUTF8 := GetCurrentDirUTF8;

  ManagerSettings.EntryClientDirUTF8 := SysToUTF8(ExtractFilePath(UTF8ToSys(Application.ExeName)));

  RecentFiles := TStringList.Create;
  RecentFiles.CaseSensitive := true;
end;

finalization

begin
  RecentFiles.Free
end;

end.

