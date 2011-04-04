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

  {$IFDEF EPI_SHOWREVISION}
    {$I revision.inc}
  {$ELSE}
    const RevisionStr = '(DEBUG)';
  {$ENDIF}

function GetManagerVersion: String;
function SaveSettingToIni(Const FileName: string): boolean;
function LoadSettingsFromIni(Const FileName: string): boolean;

procedure SaveFormPosition(Const AForm: TForm; Const SectionName: string);
procedure LoadFormPosition(Var AForm: TForm; Const SectionName: string);

procedure AddToRecent(Const AFilename: string);


procedure RegisterSettingFrame(const Order: byte;
  const AValue: TCustomFrameClass; const AName: string);

const
  ManagerVersion: TEpiVersionInfo = (
    VersionNo: 0;
    MajorRev:  7;
    MinorRev:  2;
    BuildNo:   0;
  );

implementation

{$R *.lfm}

uses
  settings2_interface, settings2_var, epidatafilestypes,
  IniFiles, strutils;

var
  Frames: TStringList = nil;

function GetManagerVersion: String;
begin
  result := GetEpiVersionInfo(ManagerVersion);
end;

function SaveSettingToIni(Const FileName: string): boolean;
var
  Ini: TIniFile;
  Sec: string;
  i: Integer;
begin
  Result := false;

  try
    Ini := TIniFile.Create(FileName);
    With Ini do
    with ManagerSettings do
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
      WriteInteger(Sec, 'PasteAsType', PasteSpecialType);
      WriteInteger(Sec, 'SaveAsType', SaveType);
      WriteBool(Sec, 'SaveWindowPositions', SaveWindowPositions);
      WriteBool(Sec, 'ShowWelcome', ShowWelcome);
      WriteBool(Sec, 'ShowWorkToolbar', ShowWorkToolBar);
      WriteBool(Sec, 'MultipleInstances', MultipleInstances);
    end;

    // Read recent files.
    for i := 0 to RecentFiles.Count - 1 do
      Ini.WriteString('recent', 'file'+inttostr(i), RecentFiles[i]);

    Result := true;
  finally
    Ini.Free;
  end;
end;

function LoadSettingsFromIni(Const FileName: string): boolean;
var
  Ini: TIniFile;
  Sec: String;
  i: Integer;
  S: String;
begin
  Result := false;
  ManagerSettings.IniFileName := FileName;

  if not FileExistsUTF8(FileName) then exit;

  try
    Ini := TIniFile.Create(FileName);
    With Ini do
    with ManagerSettings do
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
      PasteSpecialType    := ReadInteger(Sec, 'PasteAsType', PasteSpecialType);
      SaveType            := ReadInteger(Sec, 'SaveAsType', SaveType);
      SaveWindowPositions := ReadBool(Sec, 'SaveWindowPositions', SaveWindowPositions);
      ShowWelcome         := ReadBool(Sec, 'ShowWelcome', ShowWelcome);
      ShowWorkToolBar     := ReadBool(Sec, 'ShowWorkToolBar', ShowWorkToolBar);
      MultipleInstances   := ReadBool(Sec, 'MultipleInstances', MultipleInstances);
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

procedure SaveFormPosition(const AForm: TForm; const SectionName: string);
var
  Ini: TIniFile;
begin
  if ManagerSettings.IniFileName = '' then exit;

  try
    Ini := TIniFile.Create(ManagerSettings.IniFileName);
    With Ini, AForm do
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

procedure LoadFormPosition(var AForm: TForm; const SectionName: string);
var
  Ini: TIniFile;
begin
  if ManagerSettings.IniFileName = '' then exit;

  try
    Ini := TIniFile.Create(ManagerSettings.IniFileName);
    With Ini do
    with AForm do
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
  SaveSettingToIni(ManagerSettings.IniFileName);
end;

{ TSettingsForm }

procedure TSettingsForm.SettingsViewChanging(Sender: TObject; Node: TTreeNode;
  var AllowChange: Boolean);
begin
  // Happens before the change...
  if csDestroying in ComponentState then exit;

  FActiveFrame := TFrame(Node.Data);
  AllowChange := (FActiveFrame as ISettingsFrame).ApplySettings;
  if not AllowChange then exit;

  FActiveFrame.Hide;
end;

procedure TSettingsForm.SettingsViewChange(Sender: TObject; Node: TTreeNode);
begin
  // Happens after the change...
  if csDestroying in ComponentState then exit;

  FActiveFrame := TFrame(Node.Data);
  FActiveFrame.Show;
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

  for i := 0 to Frames.Count - 1 do
  begin
    FrameClass := TCustomFrameClass(Frames.Objects[i]);
    Frame := FrameClass.Create(Self);
    (Frame as ISettingsFrame).SetSettings(@ManagerSettings);
    Frame.Hide;
    Frame.Align := alClient;
    Frame.Parent := Self;
    SettingsView.Items.AddObject(nil, Frames[i], Pointer(Frame));
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

procedure RegisterSettingFrame(const Order: byte;
  const AValue: TCustomFrameClass; const AName: string);
begin
  if not Assigned(Frames) then
    Frames := TStringList.Create;

  if not Supports(AValue, ISettingsFrame) then
    Raise Exception.CreateFmt('Class %s does not support required interface', [AValue.ClassName]);

  if Order >= Frames.Count then
    Frames.AddObject(AName, TObject(AValue))
  else
    Frames.InsertObject(Order, AName, TObject(AValue));
end;

procedure FinalizeFrames;
var
  i: integer;
begin
  for i := 0 to Frames.Count - 1 do
    Frames.Strings[i] := '';
  Frames.Free;
end;

initialization

begin
  ManagerSettings.WorkingDirUTF8 := GetCurrentDirUTF8 + DirectorySeparator + 'data';
  if not DirectoryExistsUTF8(ManagerSettings.WorkingDirUTF8) then
    ManagerSettings.WorkingDirUTF8 := GetCurrentDirUTF8;

  ManagerSettings.TutorialDirUTF8 := GetCurrentDirUTF8 + DirectorySeparator + 'tutorials';
  if not DirectoryExistsUTF8(ManagerSettings.TutorialDirUTF8) then
    ManagerSettings.TutorialDirUTF8 := GetCurrentDirUTF8;

  RecentFiles := TStringList.Create;
end;

finalization

begin
  FinalizeFrames;
  RecentFiles.Free
end;

end.

