unit project_settings_general_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, MaskEdit, ExtCtrls,
  settings2_interface, settings2_var, settings2,
  project_settings_interface, epicustombase, episettings, epidatafiles;

type

  { TProjectSettings_GeneralFrame }

  TProjectSettings_GeneralFrame = class(TFrame, IProjectSettingsFrame, ISettingsFrame)
    BackupOnShutdownChkBox: TCheckBox;
    Label3: TLabel;
    AutoIncStartEdit: TMaskEdit;
    Label1: TLabel;
    BackupIntervalEdit: TMaskEdit;
  private
    { private declarations }
    FProjectSettings: TEpiProjectSettings;
    FManagerSettings: PManagerSettings;
  public
    { public declarations }
    procedure SetProjectSettings(AValue: TEpiCustomBase);
    procedure SetSettings(Data: PManagerSettings);
    function  ApplySettings: boolean;
  end;

implementation

{$R *.lfm}

uses
  Dialogs, epidocument, project_settings_study_frame;

{ TProjectSettings_GeneralFrame }

procedure TProjectSettings_GeneralFrame.SetProjectSettings(AValue: TEpiCustomBase);
begin
  FProjectSettings               := TEpiDocument(AValue).ProjectSettings;

  BackupIntervalEdit.Text        := IntToStr(FProjectSettings.BackupInterval);
  BackupOnShutdownChkBox.Checked := FProjectSettings.BackupOnShutdown;
  AutoIncStartEdit.Text          := IntToStr(FProjectSettings.AutoIncStartValue);
end;

procedure TProjectSettings_GeneralFrame.SetSettings(Data: PManagerSettings);
begin
  FManagerSettings := Data;

  with FManagerSettings^ do
  begin
    BackupIntervalEdit.Text        := IntToStr(TimedRecoveryInterval);
    BackupOnShutdownChkBox.Checked := SaveBackup;
    AutoIncStartEdit.Text          := IntToStr(AutoIncStart);
  end;
end;

function TProjectSettings_GeneralFrame.ApplySettings: boolean;
var
  I: LongInt;
begin
  result := true;

  I := StrToInt(BackupIntervalEdit.Text);
  if (I > 0) and (I < 10) and (Assigned(FProjectSettings)) then
  begin
    if MessageDlg('Warning',
         'Setting backup time <10 minuts may cause problem on slow drives (eg. network drives)' + LineEnding +
         'Accept backup time ' + BackupIntervalEdit.Text + '?',
         mtWarning, mbYesNo, 0, mbNo) = mrNo then Exit(false);
  end;

  if Assigned(FProjectSettings) then
  begin
    FProjectSettings.BackupInterval   := I;
    FProjectSettings.BackupOnShutdown := BackupOnShutdownChkBox.Checked;
    FProjectSettings.AutoIncStartValue := StrToInt(AutoIncStartEdit.Text);
  end;

  if Assigned(FManagerSettings) then
  with FManagerSettings^ do
  begin
    TimedRecoveryInterval := I;
    SaveBackup            := BackupOnShutdownChkBox.Checked;
    AutoIncStart          := StrToInt(AutoIncStartEdit.Text);
  end;
end;

end.
