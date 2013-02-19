unit project_settings_general_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, MaskEdit, ExtCtrls,
  settings2_interface, settings2_var, settings2,
  project_settings_interface, epicustombase, episettings, epidatafiles;

type

  { TProjectSettings_BackupFrame }

  TProjectSettings_BackupFrame = class(TFrame, IProjectSettingsFrame, ISettingsFrame)
    BackupOnShutdownChkBox: TCheckBox;
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
  epidocument, Dialogs;

{ TProjectSettings_BackupFrame }

procedure TProjectSettings_BackupFrame.SetProjectSettings(AValue: TEpiCustomBase);
begin
  FProjectSettings               := TEpiDocument(AValue).ProjectSettings;

  BackupIntervalEdit.Text        := IntToStr(FProjectSettings.BackupInterval);
  BackupOnShutdownChkBox.Checked := FProjectSettings.BackupOnShutdown;
end;

procedure TProjectSettings_BackupFrame.SetSettings(Data: PManagerSettings);
begin
  FManagerSettings := Data;

  with FManagerSettings^ do
  begin
    BackupIntervalEdit.Text        := IntToStr(TimedRecoveryInterval);
    BackupOnShutdownChkBox.Checked := SaveBackup;
  end;
end;

function TProjectSettings_BackupFrame.ApplySettings: boolean;
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
  end;

  if Assigned(FManagerSettings) then
  with FManagerSettings^ do
  begin
    TimedRecoveryInterval := I;
    SaveBackup            := BackupOnShutdownChkBox.Checked;
  end;
end;

end.
