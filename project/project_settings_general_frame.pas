unit project_settings_general_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, MaskEdit,
  project_settings_interface, episettings;

type

  { TProjectSettings_GeneralFrame }

  TProjectSettings_GeneralFrame = class(TFrame, IProjectSettingsFrame)
    BackupOnShutdownChkBox: TCheckBox;
    Label1: TLabel;
    BackupIntervalEdit: TMaskEdit;
  private
    { private declarations }
    FProjectSettings: TEpiProjectSettings;
  public
    { public declarations }
    procedure SetProjectSettings(AValue: TEpiProjectSettings);
    function  ApplySettings: boolean;
  end;

implementation

{$R *.lfm}

uses
  Dialogs;

{ TProjectSettings_GeneralFrame }

procedure TProjectSettings_GeneralFrame.SetProjectSettings(AValue: TEpiProjectSettings);
begin
  FProjectSettings := AValue;
  BackupIntervalEdit.Text := IntToStr(FProjectSettings.BackupInterval);
  BackupOnShutdownChkBox.Enabled := FProjectSettings.BackupOnShutdown;
end;

function TProjectSettings_GeneralFrame.ApplySettings: boolean;
var
  I: LongInt;
begin
  I := StrToInt(BackupIntervalEdit.Text);
  result := true;
  if (I > 0) and (I < 10) then
  begin
    if MessageDlg('Warning',
         'Setting backup time <10 minuts may cause problem on slow drives (eg. network drives)' + LineEnding +
         'Accept backup time ' + BackupIntervalEdit.Text + '?',
         mtWarning, mbYesNo, 0, mbNo) = mrNo then Exit(false);
  end;
  FProjectSettings.BackupInterval := I;
  FProjectSettings.BackupOnShutdown := BackupOnShutdownChkBox.Enabled;
end;

end.

