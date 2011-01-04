unit project_settings_general_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, MaskEdit, ExtCtrls,
  project_settings_interface, epicustombase, episettings, epidatafiles;

type

  { TProjectSettings_GeneralFrame }

  TProjectSettings_GeneralFrame = class(TFrame, IProjectSettingsFrame)
    BackupOnShutdownChkBox: TCheckBox;
    Bevel1: TBevel;
    Label3: TLabel;
    AutoIncStartEdit: TMaskEdit;
    ProjectTitleEdit: TEdit;
    Label1: TLabel;
    BackupIntervalEdit: TMaskEdit;
    Label2: TLabel;
  private
    { private declarations }
    FProjectSettings: TEpiProjectSettings;
    FDataFiles: TEpiDataFiles;
  public
    { public declarations }
    procedure SetProjectSettings(AValue: TEpiCustomBase);
    function  ApplySettings: boolean;
  end;

implementation

{$R *.lfm}

uses
  Dialogs, epidocument;

{ TProjectSettings_GeneralFrame }

procedure TProjectSettings_GeneralFrame.SetProjectSettings(AValue: TEpiCustomBase);
begin
  FProjectSettings               := TEpiDocument(AValue).ProjectSettings;
  FDataFiles                     := TEpiDocument(AValue).DataFiles;

  BackupIntervalEdit.Text        := IntToStr(FProjectSettings.BackupInterval);
  BackupOnShutdownChkBox.Checked := FProjectSettings.BackupOnShutdown;
  AutoIncStartEdit.Text          := IntToStr(FProjectSettings.AutoIncStartValue);
  ProjectTitleEdit.Text          := FDataFiles[0].Name.Text;
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
  FProjectSettings.BackupInterval   := I;
  FProjectSettings.BackupOnShutdown := BackupOnShutdownChkBox.Checked;
  FProjectSettings.AutoIncStartValue := StrToInt(AutoIncStartEdit.Text);
  FDataFiles[0].Name.Text           := ProjectTitleEdit.Text;
end;

end.
