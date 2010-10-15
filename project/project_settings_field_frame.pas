unit project_settings_field_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls,
  project_settings_interface, episettings;

type

  { TProjectSettings_FieldFrame }

  TProjectSettings_FieldFrame = class(TFrame, IProjectSettingsFrame)
    ShowFieldNamesChkBox: TCheckBox;
    ShowFieldBordersChkBox: TCheckBox;
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

{ TProjectSettings_FieldFrame }

procedure TProjectSettings_FieldFrame.SetProjectSettings(AValue: TEpiProjectSettings);
begin
  FProjectSettings := AValue;

  ShowFieldNamesChkBox.Checked   := FProjectSettings.ShowFieldNames;
  ShowFieldBordersChkBox.Checked := FProjectSettings.ShowFieldBorders;
end;

function TProjectSettings_FieldFrame.ApplySettings: boolean;
begin
  FProjectSettings.ShowFieldNames   := ShowFieldNamesChkBox.Checked;
  FProjectSettings.ShowFieldBorders := ShowFieldBordersChkBox.Checked;

  result := true;
end;

end.

