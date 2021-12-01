unit project_settings_field_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls,
  project_settings_interface, epicustombase, episettings,
  // for use in settings interface
  settings2_interface, settings2_var, settings2;

type

  { TProjectSettings_FieldFrame }

  TProjectSettings_FieldFrame = class(TFrame, IProjectSettingsFrame, ISettingsFrame)
    ShowValueLabelTextChkBox: TCheckBox;
    ShowFieldNamesChkBox: TCheckBox;
    ShowFieldBordersChkBox: TCheckBox;
  private
    { private declarations }
    FProjectSettings: TEpiProjectSettings;
    FManagerSettings: PManagerSettings;
  public
    { public declarations }
    procedure SetProjectSettings(AValue: TEpiCustomBase);
    function  ApplySettings: boolean;
    procedure SetSettings(Data: PManagerSettings);
  end;

implementation

{$R *.lfm}

uses
  epidocument;


{ TProjectSettings_FieldFrame }

procedure TProjectSettings_FieldFrame.SetProjectSettings(AValue: TEpiCustomBase);
begin
  FProjectSettings :=  TEpiDocument(AValue).ProjectSettings;

  ShowFieldNamesChkBox.Checked   := FProjectSettings.ShowFieldNames;
  ShowFieldBordersChkBox.Checked := FProjectSettings.ShowFieldBorders;
  ShowValueLabelTextChkBox.Visible := false;
end;

function TProjectSettings_FieldFrame.ApplySettings: boolean;
begin
  result := true;

  if Assigned(FProjectSettings) then
  begin
    FProjectSettings.ShowFieldNames   := ShowFieldNamesChkBox.Checked;
    FProjectSettings.ShowFieldBorders := ShowFieldBordersChkBox.Checked;
  end;

  if Assigned(FManagerSettings) then
  with FManagerSettings^ do
  begin
    ShowNames   := ShowFieldNamesChkBox.Checked;
    ShowBorders := ShowFieldBordersChkBox.Checked;
    ShowValuelabelText := ShowValueLabelTextChkBox.Checked;
  end;
end;

procedure TProjectSettings_FieldFrame.SetSettings(Data: PManagerSettings);
begin
  ShowValueLabelTextChkBox.Visible := true;

  FManagerSettings := Data;
  with FManagerSettings^ do
  begin
    ShowFieldNamesChkBox.Checked   := ShowNames;
    ShowFieldBordersChkBox.Checked := ShowBorders;
    ShowValueLabelTextChkBox.Checked := ShowValuelabelText;
  end;
end;

end.

