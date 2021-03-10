unit project_settings_autoincrement_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, maskedit,
  settings2_interface, project_settings_interface, episettings,
  settings2_var, epicustombase;

type

  { TProjectSettings_AutoIncFrame }

  TProjectSettings_AutoIncFrame = class(TFrame, IProjectSettingsFrame, ISettingsFrame)
    AutoIncStartEdit: TMaskEdit;
    Label3: TLabel;
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
  epidocument;

{ TProjectSettings_AutoIncFrame }

procedure TProjectSettings_AutoIncFrame.SetProjectSettings(
  AValue: TEpiCustomBase);
begin
  FProjectSettings               := TEpiDocument(AValue).ProjectSettings;

  AutoIncStartEdit.Text          := IntToStr(FProjectSettings.AutoIncStartValue);
end;

procedure TProjectSettings_AutoIncFrame.SetSettings(Data: PManagerSettings);
begin
  FManagerSettings := Data;

  with FManagerSettings^ do
    AutoIncStartEdit.Text          := IntToStr(AutoIncStart);
end;

function TProjectSettings_AutoIncFrame.ApplySettings: boolean;
begin
  result := true;

  if Assigned(FProjectSettings) then
    FProjectSettings.AutoIncStartValue := StrToInt(AutoIncStartEdit.Text);

  if Assigned(FManagerSettings) then
  with FManagerSettings^ do
    AutoIncStart          := StrToInt(AutoIncStartEdit.Text);
end;

end.

