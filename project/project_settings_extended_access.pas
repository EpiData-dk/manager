unit project_settings_extended_access;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls,
  project_settings_interface, settings2_interface, epiadmin,
  settings2_var, epicustombase;

type

  { TProjectSettings_ExternalAccessFrame }

  TProjectSettings_ExternalAccessFrame = class(TFrame, IProjectSettingsFrame, ISettingsFrame)
    Label1: TLabel;
    DBPCEdit: TEdit;
  private
    FAdmin: TEpiAdmin;
    FManagerSettings: PManagerSettings;
  public
    // IProjectSettingsFrame
    procedure SetProjectSettings(AValue: TEpiCustomBase);
    // ISettingsFrame
    procedure SetSettings(Data: PManagerSettings);
    // Both
    function  ApplySettings: boolean;
  end;

implementation

{$R *.lfm}

uses
  epidocument;

{ TProjectSettings_ExternalAccessFrame }

procedure TProjectSettings_ExternalAccessFrame.SetProjectSettings(AValue: TEpiCustomBase);
begin
  FAdmin := TEpiDocument(AValue).Admin;

  DBPCEdit.Text := IntToStr(FAdmin.DaysBetweenPasswordChange);
end;

function TProjectSettings_ExternalAccessFrame.ApplySettings: boolean;
var
  DaysBetweenPassword: int64;
begin
  Result := TryStrToInt64(DBPCEdit.Text, DaysBetweenPassword);
  if (not result) then
    Exit;

  if (Assigned(FAdmin)) then
    FAdmin.DaysBetweenPasswordChange := StrToInt(DBPCEdit.Text);

  if (Assigned(FManagerSettings)) then
    FManagerSettings^.DaysBetweenPassword := StrToInt(DBPCEdit.Text);

  Result := true;
end;

procedure TProjectSettings_ExternalAccessFrame.SetSettings(Data: PManagerSettings);
begin
  FManagerSettings := Data;

  DBPCEdit.Text := IntToStr(FManagerSettings^.DaysBetweenPassword);
end;

end.

