unit visualdesign_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, MaskEdit,
  settings2_interface, settings2_var;

type

  { TVisualDesign }

  TVisualDesign = class(TFrame, ISettingsFrame)
    DefaultRightPosEdit: TMaskEdit;
    FieldFieldEdit: TMaskEdit;
    FieldLabelEdit: TMaskEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    LabelLabelEdit: TMaskEdit;
    SnapFieldsChkBox: TCheckBox;
    SnapThresholdEdit: TMaskEdit;
  private
    { private declarations }
    FData: PManagerSettings;
  public
    { public declarations }
    procedure SetSettings(Data: PManagerSettings);
    function  ApplySettings: boolean;
  end;

implementation

{$R *.lfm}

uses
  settings2;


{ TVisualDesign }

procedure TVisualDesign.SetSettings(Data: PManagerSettings);
begin
  FData := Data;

  with FData^ do
  begin
    DefaultRightPosEdit.Text          := IntToStr(DefaultRightPostion);
    SnapFieldsChkBox.Checked          := ManagerSettings.SnapFields;
    SnapThresholdEdit.Text            := IntToStr(ManagerSettings.SnappingThresHold);
    FieldFieldEdit.Text               := IntToStr(ManagerSettings.SpaceBtwFieldField);
    FieldLabelEdit.Text               := IntToStr(ManagerSettings.SpaceBtwFieldLabel);
    LabelLabelEdit.Text               := IntToStr(ManagerSettings.SpaceBtwLabelLabel);
  end;
end;

function TVisualDesign.ApplySettings: boolean;
var
  S: String;
begin
  // Visual desing:
  with FData^ do
  begin
    S := Trim(DefaultRightPosEdit.Text);
    if not((S = '') or (StrToInt(S) <= 0)) then
      DefaultRightPostion := StrToInt(S);
    SnapFields            := SnapFieldsChkBox.Checked;
    S := Trim(SnapThresholdEdit.Text);
    if not((S = '') or (StrToInt(S) <= 0)) then
      SnappingThresHold := StrToInt(S);
    S := Trim(FieldFieldEdit.Text);
    if not((S = '') or (StrToInt(S) <= 0)) then
      SpaceBtwFieldField := StrToInt(S);
    S := Trim(FieldLabelEdit.Text);
    if not((S = '') or (StrToInt(S) <= 0)) then
      SpaceBtwFieldLabel := StrToInt(S);
    S := Trim(LabelLabelEdit.Text);
    if not((S = '') or (StrToInt(S) <= 0)) then
      SpaceBtwLabelLabel := StrToInt(S);
  end;
  Result := true;
end;

initialization

begin
  RegisterSettingFrame(0, TVisualDesign, 'Visual Design');
end;

end.

