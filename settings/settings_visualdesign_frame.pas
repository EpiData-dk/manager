unit settings_visualdesign_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, MaskEdit, EditBtn,
  Dialogs, settings2_interface, settings2_var, Graphics, ExtCtrls;

type

  { TSettings_VisualDesign }

  TSettings_VisualDesign = class(TFrame, ISettingsFrame)
    DefaultRightPosEdit: TMaskEdit;
    DistGrpBox: TGroupBox;
    FieldFieldEdit: TMaskEdit;
    FieldLabelEdit: TMaskEdit;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    LabelLabelEdit: TMaskEdit;
    LabelLeftPosition: TMaskEdit;
    PositionsGrpBox: TGroupBox;
    OutputFormatRadioGrp: TRadioGroup;
    ScrollBox1: TScrollBox;
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


{ TSettings_VisualDesign }

procedure TSettings_VisualDesign.SetSettings(Data: PManagerSettings);
begin
  FData := Data;

  with FData^ do
  begin
    DefaultRightPosEdit.Text          := IntToStr(DefaultRightPosition);
    LabelLeftPosition.Text               := IntToStr(DefaultLabelPosition);
    FieldFieldEdit.Text               := IntToStr(SpaceBtwFieldField);
    FieldLabelEdit.Text               := IntToStr(SpaceBtwFieldLabel);
    LabelLabelEdit.Text               := IntToStr(SpaceBtwLabelLabel);

    OutputFormatRadioGrp.ItemIndex := ReportOutputFormat;
  end;
end;

function TSettings_VisualDesign.ApplySettings: boolean;
var
  S: String;
begin
  // Visual desing:
  with FData^ do
  begin
    S := Trim(DefaultRightPosEdit.Text);
    if not((S = '') or (StrToInt(S) <= 0)) then
      DefaultRightPosition := StrToInt(S);
    S := Trim(LabelLeftPosition.Text);
    if not((S = '') or (StrToInt(S) <= 0)) then
      DefaultLabelPosition := StrToInt(S);
    S := Trim(FieldFieldEdit.Text);
    if not((S = '') or (StrToInt(S) <= 0)) then
      SpaceBtwFieldField := StrToInt(S);
    S := Trim(FieldLabelEdit.Text);
    if not((S = '') or (StrToInt(S) <= 0)) then
      SpaceBtwFieldLabel := StrToInt(S);
    S := Trim(LabelLabelEdit.Text);
    if not((S = '') or (StrToInt(S) <= 0)) then
      SpaceBtwLabelLabel := StrToInt(S);

    ReportOutputFormat := OutputFormatRadioGrp.ItemIndex;
  end;
  Result := true;
end;

end.

