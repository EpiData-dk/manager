unit settings_fielddefinitions_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls, MaskEdit,
  settings2_interface, settings2_var;

type

  { TSettings_FieldDefinitionFrame }

  TSettings_FieldDefinitionFrame = class(TFrame, ISettingsFrame)
    DecimalLengthEdit: TMaskEdit;
    DefaultDateCombo: TComboBox;
    FieldNamingAutoRadio: TRadioButton;
    FieldNamingFirstWordRadio: TRadioButton;
    FieldNamingGroup: TRadioGroup;
    FloatIntEdit: TMaskEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    IntLengthEdit: TMaskEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label9: TLabel;
    PrefixEdit: TEdit;
    StringLengthEdit: TMaskEdit;
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
  settings2, epidatafilestypes;

{ TSettings_FieldDefinitionFrame }

procedure TSettings_FieldDefinitionFrame.SetSettings(Data: PManagerSettings);
begin
  FData := Data;
  with FData^ do
  begin
    // Field definitions
    IntLengthEdit.Text                := IntToStr(IntFieldLength);
    FloatIntEdit.Text                 := IntToStr(FloatIntLength);
    DecimalLengthEdit.Text            := IntToSTr(FloatDecimalLength);
    StringLengthEdit.Text             := IntToStr(StringFieldLength);
    case DefaultDateType of
      ftDMYDate: DefaultDateCombo.ItemIndex := 0;
      ftMDYDate: DefaultDateCombo.ItemIndex := 1;
      ftYMDDate: DefaultDateCombo.ItemIndex := 2;
    end;
    PrefixEdit.Text                   := FieldNamePrefix;
{    FieldNamingAutoRadio.Checked      := (FieldNamingStyle = fnAuto);
    FieldNamingFirstWordRadio.Checked := (FieldNamingStyle = fnFirstWord);    }
  end;
end;

function TSettings_FieldDefinitionFrame.ApplySettings: boolean;
var
  S: String;
begin
  with FData^ do
  begin
    // Field definitions:
    S := Trim(IntLengthEdit.Text);
    if not((S = '') or (StrToInt(S) <= 0)) then
      IntFieldLength := StrToInt(S);
    S := Trim(FloatIntEdit.Text);
    if not((S = '') or (StrToInt(S) <= 0)) then
      FloatIntLength := StrToInt(S);
    S := Trim(DecimalLengthEdit.Text);
    if not((S = '') or (StrToInt(S) <= 0)) then
      FloatDecimalLength := StrToInt(S);

    S := Trim(StringLengthEdit.Text);
    if not((S = '') or (StrToInt(S) <= 0)) then
      StringFieldLength := StrToInt(S);
    case DefaultDateCombo.ItemIndex of
      0: DefaultDateType := ftDMYDate;
      1: DefaultDateType := ftMDYDate;
      2: DefaultDateType := ftYMDDate;
    end;
    FieldNamePrefix       := PrefixEdit.Text;
  {  if FieldNamingAutoRadio.Checked then
      FieldNamingStyle    := fnAuto
    else
      FieldNamingStyle    := fnFirstWord;    }
  end;
  Result := true;
end;

initialization

begin
  RegisterSettingFrame(2, TSettings_FieldDefinitionFrame, 'Field Definitions');
end;

end.

