unit settings;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, MaskEdit, ExtCtrls, ComCtrls, UDataFileTypes;

type

  { TSettingsForm }

  TSettingsForm = class(TForm)
    Bevel1: TBevel;
    CancelBtn: TButton;
    DefaultDateCombo: TComboBox;
    FloatLengthEdit: TMaskEdit;
    DecimalLengthEdit: TMaskEdit;
    Label13: TLabel;
    Label14: TLabel;
    StringLengthEdit: TMaskEdit;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label9: TLabel;
    IntLengthEdit: TMaskEdit;
    ShowFieldBorderChkBox: TCheckBox;
    DefaultRightPosEdit: TMaskEdit;
    FieldNamingAutoRadio: TRadioButton;
    FieldNamingFirstWordRadio: TRadioButton;
    FieldNamingGroup: TRadioGroup;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    SnapThresholdEdit: TMaskEdit;
    FieldFieldEdit: TMaskEdit;
    FieldLabelEdit: TMaskEdit;
    LabelLabelEdit: TMaskEdit;
    OkBtn: TButton;
    PageControl1: TPageControl;
    Panel1: TPanel;
    PrefixEdit: TEdit;
    ShowFieldNameChkBox: TCheckBox;
    BasicSheet: TTabSheet;
    AdvSheet: TTabSheet;
    SnapFieldsChkBox: TCheckBox;
    LengthSheet: TTabSheet;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure SnapFieldsChkBoxChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  published
//    property OnChange;
  end;

  TManagerSettings = record
    // Basic:
    FieldNamePrefix:       string;
    DefaultRightPostion:   Integer;
    ShowFieldNamesInLabel: boolean;
    ShowFieldBorder:       boolean;
    DefaultDateType:       TFieldType;
    FieldNamingStyle:      TFieldNaming;

    // Lengths:
    IntFieldLength:        Integer;
    FloatFieldLength:      Integer;
    FloatDecimalLength:    Integer;
    StringFieldLength:     Integer;


    // Advanced:
    SnapFields:            boolean;
    SnappingThresHold:     Integer;
    SpaceBtwFieldField:    Integer;
    SpaceBtwFieldLabel:    Integer;
    SpaceBtwLabelLabel:    Integer;

    // Not shown in dialog.
    WorkingDirUTF8:         string;
  end;

  TManagerVersion = record
    VersionNo: Integer;
    MajorRev:  Integer;
    MinorRev:  Integer;
    BuildNo:   Integer;
  end;

var
  ManagerSettings: TManagerSettings = (
    // Basic:
    FieldNamePrefix:       'V';
    DefaultRightPostion:   200;
    ShowFieldNamesInLabel: true;
    ShowFieldBorder:       true;
    DefaultDateType:       ftEuroDate;
    FieldNamingStyle:      fnFirstWord;

    // Lengths:
    IntFieldLength:        2;
    FloatFieldLength:      5;
    FloatDecimalLength:    2;
    StringFieldLength:     20;

    // Advanced:
    SnapFields:            true;
    SnappingThresHold:     10;
    SpaceBtwFieldField:    10;
    SpaceBtwFieldLabel:    10;
    SpaceBtwLabelLabel:    10;

    // Not shown in dialog.
    WorkingDirUTF8:        '';
  );

const
  ManagerVersion: TManagerVersion = (
    VersionNo: 0;
    MajorRev:  3;
    MinorRev:  0;
    BuildNo:   7;
  );


function GetManagerVersion: String;

implementation

{$I revision.inc}

function GetManagerVersion: String;
begin
  with ManagerVersion do
    result := IntToStr(VersionNo) + '.' +
              IntToStr(MajorRev) + '.' +
              IntToStr(MinorRev) + '.' +
              IntToStr(BuildNo) + ' r' + RevisionStr;
end;

{ TSettingsForm }

procedure TSettingsForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  S: String;
begin
  CanClose := true;
  if ModalResult = mrCancel then
    exit;

  // Basic:
  ManagerSettings.FieldNamePrefix       := PrefixEdit.Text;
  S := Trim(DefaultRightPosEdit.Text);
  if not((S = '') or (StrToInt(S) <= 0)) then
    ManagerSettings.DefaultRightPostion := StrToInt(S);
  ManagerSettings.ShowFieldNamesInLabel := ShowFieldNameChkBox.Checked;
  ManagerSettings.ShowFieldBorder       := ShowFieldBorderChkBox.Checked;
  case DefaultDateCombo.ItemIndex of
    0: ManagerSettings.DefaultDateType := ftEuroDate;
    1: ManagerSettings.DefaultDateType := ftDate;
    2: ManagerSettings.DefaultDateType := ftYMDDate;
  end;
  if FieldNamingAutoRadio.Checked then
    ManagerSettings.FieldNamingStyle    := fnAuto
  else
    ManagerSettings.FieldNamingStyle    := fnFirstWord;

  // Lengths:
  S := Trim(IntLengthEdit.Text);
  if not((S = '') or (StrToInt(S) <= 0)) then
    ManagerSettings.IntFieldLength := StrToInt(S);
  S := Trim(FloatLengthEdit.Text);
  if not((S = '') or (StrToInt(S) <= 0)) then
    ManagerSettings.FloatFieldLength := StrToInt(S);
  S := Trim(DecimalLengthEdit.Text);
  if not((S = '') or (StrToInt(S) <= 0)) then
    ManagerSettings.FloatDecimalLength := StrToInt(S);
  S := Trim(StringLengthEdit.Text);
  if not((S = '') or (StrToInt(S) <= 0)) then
    ManagerSettings.StringFieldLength := StrToInt(S);

  // Advanced:
  ManagerSettings.SnapFields            := SnapFieldsChkBox.Checked;
  S := Trim(SnapThresholdEdit.Text);
  if not((S = '') or (StrToInt(S) <= 0)) then
    ManagerSettings.SnappingThresHold := StrToInt(S);
  S := Trim(FieldFieldEdit.Text);
  if not((S = '') or (StrToInt(S) <= 0)) then
    ManagerSettings.SpaceBtwFieldField := StrToInt(S);
  S := Trim(FieldLabelEdit.Text);
  if not((S = '') or (StrToInt(S) <= 0)) then
    ManagerSettings.SpaceBtwFieldLabel := StrToInt(S);
  S := Trim(LabelLabelEdit.Text);
  if not((S = '') or (StrToInt(S) <= 0)) then
    ManagerSettings.SpaceBtwLabelLabel := StrToInt(S);

end;

procedure TSettingsForm.FormCreate(Sender: TObject);
begin
  with ManagerSettings do
  begin
    // Basic:
    PrefixEdit.Text                   := FieldNamePrefix;
    DefaultRightPosEdit.Text          := IntToStr(DefaultRightPostion);
    ShowFieldNameChkBox.Checked       := ShowFieldNamesInLabel;
    ShowFieldBorderChkBox.Checked     := ShowFieldBorder;
    case DefaultDateType of
      ftEuroDate: DefaultDateCombo.ItemIndex := 0;
      ftDate:     DefaultDateCombo.ItemIndex := 1;
      ftYMDDate:  DefaultDateCombo.ItemIndex := 2;
    end;
    FieldNamingAutoRadio.Checked      := (FieldNamingStyle = fnAuto);
    FieldNamingFirstWordRadio.Checked := (FieldNamingStyle = fnFirstWord);

    // Lengths:
    IntLengthEdit.Text                := IntToStr(IntFieldLength);
    FloatLengthEdit.Text              := IntToStr(FloatFieldLength);
    DecimalLengthEdit.Text            := IntToSTr(FloatDecimalLength);
    StringLengthEdit.Text             := IntToStr(StringFieldLength);

    // Advanced:
    SnapFieldsChkBox.Checked          := ManagerSettings.SnapFields;
    SnapThresholdEdit.Text            := IntToStr(ManagerSettings.SnappingThresHold);
    FieldFieldEdit.Text               := IntToStr(ManagerSettings.SpaceBtwFieldField);
    FieldLabelEdit.Text               := IntToStr(ManagerSettings.SpaceBtwFieldLabel);
    LabelLabelEdit.Text               := IntToStr(ManagerSettings.SpaceBtwLabelLabel);
  end;
end;

procedure TSettingsForm.SnapFieldsChkBoxChange(Sender: TObject);
begin
  SnapThresholdEdit.Enabled := SnapFieldsChkBox.Checked;
end;

initialization
  {$I settings.lrs}

begin
  ManagerSettings.WorkingDirUTF8 := GetCurrentDirUTF8;
end;

end.

