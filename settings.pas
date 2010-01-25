unit settings;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, MaskEdit, ExtCtrls, ComCtrls, ActnList, EditBtn, UDataFileTypes;

type

  { TSettingsForm }

  TSettingsForm = class(TForm)
    Bevel2: TBevel;
    Bevel3: TBevel;
    CloseAction: TAction;
    ActionList1: TActionList;
    Bevel1: TBevel;
    CancelBtn: TButton;
    DefaultPasteCombo: TComboBox;
    DefaultDateCombo: TComboBox;
    Label9: TLabel;
    WorkingDirEdit: TDirectoryEdit;
    FieldFieldEdit: TMaskEdit;
    FieldLabelEdit: TMaskEdit;
    FieldNamingAutoRadio: TRadioButton;
    FieldNamingFirstWordRadio: TRadioButton;
    FieldNamingGroup: TRadioGroup;
    FloatIntEdit: TMaskEdit;
    DecimalLengthEdit: TMaskEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    LabelLabelEdit: TMaskEdit;
    PrefixEdit: TEdit;
    SnapFieldsChkBox: TCheckBox;
    SnapThresholdEdit: TMaskEdit;
    StringLengthEdit: TMaskEdit;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    IntLengthEdit: TMaskEdit;
    ShowFieldBorderChkBox: TCheckBox;
    DefaultRightPosEdit: TMaskEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label8: TLabel;
    OkBtn: TButton;
    PageControl1: TPageControl;
    Panel1: TPanel;
    ShowFieldNameChkBox: TCheckBox;
    VisualDesignSheet: TTabSheet;
    AdvSheet: TTabSheet;
    FieldDefSheet: TTabSheet;
    procedure CloseActionExecute(Sender: TObject);
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
    // Visual design:
    DefaultRightPostion:   Integer;
    ShowFieldNamesInLabel: boolean;
    ShowFieldBorder:       boolean;
    SnapFields:            boolean;
    SnappingThresHold:     Integer;
    SpaceBtwFieldField:    Integer;
    SpaceBtwFieldLabel:    Integer;
    SpaceBtwLabelLabel:    Integer;

    // Field definitions:
    IntFieldLength:        Integer;
    FloatIntLength:        Integer;
    FloatDecimalLength:    Integer;
    StringFieldLength:     Integer;
    DefaultDateType:       TFieldType;
    FieldNamePrefix:       string;
    FieldNamingStyle:      TFieldNaming;

    // Advanced:
    WorkingDirUTF8:         string;
    PasteSpecialType:       TFieldType;

    // Not shown in dialog.
    SelectedControlColour: Integer;
    LabelNamePrefix:        string;
  end;

  TManagerVersion = record
    VersionNo: Integer;
    MajorRev:  Integer;
    MinorRev:  Integer;
    BuildNo:   Integer;
  end;

var
  ManagerSettings: TManagerSettings = (
    // Visual design:
    DefaultRightPostion:   200;
    ShowFieldNamesInLabel: true;
    ShowFieldBorder:       true;
    SnapFields:            true;
    SnappingThresHold:     10;
    SpaceBtwFieldField:    10;
    SpaceBtwFieldLabel:    25;
    SpaceBtwLabelLabel:    5;

    // Field definitions:
    IntFieldLength:        2;
    FloatIntLength:        2;
    FloatDecimalLength:    2;
    StringFieldLength:     20;
    DefaultDateType:       ftEuroDate;
    FieldNamePrefix:       'V';
    FieldNamingStyle:      fnFirstWord;

    // Advanced:
    WorkingDirUTF8:        '';
    PasteSpecialType:      ftRes4  ; // ftQuestion;

    // Not shown in dialog.
    SelectedControlColour: $00B6F5F5;
    LabelNamePrefix:       'label_';
  );

const
  ManagerVersion: TManagerVersion = (
    VersionNo: 0;
    MajorRev:  3;
    MinorRev:  2;
    BuildNo:   9;
  );


function GetManagerVersion: String;

implementation

{$IFDEF EPI_RELEASE}
  {$I revision.inc}
{$ELSE}
  const RevisionStr = '0';
{$ENDIF}

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

  // Visual desing:
  S := Trim(DefaultRightPosEdit.Text);
  if not((S = '') or (StrToInt(S) <= 0)) then
    ManagerSettings.DefaultRightPostion := StrToInt(S);
  ManagerSettings.ShowFieldNamesInLabel := ShowFieldNameChkBox.Checked;
  ManagerSettings.ShowFieldBorder       := ShowFieldBorderChkBox.Checked;
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

  // Field definitions:
  S := Trim(IntLengthEdit.Text);
  if not((S = '') or (StrToInt(S) <= 0)) then
    ManagerSettings.IntFieldLength := StrToInt(S);
  S := Trim(FloatIntEdit.Text);
  if not((S = '') or (StrToInt(S) <= 0)) then
    ManagerSettings.FloatIntLength := StrToInt(S);
  S := Trim(DecimalLengthEdit.Text);
  if not((S = '') or (StrToInt(S) <= 0)) then
    ManagerSettings.FloatDecimalLength := StrToInt(S);

  S := Trim(StringLengthEdit.Text);
  if not((S = '') or (StrToInt(S) <= 0)) then
    ManagerSettings.StringFieldLength := StrToInt(S);
  case DefaultDateCombo.ItemIndex of
    0: ManagerSettings.DefaultDateType := ftEuroDate;
    1: ManagerSettings.DefaultDateType := ftDate;
    2: ManagerSettings.DefaultDateType := ftYMDDate;
  end;
  ManagerSettings.FieldNamePrefix       := PrefixEdit.Text;
  if FieldNamingAutoRadio.Checked then
    ManagerSettings.FieldNamingStyle    := fnAuto
  else
    ManagerSettings.FieldNamingStyle    := fnFirstWord;

  // Advanced:
  ManagerSettings.WorkingDirUTF8        := WorkingDirEdit.Text;
  case DefaultPasteCombo.ItemIndex of
    0: ManagerSettings.PasteSpecialType := ftQuestion;
    1: ManagerSettings.PasteSpecialType := ftFloat;
    2: ManagerSettings.PasteSpecialType := ftInteger;
    3: ManagerSettings.PasteSpecialType := ftDate;
    4: ManagerSettings.PasteSpecialType := ftRes4;
  end;
end;

procedure TSettingsForm.CloseActionExecute(Sender: TObject);
begin
  CancelBtn.Click;
end;

procedure TSettingsForm.FormCreate(Sender: TObject);
begin
  with ManagerSettings do
  begin
    // Visual desing:
    DefaultRightPosEdit.Text          := IntToStr(DefaultRightPostion);
    ShowFieldNameChkBox.Checked       := ShowFieldNamesInLabel;
    ShowFieldBorderChkBox.Checked     := ShowFieldBorder;
    SnapFieldsChkBox.Checked          := ManagerSettings.SnapFields;
    SnapThresholdEdit.Text            := IntToStr(ManagerSettings.SnappingThresHold);
    FieldFieldEdit.Text               := IntToStr(ManagerSettings.SpaceBtwFieldField);
    FieldLabelEdit.Text               := IntToStr(ManagerSettings.SpaceBtwFieldLabel);
    LabelLabelEdit.Text               := IntToStr(ManagerSettings.SpaceBtwLabelLabel);

    // Field definitions
    IntLengthEdit.Text                := IntToStr(IntFieldLength);
    FloatIntEdit.Text                 := IntToStr(FloatIntLength);
    DecimalLengthEdit.Text            := IntToSTr(FloatDecimalLength);
    StringLengthEdit.Text             := IntToStr(StringFieldLength);
    case DefaultDateType of
      ftEuroDate: DefaultDateCombo.ItemIndex := 0;
      ftDate:     DefaultDateCombo.ItemIndex := 1;
      ftYMDDate:  DefaultDateCombo.ItemIndex := 2;
    end;
    PrefixEdit.Text                   := FieldNamePrefix;
    FieldNamingAutoRadio.Checked      := (FieldNamingStyle = fnAuto);
    FieldNamingFirstWordRadio.Checked := (FieldNamingStyle = fnFirstWord);

    // Advanced:
    WorkingDirEdit.Text               := WorkingDirUTF8;
    case PasteSpecialType of
      ftRes4:     DefaultPasteCombo.ItemIndex := 0;
      ftFloat:    DefaultPasteCombo.ItemIndex := 1;
      ftInteger:  DefaultPasteCombo.ItemIndex := 2;
      ftDate:     DefaultPasteCombo.ItemIndex := 3;
      ftQuestion: DefaultPasteCombo.ItemIndex := 4;
    end;
  end;
end;

procedure TSettingsForm.SnapFieldsChkBoxChange(Sender: TObject);
begin
  SnapThresholdEdit.Enabled := SnapFieldsChkBox.Checked;
end;

initialization
  {$I settings.lrs}

begin
  ManagerSettings.WorkingDirUTF8 := GetCurrentDirUTF8 + {$IFDEF UNIX}'/data'{$ELSE}'\data'{$ENDIF};
end;

end.

