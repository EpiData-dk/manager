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
    CancelBtn: TButton;
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
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure SnapFieldsChkBoxChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

  TManagerSettings = record
    // Basic:
    FieldNamePrefix:       string;
    DefaultRightPostion:   Integer;
    ShowFieldNamesInLabel: boolean;
    ShowFieldBorder:       boolean;
    FieldNamingStyle:      TFieldNaming;

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
    FieldNamingStyle:      fnFirstWord;

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
    MajorRev:  2;
    MinorRev:  2;
    BuildNo:   6;
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
  if FieldNamingAutoRadio.Checked then
    ManagerSettings.FieldNamingStyle    := fnAuto
  else
    ManagerSettings.FieldNamingStyle    := fnFirstWord;

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
  // Basic:
  PrefixEdit.Text                   := ManagerSettings.FieldNamePrefix;
  DefaultRightPosEdit.Text          := IntToStr(ManagerSettings.DefaultRightPostion);
  ShowFieldNameChkBox.Checked       := ManagerSettings.ShowFieldNamesInLabel;
  ShowFieldBorderChkBox.Checked     := ManagerSettings.ShowFieldBorder;
  FieldNamingAutoRadio.Checked      := (ManagerSettings.FieldNamingStyle = fnAuto);
  FieldNamingFirstWordRadio.Checked := (ManagerSettings.FieldNamingStyle = fnFirstWord);

  // Advanced:
  SnapFieldsChkBox.Checked          := ManagerSettings.SnapFields;
  SnapThresholdEdit.Text            := IntToStr(ManagerSettings.SnappingThresHold);
  FieldFieldEdit.Text               := IntToStr(ManagerSettings.SpaceBtwFieldField);
  FieldLabelEdit.Text               := IntToStr(ManagerSettings.SpaceBtwFieldLabel);
  LabelLabelEdit.Text               := IntToStr(ManagerSettings.SpaceBtwLabelLabel);
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

