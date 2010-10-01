unit settings;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, MaskEdit, ExtCtrls, ComCtrls, ActnList, EditBtn, Buttons,
  epidatafilestypes, epiversionutils;

type

  { TSettingsForm }

  TSettingsForm = class(TForm)
    Bevel2: TBevel;
    Bevel3: TBevel;
    CancelBtn: TBitBtn;
    OkBtn: TBitBtn;
    CloseAction: TAction;
    ActionList1: TActionList;
    Bevel1: TBevel;
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
    DefaultRightPosEdit: TMaskEdit;
    Label3: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    VisualDesignSheet: TTabSheet;
    AdvSheet: TTabSheet;
    FieldDefSheet: TTabSheet;
    procedure CloseActionExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
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
    DefaultDateType:       TEpiFieldType;
    FieldNamePrefix:       string;
//    FieldNamingStyle:      TFieldNaming;

    // Advanced:
    WorkingDirUTF8:        string;
    PasteSpecialType:      byte;     // Index into list:
                                     //   0: QES
                                     //   1: Heading
                                     //   2: Int
                                     //   3: Float
                                     //   4: String

    // Not shown in dialog.
    SelectedControlColour: Integer;
    LabelNamePrefix:       string;
    IniFileName:           string;
  end;

var
  ManagerSettings: TManagerSettings = (
    // Visual design:
    DefaultRightPostion:   200;
    SnapFields:            true;
    SnappingThresHold:     10;
    SpaceBtwFieldField:    10;
    SpaceBtwFieldLabel:    25;
    SpaceBtwLabelLabel:    5;

    // Field definitions:
    IntFieldLength:        2;
    FloatIntLength:        5;
    FloatDecimalLength:    2;
    StringFieldLength:     20;
    DefaultDateType:       ftDMYDate;
    FieldNamePrefix:       'V';
//    FieldNamingStyle:      fnFirstWord;

    // Advanced:
    WorkingDirUTF8:        '';
    PasteSpecialType:      1;  //Heading.

    // Not shown in dialog.
    SelectedControlColour: $00B6F5F5;
    LabelNamePrefix:       'label_';
    IniFileName:           '';
  );

{$IFDEF EPI_SHOWREVISION}
  {$I revision.inc}
{$ELSE}
  const RevisionStr = '(DEBUG)';
{$ENDIF}

function GetManagerVersion: String;
function SaveSettingToIni(Const FileName: string): boolean;
function LoadSettingsFromIni(Const FileName: string): boolean;

const
  ManagerVersion: TEpiVersionInfo = (
    VersionNo: 0;
    MajorRev:  5;
    MinorRev:  6;
    BuildNo:   0;
  );

implementation

{$R *.lfm}

uses
  IniFiles;

function GetManagerVersion: String;
begin
  result := GetEpiVersionInfo(ManagerVersion);
end;

function SaveSettingToIni(Const FileName: string): boolean;
var
  Ini: TIniFile;
  Sec: string;
begin
  Result := false;

  try
    Ini := TIniFile.Create(FileName);
    With Ini do
    with ManagerSettings do
    begin
      {  // Visual design:
        DefaultRightPostion:   Integer;
        SnapFields:            boolean;
        SnappingThresHold:     Integer;
        SpaceBtwFieldField:    Integer;
        SpaceBtwFieldLabel:    Integer;
        SpaceBtwLabelLabel:    Integer;}
      Sec := 'visual';
      WriteInteger(Sec, 'DefaultRightPostion',   DefaultRightPostion);
      WriteBool   (Sec, 'SnapFields',            SnapFields);
      WriteInteger(Sec, 'SnappingThresHold',     SnappingThresHold);
      WriteInteger(Sec, 'SpaceBtwFieldField',    SpaceBtwFieldField);
      WriteInteger(Sec, 'SpaceBtwFieldLabel',    SpaceBtwFieldLabel);
      WriteInteger(Sec, 'SpaceBtwLabelLabel',    SpaceBtwLabelLabel);

      {  // Field definitions:
      IntFieldLength:        Integer;
      FloatIntLength:        Integer;
      FloatDecimalLength:    Integer;
      StringFieldLength:     Integer;
      DefaultDateType:       TEpiFieldType;
      FieldNamePrefix:       string;
    //    FieldNamingStyle:      TFieldNaming;}
      Sec := 'fielddefs';
      WriteInteger(Sec, 'IntFieldLength',     IntFieldLength);
      WriteInteger(Sec, 'FloatIntLength',     FloatIntLength);
      WriteInteger(Sec, 'FloatDecimalLength', FloatDecimalLength);
      WriteInteger(Sec, 'StringFieldLength',  StringFieldLength);
      WriteInteger(Sec, 'DefaultDateType',    Word(DefaultDateType));

  {    // Advanced:
      WorkingDirUTF8:        string;
      PasteSpecialType:      byte}
      Sec := 'advanced';
      WriteString(Sec, 'WorkingDirectory', WorkingDirUTF8);
      WriteInteger(Sec, 'PasteAsType', PasteSpecialType);
      Result := true;
    end;
  finally
    Ini.Free;
  end;
end;

function LoadSettingsFromIni(Const FileName: string): boolean;
var
  Ini: TIniFile;
  Sec: String;
begin
  Result := false;
  ManagerSettings.IniFileName := FileName;

  if not FileExistsUTF8(FileName) then exit;

  Ini := TIniFile.Create(FileName);
  With Ini do
  with ManagerSettings do
  begin
    {  // Visual design:
      DefaultRightPostion:   Integer;
      SnapFields:            boolean;
      SnappingThresHold:     Integer;
      SpaceBtwFieldField:    Integer;
      SpaceBtwFieldLabel:    Integer;
      SpaceBtwLabelLabel:    Integer;}
    Sec := 'visual';
    DefaultRightPostion   := ReadInteger(Sec, 'DefaultRightPostion',   DefaultRightPostion);
    SnapFields            := ReadBool   (Sec, 'SnapFields',            SnapFields);
    SnappingThresHold     := ReadInteger(Sec, 'SnappingThresHold',     SnappingThresHold);
    SpaceBtwFieldField    := ReadInteger(Sec, 'SpaceBtwFieldField',    SpaceBtwFieldField);
    SpaceBtwFieldLabel    := ReadInteger(Sec, 'SpaceBtwFieldLabel',    SpaceBtwFieldLabel);
    SpaceBtwLabelLabel    := ReadInteger(Sec, 'SpaceBtwLabelLabel',    SpaceBtwLabelLabel);

    {  // Field definitions:
    IntFieldLength:        Integer;
    FloatIntLength:        Integer;
    FloatDecimalLength:    Integer;
    StringFieldLength:     Integer;
    DefaultDateType:       TEpiFieldType;
    FieldNamePrefix:       string;
  //    FieldNamingStyle:      TFieldNaming;}
    Sec := 'fielddefs';
    IntFieldLength     := ReadInteger(Sec, 'IntFieldLength',     IntFieldLength);
    FloatIntLength     := ReadInteger(Sec, 'FloatIntLength',     FloatIntLength);
    FloatDecimalLength := ReadInteger(Sec, 'FloatDecimalLength', FloatDecimalLength);
    StringFieldLength  := ReadInteger(Sec, 'StringFieldLength',  StringFieldLength);
    DefaultDateType    := TEpiFieldType(ReadInteger(Sec, 'DefaultDateType', Word(DefaultDateType)));

{    // Advanced:
    WorkingDirUTF8:        string;
    PasteSpecialType:      byte;}
    Sec := 'advanced';
    WorkingDirUTF8   := ReadString(Sec, 'WorkingDirectory', WorkingDirUTF8);
    PasteSpecialType := ReadInteger(Sec, 'PasteAsType', PasteSpecialType);
  end;
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
    0: ManagerSettings.DefaultDateType := ftDMYDate;
    1: ManagerSettings.DefaultDateType := ftMDYDate;
    2: ManagerSettings.DefaultDateType := ftYMDDate;
  end;
  ManagerSettings.FieldNamePrefix       := PrefixEdit.Text;
{  if FieldNamingAutoRadio.Checked then
    ManagerSettings.FieldNamingStyle    := fnAuto
  else
    ManagerSettings.FieldNamingStyle    := fnFirstWord;    }

  // Advanced:
  ManagerSettings.WorkingDirUTF8        := WorkingDirEdit.Text;
  ManagerSettings.PasteSpecialType      := DefaultPasteCombo.ItemIndex;

  SaveSettingToIni(ManagerSettings.IniFileName);
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
      ftDMYDate: DefaultDateCombo.ItemIndex := 0;
      ftMDYDate: DefaultDateCombo.ItemIndex := 1;
      ftYMDDate: DefaultDateCombo.ItemIndex := 2;
    end;
    PrefixEdit.Text                   := FieldNamePrefix;
{    FieldNamingAutoRadio.Checked      := (FieldNamingStyle = fnAuto);
    FieldNamingFirstWordRadio.Checked := (FieldNamingStyle = fnFirstWord);    }

    // Advanced:
    WorkingDirEdit.Text               := WorkingDirUTF8;
    DefaultPasteCombo.ItemIndex       := PasteSpecialType;
  end;
end;

procedure TSettingsForm.FormShow(Sender: TObject);
begin
  PageControl1.ActivePage := VisualDesignSheet;
end;


procedure TSettingsForm.SnapFieldsChkBoxChange(Sender: TObject);
begin
  SnapThresholdEdit.Enabled := SnapFieldsChkBox.Checked;
end;

initialization

begin
  ManagerSettings.WorkingDirUTF8 := GetCurrentDirUTF8 + {$IFDEF UNIX}'/data'{$ELSE}'\data'{$ENDIF};
  if not DirectoryExistsUTF8(ManagerSettings.WorkingDirUTF8) then
    ManagerSettings.WorkingDirUTF8 := GetCurrentDirUTF8;
end;

end.

