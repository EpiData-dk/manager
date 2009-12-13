unit settings;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, MaskEdit, ExtCtrls, UDataFileTypes;

type

  { TSettingsForm }

  TSettingsForm = class(TForm)
    OkBtn: TButton;
    CancelBtn: TButton;
    SnapFieldsChkBox: TCheckBox;
    Label2: TLabel;
    Label3: TLabel;
    DefaultRightPosEdit: TMaskEdit;
    FieldNamingGroup: TRadioGroup;
    FieldNamingAutoRadio: TRadioButton;
    FieldNamingFirstWordRadio: TRadioButton;
    Label4: TLabel;
    ShowFieldNameChkBox: TCheckBox;
    PrefixEdit: TEdit;
    Label1: TLabel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

  TManagerSettings = record
    FieldNamePrefix:       string;
    FieldNamingStyle:      TFieldNaming;
    SpaceBetweenFields:    Integer;
    SnappingThresHold:     Integer;
    DefaultRightPostion:   Integer;
    ShowFieldNamesInLabel: boolean;
    SnapFields:            boolean;
  end;

  TManagerVersion = record
    VersionNo: Integer;
    MajorRev:  Integer;
    MinorRev:  Integer;
    BuildNo:   Integer;
  end;

var
  ManagerSettings: TManagerSettings = (
    FieldNamePrefix:       'V';
    FieldNamingStyle:      fnAuto;
    SpaceBetweenFields:    10;
    SnappingThresHold:     10;
    DefaultRightPostion:   200;
    ShowFieldNamesInLabel: true;
    SnapFields:            true;
  );

const
  ManagerVersion: TManagerVersion = (
    VersionNo: 0;
    MajorRev:  1;
    MinorRev:  1;
    BuildNo:   3;
  );


function GetManagerVersion: String;

implementation

function GetManagerVersion: String;
begin
  with ManagerVersion do
    result := IntToStr(VersionNo) + '.' +
              IntToStr(MajorRev) + '.' +
              IntToStr(MinorRev) + '.' +
              IntToStr(BuildNo);
end;

{ TSettingsForm }

procedure TSettingsForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  S: String;
begin
  CanClose := true;
  if ModalResult = mrCancel then
    exit;

  CanClose := false;
  S := Trim(DefaultRightPosEdit.Text);
  if (S = '') or (StrToInt(S) <= 0) then
  begin
    DefaultRightPosEdit.SetFocus;
    Exit;
  end;

  ManagerSettings.FieldNamePrefix       := PrefixEdit.Text;
  ManagerSettings.ShowFieldNamesInLabel := ShowFieldNameChkBox.Checked;
  ManagerSettings.SnapFields            := SnapFieldsChkBox.Checked;
  ManagerSettings.DefaultRightPostion   := StrToInt(Trim(DefaultRightPosEdit.Text));
  if FieldNamingAutoRadio.Checked then
    ManagerSettings.FieldNamingStyle    := fnAuto
  else
    ManagerSettings.FieldNamingStyle    := fnFirstWord;

  CanClose := true;
end;

procedure TSettingsForm.FormCreate(Sender: TObject);
begin
  PrefixEdit.Text := ManagerSettings.FieldNamePrefix;
  ShowFieldNameChkBox.Checked := ManagerSettings.ShowFieldNamesInLabel;
  SnapFieldsChkBox.Checked := ManagerSettings.SnapFields;
  DefaultRightPosEdit.Text := IntToStr(ManagerSettings.DefaultRightPostion);
  FieldNamingAutoRadio.Checked := (ManagerSettings.FieldNamingStyle = fnAuto);
  FieldNamingFirstWordRadio.Checked := (ManagerSettings.FieldNamingStyle = fnFirstWord);
end;

initialization
  {$I settings.lrs}

end.

