unit settings;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, MaskEdit;

type

  { TSettingsForm }

  TSettingsForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Label2: TLabel;
    Label3: TLabel;
    DefaultRightPosEdit: TMaskEdit;
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

  TBuilderSettings = record
    FieldNamePrefix: string;
    ShowFieldNamesInLabel: boolean;
    SpaceBetweenFields: Integer;
    SnappingThresHold: Integer;
    DefaultRightPostion: Integer;
  end;

var
  BuilderSettings: TBuilderSettings = (
    FieldNamePrefix: 'V';
    ShowFieldNamesInLabel: true;
    SpaceBetweenFields: 10;
    SnappingThresHold: 10;
    DefaultRightPostion: 200;
  );

implementation

{ TSettingsForm }

procedure TSettingsForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  S: String;
begin
  CanClose := true;
  if ModalResult = mrCancel then
    exit;

  CanClose := false;
  S := UTF8Encode(Trim(UTF8Decode(PrefixEdit.Text)));
  if S = '' then
  begin
    PrefixEdit.SetFocus;
    Exit;
  end;

  S := Trim(DefaultRightPosEdit.Text);
  if (S = '') or (StrToInt(S) <= 0) then
  begin
    DefaultRightPosEdit.SetFocus;
    Exit;
  end;

  BuilderSettings.FieldNamePrefix := PrefixEdit.Text;
  BuilderSettings.ShowFieldNamesInLabel := ShowFieldNameChkBox.Checked;
  BuilderSettings.DefaultRightPostion := StrToInt(Trim(DefaultRightPosEdit.Text));

  CanClose := true;
end;

procedure TSettingsForm.FormCreate(Sender: TObject);
begin
  PrefixEdit.Text := BuilderSettings.FieldNamePrefix;
  ShowFieldNameChkBox.Checked := BuilderSettings.ShowFieldNamesInLabel;
  DefaultRightPosEdit.Text := IntToStr(BuilderSettings.DefaultRightPostion);
end;

initialization
  {$I settings.lrs}

end.

