unit settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls;

type

  { TSettingsForm }

  TSettingsForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ShowFieldNameChkBox: TCheckBox;
    PrefixEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
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
  end;

var
  BuilderSettings: TBuilderSettings = (
    FieldNamePrefix: 'V';
    ShowFieldNamesInLabel: true;
    SpaceBetweenFields: 10;
    SnappingThresHold: 10;
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
    Exit;

  BuilderSettings.FieldNamePrefix := PrefixEdit.Text;
  BuilderSettings.ShowFieldNamesInLabel := ShowFieldNameChkBox.Checked;

  CanClose := true;
end;

procedure TSettingsForm.FormCreate(Sender: TObject);
begin
  PrefixEdit.Text := BuilderSettings.FieldNamePrefix;
  ShowFieldNameChkBox.Checked := BuilderSettings.ShowFieldNamesInLabel;
end;

initialization
  {$I settings.lrs}

end.

