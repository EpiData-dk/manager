unit settings_export;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls,
  settings2_interface, settings2_var;

type

  { TSettings_ExportFrame }

  TSettings_ExportFrame = class(TFrame, ISettingsFrame)
    EncodingCmbBox: TComboBox;
    ExportDeletedChkBox: TCheckBox;
    ExportTypeCombo: TComboBox;
    Label1: TLabel;
    Label5: TLabel;
  private
    { private declarations }
    FData: PManagerSettings;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    procedure SetSettings(Data: PManagerSettings);
    function  ApplySettings: boolean;
  end;

implementation

{$R *.lfm}

uses
  epieximtypes;

{ TSettings_ExportFrame }

constructor TSettings_ExportFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  with ExportTypeCombo.Items do
  begin
    Clear;
    AddObject('Stata', TObject(0));
    AddObject('CSV',   TObject(1));
    AddObject('SPSS',  TObject(2));
    AddObject('SAS',   TObject(3));
  end;

  // Encodings
  with EncodingCmbBox.Items do
  begin
    Clear;
    AddObject('Unicode (UTF-8)',        TObject(eeUTF8));
    AddObject('Centel Europe (CP1250)', TObject(eeCP1250));
    AddObject('Cyrillic (CP1251)',      TObject(eeCP1251));
    AddObject('Latin 1(CP1252)',        TObject(eeCP1252));
    AddObject('Greek (CP1253)',         TObject(eeCP1253));
    AddObject('Turkish (CP1254)',       TObject(eeCP1254));
    AddObject('Hebrew (CP1255)',        TObject(eeCP1255));
    AddObject('Arabic (CP1256)',        TObject(eeCP1256));
    AddObject('Baltic (CP1257)',        TObject(eeCP1257));
    AddObject('Vietnamese (CP1258)',    TObject(eeCP1258));
    AddObject('Thai (CP874)',           TObject(eeCP874));
    AddObject('Russian (KOI8)',         TObject(eeKOI8));
    AddObject('Chinese simple (CP936)', TObject(eeCP936));
    AddObject('Chinese complex (CP950)',TObject(eeCP950));
    AddObject('Korean (CP949)',         TObject(eeCP949));
    AddObject('Japanes (CP932)',        TObject(eeCP932));
  end;
end;

procedure TSettings_ExportFrame.SetSettings(Data: PManagerSettings);
begin
  FData := Data;
  with FData^ do
  begin
    ExportTypeCombo.ItemIndex   := ExportTypeCombo.Items.IndexOfObject(TObject(PtrUInt(ExportType)));
    EncodingCmbBox.ItemIndex    := EncodingCmbBox.Items.IndexOfObject(TObject(PtrUInt(ExportEncoding)));
    ExportDeletedChkBox.Checked := ExportDeleted;
  end;
end;

function TSettings_ExportFrame.ApplySettings: boolean;
begin
  with FData ^ do
  begin
    ExportType     := PtrUInt(ExportTypeCombo.Items.Objects[ExportTypeCombo.ItemIndex]);
    ExportEncoding := TEpiEncoding(PtrUInt(EncodingCmbBox.Items.Objects[EncodingCmbBox.ItemIndex]));
    ExportDeleted  := ExportDeletedChkBox.Checked;
  end;
  result := true;
end;

end.

