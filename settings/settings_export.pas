unit settings_export;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls,
  settings2_interface, settings2_var;

type

  { TSettings_ExportFrame }

  TSettings_ExportFrame = class(TFrame, ISettingsFrame)
    StataVersionCmbBx: TComboBox;
    StataEncodingCmbBx: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
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
  with StataVersionCmbBx.Items do
  begin
    Clear;
    AddObject('Stata 4',      TObject($69));
    AddObject('Stata 5, 6',   TObject($6C));
    AddObject('Stata 7',      TObject($6E));
    AddObject('Stata 8, 9',   TObject($71));
    AddObject('Stata 10, 11', TObject($72));
    AddObject('Stata 12',     TObject($73));
  end;

  with StataEncodingCmbBx.Items do
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
    StataVersionCmbBx.ItemIndex := StataVersionCmbBx.Items.IndexOfObject(TObject(PtrUInt(StataExportVersion)));
    StataEncodingCmbBx.ItemIndex := StataEncodingCmbBx.Items.IndexOfObject(TObject(PtrUInt(StataExportEncoding)));
  end;
end;

function TSettings_ExportFrame.ApplySettings: boolean;
begin
  with FData ^ do
  begin
    StataExportVersion := TEpiStataVersion(PtrUInt(StataVersionCmbBx.Items.Objects[StataVersionCmbBx.ItemIndex]));
    StataExportEncoding := TEpiEncoding(PtrUInt(StataEncodingCmbBx.Items.Objects[StataEncodingCmbBx.ItemIndex]));
  end;
  result := true;
end;

end.

