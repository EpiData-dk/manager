unit export_ddi_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls,
  export_frame_types, epiexportsettings, epidocument,
  epimiscutils, settings2_interface, settings2_var;

type

  { TExportDDIFrame }

  TExportDDIFrame = class(TFrame, IExportSettingsPresenterFrame, ISettingsFrame)
    RemoveVLCheckBox: TCheckBox;
    FilterTagIsUserIdChkBox: TCheckBox;
    ComboBox1: TComboBox;
    Label1: TLabel;
    XMLOptionPanel: TPanel;
  private
    { private declarations }
    ValueLabelFrame: TCustomFrame;
    CSVFrame: TCustomFrame;
    FData: PManagerSettings;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function UpdateExportSetting(Setting: TEpiExportSetting): boolean;
    function GetFrameCaption: string;
    function GetExportName: string;
    function GetFileDialogExtensions: TEpiDialogFilters;
    procedure SetSettings(Data: PManagerSettings);
    function  ApplySettings: boolean;
    function ExportHeadings: boolean;
    function CheckExportAllowed(Const Setting: TEpiExportSetting;
      Const Doc: TEpiDocument;
      out ErrorText: string): boolean;
  end;

implementation

{$R *.lfm}

uses
  export_form,
  export_customvaluelabel_frame,
  export_csv_frame,
  settings2, epi_iso639, epistringutils;

{ TExportDDIFrame }

constructor TExportDDIFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  CSVFrame := TExportCSVFrame.Create(Self);
  CSVFrame.Parent := self;
  CSVFrame.AutoSize := true;
  CSVFrame.AnchorToNeighbour(akTop, 0, ComboBox1);
  CSVFrame.AnchorParallel(akLeft, 0, Self);
  CSVFrame.AnchorParallel(akRight, 0, Self);

  ValueLabelFrame := TCustomValueLabelFrame.Create(self);
  ValueLabelFrame.Parent := self;
  ValueLabelFrame.AnchorToNeighbour(akTop, 10, CSVFrame);
  ValueLabelFrame.AnchorParallel(akLeft, 10, Self);
  ValueLabelFrame.AnchorParallel(akRight, 10, Self);
  ValueLabelFrame.AutoSize := true;
//  ValueLabelFrame.AnchorParallel(akBottom, 10, Self);

  XMLOptionPanel.AnchorToNeighbour(akTop, 8, ValueLabelFrame);
{  XMLOptionPanel.AnchorParallel(akLeft, 10, Self);
  XMLOptionPanel.AnchorParallel(akRight, 10, Self);
  XMLOptionPanel.AnchorParallel(akBottom, 0, Self); }
  XMLOptionPanel.Caption := '';

  ComboBox1.Items.BeginUpdate;
  Epi_ISO639_AddLangAndDesciption(ComboBox1.Items);
  ComboBox1.Sorted := true;
  ComboBox1.Items.EndUpdate;
  ComboBox1.ItemIndex := -1;

  // SETUP ACCORDING TO MANAGERSETTINGS.
  SetSettings(@ManagerSettings);
end;

destructor TExportDDIFrame.Destroy;
begin
  inherited Destroy;
end;

function TExportDDIFrame.UpdateExportSetting(Setting: TEpiExportSetting
  ): boolean;
var
  CSVSettings: TEpiCSVExportSetting;
begin
  (ValueLabelFrame as IExportSettingsFrame).UpdateExportSetting(Setting);
  with TEpiDDIExportSetting(Setting) do
  begin
    SoftwareName := 'EpiData Manager';
    SoftwareVersion := GetManagerVersion;
    Version := '1.0.0';
    ExportLang := TString(ComboBox1.Items.Objects[ComboBox1.ItemIndex]).Str;
    RemoveMissingVL := RemoveVLCheckBox.Checked;
    FilterTagIsUserId := FilterTagIsUserIdChkBox.Checked;
  end;

  CSVSettings := TEpiCSVExportSetting.Create;
  CSVSettings.Assign(Setting);

  (CSVFrame as IExportSettingsPresenterFrame).UpdateExportSetting(CSVSettings);
  Setting.AdditionalExportSettings := CSVSettings;
end;

function TExportDDIFrame.GetFrameCaption: string;
begin
  result := 'DDI Options'
end;

function TExportDDIFrame.GetExportName: string;
begin
  result := 'DDI'
end;

function TExportDDIFrame.GetFileDialogExtensions: TEpiDialogFilters;
begin
  result := [dfDDI]
end;

procedure TExportDDIFrame.SetSettings(Data: PManagerSettings);
begin
  Fdata := Data;
  TCustomValueLabelFrame(ValueLabelFrame).ExportValueLabelsChkBox.Checked := FData^.ExportDDIValueLabels;
end;

function TExportDDIFrame.ApplySettings: boolean;
begin
  FData^.ExportDDIValueLabels := TCustomValueLabelFrame(ValueLabelFrame).ExportValueLabelsChkBox.Checked;
  result := true;
end;

function TExportDDIFrame.ExportHeadings: boolean;
begin
  result := true;
end;

function TExportDDIFrame.CheckExportAllowed(const Setting: TEpiExportSetting;
  const Doc: TEpiDocument; out ErrorText: string): boolean;
begin
  result := true;

  if Doc.Study.Agency = '' then
  begin
    ErrorText :=
      'A DDI Export CANNOT contain an empty Agency' + LineEnding +
      LineEnding +
      'Please open Study Information and provide the details before exporting.';
    result := false;
  end;

  if ComboBox1.ItemIndex < 0 then
  begin
    ErrorText :=
      'When exporting to DDI you MUST select a language for your project' + LineEnding +
      LineEnding +
      'Please select the language on the ' + GetFrameCaption + ' tab.';
    Result := false;
  end;
end;

initialization
  RegisterExportFrame(TExportDDIFrame, TEpiDDIExportSetting);

end.

