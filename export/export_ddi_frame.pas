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
    VarPrefixEdit: TEdit;
    RenameVariablesChkBox: TCheckBox;
    SectionHeaderIsQTextChkBox: TCheckBox;
    RemoveVLCheckBox: TCheckBox;
    FilterTagIsUserIdChkBox: TCheckBox;
    ComboBox1: TComboBox;
    Label1: TLabel;
    XMLOptionPanel: TPanel;
    procedure RenameVariablesChkBoxChange(Sender: TObject);
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

procedure TExportDDIFrame.RenameVariablesChkBoxChange(Sender: TObject);
begin
  VarPrefixEdit.Enabled := RenameVariablesChkBox.Checked;
end;

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
var
  i: Integer;
begin
  for i := 0 to ComboBox1.Items.Count - 1 do
    ComboBox1.Items.Objects[i].Free;

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
    RenameVariablesPrefix := '';
    if RenameVariablesChkBox.Checked then
      RenameVariablesPrefix := VarPrefixEdit.Text;
    SectionCaptionIsQText := SectionHeaderIsQTextChkBox.Checked;
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
var
  i: Integer;
begin
  Fdata := Data;

  (CSVFrame as ISettingsFrame).SetSettings(Data);
  with FData^ do
  begin
    TCustomValueLabelFrame(ValueLabelFrame).ExportValueLabelsChkBox.Checked := ExportDDIValueLabels;
    RemoveVLCheckBox.Checked := ExportDDIValueLabels;
    FilterTagIsUserIdChkBox.Checked := ExportDDIFilterTagIsUserId;
    SectionHeaderIsQTextChkBox.Checked := ExportDDISectionCaptionIsQText;
    RenameVariablesChkBox.Checked := ExportDDIRenameVariables;
    VarPrefixEdit.Text := ExportDDIRenameVariablesPrefix;

    for i := 0 to ComboBox1.Items.Count - 1 do
    begin
      if TString(ComboBox1.Items.Objects[i]).Str = ExportDDILanguageISO then
      begin
        ComboBox1.ItemIndex := i;
        break;
      end;
    end;
  end;
end;

function TExportDDIFrame.ApplySettings: boolean;
begin
  if VarPrefixEdit.Text = '' then
    Exit(False);

  with FData^ do
  begin
    ExportDDIValueLabels           := TCustomValueLabelFrame(ValueLabelFrame).ExportValueLabelsChkBox.Checked;
    ExportDDIValueLabels           := RemoveVLCheckBox.Checked;
    ExportDDIFilterTagIsUserId     := FilterTagIsUserIdChkBox.Checked;
    ExportDDISectionCaptionIsQText := SectionHeaderIsQTextChkBox.Checked;
    ExportDDIRenameVariables       := RenameVariablesChkBox.Checked;
    ExportDDIRenameVariablesPrefix := VarPrefixEdit.Text;
    ExportDDILanguageISO           := TString(ComboBox1.Items.Objects[ComboBox1.ItemIndex]).Str;
  end;
  result := (CSVFrame as ISettingsFrame).ApplySettings;
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
      'Please open in Manager (Recent files).' + LineEnding +
      'Complete "Study Information" in menu "Project Details"';
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

  if RenameVariablesChkBox.Checked and
     (VarPrefixEdit.Text = '')
  then
    begin
      ErrorText := 'The variable name cannot be empty!';
      Result := false;
    end;
end;

initialization
  RegisterExportFrame(TExportDDIFrame, TEpiDDIExportSetting);

end.

