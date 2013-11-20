unit export_stata_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls,
  StdCtrls, ExtCtrls, export_frame_types, epiexportsettings,
  epimiscutils, settings2_interface, settings2_var, epidocument;

type

  { TExportStataFrame }

  TExportStataFrame = class(TFrame, IExportSettingsPresenterFrame, ISettingsFrame)
    VersionComboBox: TComboBox;
    Label4: TLabel;
    FieldNamingRGrp: TRadioGroup;
  private
    { private declarations }
    LocalFrame: TFrame;
    FData: PManagerSettings;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
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
  export_customvaluelabel_frame, epieximtypes, export_form,
  epiversionutils, settings2;

{ TExportStataFrame }

constructor TExportStataFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  LocalFrame := TCustomValueLabelFrame.Create(self);
  LocalFrame.Parent := self;
  LocalFrame.AnchorToNeighbour(akTop, 10, FieldNamingRGrp);
  LocalFrame.AnchorParallel(akLeft, 20, Self);
  LocalFrame.AnchorParallel(akRight, 20, Self);
  LocalFrame.AnchorParallel(akBottom, 10, Self);

  with VersionComboBox.Items do
  begin
    Clear;
    AddObject('Stata 4',      TObject(dta4));
    AddObject('Stata 5, 6',   TObject(dta6));
    AddObject('Stata 7',      TObject(dta7));
    AddObject('Stata 8, 9',   TObject(dta8));
    AddObject('Stata 10, 11', TObject(dta10));
    AddObject('Stata 12',     TObject(dta12));
  end;

  with FieldNamingRGrp.Items do
  begin
    Clear;
    AddObject('UPPERCASE', TObject(fncUpper));
    AddObject('lowercase', TObject(fncLower));
    AddObject('Leave As Is', TObject(fncAsIs));
  end;

  SetSettings(@ManagerSettings);
end;

function TExportStataFrame.UpdateExportSetting(Setting: TEpiExportSetting): boolean;
begin
  with TEpiStataExportSetting(Setting) do
  begin
    Version := TEpiStataVersion(PtrUInt(VersionComboBox.Items.Objects[VersionComboBox.ItemIndex]));
    FieldNameCase := TEpiFieldNamingCase(PtrUInt(FieldNamingRGrp.Items.Objects[FieldNamingRGrp.ItemIndex]));
    with ExportLines do
    begin
      Add('Exported from EpiData Manager ' + GetEpiVersionInfo(HINSTANCE));
      Add('On: ' + FormatDateTime('YYYY/MM/DD HH:NN:SS', Now));
      Add('Title: ' + Doc.Study.Title.Text);
      Add('Version: ' + Doc.Study.Version);
    end;
  end;

  result := result and
    (LocalFrame as IExportSettingsFrame).UpdateExportSetting(Setting);
end;

function TExportStataFrame.GetFrameCaption: string;
begin
  result := 'Stata Options';
end;

function TExportStataFrame.GetExportName: string;
begin
  result := 'Stata';
end;

function TExportStataFrame.GetFileDialogExtensions: TEpiDialogFilters;
begin
  result := [dfDTA];
end;

procedure TExportStataFrame.SetSettings(Data: PManagerSettings);
begin
  FData := Data;
  with FData^ do
  begin
    VersionComboBox.ItemIndex := VersionComboBox.Items.IndexOfObject(TObject(PtrUInt(ExportStataVersion)));
    FieldNamingRGrp.ItemIndex := FieldNamingRGrp.Items.IndexOfObject(TObject(PtrUInt(ExportStataFieldCase)));
    TCustomValueLabelFrame(LocalFrame).ExportValueLabelsChkBox.Checked := ExportStataValueLabels;
  end;
end;

function TExportStataFrame.ApplySettings: boolean;
begin
  with FData^ do
  begin
    ExportStataVersion := TEpiStataVersion(PtrUInt(VersionComboBox.Items.Objects[VersionComboBox.ItemIndex]));
    ExportStataFieldCase := TEpiFieldNamingCase(PtrUInt(FieldNamingRGrp.Items.Objects[FieldNamingRGrp.ItemIndex]));
    ExportStataValueLabels := TCustomValueLabelFrame(LocalFrame).ExportValueLabelsChkBox.Checked;
  end;
  result := true;
end;

function TExportStataFrame.ExportHeadings: boolean;
begin
  result := false;
end;

function TExportStataFrame.CheckExportAllowed(const Setting: TEpiExportSetting;
  const Doc: TEpiDocument; out ErrorText: string): boolean;
begin
  result := true;
end;

initialization
  RegisterExportFrame(TExportStataFrame, TEpiStataExportSetting);

end.

