unit export_ddi_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls,
  export_frame_types, epiexportsettings, epidocument,
  epimiscutils, settings2_interface, settings2_var;

type

  { TExportDDIFrame }

  TExportDDIFrame = class(TFrame, IExportSettingsPresenterFrame, ISettingsFrame)
  private
    { private declarations }
    LocalFrame: TCustomFrame;
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
  end;

implementation

{$R *.lfm}

uses
  export_form, export_customvaluelabel_frame,
  settings2;

{ TExportDDIFrame }

constructor TExportDDIFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  LocalFrame := TCustomValueLabelFrame.Create(self);
  LocalFrame.Parent := self;
  LocalFrame.AnchorClient(10);
  // SETUP ACCORDING TO MANAGERSETTINGS.
  SetSettings(@ManagerSettings);
end;

destructor TExportDDIFrame.Destroy;
begin
  inherited Destroy;
end;

function TExportDDIFrame.UpdateExportSetting(Setting: TEpiExportSetting
  ): boolean;
begin
  (LocalFrame as IExportSettingsFrame).UpdateExportSetting(Setting);
  with TEpiDDIExportSetting(Setting) do
  begin
    SoftwareName := 'EpiData Manager';
    SoftwareVersion := GetManagerVersion;
    Version := '1.0.0';
  end;
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
  TCustomValueLabelFrame(LocalFrame).ExportValueLabelsChkBox.Checked := FData^.ExportDDIValueLabels;
end;

function TExportDDIFrame.ApplySettings: boolean;
begin
  FData^.ExportDDIValueLabels := TCustomValueLabelFrame(LocalFrame).ExportValueLabelsChkBox.Checked;
  result := true;
end;

initialization
  RegisterExportFrame(TExportDDIFrame, TEpiDDIExportSetting);

end.

