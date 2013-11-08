unit export_epx_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls,
  export_frame_types, epiexportsettings, epimiscutils,
  export_customvaluelabel_frame, settings2_interface,
  settings2_var;

type

  { TExportEPXFrame }

  TExportEPXFrame = class(TFrame, IExportSettingsPresenterFrame, ISettingsFrame)
  private
    Frame: TCustomValueLabelFrame;
    FData: PManagerSettings;
  public
    // IExportSettingsPresenterFrame
    constructor Create(TheOwner: TComponent); override;
    function UpdateExportSetting(Setting: TEpiExportSetting): boolean;
    function GetFrameCaption: string;
    function GetExportName: string;
    function GetFileDialogExtensions: TEpiDialogFilters;
    function ExportHeadings: boolean;
    // ISettingsFrame
    procedure SetSettings(Data: PManagerSettings);
    function  ApplySettings: boolean;
  end;

implementation

uses
  export_form;

{$R *.lfm}

{ TExportEPXFrame }

constructor TExportEPXFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  Frame := TCustomValueLabelFrame.Create(self);
  Frame.Parent := Self;
  Frame.AnchorClient(10);
  SetSettings(@ManagerSettings);
end;

function TExportEPXFrame.UpdateExportSetting(Setting: TEpiExportSetting
  ): boolean;
begin
  Frame.UpdateExportSetting(Setting);
end;

function TExportEPXFrame.GetFrameCaption: string;
begin
  Result := 'EPX Options';
end;

function TExportEPXFrame.GetExportName: string;
begin
  result := 'EPX';
end;

function TExportEPXFrame.GetFileDialogExtensions: TEpiDialogFilters;
begin
  result := [dfEPX];
end;

function TExportEPXFrame.ExportHeadings: boolean;
begin
  Result := true;
end;

procedure TExportEPXFrame.SetSettings(Data: PManagerSettings);
begin
  FData := Data;
  Frame.ExportValueLabelsChkBox.Checked := FData^.ExportEPXValueLabels;
end;

function TExportEPXFrame.ApplySettings: boolean;
begin
  FData^.ExportEPXValueLabels := Frame.ExportValueLabelsChkBox.Checked;
  result := true;
end;

initialization
  RegisterExportFrame(TExportEPXFrame, TEpiEPXExportSetting);


end.

