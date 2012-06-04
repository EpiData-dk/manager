unit export_sas_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls,
  epiexportsettings, export_frame_types, epimiscutils, settings2_interface,
  settings2_var;

type

  { TExportSASFrame }

  TExportSASFrame = class(TFrame, IExportSettingsPresenterFrame, ISettingsFrame)
  private
    { private declarations }
    FFrame: TCustomFrame;
    FData: PManagerSettings;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    function UpdateExportSetting(Setting: TEpiExportSetting): boolean;
    function GetFrameCaption: string;
    function GetExportName: string;
    function GetFileDialogExtensions: TEpiDialogFilters;
    procedure SetSettings(Data: PManagerSettings);
    function ApplySettings: boolean;
  end;

implementation

{$R *.lfm}

uses
  export_form, export_customvaluelabel_frame;

{ TExportSASFrame }

constructor TExportSASFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FFrame := TCustomValueLabelFrame.Create(self);
  FFrame.Parent := self;
  FFrame.Align := alClient;

  // SETUP ACCORDING TO MANAGERSETTINGS.
  SetSettings(@ManagerSettings);
end;

function TExportSASFrame.UpdateExportSetting(Setting: TEpiExportSetting
  ): boolean;
begin
  result := (FFrame as IExportSettingsFrame).UpdateExportSetting(Setting);
end;

function TExportSASFrame.GetFrameCaption: string;
begin
  result := 'SAS Options'
end;

function TExportSASFrame.GetExportName: string;
begin
  result := 'SAS';
end;

function TExportSASFrame.GetFileDialogExtensions: TEpiDialogFilters;
begin
  result := [dfSAS];
end;

procedure TExportSASFrame.SetSettings(Data: PManagerSettings);
begin
  Fdata := Data;
  TCustomValueLabelFrame(FFrame).ExportValueLabelsChkBox.Checked := FData^.ExportSPSSValueLabels;
end;

function TExportSASFrame.ApplySettings: boolean;
begin
  FData^.ExportSPSSValueLabels := TCustomValueLabelFrame(FFrame).ExportValueLabelsChkBox.Checked;
  result := true;
end;

initialization
  RegisterExportFrame(TExportSASFrame, TEpiSASExportSetting);

end.
