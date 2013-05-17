unit export_ddi_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls,
  export_frame_types, epiexportsettings,
  epimiscutils, settings2_interface, settings2_var;

type

  { TExportDDIFrame }

  TExportDDIFrame = class(TFrame, IExportSettingsPresenterFrame, ISettingsFrame)
  private
    { private declarations }
    LocalFrame: TCustomFrame;
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
//  LocalFrame.Align := alClient;
  LocalFrame.AnchorClient(10);
{  LocalFrame.AnchorToNeighbour(akTop, 10, FieldNamingRGrp);
  LocalFrame.AnchorParallel(akLeft, 20, Self);
  LocalFrame.AnchorParallel(akRight, 20, Self);
  LocalFrame.AnchorParallel(akBottom, 10, Self); }
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
  (LocalFrame as ISettingsFrame).SetSettings(Data);
end;

function TExportDDIFrame.ApplySettings: boolean;
begin
  (LocalFrame as ISettingsFrame).ApplySettings;
end;

initialization
  RegisterExportFrame(TExportDDIFrame, TEpiDDIExportSetting);

end.

