unit export_spss_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls,
  epiexportsettings, export_frame_types, epimiscutils;

type

  { TExportSPSSFrame }

  TExportSPSSFrame = class(TFrame, IExportSettingsPresenterFrame)
  private
    { private declarations }
    FFrame: TFrame;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    function UpdateExportSetting(Setting: TEpiExportSetting): boolean;
    function GetFrameCaption: string;
    function GetExportName: string;
    function GetFileDialogExtensions: TEpiDialogFilters;
  end;

implementation

{$R *.lfm}

uses
  export_form, export_customvaluelabel_frame;

{ TExportSPSSFrame }

constructor TExportSPSSFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FFrame := TCustomValueLabelFrame.Create(self);
  FFrame.Parent := self;
  FFrame.Align := alClient;
end;

function TExportSPSSFrame.UpdateExportSetting(Setting: TEpiExportSetting
  ): boolean;
begin
  result := (FFrame as IExportSettingsFrame).UpdateExportSetting(Setting);
end;

function TExportSPSSFrame.GetFrameCaption: string;
begin
  result := 'SPSS Options';
end;

function TExportSPSSFrame.GetExportName: string;
begin
  result := 'SPSS';
end;

function TExportSPSSFrame.GetFileDialogExtensions: TEpiDialogFilters;
begin
  result := [dfSPSS];
end;

initialization
  RegisterExportFrame(TExportSPSSFrame, TEpiSPSSExportSetting);

end.

