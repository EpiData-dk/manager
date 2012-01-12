unit export_csv_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, epiexportsettings,
  export_frame_types, epimiscutils;

type

  { TCSVFrame }

  TCSVFrame = class(TFrame, IExportSettingsPresenterFrame)
  private
    { private declarations }
  public
    { public declarations }
    function GetExportName: string;
    function GetFrameCaption: string;
    function UpdateExportSetting(Setting: TEpiExportSetting): boolean;
    function GetFileDialogExtensions: TEpiDialogFilters;
  end; 

implementation

{$R *.lfm}

uses
  export_form;

{ TCSVFrame }

function TCSVFrame.GetExportName: string;
begin
  result := 'CSV File';
end;

function TCSVFrame.GetFrameCaption: string;
begin
  result := 'CSV Export';
end;

function TCSVFrame.UpdateExportSetting(Setting: TEpiExportSetting): boolean;
begin
  result := true;
end;

function TCSVFrame.GetFileDialogExtensions: TEpiDialogFilters;
begin
  result := [dfText];
end;

initialization
  RegisterExportFrame(TCSVFrame, TEpiCSVExportSetting);

end.

