unit export_frame_types;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, epiexportsettings, epimiscutils;

type
  IExportSettingsFrame = interface ['IExportSettingsFrame']
    function UpdateExportSetting(Setting: TEpiExportSetting): boolean;
  end;

  IExportSettingsPresenterFrame = interface(IExportSettingsFrame) ['IExportSettingsPresenterFrame']
    function GetFrameCaption: string;
    function GetExportName: string;
    function GetFileDialogExtensions: TEpiDialogFilters;
  end;

implementation

end.

