unit export_frame_types;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, epiexportsettings, epimiscutils, epidocument;

type
  IExportSettingsFrame = interface ['IExportSettingsFrame']
    function UpdateExportSetting(Setting: TEpiExportSetting): boolean;
  end;

  { IExportSettingsPresenterFrame }

  IExportSettingsPresenterFrame = interface(IExportSettingsFrame) ['IExportSettingsPresenterFrame']
    function GetFrameCaption: string;
    function GetExportName: string;
    function GetFileDialogExtensions: TEpiDialogFilters;
    function ExportHeadings: boolean;
    function CheckExportAllowed(Const Setting: TEpiExportSetting;
      Const Doc: TEpiDocument;
      out ErrorText: string): boolean;
  end;

implementation

end.

