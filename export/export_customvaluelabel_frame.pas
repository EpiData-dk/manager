unit export_customvaluelabel_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls,
  export_frame_types, epiexportsettings;

type

  { TCustomValueLabelFrame }

  TCustomValueLabelFrame = class(TFrame, IExportSettingsFrame)
    ExportValueLabelsChkBox: TCheckBox;
  private
    { private declarations }
  public
    { public declarations }
    function UpdateExportSetting(Setting: TEpiExportSetting): boolean;
  end;

implementation

{$R *.lfm}

{ TCustomValueLabelFrame }

function TCustomValueLabelFrame.UpdateExportSetting(Setting: TEpiExportSetting
  ): boolean;
begin
  with TEpiCustomValueLabelExportSetting(Setting) do
  begin
    ExportValueLabels := ExportValueLabelsChkBox.Checked;
  end;
end;

end.

