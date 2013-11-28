unit export_customtext_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, MaskEdit,
  export_frame_types, epiexportsettings;

type

  { TExportCustomTextFrame }

  TExportCustomTextFrame = class(TFrame, IExportSettingsFrame)
    ByteOrderMarkCheckBox: TCheckBox;
    ExportFieldNameChkBox: TCheckBox;
    QuoteCharEdit: TEdit;
    Label1: TLabel;
  private
    { private declarations }
  public
    { public declarations }
    function UpdateExportSetting(Setting: TEpiExportSetting): boolean;
  end; 

implementation

{$R *.lfm}

{ TExportCustomTextFrame }

function TExportCustomTextFrame.UpdateExportSetting(Setting: TEpiExportSetting): boolean;
begin
  with TEpiCustomTextExportSettings(Setting) do
  begin
    QuoteChar := QuoteCharEdit.Text;
    ExportFieldNames := ExportFieldNameChkBox.Checked;
    ByteOrderMark := ByteOrderMarkCheckBox.Checked;
  end;
  result := true;
end;

end.

