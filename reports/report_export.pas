unit report_export;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, report_base, epidocument, epiexportsettings;

type

  { TReportExport }

  TReportExport = class(TReportFileListBase)
  private
    FExportSettings: TEpiExportSetting;
  protected
    function GetTitle: string; override;
    procedure DoDocumentReport(const Doc: TEpiDocument; const FileName: string;
      const Index: Integer); override;
  public
    property ExportSettings: TEpiExportSetting read FExportSettings write FExportSettings;
  end;

implementation


uses
  epireport_base, epireport_report_controllist, epireport_report_studyinfo,
  epireport_report_exportsettings;

resourcestring
  rsReportExportTitle = 'Export Report';

{ TReportExport }

function TReportExport.GetTitle: string;
begin
  result := rsReportExportTitle;
end;

procedure TReportExport.DoDocumentReport(const Doc: TEpiDocument;
  const FileName: string; const Index: Integer);
var
  R: TEpiReportBase;
begin
  inherited DoDocumentReport(Doc, FileName, Index);

  Generator.Heading('Selections for Export');

  R := TEpiReportExportSettings.Create(Generator);
  TEpiReportExportSettings(R).ExportSetting := ExportSettings;
  R.RunReport;
  R.Free;

  Generator.Line('');

  R := TEpiReportControlList.Create(Generator);
  with TEpiReportControlList(R) do
  begin
    ControlItems := Doc.DataFiles[0].ControlItems;
    ControlItemsAreSubItemized := false;
    ExtendedList := true;
  end;
  R.RunReport;
  R.Free;

  Generator.Line('');

  R := TEpiReportStudyInfo.Create(Generator);
  TEpiReportStudyInfo(R).Document := Doc;
  R.RunReport;
  R.Free;
end;


end.

