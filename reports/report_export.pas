unit report_export;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, report_base, epidocument;

type

  { TReportExport }

  TReportExport = class(TReportFileListBase)
  protected
    function GetTitle: string; override;
    procedure DoDocumentReport(const Doc: TEpiDocument; const FileName: string;
      const Index: Integer); override;
  end;

implementation


uses
  epireport_base, epireport_report_controllist, epireport_report_studyinfo;

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

  R := TEpiReportControlList.Create(Generator);
  with TEpiReportControlList(R) do
  begin
    ControlItems := Doc.DataFiles[0].ControlItems;
    ControlItemsAreSubItemized := false;
    ExtendedList := true;
  end;
  R.RunReport;

  Generator.Line('');

  R := TEpiReportStudyInfo.Create(Generator);
  TEpiReportStudyInfo(R).Document := Doc;
  R.RunReport;
end;


end.

