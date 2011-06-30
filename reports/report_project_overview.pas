unit report_project_overview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, report_base;

type

  { TReportProjectOverview }

  TReportProjectOverview = class(TReportListBase)
  protected
    function GetTitle: string; override;
  public
    function RunReport: string; override;
  end;

implementation

uses
  epidocument,
  epireport_base, epireport_types, epireport_project_overview,
  epireport_htmlgenerator, epireport_filelist;


resourcestring
  rsReportProjectOverviewTitle = 'Project Overview.';

{ TReportProjectOverview }

function TReportProjectOverview.GetTitle: string;
begin
  Result := rsReportProjectOverviewTitle;
end;

function TReportProjectOverview.RunReport: string;
var
  Doc: TEpiDocument;
  i: Integer;
  R: TEpiReportBase;
begin
  Result := TEpiReportHTMLGenerator.HtmlHeader(ReportTitle, StyleSheet);
  Result +=
    '<h3>Report: ' + ReportTitle + ' Created ' + FormatDateTime('YYYY/MM/DD HH:NN:SS', Now)  + '</h3>';

  R := TEpiReportFileListHtml.Create(Documents);
  R.RunReport;
  Result += R.ReportText;
  R.Free;

  for i := 0 to Documents.Count - 1 do
  begin
    Result += '<h2>File: ' + Documents[i] + '</h2>';
    R := TEpiReportProjectOverViewHtml.Create(TEpiDocument(Documents.Objects[i]));
    R.RunReport;

    Result += R.ReportText +
      '<div style="page-break-after:always;">' + LineEnding ;

    R.Free;
  end;

  Result += TEpiReportHTMLGenerator.HtmlFooter;
end;

end.

