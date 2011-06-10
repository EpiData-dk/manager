unit report_combinedlist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, report_base;

type

  { TReportCombinedList }

  TReportCombinedList = class(TReportListBase)
  protected
    function GetTitle: string; override;
  public
    function RunReport: string; override;
  end;

implementation


uses
  epidocument,
  epireport_base, epireport_fieldlist_simple,
  epireport_valuelabels,
  epireport_htmlgenerator, epireport_filelist;


resourcestring
  rsReportCombinedListTitle = 'Report: Combined list of fields/question and valuelabels.';

{ TReportCombinedList }

function TReportCombinedList.GetTitle: string;
begin
  Result := rsReportCombinedListTitle;
end;

function TReportCombinedList.RunReport: string;
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
    R := TEpiReportSimpleFieldListHtml.Create(TEpiDocument(Documents.Objects[i]), stEntryFlow);
    R.RunReport;
    Result += R.ReportText;
    R.Free;

    R := TEpiReportValueLabelsHtml.Create(TEpiDocument(Documents.Objects[i]), false);
    R.RunReport;
    Result += R.ReportText +
      '<div style="page-break-after:always;">' + LineEnding ;
    R.Free;
  end;

  Result += TEpiReportHTMLGenerator.HtmlFooter;
end;

end.

