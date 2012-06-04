unit report_double_entry_validation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, report_base, epidatafiles, epitools_val_dbl_entry;

type

  { TReportDoubleEntryValidation }

  TReportDoubleEntryValidation = class(TReportListBase)
  private
    FCompareFields: TEpiFields;
    FDblEntryValidateOptions: TEpiToolsDblEntryValidateOptions;
    FKeyFields: TEpiFields;
  protected
    function GetTitle: string; override;
  public
    function RunReport: string; override;
    property    KeyFields: TEpiFields read FKeyFields write FKeyFields;
    property    CompareFields: TEpiFields read FCompareFields write FCompareFields;
    property    DblEntryValidateOptions: TEpiToolsDblEntryValidateOptions read FDblEntryValidateOptions write FDblEntryValidateOptions;
  end;

implementation

uses
  epidocument,
  epireport_base, epireport_types, epireport_doubleentry_validate,
  epireport_htmlgenerator, epireport_filelist;

resourcestring
  rsReportDoubleEntryValidation = 'Double Entry Validation Report.';

{ TReportDoubleEntryValidation }

function TReportDoubleEntryValidation.GetTitle: string;
begin
  result := rsReportDoubleEntryValidation;
end;

function TReportDoubleEntryValidation.RunReport: string;
var
  Doc: TEpiDocument;
  i: Integer;
  R: TEpiReportDoubleEntryValidationHtml;
  Rf: TEpiReportFileListHtml;
begin
  Result := TEpiReportHTMLGenerator.HtmlHeader(ReportTitle, StyleSheet);
  Result +=
    '<h3>Report: ' + ReportTitle + ' Created ' + FormatDateTime('YYYY/MM/DD HH:NN:SS', Now)  + '</h3>';

  Rf := TEpiReportFileListHtml.Create(Documents);
  Rf.RunReport;
  Result += Rf.ReportText;
  Rf.Free;

  R := TEpiReportDoubleEntryValidationHtml.Create(
    TEpiDocument(Documents.Objects[0]),
    TEpiDocument(Documents.Objects[1]),
    false);

  R.CompareFields := FCompareFields;
  R.KeyFields     := FKeyFields;
  R.DblEntryValidateOptions := FDblEntryValidateOptions;
  R.RunReport;
  Result += R.ReportText;
  R.Free;

  Result += TEpiReportHTMLGenerator.HtmlFooter;
end;

end.

