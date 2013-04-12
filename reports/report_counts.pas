unit report_counts;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, SysUtils, report_base, report_types, epidatafiles,
  epidocument;

type

  { TReportCounts }

  TReportCounts = class(TReportFileListBase, IReportFrameProvider)
  private
    FFieldList: TEpiFields;
  protected
    function GetTitle: string; override;
    procedure DoRunReport; override;
    procedure DoDocumentReport(const Doc: TEpiDocument; const FileName: string
       ); override;
  public
    function GetFrameClass: TCustomFrameClass;
    property FieldList: TEpiFields read FFieldList write FFieldList;
  end;

implementation

uses
  epireport_report_countbyid, report_optionframe_counts;


resourcestring
  rsReportCountsTitle = 'Count of observations in files.';

{ TReportCounts }

function TReportCounts.GetTitle: string;
begin
  result := rsReportCountsTitle;
end;

procedure TReportCounts.DoRunReport;
var
  R: TEpiReportCountById;
  S: String;
  i: Integer;
begin
  inherited DoRunReport;

  if FieldList.Count = 0 then exit;

  S := FieldList[0].Name;
  for i := 1 to FieldList.Count - 1 do
    S += ', ' + FieldList[i].Name;

  Generator.Heading('Count of observations for unique combined values of: ' + S);
  R := TEpiReportCountById.Create(Generator);
  R.Documents := Documents;
  R.FieldList := FFieldList;
  R.RunReport;
  R.Free;
end;

procedure TReportCounts.DoDocumentReport(const Doc: TEpiDocument;
  const FileName: string);
begin
  // Do not inherit -> we do not want per file reports.
  //inherited DoDocumentReport(Doc, FileName);
end;

function TReportCounts.GetFrameClass: TCustomFrameClass;
begin
  result := TReportOptionsFrameCounts;
end;

end.

