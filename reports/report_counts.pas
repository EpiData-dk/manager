unit report_counts;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, SysUtils, report_base, report_types, epidatafiles,
  epidocument;

type

  TReportCountsOption = record
    DataFiles: TEpiDataFiles;
    FieldNames: TStrings;
  end;

  { TReportCounts }

  TReportCounts = class(TReportFileListBase, IReportFrameProvider)
  private
    FFieldList: TEpiFields;
    FOptions: TReportCountsOption;
  protected
    function GetTitle: string; override;
    procedure DoRunReport; override;
    procedure DoDocumentReport(const Doc: TEpiDocument; const FileName: string;
      const Index: Integer); override;
  public
    function GetFrameClass: TCustomFrameClass;
    property FieldList: TEpiFields read FFieldList write FFieldList;
    property Options: TReportCountsOption read FOptions write FOptions;
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

//  if FieldList.Count = 0 then exit;

  S := Options.FieldNames[0];
  for i := 1 to Options.FieldNames.Count - 1 do
    S += ', ' + Options.FieldNames[i];

  Generator.Heading('Count of observations for unique combined values of: ' + S);
  R := TEpiReportCountById.Create(Generator);
  R.DataFiles := Options.DataFiles;
  R.FieldNames := Options.FieldNames;
//  R.Documents := Documents;
//  R.FieldList := FFieldList;
  R.RunReport;
  R.Free;
end;

procedure TReportCounts.DoDocumentReport(const Doc: TEpiDocument;
  const FileName: string; const Index: Integer);
begin
  // Do not inherit -> we do not want per file reports.
  //inherited DoDocumentReport(Doc, FileName);
end;

function TReportCounts.GetFrameClass: TCustomFrameClass;
begin
  result := TReportOptionsFrameCounts;
end;

end.

