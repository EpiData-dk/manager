unit report_valuelabellist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

type

  { TReportFieldLists }

  { TReportValueLabelList }

  TReportValueLabelList = class
  private
    FDocuments: TStringList;
  public
    constructor Create(const FileNames: TStringList);
    destructor Destroy; override;
    function RunReport: string;
  end;

implementation


uses
  epidocument,
  epireport_base, epireport_valuelabels,
  epireport_htmlgenerator, epireport_filelist;

{ TReportValueLabelList }

constructor TReportValueLabelList.Create(const FileNames: TStringList);
var
  Doc: TEpiDocument;
  i: Integer;
begin
  FDocuments := TStringList.Create;
  for i := 0 to FileNames.Count - 1 do
  begin
    Doc := TEpiDocument.Create('');
    Doc.LoadFromFile(FileNames[i]);
    FDocuments.AddObject(FileNames[i], Doc);
  end;
end;

destructor TReportValueLabelList.Destroy;
var
  i: Integer;
begin
  for i := 0 to FDocuments.Count - 1 do
    FDocuments.Objects[i].Free;

  FDocuments.Free;
  inherited Destroy;
end;

function TReportValueLabelList.RunReport: string;
var
  Doc: TEpiDocument;
  i: Integer;
  R: TEpiReportBase;
begin
  Result := TEpiReportHTMLGenerator.HtmlHeader('Report: List of valuelabels.');

  R := TEpiReportFileListHtml.Create(FDocuments);
  R.RunReport;
  Result += R.ReportText;
  R.Free;

  for i := 0 to FDocuments.Count - 1 do
  begin
    Result += '<h2>File: ' + FDocuments[i] + '</h2>';
    R := TEpiReportValueLabelsHtml.Create(TEpiDocument(FDocuments.Objects[i]), false);
    R.RunReport;

    Result += R.ReportText +
      '<div style="page-break-after:always;">' + LineEnding ;

    R.Free;
  end;

  Result += TEpiReportHTMLGenerator.HtmlFooter;
end;

end.

