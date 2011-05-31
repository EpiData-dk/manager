unit report_combinedlist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

type

  { TReportCombinedList }

  TReportCombinedList = class
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
  epireport_base, epireport_fieldlist_simple,
  epireport_valuelabels,
  epireport_htmlgenerator, epireport_filelist;

{ TReportCombinedList }

constructor TReportCombinedList.Create(const FileNames: TStringList);
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

destructor TReportCombinedList.Destroy;
var
  i: Integer;
begin
  for i := 0 to FDocuments.Count - 1 do
    FDocuments.Objects[i].Free;

  FDocuments.Free;
  inherited Destroy;
end;

function TReportCombinedList.RunReport: string;
var
  Doc: TEpiDocument;
  i: Integer;
  R: TEpiReportBase;
begin
  Result := TEpiReportHTMLGenerator.HtmlHeader('Report: Combined list of fields/question and valuelabels.');

  R := TEpiReportFileListHtml.Create(FDocuments);
  R.RunReport;
  Result += R.ReportText;
  R.Free;

  for i := 0 to FDocuments.Count - 1 do
  begin
    Result += '<h2>File: ' + FDocuments[i] + '</h2>';
    R := TEpiReportSimpleFieldListHtml.Create(TEpiDocument(FDocuments.Objects[i]), stEntryFlow);
    R.RunReport;
    Result += R.ReportText;
    R.Free;

    R := TEpiReportValueLabelsHtml.Create(TEpiDocument(FDocuments.Objects[i]), false);
    R.RunReport;
    Result += R.ReportText +
      '<div style="page-break-after:always;">' + LineEnding ;
    R.Free;
  end;

  Result += TEpiReportHTMLGenerator.HtmlFooter;
end;

end.

