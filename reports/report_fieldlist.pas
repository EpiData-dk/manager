unit report_fieldlist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TReportFieldLists }

  TReportFieldLists = class
  private
    FFileNames: TStringList;
  public
    constructor Create(const FileNames: TStringList);
    function RunReport: string;
  end;

implementation


uses
  epidocument,
  epireport_base, epireport_fieldlist_simple,
  epireport_htmlgenerator;

{ TReportFieldLists }

constructor TReportFieldLists.Create(const FileNames: TStringList);
begin
  FFileNames := TStringList.Create;
  FFileNames.Assign(FileNames);
end;

function TReportFieldLists.RunReport: string;
var
  Doc: TEpiDocument;
  i: Integer;
  R: TEpiReportSimpleFieldListHtml;
begin
  Result := TEpiReportHTMLGenerator.HtmlHeader;

  for i := 0 to FFileNames.Count - 1 do
  begin
    Doc := TEpiDocument.Create('');
    Doc.LoadFromFile(FFileNames[i]);

    R := TEpiReportSimpleFieldListHtml.Create(Doc, stEntryFlow, false);
    R.RunReport;

    Result += R.ReportText;
  end;

  Result += TEpiReportHTMLGenerator.HtmlFooter;
end;

end.

