unit report_combinedlist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, report_base;

type

  { TReportCombinedList }

  TReportCombinedList = class(TReportFileListBase)
  protected
    function GetTitle: string; override;
    procedure DoRunReport; override;
  end;

implementation


uses
  epidocument,
  epireport_base, epireport_types, epireport_fieldlist_simple,
  epireport_valuelabels,
  epireport_generator_html, epireport_filelist;


resourcestring
  rsReportCombinedListTitle = 'Combined list of fields/question and valuelabels.';

{ TReportCombinedList }

function TReportCombinedList.GetTitle: string;
begin
  Result := rsReportCombinedListTitle;
end;

procedure TReportCombinedList.DoRunReport;
var
  Doc: TEpiDocument;
  i: Integer;
  R: TEpiReportBase;
begin
  inherited DoRunReport;

  for i := 0 to Documents.Count - 1 do
  begin
    Generator.Heading('File: ' + Documents[i]);
    R := TEpiReportSimpleFieldList.Create(Generator);
    TEpiReportSimpleFieldList(R).EpiDataFiles := TEpiDocument(Documents.Objects[i]).DataFiles;
    R.RunReport;
    R.Free;

    R := TEpiReportValueLabels.Create(Generator);
    TEpiReportValueLabels(R).EpiValueLabels := TEpiDocument(Documents.Objects[i]).ValueLabelSets;
    R.RunReport;
    R.Free;
  end;
end;

end.

