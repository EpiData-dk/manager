unit report_codebook;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, report_base;

type

  { TReportCodeBook }

  TReportCodeBook = class(TReportFileListBase)
  protected
    function GetTitle: string; override;
    procedure DoRunReport; override;
  end;

implementation

uses
  epidocument,
  epireport_base, epireport_fieldlist_extended, epireport_valuelabels;


resourcestring
  rsReportCodeBook = 'CodeBook';

{ TReportCodeBook }

function TReportCodeBook.GetTitle: string;
begin
  Result := rsReportCodeBook;
end;

procedure TReportCodeBook.DoRunReport;
var
  i: Integer;
  R: TEpiReportBase;
begin
  inherited DoRunReport;

  for i := 0 to Documents.Count - 1 do
  begin
    Generator.Heading('File: ' + Documents[i]);
    R := TEpiReportExtendedFieldList.Create(Generator);
    TEpiReportExtendedFieldList(R).EpiDataFiles := TEpiDocument(Documents.Objects[i]).DataFiles;
    R.RunReport;
    R.Free;

    R := TEpiReportValueLabels.Create(Generator);
    TEpiReportValueLabels(R).EpiValueLabels := TEpiDocument(Documents.Objects[i]).ValueLabelSets;
    R.RunReport;
    R.Free;
  end;
end;

end.

