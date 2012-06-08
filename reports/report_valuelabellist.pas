unit report_valuelabellist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, report_base;

type

  { TReportValueLabelList }

  TReportValueLabelList = class(TReportFileListBase)
  protected
    function GetTitle: string; override;
    procedure DoRunReport; override;
  end;

implementation

uses
  epidocument, epireport_base, epireport_valuelabels;

resourcestring
  rsReportValueLabelListTitle = 'Report: List of valuelabels.';

{ TReportValueLabelList }

function TReportValueLabelList.GetTitle: string;
begin
  Result := rsReportValueLabelListTitle;
end;

procedure TReportValueLabelList.DoRunReport;
var
  Doc: TEpiDocument;
  i: Integer;
  R: TEpiReportValueLabels;
begin
  inherited DoRunReport;

  for i := 0 to Documents.Count - 1 do
  begin
    Generator.Heading('File: ' + Documents[i]);
    R := TEpiReportValueLabels.Create(Generator);
    R.EpiValueLabels := TEpiDocument(Documents.Objects[i]).ValueLabelSets;
    R.RunReport;
    R.Free;
  end;
end;

end.

