unit report_valuelabellist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, report_base, epidocument;

type

  { TReportValueLabelList }

  TReportValueLabelList = class(TReportFileListBase)
  protected
    function GetTitle: string; override;
    procedure DoDocumentReport(const Doc: TEpiDocument; const FileName: string
       ); override;
  end;

implementation

uses
  epireport_base, epireport_report_valuelabelsetlist;

resourcestring
  rsReportValueLabelListTitle = 'List of valuelabels.';

{ TReportValueLabelList }

function TReportValueLabelList.GetTitle: string;
begin
  Result := rsReportValueLabelListTitle;
end;

procedure TReportValueLabelList.DoDocumentReport(const Doc: TEpiDocument;
  const FileName: string);
var
  R: TEpiReportValueLabelSetList;
  i: Integer;
begin
  inherited DoDocumentReport(Doc, FileName);

  for i := 0 to Doc.ValueLabelSets.Count - 1 do
  begin
    R := TEpiReportValueLabelSetList.Create(Generator);
    R.ValueLabelSet := Doc.ValueLabelSets[i];
    R.RunReport;
    R.Free;
    if i < Doc.ValueLabelSets.Count then
      Generator.Line('');
  end;
end;

end.

