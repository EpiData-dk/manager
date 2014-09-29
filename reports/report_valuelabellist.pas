unit report_valuelabellist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, report_base, epidocument, epiopenfile;

type

  { TReportValueLabelList }

  TReportValueLabelList = class(TReportFileListBase)
  protected
    function GetTitle: string; override;
    procedure DoDocumentReport(const Doc: TEpiDocumentFile;
      const Index: Integer); override;
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

procedure TReportValueLabelList.DoDocumentReport(const Doc: TEpiDocumentFile;
  const Index: Integer);
var
  R: TEpiReportValueLabelSetList;
  i: Integer;
begin
  inherited DoDocumentReport(Doc, Index);

  for i := 0 to Doc.Document.ValueLabelSets.Count - 1 do
  begin
    R := TEpiReportValueLabelSetList.Create(Generator);
    R.ValueLabelSet := Doc.Document.ValueLabelSets[i];
    R.RunReport;
    R.Free;
    if i < Doc.Document.ValueLabelSets.Count then
      Generator.Line('');
  end;
end;

end.

