unit report_codebook;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, report_base, epidocument;

type

  { TReportCodeBook }

  TReportCodeBook = class(TReportFileListBase)
  protected
    function GetTitle: string; override;
    procedure DoDocumentReport(const Doc: TEpiDocument; const FileName: string
      ); override;
  end;

implementation

uses
  epireport_base, epireport_report_fieldlist, epireport_report_fieldinfo,
  epireport_report_valuelabelsetlist;


resourcestring
  rsReportCodeBook = 'CodeBook';

{ TReportCodeBook }

function TReportCodeBook.GetTitle: string;
begin
  Result := rsReportCodeBook;
end;

procedure TReportCodeBook.DoDocumentReport(const Doc: TEpiDocument;
  const FileName: string);
var
  i: Integer;
  R: TEpiReportBase;
begin
  inherited DoDocumentReport(Doc, FileName);

  R := TEpiReportFieldList.Create(Generator);
  with TEpiReportFieldList(R) do
  begin
    Fields := Doc.DataFiles[0].Fields;
    ExtendedList := true;
  end;
  R.RunReport;
  R.Free;
  Generator.Line('');

  for i := 0 to Doc.DataFiles[0].Fields.Count - 1 do
  begin
    R := TEpiReportFieldInfo.Create(Generator);
    TEpiReportFieldInfo(R).Field := Doc.DataFiles[0].Fields[i];
    R.RunReport;
    R.Free;
    Generator.Line('');
  end;

  for i := 0 to Doc.ValueLabelSets.Count - 1 do
  begin
    R := TEpiReportValueLabelSetList.Create(Generator);
    TEpiReportValueLabelSetList(R).ValueLabelSet := Doc.ValueLabelSets[i];
    R.RunReport;
    R.Free;
    if not (i = Doc.ValueLabelSets.Count - 1) then
      Generator.Line('');
  end;
end;

end.

