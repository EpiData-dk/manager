unit report_fieldlist_extended;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, report_base;

type

  { TReportFieldListExtended }

  TReportFieldListExtended = class(TReportFileListBase)
  protected
    function GetTitle: string; override;
    procedure DoRunReport; override;
  end;

implementation

uses
  epidocument,
  epireport_base, epireport_fieldlist_extended;


resourcestring
  rsReportFieldListTitleExtended = 'Extended list of questions/fields.';

{ TReportFieldExtended }

function TReportFieldListExtended.GetTitle: string;
begin
  Result := rsReportFieldListTitleExtended;
end;

procedure TReportFieldListExtended.DoRunReport;
var
  Doc: TEpiDocument;
  i: Integer;
  R: TEpiReportExtendedFieldList;
begin
  inherited DoRunReport;

  for i := 0 to Documents.Count - 1 do
  begin
    Generator.Heading('File: ' + Documents[i]);
    R := TEpiReportExtendedFieldList.Create(Generator);
    R.RunReport;
    R.Free;
  end;
end;

end.

