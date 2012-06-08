unit report_fieldlist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, report_base;

type

  { TReportFieldLists }

  TReportFieldLists = class(TReportFileListBase)
  protected
    function GetTitle: string; override;
    procedure DoRunReport; override;
  end;

implementation


uses
  epidocument, epireport_base, epireport_fieldlist_simple;


resourcestring
  rsReportFieldListTitle = 'List of questions/fields.';

{ TReportFieldLists }

function TReportFieldLists.GetTitle: string;
begin
  Result := rsReportFieldListTitle;
end;

procedure TReportFieldLists.DoRunReport;
var
  Doc: TEpiDocument;
  i: Integer;
  R: TEpiReportSimpleFieldList;
begin
  inherited DoRunReport;

  for i := 0 to Documents.Count - 1 do
  begin
    Generator.Heading('File: ' + Documents[i]);
    R := TEpiReportSimpleFieldList.Create(Generator);
    R.RunReport;
    R.Free;
  end;
end;

end.

