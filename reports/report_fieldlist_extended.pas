unit report_fieldlist_extended;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, report_base, epidocument;

type

  { TReportFieldListExtended }

  TReportFieldListExtended = class(TReportFileListBase)
  protected
    function GetTitle: string; override;
    procedure DoDocumentReport(const Doc: TEpiDocument; const FileName: string
      ); override;
  end;

implementation

uses
  epireport_base, epireport_report_fieldlist;


resourcestring
  rsReportFieldListTitleExtended = 'Extended list of questions/fields.';

{ TReportFieldExtended }

function TReportFieldListExtended.GetTitle: string;
begin
  Result := rsReportFieldListTitleExtended;
end;

procedure TReportFieldListExtended.DoDocumentReport(const Doc: TEpiDocument;
  const FileName: string);
var
  R: TEpiReportFieldList;
begin
  inherited DoDocumentReport(Doc, FileName);

  R := TEpiReportFieldList.Create(Generator);
  R.ExtendedList := true;
  R.Fields := Doc.DataFiles[0].Fields;
  R.RunReport;
  R.Free;
end;

end.

