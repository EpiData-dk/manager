unit report_project_validation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, report_base, epidocument;

type

  { TReportProjectOverview }

  { TReportProjectValidation }

  TReportProjectValidation = class(TReportFileListBase)
  protected
    function GetTitle: string; override;
    procedure DoDocumentReport(const Doc: TEpiDocument; const FileName: string;
      const Index: Integer); override;
  end;

implementation

uses
  epireport_base, epireport_report_projectvalidator;

resourcestring
  rsReportProjectValidationTitle = 'Project Validation.';


{ TReportProjectValidation }

function TReportProjectValidation.GetTitle: string;
begin
  result := rsReportProjectValidationTitle;
end;

procedure TReportProjectValidation.DoDocumentReport(const Doc: TEpiDocument;
  const FileName: string; const Index: Integer);
var
  R: TEpiReportProjectValidator;
begin
  inherited DoDocumentReport(Doc, FileName, Index);

  R := TEpiReportProjectValidator.Create(Generator);
  R.Document := Doc;
  R.RunReport;
  R.Free;
end;


end.

