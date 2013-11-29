unit report_project_validation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, report_base, epidocument, epidatafiles, report_types, forms;

type

  { TReportProjectValidation }

  TReportProjectValidation = class(TReportFileListBase, IReportFrameProvider)
  private
    FKeyFields: TEpiFields;
    FValidationFields: TEpiFields;
  protected
    function GetTitle: string; override;
    procedure DoDocumentReport(const Doc: TEpiDocument; const FileName: string;
      const Index: Integer); override;
  public
    { IReportFrameProvider }
    function GetFrameClass: TCustomFrameClass;
  public
    property KeyFields: TEpiFields read FKeyFields;
    property ValidationFields: TEpiFields read FValidationFields;
  end;

implementation

uses
  epireport_base, epireport_report_projectvalidator,
  report_project_validation_frame;

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

function TReportProjectValidation.GetFrameClass: TCustomFrameClass;
begin
  result := TProjectValidationFrame;
end;


end.

