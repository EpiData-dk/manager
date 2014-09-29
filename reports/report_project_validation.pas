unit report_project_validation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, report_base, epidocument, epidatafiles, report_types,
  forms, epitools_projectvalidate, epirelations, epireport_report_projectvalidator,
  epiopenfile;

type

  TReportProjectValidateOptions = array of TEpiReportProjectValidateOption;

  { TReportProjectValidation }

  TReportProjectValidation = class(TReportFileListBase, IReportFrameProvider)
  private
    FKeyFields: TEpiFields;
    FOptions: TReportProjectValidateOptions;
    FValidationFields: TEpiFields;
  protected
    function GetTitle: string; override;
    procedure DoDocumentReport(const Doc: TEpiDocumentFile;
      const Index: Integer); override;
  public
    { IReportFrameProvider }
    function GetFrameClass: TCustomFrameClass;
    class function ReportFrameClass: TCustomFrameClass; override;
  public
    property Options: TReportProjectValidateOptions read FOptions write FOptions;
  end;

implementation

uses
  epireport_base,
  report_project_validation_frame,
  report_project_validation_frame2;

resourcestring
  rsReportProjectValidationTitle = 'Project Validation.';


{ TReportProjectValidation }

function TReportProjectValidation.GetTitle: string;
begin
  result := rsReportProjectValidationTitle;
end;

procedure TReportProjectValidation.DoDocumentReport(
  const Doc: TEpiDocumentFile; const Index: Integer);
var
  R: TEpiReportProjectValidator;
  I: Integer;
begin
  inherited DoDocumentReport(Doc,Index);

  for I := Low(Options) to High(Options) do
    if Options[i].Document = Doc.Document then
      Break;

  R := TEpiReportProjectValidator.Create(Generator);
  R.Option := Options[I];
  R.RunReport;
  R.Free;
end;

function TReportProjectValidation.GetFrameClass: TCustomFrameClass;
begin
  result := TProjectValidationFrame;
end;

class function TReportProjectValidation.ReportFrameClass: TCustomFrameClass;
begin
  result := TProjectValidationFrame2;
end;


end.

