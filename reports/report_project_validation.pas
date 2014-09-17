unit report_project_validation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, report_base, epidocument, epidatafiles, report_types,
  forms, epitools_projectvalidate;

type
{
  TReportProjectValidateOption = record
    Document: TEpiDocument;
    Options:  TEpiToolsProjectValidateOptions;
    FieldLists: array of
      record
        Relation: TEpiMasterRelation;
        SortFields: TEpiFields;
        CompareFields: TEpiFields;
      end;
  end;           }

  { TReportProjectValidation }

  TReportProjectValidation = class(TReportFileListBase, IReportFrameProvider)
  private
    FKeyFields: TEpiFields;
    FOptions: TEpiToolsProjectValidateOptions;
    FValidationFields: TEpiFields;
  protected
    function GetTitle: string; override;
    procedure DoDocumentReport(const Doc: TEpiDocument; const FileName: string;
      const Index: Integer); override;
  public
    { IReportFrameProvider }
    function GetFrameClass: TCustomFrameClass;
  public
    property KeyFields: TEpiFields read FKeyFields write FKeyFields;
    property ValidationFields: TEpiFields read FValidationFields write FValidationFields;
    property Options: TEpiToolsProjectValidateOptions read FOptions write FOptions;
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
  R.KeyFields := KeyFields;
  R.ValidationFields := ValidationFields;
  R.Options := Options;
  R.RunReport;
  R.Free;
end;

function TReportProjectValidation.GetFrameClass: TCustomFrameClass;
begin
  result := TProjectValidationFrame;
end;


end.

