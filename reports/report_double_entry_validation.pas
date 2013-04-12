unit report_double_entry_validation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, report_base, epidatafiles, epidocument,
  epitools_val_dbl_entry,
  report_types;

type

  { TReportDoubleEntryValidation }

  TReportDoubleEntryValidation = class(TReportFileListBase, IReportFrameProvider)
  private
    FCompareFields: TEpiFields;
    FDblEntryValidateOptions: TEpiToolsDblEntryValidateOptions;
    FKeyFields: TEpiFields;
  protected
    function GetTitle: string; override;
    procedure DoRunReport; override;
    procedure DoDocumentReport(const Doc: TEpiDocument; const FileName: string
       ); override;
  public
    function GetFrameClass: TCustomFrameClass;
    property KeyFields: TEpiFields read FKeyFields write FKeyFields;
    property CompareFields: TEpiFields read FCompareFields write FCompareFields;
    property DblEntryValidateOptions: TEpiToolsDblEntryValidateOptions read FDblEntryValidateOptions write FDblEntryValidateOptions;
  end;

implementation

uses
  validate_double_entry_frame,
  epireport_report_doubleentryvalidate;

resourcestring
  rsReportDoubleEntryValidation = 'Double Entry Validation Report.';

{ TReportDoubleEntryValidation }

function TReportDoubleEntryValidation.GetTitle: string;
begin
  result := rsReportDoubleEntryValidation;
end;

procedure TReportDoubleEntryValidation.DoRunReport;
var
  R: TEpiReportDoubleEntryValidation;
begin
  inherited DoRunReport;

  R := TEpiReportDoubleEntryValidation.Create(Generator);
  R.MainDF := TEpiDocument(Documents.Objects[0]).DataFiles[0];
  R.DuplDF := TEpiDocument(Documents.Objects[1]).DataFiles[0];
  R.CompareFields := FCompareFields;
  R.KeyFields     := FKeyFields;
  R.DblEntryValidateOptions := FDblEntryValidateOptions;
  R.RunReport;
  R.Free;
end;

procedure TReportDoubleEntryValidation.DoDocumentReport(
  const Doc: TEpiDocument; const FileName: string);
begin
  // do not do inherited -> we do not wan't a per file overview.
  // inherited DoDocumentReport(Doc, FileName);
end;

function TReportDoubleEntryValidation.GetFrameClass: TCustomFrameClass;
begin
  result := TValidateDoubleEntryFrame;
end;

end.

