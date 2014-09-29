unit report_double_entry_validation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, report_base, epidatafiles, epidocument,
  epitools_val_dbl_entry,
  report_types;

type

  TReportDoubleEntryValidationOption = record
    MainDF: TEpiDataFile;
    DuplDF: TEpiDataFile;
    Keyfields: TEpiFields;
    Comparefields: TEpiFields;
    ValidateOptions: TEpiToolsDblEntryValidateOptions;
  end;
  TReportDoubleEntryValidationOptions = array of TReportDoubleEntryValidationOption;

  { TReportDoubleEntryValidation }

  TReportDoubleEntryValidation = class(TReportBase)
  private
    FCompareFields: TEpiFields;
    FDblEntryValidateOptions: TEpiToolsDblEntryValidateOptions;
    FKeyFields: TEpiFields;
    FReportOptions: TReportDoubleEntryValidationOptions;
    procedure DoDataFileReport(Const ReportOption: TReportDoubleEntryValidationOption);
  protected
    function GetTitle: string; override;
    procedure DoBeginReport; override;
    procedure DoRunReport; override;
  public
    class function ReportFrameClass: TCustomFrameClass; override;
    property ReportOptions: TReportDoubleEntryValidationOptions read FReportOptions write FReportOptions;
  end;

implementation

uses
  validate_double_entry_frame,
  epireport_report_doubleentryvalidate;

resourcestring
  rsReportDoubleEntryValidation = 'Double Entry Validation Report.';

{ TReportDoubleEntryValidation }

procedure TReportDoubleEntryValidation.DoDataFileReport(
  const ReportOption: TReportDoubleEntryValidationOption);
var
  R: TEpiReportDoubleEntryValidation;
begin
  R := TEpiReportDoubleEntryValidation.Create(Generator);
  R.MainDF := ReportOption.MainDf;
  R.DuplDF := ReportOption.DuplDF;

  R.CompareFields := ReportOption.CompareFields;
  R.KeyFields     := ReportOption.KeyFields;

  R.DblEntryValidateOptions := ReportOption.ValidateOptions;
  R.RunReport;
  R.Free;
end;

function TReportDoubleEntryValidation.GetTitle: string;
begin
  result := rsReportDoubleEntryValidation;
end;

procedure TReportDoubleEntryValidation.DoBeginReport;
begin
  if not Assigned(ReportOptions) then
    Exception.Create('TReportDoubleEntryValidation: No ReportOptions assigned!');

  inherited DoBeginReport;
end;

procedure TReportDoubleEntryValidation.DoRunReport;
var
  RO: TReportDoubleEntryValidationOption;
begin
  inherited DoRunReport;
  Generator.Line('');

  // All required information is present in the ReportOptions.

  for RO in ReportOptions do
  begin
    Generator.Section('DataForm: ' + RO.MainDF.Caption.Text);
    Generator.Line('');

    DoDataFileReport(RO);

    Generator.Line('');
  end;
end;

class function TReportDoubleEntryValidation.ReportFrameClass: TCustomFrameClass;
begin
  result := TValidateDoubleEntryFrame;
end;

end.

