unit report_double_entry_validation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, report_base, epidatafiles, epitools_val_dbl_entry,
  report_types;

type

  { TReportDoubleEntryValidation }

  TReportDoubleEntryValidation = class(TReportBase, IReportFrameProvider)
  private
    FCompareFields: TEpiFields;
    FDblEntryValidateOptions: TEpiToolsDblEntryValidateOptions;
    FKeyFields: TEpiFields;
  protected
    function GetTitle: string; override;
    procedure DoRunReport; override;
  public
    function GetFrameClass: TCustomFrameClass;
    property KeyFields: TEpiFields read FKeyFields write FKeyFields;
    property CompareFields: TEpiFields read FCompareFields write FCompareFields;
    property DblEntryValidateOptions: TEpiToolsDblEntryValidateOptions read FDblEntryValidateOptions write FDblEntryValidateOptions;
  end;

implementation

uses
  validate_double_entry_frame, epidocument,
  epireport_base, epireport_doubleentry_validate,
  epireport_filelist;

resourcestring
  rsReportDoubleEntryValidation = 'Double Entry Validation Report.';

{ TReportDoubleEntryValidation }

function TReportDoubleEntryValidation.GetTitle: string;
begin
  result := rsReportDoubleEntryValidation;
end;

procedure TReportDoubleEntryValidation.DoRunReport;
var
  Doc: TEpiDocument;
  i: Integer;
  R: TEpiReportDoubleEntryValidation;
  Rf: TEpiReportFileList;
begin
  inherited DoRunReport;

  Rf := TEpiReportFileList.Create(Generator);
  Rf.FileList := Documents;
  Rf.RunReport;
  Rf.Free;

  R := TEpiReportDoubleEntryValidation.Create(Generator);
  R.MainDF := TEpiDocument(Documents.Objects[0]).DataFiles[0];
  R.DuplDF := TEpiDocument(Documents.Objects[1]).DataFiles[0];
  R.CompareFields := FCompareFields;
  R.KeyFields     := FKeyFields;
  R.DblEntryValidateOptions := FDblEntryValidateOptions;
  R.RunReport;
  R.Free;
end;

function TReportDoubleEntryValidation.GetFrameClass: TCustomFrameClass;
begin
  result := TValideDoubleEntryFrame;
end;

end.

