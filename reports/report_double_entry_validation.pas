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
    procedure DoDataFileReport(Const MainDf, DuplDF: TEpiDataFile);
  protected
    function GetTitle: string; override;
    procedure DoRunReport; override;
    procedure DoDocumentReport(const Doc: TEpiDocument; const FileName: string;
      const Index: Integer); override;
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

procedure TReportDoubleEntryValidation.DoDataFileReport(const MainDf,
  DuplDF: TEpiDataFile);
var
  R: TEpiReportDoubleEntryValidation;
begin
  R := TEpiReportDoubleEntryValidation.Create(Generator);
  R.MainDF := MainDf;
  R.DuplDF := DuplDF;

  R.CompareFields := FCompareFields; //MainDF.Fields;
  R.KeyFields     := FKeyFields; // MainDF.KeyFields;

  R.DblEntryValidateOptions := FDblEntryValidateOptions;
  R.RunReport;
  R.Free;
end;

function TReportDoubleEntryValidation.GetTitle: string;
begin
  result := rsReportDoubleEntryValidation;
end;

procedure TReportDoubleEntryValidation.DoRunReport;
var
  R: TEpiReportDoubleEntryValidation;
  MainDoc: TEpiDocument;
  DuplDoc: TEpiDocument;
  OrderedMainDF: TEpiDataFiles;
  MainDF: TEpiDataFile;
  DuplDF: TEpiDataFile;
begin
  inherited DoRunReport;
  Generator.Line('');

  MainDoc := TEpiDocument(Documents.Objects[0]);
  DuplDoc := TEpiDocument(Documents.Objects[1]);


  OrderedMainDF := MainDoc.Relations.GetOrderedDataFiles;

  for MainDF in OrderedMainDF do
  begin
    Generator.Section('DataForm: ' + MainDF.Caption.Text);
    Generator.Line('');

    DuplDF := TEpiDataFile(DuplDoc.DataFiles.GetItemByName(MainDF.Name));

    DoDataFileReport(MainDF, DuplDF);

    Generator.Line('');
  end;

end;

procedure TReportDoubleEntryValidation.DoDocumentReport(
  const Doc: TEpiDocument; const FileName: string; const Index: Integer);
begin
  // do not do inherited -> we do not wan't a per file overview.
  // inherited DoDocumentReport(Doc, FileName);
end;

function TReportDoubleEntryValidation.GetFrameClass: TCustomFrameClass;
begin
  result := TValidateDoubleEntryFrame;
end;

end.

