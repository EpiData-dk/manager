unit report_codebook;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, report_base, epidocument, epidatafiles, epidatafilerelations,
  epiopenfile;

type

  { TReportCodeBook }

  TReportCodeBook = class(TReportFileListBase)
  private
    procedure DataFileReport(const Relation: TEpiMasterRelation;
      const Depth: Cardinal; const Index: Cardinal; var aContinue: boolean;
      Data: Pointer = nil);
  protected
    function GetTitle: string; override;
    procedure DoDocumentReport(const Doc: TEpiDocumentFile;
      const Index: Integer); override;
  end;

implementation

uses
  epireport_base, epireport_report_controllist, epireport_report_fieldinfo,
  epidatafilerelations_helper;


resourcestring
  rsReportCodeBook = 'CodeBook';

{ TReportCodeBook }

procedure TReportCodeBook.DataFileReport(const Relation: TEpiMasterRelation;
  const Depth: Cardinal; const Index: Cardinal; var aContinue: boolean;
  Data: Pointer);
var
  DF: TEpiDataFile;
  F: TEpiField;
  R: TEpiReportBase;
begin
  DF := Relation.Datafile;

  Generator.Line('');
  Generator.Section('Dataform: ' + DF.Caption.Text);
  Generator.Line('');

  R := TEpiReportControlList.Create(Generator);
  with TEpiReportControlList(R) do
  begin
    ControlItems := DF.ControlItems;
    ExtendedList := true;
  end;
  R.RunReport;
  R.Free;

  Generator.Line('');
  Generator.Line('.-^-..-^-..-^-..-^-..-^-..-^-..-^-..-^-..-^-..-^-..-^-..-^-.');
  Generator.Line('');

  for F in DF.Fields do
  begin
    R := TEpiReportFieldInfo.Create(Generator);
    TEpiReportFieldInfo(R).Field := F;
    R.RunReport;
    R.Free;

    Generator.Line('');
    Generator.Line('.-^-..-^-..-^-..-^-..-^-..-^-..-^-..-^-..-^-..-^-..-^-..-^-.');
    Generator.Line('');
  end;
end;

function TReportCodeBook.GetTitle: string;
begin
  Result := rsReportCodeBook;
end;

procedure TReportCodeBook.DoDocumentReport(const Doc: TEpiDocumentFile;
  const Index: Integer);
var
  i: Integer;
  R: TEpiReportBase;
begin
  inherited DoDocumentReport(Doc, Index);

  Doc.Document.Relations.OrderedWalk(@DataFileReport);
end;

end.

