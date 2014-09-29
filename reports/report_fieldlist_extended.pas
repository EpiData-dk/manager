unit report_fieldlist_extended;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, report_base, epidocument, epidatafiles, epiopenfile;

type

  { TReportFieldListExtended }

  TReportFieldListExtended = class(TReportFileListBase)
  protected
    function GetTitle: string; override;
    procedure DoDocumentReport(const Doc: TEpiDocumentFile;
      const Index: Integer); override;
  end;

implementation

uses
  epireport_report_fieldlist;


resourcestring
  rsReportFieldListTitleExtended = 'Extended list of questions/fields.';

{ TReportFieldExtended }

function TReportFieldListExtended.GetTitle: string;
begin
  Result := rsReportFieldListTitleExtended;
end;

procedure TReportFieldListExtended.DoDocumentReport(
  const Doc: TEpiDocumentFile; const Index: Integer);
var
  R: TEpiReportFieldList;
  OrderedDFs: TEpiDataFiles;
  DF: TEpiDataFile;
begin
  inherited DoDocumentReport(Doc, Index);

  OrderedDFs := Doc.Document.Relations.GetOrderedDataFiles;
  for DF in OrderedDFs do
  begin
    Generator.Line('');
    R := TEpiReportFieldList.Create(Generator);
    R.TableHeaderEx := R.TableHeaderEx + ' ' + DF.Caption.Text;
    R.ExtendedList := true;
    R.Fields := DF.Fields;
    R.RunReport;
    R.Free;
    Generator.Line('');
  end;
end;

end.

