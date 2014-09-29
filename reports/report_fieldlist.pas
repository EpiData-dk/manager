unit report_fieldlist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, report_base, epidocument, epidatafiles,
  epiopenfile;

type

  { TReportFieldLists }

  TReportFieldLists = class(TReportFileListBase)
  protected
    function GetTitle: string; override;
    procedure DoDocumentReport(const Doc: TEpiDocumentFile;
      const Index: Integer); override;
  end;

implementation


uses
  epireport_base, epireport_report_fieldlist,
  epireport_types, epireport_report_controllist;

resourcestring
  rsReportFieldListTitle = 'List of questions/fields.';

{ TReportFieldLists }

function TReportFieldLists.GetTitle: string;
begin
  Result := rsReportFieldListTitle;
end;

procedure TReportFieldLists.DoDocumentReport(const Doc: TEpiDocumentFile;
  const Index: Integer);
var
  R: TEpiReportControlList;
  OrderedDFs: TEpiDataFiles;
  DF: TEpiDataFile;
begin
  inherited DoDocumentReport(Doc, Index);

  OrderedDFs := Doc.Document.Relations.GetOrderedDataFiles;

  for DF in OrderedDFs do
  begin
    Generator.Line('');
    R := TEpiReportControlList.Create(Generator);
    R.TableHeader := 'List Overview: ' + DF.Caption.Text;
    R.ControlItems := DF.ControlItems;
    R.ExtendedList := false;
    R.RunReport;
    R.Free;
    Generator.Line('');
  end;
end;

end.

