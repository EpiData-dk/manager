unit report_fieldlist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, report_base, epidocument;

type

  { TReportFieldLists }

  TReportFieldLists = class(TReportFileListBase)
  protected
    function GetTitle: string; override;
    procedure DoDocumentReport(const Doc: TEpiDocument; const FileName: string;
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

procedure TReportFieldLists.DoDocumentReport(const Doc: TEpiDocument;
  const FileName: string; const Index: Integer);
var
  R: TEpiReportControlList;
begin
  inherited DoDocumentReport(Doc, FileName, Index);

  R := TEpiReportControlList.Create(Generator);
  R.ControlItems := Doc.DataFiles[0].ControlItems;
  R.ExtendedList := false;
//  R.SortType := stEntryFlow;
  R.RunReport;
  R.Free;
end;

end.

