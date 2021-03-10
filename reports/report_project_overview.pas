unit report_project_overview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, report_base, epidocument, epiopenfile;

type

  { TReportProjectOverview }

  TReportProjectOverview = class(TReportFileListBase)
  protected
    function GetTitle: string; override;
    procedure DoDocumentReport(const Doc: TEpiDocumentFile;
      const Index: Integer); override;
  end;

implementation

uses
  epireport_base,
  epireport_report_projectheading,
  epireport_report_studyinfo;


resourcestring
  rsReportProjectOverviewTitle = 'Project Overview.';

{ TReportProjectOverview }

function TReportProjectOverview.GetTitle: string;
begin
  Result := rsReportProjectOverviewTitle;
end;

procedure TReportProjectOverview.DoDocumentReport(const Doc: TEpiDocumentFile;
  const Index: Integer);
var
  R: TEpiReportStudyInfo;
begin
  inherited DoDocumentReport(Doc, Index);

  R := TEpiReportStudyInfo.Create(Generator);
  R.Document := Doc.Document;
  R.RunReport;
  R.Free;
end;


end.

