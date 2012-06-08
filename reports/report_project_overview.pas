unit report_project_overview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, report_base;

type

  { TReportProjectOverview }

  TReportProjectOverview = class(TReportFileListBase)
  protected
    function GetTitle: string; override;
    procedure DoRunReport; override;
  end;

implementation

uses
  epidocument,
  epireport_project_overview;


resourcestring
  rsReportProjectOverviewTitle = 'Project Overview.';

{ TReportProjectOverview }

function TReportProjectOverview.GetTitle: string;
begin
  Result := rsReportProjectOverviewTitle;
end;

procedure TReportProjectOverview.DoRunReport;
var
  Doc: TEpiDocument;
  i: Integer;
  R: TEpiReportProjectOverView;
begin
  inherited DoRunReport;

  for i := 0 to Documents.Count - 1 do
  begin
    Generator.Heading('File: ' + Documents[i]);;
    R := TEpiReportProjectOverView.Create(Generator);
    R.Document := TEpiDocument(Documents.Objects[i]);
    R.RunReport;
    R.Free;
  end;
end;

end.

