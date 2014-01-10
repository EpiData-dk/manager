unit report_export;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, report_base, epidocument, epiexportsettings;

type

  { TReportExport }

  TReportExport = class(TReportFileListBase)
  private
    FExportSettings: TEpiExportSetting;
    procedure CreateFileTable;
  protected
    function GetTitle: string; override;
    procedure DoDocumentReport(const Doc: TEpiDocument; const FileName: string;
      const Index: Integer); override;
  public
    property ExportSettings: TEpiExportSetting read FExportSettings write FExportSettings;
  end;

implementation


uses
  epireport_base, epireport_report_controllist, epireport_report_studyinfo,
  epireport_report_exportsettings;

resourcestring
  rsReportExportTitle = 'Export Report';

{ TReportExport }

procedure TReportExport.CreateFileTable;
var
  TmpSetting: TEpiExportSetting;
  Rows: Integer;
  Ext: String;
  RowNo: Integer;
begin
  TmpSetting := ExportSettings;
  Rows := 2;
  while Assigned(TmpSetting) do
  begin
    Inc(Rows);
    TmpSetting := TmpSetting.AdditionalExportSettings;
  end;
  Generator.TableHeader('Files created', 2, Rows);

  Generator.TableCell('Content', 0, 0);
  Generator.TableCell('File', 1, 0);
  Generator.TableCell('Report', 0, 1);
  Generator.TableCell(ChangeFileExt(ExportSettings.ExportFileName, '.log'), 1, 1);

  RowNo := 2;
  TmpSetting := ExportSettings;
  while Assigned(TmpSetting) do
  begin
    Ext := ExtractFileExt(TmpSetting.ExportFileName);
    Delete(Ext, 1, 1);

    Generator.TableCell(Ext, 0, RowNo);
    Generator.TableCell(TmpSetting.ExportFileName, 1, RowNo);

    Inc(RowNo);
    TmpSetting := TmpSetting.AdditionalExportSettings;
  end;

  Generator.TableFooter('');
end;

function TReportExport.GetTitle: string;
begin
  result := rsReportExportTitle;
end;

procedure TReportExport.DoDocumentReport(const Doc: TEpiDocument;
  const FileName: string; const Index: Integer);
var
  R: TEpiReportBase;
begin
  inherited DoDocumentReport(Doc, FileName, Index);

  Generator.Heading('Selections for Export');

  R := TEpiReportExportSettings.Create(Generator);
  TEpiReportExportSettings(R).ExportSetting := ExportSettings;
  R.RunReport;
  R.Free;

  Generator.Line('');

  R := TEpiReportControlList.Create(Generator);
  with TEpiReportControlList(R) do
  begin
    ControlItems := Doc.DataFiles[0].ControlItems;
    ControlItemsAreSubItemized := false;
    ExtendedList := true;
  end;
  R.RunReport;
  R.Free;

  Generator.Line('');

  CreateFileTable;

  Generator.Line('');

  R := TEpiReportStudyInfo.Create(Generator);
  TEpiReportStudyInfo(R).Document := Doc;
  R.RunReport;
  R.Free;
end;


end.

