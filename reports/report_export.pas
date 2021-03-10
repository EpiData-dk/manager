unit report_export;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, report_base, epidocument, epidatafiles, epiexportsettings,
  epiopenfile, Forms;

type

  { TReportExport }

  TReportExport = class(TReportFileListBase)
  private
    FExportSettings: TEpiExportSetting;
    FReportFileName: string;
    procedure CreateFileTable;
  protected
    function GetTitle: string; override;
    procedure DoDocumentReport(const Doc: TEpiDocumentFile;
      const Index: Integer); override;
  public
    class function ReportFrameClass: TCustomFrameClass; override;
    property ExportSettings: TEpiExportSetting read FExportSettings write FExportSettings;
    property ReportFileName: string read FReportFileName write FReportFileName;
  end;

implementation


uses
  epireport_base, epireport_report_controllist, epireport_report_studyinfo,
  epireport_report_exportsettings, epiexport;

resourcestring
  rsReportExportTitle = 'Export Report';

{ TReportExport }

procedure TReportExport.CreateFileTable;
var
  Rows: Integer;
  Ext: String;
  RowNo: Integer;
  TmpSetting: TEpiExportDatafileSettings;
  ProjSettings: TEpiCustomCompleteProjectExportSetting;
  i: Integer;
  IsCompleteProject: Boolean;
begin
  Rows := 2;

  IsCompleteProject :=
    ExportSettings.InheritsFrom(TEpiCustomCompleteProjectExportSetting) and
    (TEpiCustomCompleteProjectExportSetting(ExportSettings).ExportCompleteProject);

  if IsCompleteProject then
  begin
    Inc(Rows);
    ProjSettings := TEpiCustomCompleteProjectExportSetting(ExportSettings);
  end
  else
    for i := 0 to ExportSettings.DatafileSettings.Count -1 do
    begin
      TmpSetting := ExportSettings.DatafileSettings[i];
      while Assigned(TmpSetting) do
      begin
        Inc(Rows);
        TmpSetting := TmpSetting.AdditionalExportSettings;
      end;
    end;

  Generator.TableHeader('Files created:', 2, Rows);

  Generator.TableCell('Content:', 0, 0);
  Generator.TableCell('Filename:', 1, 0);
  Generator.TableCell('Report', 0, 1);
  Generator.TableCell(ReportFileName, 1, 1);

  RowNo := 2;
  if IsCompleteProject then
    begin
      Ext := ExtractFileExt(ProjSettings.ExportFileName);
      Delete(Ext, 1, 1);
      Generator.TableCell(Ext, 0, RowNo);
      Generator.TableCell(ProjSettings.ExportFileName, 1, RowNo);
    end
  else
    for i := 0 to ExportSettings.DatafileSettings.Count -1 do
    begin
      TmpSetting := ExportSettings.DatafileSettings[i];
      while Assigned(TmpSetting) do
      begin
        Ext := ExtractFileExt(TmpSetting.ExportFileName);
        Delete(Ext, 1, 1);

        Generator.TableCell(Ext, 0, RowNo);
        Generator.TableCell(TmpSetting.ExportFileName, 1, RowNo);

        Inc(RowNo);
        TmpSetting := TmpSetting.AdditionalExportSettings;
      end;
    end;

  Generator.TableFooter('');
end;

function TReportExport.GetTitle: string;
begin
  result := rsReportExportTitle;
end;

procedure TReportExport.DoDocumentReport(const Doc: TEpiDocumentFile;
  const Index: Integer);
var
  R: TEpiReportBase;
  TmpDoc: TEpiDocument;
  Df: TEpiDataFile;
  i: Integer;
  DFSettings: TEpiExportDatafileSettings;
begin
  inherited DoDocumentReport(Doc, Index);

  R := TEpiReportExportSettings.Create(Generator);
  TEpiReportExportSettings(R).ExportSetting := ExportSettings;
  TEpiReportExportSettings(R).TableHeader := 'Specifications for Export:';
  R.RunReport;
  R.Free;

  Generator.Line('');

  for i := 0 to ExportSettings.DatafileSettings.Count - 1 do
  begin
    DFSettings := ExportSettings.DatafileSettings[i];
    DF         := ExportSettings.PreparedDoc.DataFiles.GetDataFileByName(DFSettings.DatafileName);

    Generator.Section('Dataform: ' + DF.Caption.Text);

    if (DFSettings.ToRecord - DFSettings.FromRecord) < 0 then
      Generator.Line('Exported observations: Structure only')
    else
    if (DFSettings.ToRecord - DFSettings.FromRecord) = 0 then
      Generator.Line('Exported observation: no. ' + IntToStr(DFSettings.FromRecord + 1))
    else
      Generator.Line('Exported observations: ' + IntToStr(DFSettings.FromRecord + 1) + ' - ' + IntToStr(DFSettings.ToRecord + 1));
    Generator.Line('');

    R := TEpiReportControlList.Create(Generator);
    with TEpiReportControlList(R) do
    begin
      TableHeader                := 'Exported Items:';

      ControlItems               := Df.ControlItems;
      ControlItemsAreSubItemized := false;
      ExtendedList               := true;
    end;
    R.RunReport;
    R.Free;
    Generator.Line('');
  end;

{
  TmpDoc := TEpiExport.PrepareExportDocument(ExportSettings);
//  Df := TmpDoc.DataFiles[ExportSettings.DataFileIndex];

  R := TEpiReportControlList.Create(Generator);
  with TEpiReportControlList(R) do
  begin
    TableHeader := 'Exported Items:';
    // TODO: VERY DIRTY HACK - MOVE TO PREPAREEXPORTDOCUMENT!!!

    if (ExportSettings is TEpiDDIExportSetting) and
       (TEpiDDIExportSetting(ExportSettings).RenameVariablesPrefix <> '')
    then
    begin
      for i := 0 to Df.Fields.Count -1 do
        Df.Field[i].Name := '@rename' + IntToStr(i);
      for i := 0 to Df.Fields.Count -1 do
        Df.Field[i].Name := TEpiDDIExportSetting(ExportSettings).RenameVariablesPrefix + IntToStr(i+1);
    end;

    ControlItems := Df.ControlItems;
    ControlItemsAreSubItemized := false;
    ExtendedList := true;
  end;
  R.RunReport;
  R.Free;
  TmpDoc.Free;
                     }
//  Generator.Line('');

  CreateFileTable;

  Generator.Line('');

  R := TEpiReportStudyInfo.Create(Generator);
  TEpiReportStudyInfo(R).Document := Doc.Document;
  R.RunReport;
  R.Free;
end;

class function TReportExport.ReportFrameClass: TCustomFrameClass;
begin
  result := nil;
end;


end.

