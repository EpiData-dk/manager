unit staticreports_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, projectfilelist_frame, report_base, epidocument;

type

  { TStaticReportsForm }

  TStaticReportsForm = class(TForm)
    AddFilesBtn: TBitBtn;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Panel1: TPanel;
    Panel2: TPanel;
    RadioGroup1: TRadioGroup;
    procedure AddFilesBtnClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    FProjectList: TProjectFileListFrame;
    FReport: TReportBase;
    FReportClass: TReportBaseClass;
    function ShowDialog(out Files: TStrings): boolean;
  protected
    constructor Create(TheOwner: TComponent); override;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; ReportClass: TReportBaseClass);
    procedure   AddInitialDocument(Const FileName: string; Const Doc: TEpiDocument);
    class procedure RestoreDefaultPos;
    property    Report: TReportBase read FReport;
  end;

var
  StaticReportsForm: TStaticReportsForm;

implementation

{$R *.lfm}

uses
  settings2_var, settings2, epimiscutils, viewer_form,
  epireport_generator_html, epireport_generator_txt, epireport_generator_base;

{ TStaticReportsForm }

procedure TStaticReportsForm.FormShow(Sender: TObject);
var
  Files: TStrings;
begin
  if ShowDialog(Files) then
  begin
    FProjectList.AddFiles(Files);
    Files.Free;
  end else
    Close;
  if ManagerSettings.SaveWindowPositions then
    LoadFormPosition(Self, 'StaticReportsForm');
end;

function TStaticReportsForm.ShowDialog(out Files: TStrings): boolean;
var
  Dlg: TOpenDialog;
  F: TReportViewerForm;
begin
  // Result = true, is a confirmation that the dialog was execute
  // and the user selected some files
  Result := false;

  // Do not show the dialog if a/some document(s) was pre-loaded.
  if FProjectList.DocList.Count > 0 then exit;

  Files := nil;
  Dlg := nil;

  try
    Dlg := TOpenDialog.Create(Self);
    Dlg.InitialDir := ManagerSettings.WorkingDirUTF8;
    Dlg.Filter := GetEpiDialogFilter(dfImport + [dfCollection]);
    Dlg.Options := [ofAllowMultiSelect, ofFileMustExist, ofEnableSizing, ofViewDetail];
    if not Dlg.Execute then exit;

    Files := TStringList.Create;
    Files.Assign(Dlg.Files);
    Result := true
  finally
    Dlg.Free;
  end;
end;

procedure TStaticReportsForm.BitBtn1Click(Sender: TObject);
var
  FGeneratorClass: TEpiReportGeneratorBaseClass;
begin
  case RadioGroup1.ItemIndex of
    0: FGeneratorClass := TEpiReportHTMLGenerator;
    1: FGeneratorClass := TEpiReportTXTGenerator;
  end;
  FReport := FReportClass.Create(FProjectList.SelectedList, FGeneratorClass);
end;

procedure TStaticReportsForm.BitBtn2Click(Sender: TObject);
begin
  FReport := nil;
end;

procedure TStaticReportsForm.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  if ManagerSettings.SaveWindowPositions then
    SaveFormPosition(Self, 'StaticReportsForm');
end;

procedure TStaticReportsForm.AddFilesBtnClick(Sender: TObject);
var
  Files: TStrings;
begin
  if ShowDialog(Files) then
  begin
    FProjectList.AddFiles(Files);
    Files.Free;
  end;
end;

constructor TStaticReportsForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FProjectList := TProjectFileListFrame.Create(Self);
  with FProjectList do
  begin
    Align := alClient;
    Parent := Panel2;
  end;
end;

constructor TStaticReportsForm.Create(TheOwner: TComponent;
  ReportClass: TReportBaseClass);
var
  FakeReport: TReportBase;
begin
  Create(TheOwner);
  FReportClass := ReportClass;
  FReport := nil;

  FakeReport := FReportClass.Create(TStringList.Create, TEpiReportTXTGenerator);
  Caption := 'Generate Report: ' + FakeReport.ReportTitle;
  FakeReport.Free;
end;

procedure TStaticReportsForm.AddInitialDocument(const FileName: string;
  const Doc: TEpiDocument);
begin
  FProjectList.AddDocument(FileName, Doc);
end;

class procedure TStaticReportsForm.RestoreDefaultPos;
var
  Aform: TForm;
begin
  Aform := TForm.Create(nil);
  Aform.Width := 600;
  Aform.Height := 480;
  Aform.top := (Screen.Monitors[0].Height - Aform.Height) div 2;
  Aform.Left := (Screen.Monitors[0].Width - Aform.Width) div 2;
  SaveFormPosition(Aform, 'StaticReportsForm');
  AForm.free;
end;

end.

