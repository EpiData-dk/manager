unit staticreports_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, ComCtrls, ActnList, projectfilelist_frame, report_base,
  epidocument;

type

  { TStaticReportsForm }

  TStaticReportsForm = class(TForm)
    AddFilesAction: TAction;
    CancelAction: TAction;
    OkAction: TAction;
    ActionList1: TActionList;
    AddFilesBtn: TBitBtn;
    OkBtn: TBitBtn;
    BitBtn2: TBitBtn;
    Panel1: TPanel;
    ProgressBar1: TProgressBar;
    RadioGroup1: TRadioGroup;
    procedure AddFilesActionExecute(Sender: TObject);
    procedure CancelActionExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
    procedure OkActionExecute(Sender: TObject);
    procedure OkActionUpdate(Sender: TObject);
  private
    { private declarations }
    FOkActive: boolean;
    FProjectList: TProjectFileListFrame;
    FReport: TReportBase;
    FReportClass: TReportBaseClass;
    FActivatedOnce: boolean;
    function ShowDialog(out Files: TStrings): boolean;
    procedure DoAddFiles;
    procedure AfterFileImport(Sender: TObject; Document: TEpiDocument;
      const FileName: string);
    procedure ProjectFileListChanged(Sender: TObject);
  protected
    constructor Create(TheOwner: TComponent); override;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; ReportClass: TReportBaseClass);
    procedure   AddInitialDocument(Const FileName: string; Const Doc: TEpiDocument);
    class procedure RestoreDefaultPos;
    property    Report: TReportBase read FReport;
  end;

implementation

{$R *.lfm}

uses
  settings2_var, settings2, epimiscutils, viewer_form,
  epireport_generator_html, epireport_generator_txt, epireport_generator_base,
  report_types, ok_cancel_form;

{ TStaticReportsForm }

procedure TStaticReportsForm.FormShow(Sender: TObject);
begin
  if ManagerSettings.SaveWindowPositions then
    LoadFormPosition(Self, 'StaticReportsForm');
  RadioGroup1.ItemIndex := ManagerSettings.ReportOutputFormat;
end;

procedure TStaticReportsForm.OkActionExecute(Sender: TObject);
var
  FGeneratorClass: TEpiReportGeneratorBaseClass;
  FC: TCustomFrameClass;
  NextForm: TForm;
  Frame: TCustomFrame;
begin
  case RadioGroup1.ItemIndex of
    0: FGeneratorClass := TEpiReportHTMLGenerator;
    1: FGeneratorClass := TEpiReportTXTGenerator;
  end;
  FReport := FReportClass.Create(FProjectList.SelectedList, FGeneratorClass);

  if Supports(FReport, IReportFrameProvider) then
  begin
    FC := (FReport as IReportFrameProvider).GetFrameClass;
    if Supports(FC, IReportOptionFrame) then
    begin
      NextForm := TOkCancelForm.Create(self);
      Frame := FC.Create(NextForm);
      Frame.Align := alClient;
      Frame.Parent := NextForm;
      (Frame as IReportOptionFrame).UpdateFrame(FProjectList.SelectedList);
      NextForm.Caption := (Frame as IReportOptionFrame).GetFrameCaption;

      Self.Hide;
      ModalResult := NextForm.ShowModal;

      if ModalResult = mrOK then
        (Frame as IReportOptionFrame).ApplyReportOptions(FReport);
      NextForm.Free;
    end;
  end;
end;

procedure TStaticReportsForm.OkActionUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := FOkActive;
end;

procedure TStaticReportsForm.ProjectFileListChanged(Sender: TObject);
begin
  FOkActive := FProjectList.SelectedList.Count > 0;
end;

function TStaticReportsForm.ShowDialog(out Files: TStrings): boolean;
var
  Dlg: TOpenDialog;
  F: TReportViewerForm;
begin
  // Result = true, is a confirmation that the dialog was execute
  // and the user selected some files
  Result := false;

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

procedure TStaticReportsForm.DoAddFiles;
var
  Files: TStrings;
begin
  if ShowDialog(Files) then
  begin
    ProgressBar1.Visible := true;
    ProgressBar1.Min := 0;
    ProgressBar1.Max := Files.Count;
    ProgressBar1.Step := 1;

    Application.ProcessMessages;
    FProjectList.OnAfterImportFile := @AfterFileImport;
    FProjectList.AddFiles(Files);
    Files.Free;
    FProjectList.OnAfterImportFile := nil;

    ProgressBar1.Visible := false;
  end;
end;

procedure TStaticReportsForm.CancelActionExecute(Sender: TObject);
begin
  FReport := nil;
end;

procedure TStaticReportsForm.FormActivate(Sender: TObject);
begin
  // Do not show the dialog if a/some document(s) was pre-loaded.
  if (not FActivatedOnce) and
     (FProjectList.DocList.Count = 0)
  then
  begin
    FActivatedOnce := true;
    DoAddFiles;
  end;
end;

procedure TStaticReportsForm.AddFilesActionExecute(Sender: TObject);
begin
  DoAddFiles;
end;

procedure TStaticReportsForm.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  if ManagerSettings.SaveWindowPositions then
    SaveFormPosition(Self, 'StaticReportsForm');
end;

procedure TStaticReportsForm.AfterFileImport(Sender: TObject;
  Document: TEpiDocument; const FileName: string);
begin
  ProgressBar1.StepIt;
  Application.ProcessMessages;
end;

constructor TStaticReportsForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FActivatedOnce := false;
  FOkActive := false;
  FProjectList := TProjectFileListFrame.Create(Self);
  with FProjectList do
  begin
    Align := alClient;
    Parent := Self;
    OnSelectionChanged := @ProjectFileListChanged;
  end;
end;

constructor TStaticReportsForm.Create(TheOwner: TComponent;
  ReportClass: TReportBaseClass);
var
  FakeReport: TReportBase;
  TabSheet: TTabSheet;
  FC: TCustomFrameClass;
begin
  Create(TheOwner);
  FReportClass := ReportClass;
  FReport := nil;

  FakeReport := FReportClass.Create(TStringList.Create, TEpiReportTXTGenerator);
  Caption := 'Generate Report: ' + FakeReport.ReportTitle;
  FakeReport.Free;

  if Supports(FReportClass, IReportFrameProvider) then
    OkBtn.Caption := 'Next';
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

