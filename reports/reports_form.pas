unit reports_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ComCtrls, ActnList, report_base, epidocument, report_types,
  epiopenfile;

type

  { TReportForm }

  TReportForm = class(TForm)
    OkAction: TAction;
    ActionList1: TActionList;
    AddFilesBtn: TBitBtn;
    BitBtn2: TBitBtn;
    OkBtn: TBitBtn;
    Panel1: TPanel;
    ProgressBar1: TProgressBar;
    RadioGroup1: TRadioGroup;
    procedure OkActionUpdate(Sender: TObject);
  private
    FReportClass: TReportBaseClass;
    FReport: TReportBase;
    FFrame: TCustomFrame;
    IFrame: IReportFrame;
    procedure LoadGlyphs;
  private
    { Events }
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure AddFilesClick(Sender: TObject);
    procedure OkClick(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent; ReportClass: TReportBaseClass);
    procedure AddInitialDocumentFile(Const DocFile: TEpiDocumentFile);
    property  Report: TReportBase read FReport;
  end;

var
  ReportForm: TReportForm;

implementation

{$R *.lfm}

uses
  epiv_datamodule, epireport_generator_base, settings2_var,
  settings2,
  epireport_generator_html, epireport_generator_txt;

{ TReportForm }

procedure TReportForm.OkActionUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := IFrame.CanPressOk;
end;

procedure TReportForm.LoadGlyphs;
begin
  DM.Icons16.GetBitmap(19, AddFilesBtn.Glyph);
end;

procedure TReportForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if ManagerSettings.SaveWindowPositions then
    SaveFormPosition(Self, 'ReportsForm');
end;

procedure TReportForm.FormShow(Sender: TObject);
begin
  LoadGlyphs;
  case ManagerSettings.ReportOutputFormat of
    0: RadioGroup1.ItemIndex := 1;
    1: RadioGroup1.ItemIndex := 0;
  end;
  Caption := IFrame.GetCaption;

  if ManagerSettings.SaveWindowPositions then
    LoadFormPosition(Self, 'ReportsForm');
end;

procedure TReportForm.AddFilesClick(Sender: TObject);
var
  FileNames: TStrings;
begin
  FileNames := TStringList.Create;
  DM.OpenDlgEpiFiles(FileNames);
  IFrame.AddFiles(FileNames);
  FileNames.Free;
end;

procedure TReportForm.OkClick(Sender: TObject);
var
  FGeneratorClass: TEpiReportGeneratorBaseClass;
begin
  case RadioGroup1.ItemIndex of
    0: FGeneratorClass := TEpiReportTXTGenerator;
    1: FGeneratorClass := TEpiReportHTMLGenerator;
  end;
  FReport := FReportClass.Create(FGeneratorClass);
  IFrame.ApplyReportOptions(FReport);
end;

constructor TReportForm.Create(TheOwner: TComponent;
  ReportClass: TReportBaseClass);
begin
  inherited Create(TheOwner);

  FReportClass := ReportClass;
  FFrame       := FReportClass.ReportFrameClass.Create(Self);

  if not Supports(FFrame, IReportFrame, IFrame) then
    Exception.Create('IReportFrame interface not supported!');

  FFrame.Align := alClient;
  FFrame.Parent := Self;

  OnShow := @FormShow;
  OnClose := @FormClose;

  AddFilesBtn.OnClick := @AddFilesClick;
  OkAction.OnExecute := @OkClick;
end;

procedure TReportForm.AddInitialDocumentFile(const DocFile: TEpiDocumentFile);
begin
  IFrame.AddDocumentFile(DocFile);
end;

end.


