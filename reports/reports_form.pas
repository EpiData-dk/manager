unit reports_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ComCtrls, report_base, epidocument, report_types, epiopenfile;

type

  { TReportForm }

  TReportForm = class(TForm)
    AddFilesBtn: TBitBtn;
    BitBtn2: TBitBtn;
    OkBtn: TBitBtn;
    Panel1: TPanel;
    ProgressBar1: TProgressBar;
    RadioGroup1: TRadioGroup;
  private
    FReportClass: TReportBaseClass;
    FReport: TReportBase;
    FFrame: TCustomFrame;
    IFrame: IReportFrame;
    procedure LoadGlyphs;
  private
    { Events }
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
  epiv_datamodule, epireport_generator_base,
  epireport_generator_html, epireport_generator_txt;

{ TReportForm }

procedure TReportForm.LoadGlyphs;
begin
  DM.Icons16.GetBitmap(19, AddFilesBtn.Glyph);
end;

procedure TReportForm.FormShow(Sender: TObject);
begin
  LoadGlyphs;
  Caption := IFrame.GetCaption;
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
    0: FGeneratorClass := TEpiReportHTMLGenerator;
    1: FGeneratorClass := TEpiReportTXTGenerator;
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

  AddFilesBtn.OnClick := @AddFilesClick;
  OkBtn.OnClick       := @OkClick;
end;

procedure TReportForm.AddInitialDocumentFile(const DocFile: TEpiDocumentFile);
begin
  IFrame.AddDocumentFile(DocFile);
end;

end.


