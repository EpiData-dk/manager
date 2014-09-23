unit reports_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ComCtrls, report_base;

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
  public
    constructor Create(TheOwner: TComponent; ReportClass: TReportBaseClass);
    procedure   AddInitialDocument(Const FileName: string; Const Doc: TEpiDocument);
    { public declarations }
  end;

var
  ReportForm: TReportForm;

implementation

{$R *.lfm}

{ TReportForm }

constructor TReportForm.Create(TheOwner: TComponent;
  ReportClass: TReportBaseClass);
begin
  inherited Create(TheOwner);

  FReportClass := ReportClass;

//  FakeReport := FReportClass.Create(L, TEpiReportTXTGenerator);
//  Caption := 'Generate Report: ' + FakeReport.ReportTitle;
end;

procedure TReportForm.AddInitialDocument(const FileName: string;
  const Doc: TEpiDocument);
begin

end;

end.

