unit staticreports_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, projectfilelist_frame, report_base;

type

  { TStaticReportsForm }

  TStaticReportsForm = class(TForm)
    AddFilesBtn: TBitBtn;
    BitBtn1: TBitBtn;
    Panel1: TPanel;
    procedure AddFilesBtnClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    FProjectList: TProjectFileListFrame;
    FReport: TReportListBase;
    FReportClass: TReportListBaseClass;
    function ShowDialog(out Files: TStrings): boolean;
  protected
    constructor Create(TheOwner: TComponent); override;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; ReportClass: TReportListBaseClass);
    property    Report: TReportListBase read FReport;
  end;

var
  StaticReportsForm: TStaticReportsForm;

implementation

{$R *.lfm}

uses
  settings2_var, epimiscutils, viewer_form;

{ TStaticReportsForm }

procedure TStaticReportsForm.FormShow(Sender: TObject);
var
  Files: TStrings;
begin
  if ShowDialog(Files) then
  begin
    FProjectList.AddFiles(Files);
    Files.Free;
  end;
end;

function TStaticReportsForm.ShowDialog(out Files: TStrings): boolean;
var
  Dlg: TOpenDialog;
begin
  Result := false;
  Files := nil;
  Dlg := nil;

  try
    Dlg := TOpenDialog.Create(Self);
    Dlg.InitialDir := ManagerSettings.WorkingDirUTF8;
    Dlg.Filter := GetEpiDialogFilter(true, true, true, false, false, false, true, false, false, true, false);
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
begin
  FReport := FReportClass.Create(FProjectList.SelectedList);
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
    Parent := Self;
  end;
end;

constructor TStaticReportsForm.Create(TheOwner: TComponent;
  ReportClass: TReportListBaseClass);
begin
  Create(TheOwner);
  FReportClass := ReportClass;
  FReport := nil;;
end;

end.

