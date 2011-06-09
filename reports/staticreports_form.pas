unit staticreports_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, projectfilelist_frame, report_base;

type

  { TForm1 }

  TForm1 = class(TForm)
    AddFilesBtn: TBitBtn;
    BitBtn1: TBitBtn;
    Panel1: TPanel;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    FProjectList: TProjectFileListFrame;
    FReportClass: TReportListBaseClass;
  protected
    constructor Create(TheOwner: TComponent); override;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; ReportClass: TReportListBaseClass);
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

uses
  settings2_var, epimiscutils, viewer_form;

{ TForm1 }

procedure TForm1.FormShow(Sender: TObject);
var
  Dlg: TOpenDialog;
begin
  Dlg := TOpenDialog.Create(Self);
  Dlg.InitialDir := ManagerSettings.WorkingDirUTF8;
  Dlg.Filter := GetEpiDialogFilter(true, true, true, false, false, false, true, false, false, true, false);
  Dlg.Options := [ofAllowMultiSelect, ofFileMustExist, ofEnableSizing, ofViewDetail];
  if not Dlg.Execute then
  begin
    Close;
    Exit;
  end;

//  Caption := FReportClass.ReportTitle;
end;

procedure TForm1.BitBtn1Click(Sender: TObject);
var
  R: TReportListBase;
begin
//  ModalResult := mr;
  R := FReportClass.Create(nil);

  F := THtmlViewerForm.Create(nil);
  F.Caption := R.ReportTitle;
  F.SetHtml(R.RunReport);
  F.Show;
//  L.Free;
end;

constructor TForm1.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FProjectList := TProjectFileListFrame.Create(Self);
  with FProjectList do
  begin
    Align := alClient;
    Parent := Self;
  end;
end;

constructor TForm1.Create(TheOwner: TComponent;
  ReportClass: TReportListBaseClass);
begin
  Create(TheOwner);
  FReportClass := ReportClass;
end;

end.

