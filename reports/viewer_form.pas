unit viewer_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls;

type

  { TReportViewerForm }

  TReportViewerForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Memo1: TMemo;
    Panel1: TPanel;
    SaveDialog1: TSaveDialog;
    procedure BitBtn2Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure RestoreDefaultPos;
  end;

procedure ShowReportForm(Owner: TComponent;
  Const Caption: string;
  Const ReportString: string;
  ShowHtml: boolean = false);

procedure ReportFormRestoreDefaultPos;


implementation

{$R *.lfm}

uses
  LCLType, LCLIntf, settings2, settings2_var;

var
  CompList: TList;

{ TReportViewerForm }

procedure TReportViewerForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  if (Key = VK_ESCAPE) and (Shift = []) then Close;
end;

procedure TReportViewerForm.FormShow(Sender: TObject);
begin
  if ManagerSettings.SaveWindowPositions then
    LoadFormPosition(Self, 'ReportViewerForm');
end;

procedure TReportViewerForm.RestoreDefaultPos;
begin
  BeginFormUpdate;
  Width := 400;
  Height := 400;
  Left := 200;
  Top := 200;
  EndFormUpdate;
  SaveFormPosition(Self, 'ReportViewerForm');
end;

procedure TReportViewerForm.BitBtn2Click(Sender: TObject);
begin
  if not SaveDialog1.Execute then exit;

  Memo1.Lines.SaveToFile(SaveDialog1.FileName);
end;

procedure TReportViewerForm.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  CanClose := true;
  if ManagerSettings.SaveWindowPositions
  then
    SaveFormPosition(Self, 'ReportViewerForm');
end;

procedure ShowReportForm(Owner: TComponent; const Caption: string;
  const ReportString: string; ShowHtml: boolean);
var
  F: TReportViewerForm;
  S: TFileStream;
  Fn: String;
  i: Integer;
begin
  if ShowHtml then
  begin
    i := 0;
    repeat
      Fn := SysUtils.GetTempDir(false) + Format('epidata_report.%d.html', [i]);
      Inc(i);
    until not FileExistsUTF8(Fn);

    S := TFileStream.Create(Fn, fmCreate);
    S.Write(ReportString[1], Length(ReportString));
    S.Free;
    OpenUrl('file://' + Fn);
  end else begin
    if not Assigned(CompList) then
      CompList := TList.Create;

    F := TReportViewerForm.Create(Owner);
    F.Caption := Caption;
    F.Memo1.BringToFront;
    F.Memo1.Lines.BeginUpdate;
    F.Memo1.Text := ReportString;
    F.Memo1.Lines.EndUpdate;
    F.Show;

    CompList.Add(F);
  end;
end;

procedure ReportFormRestoreDefaultPos;
var
  C: Pointer;
begin
  if Assigned(CompList) then
    For C in CompList do
      TReportViewerForm(C).RestoreDefaultPos;
end;

finalization
  CompList.Free;

end.

