unit viewer_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, htmlview;

type

  { TReportViewerForm }

  TReportViewerForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    HTMLViewer1: THTMLViewer;
    Memo1: TMemo;
    Panel1: TPanel;
    SaveDialog1: TSaveDialog;
    procedure BitBtn2Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { private declarations }
  public
    { public declarations }
  end;

procedure ShowReportForm(Owner: TComponent;
  Const Caption: string;
  Const ReportString: string;
  ShowHtml: boolean = false);


implementation

{$R *.lfm}

uses
  LCLType;

{ TReportViewerForm }

procedure TReportViewerForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  if (Key = VK_ESCAPE) and (Shift = []) then Close;
end;

procedure TReportViewerForm.BitBtn2Click(Sender: TObject);
var
  Ss: TStringStream;
  Fs: TFileStream;
begin
  if not SaveDialog1.Execute then exit;

  if Memo1.Visible then
  begin
    Memo1.Lines.SaveToFile(SaveDialog1.FileName);
  end else begin
    Ss := TStringStream.Create(HTMLViewer1.DocumentSource);
    Ss.Position := 0;
    Fs := TFileStream.Create(SaveDialog1.FileName, fmCreate);
    Fs.CopyFrom(Ss, ss.Size);
    Fs.Free;
    Ss.Free;
  end;
end;

procedure ShowReportForm(Owner: TComponent; const Caption: string;
  const ReportString: string; ShowHtml: boolean);
var
  F: TReportViewerForm;
begin
  F := TReportViewerForm.Create(Owner);
  F.Caption := Caption;

  if ShowHtml then
  begin
    F.Memo1.Visible := false;
    F.HTMLViewer1.BringToFront;
    F.HTMLViewer1.LoadFromString(ReportString);
  end else begin
    F.HTMLViewer1.Visible := false;
    F.Memo1.BringToFront;
    F.Memo1.Lines.BeginUpdate;
    F.Memo1.Text := ReportString;
    F.Memo1.Lines.EndUpdate;
  end;
  F.Show;
end;

end.

