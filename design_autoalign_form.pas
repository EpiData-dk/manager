unit design_autoalign_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, ActnList;

type

  TAutoAlignRecord = record
    LabelsAlign:      TAlign;
    DefaultAlign:     boolean;
    EqualVertSpace:   boolean;
    RemoveEmptySpace: boolean;
  end;

  { TAutoAlignForm }

  TAutoAlignForm = class(TForm)
    CancelAction: TAction;
    ActionList1: TActionList;
    AlignFieldsChk: TCheckBox;
    Button1: TButton;
    Button2: TButton;
    EmptySpaceChkBtn: TCheckBox;
    EqualSpaceChk: TCheckBox;
    Label1: TLabel;
    Panel1: TPanel;
    RightAlignRadio: TRadioButton;
    LeftAlignRadio: TRadioButton;
    KeepAlignRadio: TRadioButton;
    procedure CancelActionExecute(Sender: TObject);
    procedure EmptySpaceChkBtnClick(Sender: TObject);
    procedure EqualSpaceChkClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    FAlignProperties: TAutoAlignRecord;
    { private declarations }
  public
    { public declarations }
    property  AlignProperties: TAutoAlignRecord read FAlignProperties;
  end; 

implementation

{ TAutoAlignForm }

procedure TAutoAlignForm.CancelActionExecute(Sender: TObject);
begin
  Close;
end;

procedure TAutoAlignForm.EmptySpaceChkBtnClick(Sender: TObject);
begin
  EqualSpaceChk.Enabled := not EmptySpaceChkBtn.Checked;
end;

procedure TAutoAlignForm.EqualSpaceChkClick(Sender: TObject);
begin
  EmptySpaceChkBtn.Enabled := not EqualSpaceChk.Checked;
end;

procedure TAutoAlignForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  with AlignProperties do
  begin
    DefaultAlign := AlignFieldsChk.Checked;
    EqualVertSpace := EqualSpaceChk.Checked;
    RemoveEmptySpace := EmptySpaceChkBtn.Checked;
    if RightAlignRadio.Checked then
      LabelsAlign := alRight
    else if LeftAlignRadio.Checked then
      LabelsAlign := alLeft;
  end;
end;

procedure TAutoAlignForm.FormCreate(Sender: TObject);
begin
  with AlignProperties do
  begin
    DefaultAlign := false;
    LabelsAlign := alNone;
    EqualVertSpace := false;
    RemoveEmptySpace := false;
  end;
end;

initialization
  {$I design_autoalign_form.lrs}

end.

