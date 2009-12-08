unit design_autoalign_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, ActnList;

type

  { TAutoAlignForm }

  TAutoAlignForm = class(TForm)
    CancelAction: TAction;
    ActionList1: TActionList;
    AlignFieldsChk: TCheckBox;
    AlignLabelsChk: TCheckBox;
    Button1: TButton;
    Button2: TButton;
    EmptySpaceChkBtn: TCheckBox;
    EqualSpaceChk: TCheckBox;
    Panel1: TPanel;
    procedure AlignFieldsChkChange(Sender: TObject);
    procedure CancelActionExecute(Sender: TObject);
    procedure EmptySpaceChkBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

implementation

{ TAutoAlignForm }

procedure TAutoAlignForm.AlignFieldsChkChange(Sender: TObject);
begin
  AlignLabelsChk.Enabled := AlignFieldsChk.Checked;
end;

procedure TAutoAlignForm.CancelActionExecute(Sender: TObject);
begin
  Close;
end;

procedure TAutoAlignForm.EmptySpaceChkBtnClick(Sender: TObject);
begin
  AlignFieldsChk.Enabled := not EmptySpaceChkBtn.Checked;
  AlignLabelsChk.Enabled := not EmptySpaceChkBtn.Checked;
  EqualSpaceChk.Enabled := not EmptySpaceChkBtn.Checked;
end;

procedure TAutoAlignForm.FormCreate(Sender: TObject);
begin
  AlignFieldsChk.Checked := true;
end;

initialization
  {$I design_autoalign_form.lrs}

end.

