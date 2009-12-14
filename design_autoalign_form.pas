unit design_autoalign_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, ActnList;

type

  TAutoAlignMethod = (aamNone, aamDefault, aamEqualSpace, aamRemoveSpace);

  TAutoAlignRecord = record
    LabelsAlign: TAlign;
    AlignMethod: TAutoAlignMethod;
  end;

  { TAutoAlignForm }

  TAutoAlignForm = class(TForm)
    CancelAction: TAction;
    ActionList1: TActionList;
    Button1: TButton;
    Button2: TButton;
    Panel1: TPanel;
    LabelsGroupBox: TRadioGroup;
    DefaultAlignRadio: TRadioButton;
    EqualVerticalSpaceRadio: TRadioButton;
    RemoveEmptySpaceRadio: TRadioButton;
    procedure CancelActionExecute(Sender: TObject);
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

procedure TAutoAlignForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  with AlignProperties do
  begin
    if DefaultAlignRadio.Checked then
      AlignMethod := aamDefault
    else if EqualVerticalSpaceRadio.Checked then
      AlignMethod := aamEqualSpace
    else if RemoveEmptySpaceRadio.Checked then
      AlignMethod := aamRemoveSpace
    else
      AlignMethod := aamNone;
    Case LabelsGroupBox.ItemIndex of
      0: LabelsAlign := alNone;
      1: LabelsAlign := alLeft;
      2: LabelsAlign := alRight;
    end;
  end;
end;

procedure TAutoAlignForm.FormCreate(Sender: TObject);
begin
  with AlignProperties do
  begin
    AlignMethod := aamNone;
    LabelsAlign := alNone;
  end;
end;

initialization
  {$I design_autoalign_form.lrs}

end.

