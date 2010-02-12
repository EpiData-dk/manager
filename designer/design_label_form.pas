unit design_label_form;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, StdCtrls, ExtCtrls, ActnList,
  UEpiDataFile;

type

  { TCreateLabelForm }
  TCreateLabelForm = class(TForm)
    CloseAction: TAction;
    ActionList1: TActionList;
    CancelBtn: TButton;
    Label1: TLabel;
    LabelEdit: TEdit;
    OkBtn: TButton;
    Panel1: TPanel;
    procedure CloseActionExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
  private
    { private declarations }
    FDf: TEpiDataFile;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; DataFile: TEpiDataFile);
    class function GetFieldName(TmpDf: TEpiDataFile): string;
  end;

implementation

uses
  Controls;

{ TCreateLabelForm }

procedure TCreateLabelForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  S: String;
begin
  CanClose := true;
  if ModalResult = mrCancel then exit;

  S := UTF8Encode(Trim(UTF8Decode(LabelEdit.Text)));
  if S = '' then
    CanClose := true;
end;

procedure TCreateLabelForm.CloseActionExecute(Sender: TObject);
begin
  CancelBtn.Click;
end;

constructor TCreateLabelForm.Create(TheOwner: TComponent; DataFile: TEpiDataFile
  );
begin
  inherited Create(TheOwner);
  FDf := DataFile;
  ActiveControl := LabelEdit;
end;

class function TCreateLabelForm.GetFieldName(TmpDf: TEpiDataFile): string;
begin
  if not Assigned(TmpDf) then
    exit;
  result := 'label_' + IntToStr(TmpDf.TextLabelCount + 1);
  if TmpDf.FieldExists(result) then
    result := TmpDf.CreateUniqueFieldName('label_');
end;

initialization
  {$I design_label_form.lrs}

end.

