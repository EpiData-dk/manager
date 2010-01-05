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
    function GetFieldName: string;
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

function TCreateLabelForm.GetFieldName: string;
var
  LastFieldNo: Integer;
begin
  result := 'label_' + IntToStr(1 + FDf.NumFields - FDf.NumDataFields);
  inc(LastFieldNo);
  if FDf.FieldExists(result) then
    result := FDf.CreateUniqueFieldName('label_');
end;

initialization
  {$I design_label_form.lrs}

end.

