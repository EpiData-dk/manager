unit design_label_form;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, StdCtrls, UEpiDataFile;

type

  { TCreateLabelForm }
  TCreateLabelForm = class(TForm)
    CancelBtn: TButton;
    OkBtn: TButton;
    LabelEdit: TEdit;
    Label1: TLabel;
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

var
  LastFieldNo: Integer = 1;

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

constructor TCreateLabelForm.Create(TheOwner: TComponent; DataFile: TEpiDataFile
  );
begin
  inherited Create(TheOwner);
  FDf := DataFile;
end;

function TCreateLabelForm.GetFieldName: string;
begin
  result := 'label_' + IntToStr(LastFieldNo);
  inc(LastFieldNo);
  if FDf.FieldExists(result) then
    result := FDf.CreateUniqueFieldName('label_');
end;

initialization
  {$I design_label_form.lrs}

end.

