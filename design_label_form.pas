unit design_label_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls;

type

  { TCreateLabelForm }

  TCreateLabelForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    LabelEdit: TEdit;
    Label1: TLabel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
  private
    { private declarations }
  public
    { public declarations }
  end; 

implementation

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

initialization
  {$I design_label_form.lrs}

end.

