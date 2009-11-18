unit Design_Field_Frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, StdCtrls, MaskEdit;

type

  { TFieldCreateForm }

  TFieldCreateForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    FieldNameEdit: TEdit;
    FieldDecimalSizeEdit: TMaskEdit;
    LabelEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    FieldSizeEdit: TMaskEdit;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
  private
    { private declarations }
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; ShowDecimals: boolean = false);
  end; 

implementation

uses
  Controls;

{ TFieldCreateForm }

procedure TFieldCreateForm.FormCloseQuery(Sender: TObject; var CanClose: boolean
  );
begin
  CanClose := true;
  if (Sender is TButton) and
     ((Sender as TButton).ModalResult = mrCancel) then exit;
  if ModalResult = mrCancel then exit;

  if Trim(UTF8Decode(FieldNameEdit.Text)) = '' then
    CanClose := false;

  if Trim(UTF8Decode(FieldSizeEdit.Text)) = '' then
    CanClose := false;
end;

constructor TFieldCreateForm.Create(TheOwner: TComponent; ShowDecimals: boolean);
begin
  inherited Create(TheOwner);
  if not ShowDecimals then exit;

  Height := Height + FieldDecimalSizeEdit.Height;
  Label4.Visible := true;
  FieldDecimalSizeEdit.Visible := true;
  FieldDecimalSizeEdit.Enabled := true;
end;

initialization
  {$I design_field_frame.lrs}

end.

