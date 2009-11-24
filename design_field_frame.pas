unit Design_Field_Frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, StdCtrls, MaskEdit,
  UEpiDataFile;

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
    FDf: TEpiDataFile;
    FNewField: boolean;
    FOldFieldName: string;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; DataFile: TEpiDataFile; ShowDecimals: boolean = false; NewField: boolean = true);
    property OldFieldName: string Read FOldFieldName write FOldFieldName;
  end; 

implementation

uses
  Controls, settings;

var
  LastFieldNo: Integer = 1;

{ TFieldCreateForm }

procedure TFieldCreateForm.FormCloseQuery(Sender: TObject; var CanClose: boolean
  );
var
  S: string;
  L, D: LongInt;
begin
  CanClose := true;

  if (Sender is TButton) and
     ((Sender as TButton).ModalResult = mrCancel) then exit;
  if ModalResult = mrCancel then exit;

  // Sanity checks.
  // - variable name
  S := UTF8Encode(Trim(UTF8Decode(FieldNameEdit.Text)));
  if S = '' then
    CanClose := false;

  if FNewField or (OldFieldName <> S) then
  begin
    if FDf.FieldExists(S) then
      CanClose := false;
    if FDf.FileProperties.DefineExists(S) then
      CanClose := false;
  end;

  // - FieldLength (no need to ut8-handle this. It's always plain ASCII.
  S := Trim(FieldSizeEdit.Text);
  if S = '' then
    CanClose := false;
  L := StrToInt(S);
  if L <= 0 then
    CanClose := false;

  // - FieldDecimals (no need to ut8-handle this. It's always plain ASCII.
  if not FieldDecimalSizeEdit.Enabled then exit;
  S := Trim(FieldDecimalSizeEdit.Text);
  if S = '' then
    CanClose := false;
  D := StrToInt(S);
  if D <= 0 then
    CanClose := false;
  if D >= (L - 1) then
    CanClose := false;
end;

constructor TFieldCreateForm.Create(TheOwner: TComponent;
  DataFile: TEpiDataFile; ShowDecimals: boolean; NewField: boolean);
begin
  inherited Create(TheOwner);
  if DataFile = nil then Close;

  FDf := DataFile;
  FNewField := NewField;

  FieldNameEdit.Text := BuilderSettings.FieldNamePrefix + IntToStr(LastFieldNo);
  FieldSizeEdit.Text := '5';
  Inc(LastFieldNo);

  if not ShowDecimals then exit;

  Height := Height + FieldDecimalSizeEdit.Height + 5;
  Label4.Visible := true;
  FieldDecimalSizeEdit.Visible := true;
  FieldDecimalSizeEdit.Enabled := true;
  FieldDecimalSizeEdit.Text    := '2';
end;

initialization
  {$I design_field_frame.lrs}

end.

