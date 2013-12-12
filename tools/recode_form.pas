unit recode_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, epidatafiles;

type

  { TRecodeForm }

  TRecodeForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    CheckBox1: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure CheckBox1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
  private
    FField: TEpiField;
    procedure SetField(AValue: TEpiField);
    function CheckContent(Const Edit: TEdit;
      Const CanHaveAny: boolean): boolean;
    procedure ApplyRecode();
    function DoError(Const Msg: string): boolean;
  public
    property Field: TEpiField read FField write SetField;
  end;

implementation

uses
  epiconvertutils, epidatafilestypes;

{$R *.lfm}

resourcestring
  rsNotAValidType = 'Not a valid %s: %s';


{ TRecodeForm }

procedure TRecodeForm.CheckBox1Click(Sender: TObject);
begin
  Edit2.Enabled := not CheckBox1.Checked;
end;

procedure TRecodeForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if ModalResult = mrOk then
    ApplyRecode();
end;

procedure TRecodeForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if ModalResult = mrCancel then exit;

  CanClose :=
    CheckContent(Edit1, true) and
    (
     CheckBox1.Checked or
     CheckContent(Edit2, false)
    );
end;

procedure TRecodeForm.SetField(AValue: TEpiField);
begin
  if FField = AValue then Exit;
  FField := AValue;

  CheckBox1.Enabled := Field.HasDefaultValue;
end;

function TRecodeForm.CheckContent(const Edit: TEdit; const CanHaveAny: boolean
  ): boolean;
var
  I64: int64;
  F: Extended;
  S: TCaption;
  W1, W2, W3: Word;
begin
  Result := true;

  S := Edit.Text;

  if CanHaveAny and
     (S = '*')
  then
    Exit; // true

  if (S = '.')
  then
    Exit; // true

  if (Field.FieldType in BoolFieldTypes) and (not TryStrToInt64(S, I64)) then
    Exit(DoError(Format(rsNotAValidType, ['boolean', S])));

  if (Field.FieldType in IntFieldTypes) and (not TryStrToInt64(S, I64)) then
    Exit(DoError(Format(rsNotAValidType, ['integer', S])));

  if (Field.FieldType in FloatFieldTypes) and (not TryStrToFloat(S, F)) then
    Exit(DoError(Format(rsNotAValidType, ['float', S])));

  if (Field.FieldType in DateFieldTypes) and (not EpiStrToDate(S, DateSeparator, Field.FieldType, W1, W2, W3, S)) then
    Exit(DoError(S));

  if (Field.FieldType in TimeFieldTypes) and (not EpiStrToTime(S, TimeSeparator, W1, W2, W3, S)) then
    Exit(DoError(S));
end;

procedure TRecodeForm.ApplyRecode;
var
  RecodeFrom: String;
  RecodeTo: String;
  i: Integer;
  RecodeCounter: Integer;
begin
  RecodeFrom := Edit1.Text;

  if CheckBox1.Checked then
    RecodeTo := Field.DefaultValueAsString
  else
    RecodeTo := Edit2.Text;

  RecodeCounter := 0;
  for i := 0 to Field.Size - 1 do
    if (RecodeFrom = '*') or
       (Field.AsString[i] = RecodeFrom)
    then
      begin
        Field.AsString[i] := RecodeTo;
        Inc(RecodeCounter);
      end;

  ShowMessage(Format('%d values recoded from %s to %s', [RecodeCounter, RecodeFrom, RecodeTo]));
end;

function TRecodeForm.DoError(const Msg: string): boolean;
begin
  ShowMessage(Msg);
  Result := false;
end;

end.

