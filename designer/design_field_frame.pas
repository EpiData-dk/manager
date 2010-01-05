unit Design_Field_Frame;

{$codepage utf8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, StdCtrls, MaskEdit, ActnList, ExtCtrls,
  UEpiDataFile, UDataFileTypes, UEpiUtils;

type

  { TFieldCreateForm }

  TFieldCreateForm = class(TForm)
    ActionList1: TActionList;
    CancelBtn: TButton;
    CloseAction: TAction;
    DefaultValueEdit: TEdit;
    FieldDecimalSizeEdit: TMaskEdit;
    FieldLengthEdit: TMaskEdit;
    FieldNameEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    LabelEdit: TEdit;
    OkBtn: TButton;
    Panel1: TPanel;
    procedure CloseActionExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    FDf: TEpiDataFile;
    FNewField: boolean;
    FOldFieldName: string;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; DataFile: TEpiDataFile; FieldType: TFieldType; NewField: boolean = true);
    class procedure AutoCreateField(DataFile: TEpiDataFile; aField: TEpiField);
    procedure ReadField(AField: TEpiField);
    procedure WriteField(AField: TEpiField);
    property OldFieldName: string Read FOldFieldName write FOldFieldName;
  end; 

implementation

uses
  Controls, settings, UEpiDataGlobals, design_frame, Graphics,
  UStringUtils, strutils;

var
  OldLength, OldDecimals: Integer;

{ TFieldCreateForm }

procedure TFieldCreateForm.FormCloseQuery(Sender: TObject; var CanClose: boolean
  );
var
  S, T: string;
  L, D: LongInt;
begin
  CanClose := true;

  if (Sender is TButton) and
     ((Sender as TButton).ModalResult = mrCancel) then exit;
  if ModalResult = mrCancel then exit;

  // Sanity checks.
  // - variable name
  S := Trim(UTF8Decode(FieldNameEdit.Text));
  T := Trim(UTF8Decode(LabelEdit.Text));
  if (S = '') and (T = '') then
  begin
    CanClose := false;
    if ManagerSettings.FieldNamePrefix <> '' then
      FieldNameEdit.SetFocus
    else
      LabelEdit.SetFocus;
    Exit;
  end;

  // If no field name is specified use FirstWord/Auto naming rules.
  if (S = '') and (T <> '') then
  begin
    case ManagerSettings.FieldNamingStyle of
      fnFirstWord: S := FirstWord(LabelEdit.Text);
      fnAuto:      S := AutoFieldName(LabelEdit.Text);
    end;
    FieldNameEdit.Text := S;
  end;

  S := UTF8Encode(S);
  T := UTF8Encode(T);

  if FNewField or (OldFieldName <> S) then
  begin
    if FDf.FieldExists(S) then
      CanClose := false;
    if FDf.FileProperties.DefineExists(S) then
      CanClose := false;
    if not CanClose then
    begin
      FieldNameEdit.SetFocus;
      Exit;
    end;
  end;

  // - FieldLength (no need to ut8-handle this. It's always plain ASCII.
  S := Trim(FieldLengthEdit.Text);
  if S = '' then
    CanClose := false;
  L := StrToInt(S);
  if L <= 0 then
    CanClose := false;

  OldLength := L;

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

  if not CanClose then exit;
  OldDecimals := D;
end;

procedure TFieldCreateForm.CloseActionExecute(Sender: TObject);
begin
  CancelBtn.Click;
end;

procedure TFieldCreateForm.FormShow(Sender: TObject);
begin
  FieldLengthEdit.SelLength := 0;
  if FieldDecimalSizeEdit.Visible then
    FieldDecimalSizeEdit.SelLength := 0;

  FieldNameEdit.SelLength := 0;
end;

constructor TFieldCreateForm.Create(TheOwner: TComponent;
  DataFile: TEpiDataFile; FieldType: TFieldType; NewField: boolean);
begin
  inherited Create(TheOwner);
  if DataFile = nil then Close;

  FDf := DataFile;
  FNewField := NewField;

{  if (FDf.Size > 0) and (NewField) then
  begin
    Label6.Enabled := true;
    Label6.Visible := true;
    DefaultValueEdit.Enabled := true;
    DefaultValueEdit.Visible := true;
    Height := Height + DefaultValueEdit.Height + 5;
  end;           }

  if ManagerSettings.FieldNamePrefix <> '' then
    FieldNameEdit.Text := ManagerSettings.FieldNamePrefix + IntToStr(FDf.NumDataFields+1);
  Case FieldType of
    ftFloat:
      begin
        Height := Height + FieldDecimalSizeEdit.Height + 5;
        FieldLengthEdit.Text := IntToStr(ManagerSettings.FloatFieldLength);
        Label4.Visible := true;
        FieldDecimalSizeEdit.Visible := true;
        FieldDecimalSizeEdit.Enabled := true;
        FieldDecimalSizeEdit.Text := IntToStr(ManagerSettings.FloatDecimalLength);
      end;
    ftDate, ftToday,
    ftEuroDate, ftEuroToday,
    ftYMDDate, ftYMDToday:
      begin
        FieldLengthEdit.Text := '10';
        FieldLengthEdit.Visible := false;
        Label3.Visible:= false ;
        Height := Height - FieldLengthEdit.Height - 5
      end;
    ftString, ftCrypt,ftSoundex, ftUpperAlfa:
      FieldLengthEdit.Text := IntToStr(ManagerSettings.StringFieldLength);
    ftInteger, ftIDNUM:
      FieldLengthEdit.Text := IntToStr(ManagerSettings.IntFieldLength);
  end;

  // show fieldtype:
  Label5.Caption := 'Type: ' + FieldTypeToFieldTypeName(FieldType, nil);

  ActiveControl := LabelEdit;
end;

class procedure TFieldCreateForm.AutoCreateField(DataFile: TEpiDataFile;
  aField: TEpiField);
var
  LastFieldNo: Integer;
begin
  if not Assigned(aField) then exit;
  if ManagerSettings.FieldNamePrefix = '' then exit;

  aField.FieldLength := OldLength;
  if aField.FieldType = ftFloat then
    aField.FieldDecimals := OldDecimals;

  LastFieldNo := DataFile.NumDataFields + 1;
  repeat
    aField.FieldName := ManagerSettings.FieldNamePrefix + IntToStr(LastFieldNo);
    inc(LastFieldNo);
  until not DataFile.FieldExists(aField.FieldName);
end;

procedure TFieldCreateForm.ReadField(AField: TEpiField);
begin
  FieldNameEdit.Text := AField.FieldName;
  OldFieldName := FieldNameEdit.Text;
  FieldLengthEdit.Text := IntToStr(AField.FieldLength);
  LabelEdit.Text     := AField.VariableLabel;
  if FieldDecimalSizeEdit.Visible then
    FieldDecimalSizeEdit.Text := IntToStr(AField.FieldDecimals);
end;

procedure TFieldCreateForm.WriteField(AField: TEpiField);
begin
  AField.FieldName             := FieldNameEdit.Text;
  AField.FieldLength           := StrToInt(FieldLengthEdit.Text);
  AField.VariableLabel         := LabelEdit.Text;
  if FieldDecimalSizeEdit.Visible then
    AField.FieldDecimals := StrToInt(FieldDecimalSizeEdit.Text);
end;

initialization
  {$I design_field_frame.lrs}

end.

