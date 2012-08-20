unit design_properties_fieldframe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, StdCtrls, ExtCtrls,
  Buttons, JvDesignSurface, design_types, epidatafilestypes,
  epidatafiles;

type

  { TFieldPropertiesFrame }

  TFieldPropertiesFrame = class(TFrame, IDesignPropertiesFrame)
    AddEditValueLabelBtn: TButton;
    AddJumpBtn: TSpeedButton;
    AsDaysRadio: TRadioButton;
    AsMonthRadio: TRadioButton;
    AsTimeRadio: TRadioButton;
    AsWeeksRadio: TRadioButton;
    AsYearRadio: TRadioButton;
    AutoValuesGrpBox: TGroupBox;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    CalcFieldLabel: TLabel;
    CalcTabSheet: TTabSheet;
    CombineDateGrpBox: TGroupBox;
    CombineDateRadio: TRadioButton;
    CombineStringGrpBox: TGroupBox;
    CombineStringRadio: TRadioButton;
    CompareGroupBox: TGroupBox;
    CompareToCombo: TComboBox;
    CompareTypeCombo: TComboBox;
    ConfirmEntryChkBox: TCheckBox;
    DateResultCombo: TComboBox;
    DayCombo: TComboBox;
    DecimalsEdit: TEdit;
    DecimalsLabel: TLabel;
    DefaultValueEdit: TEdit;
    DefaulValueLabel: TLabel;
    Delim1Edit: TEdit;
    Delim2Edit: TEdit;
    EndDateCombo: TComboBox;
    EndLabel: TLabel;
    EndTimeCombo: TComboBox;
    EntryRadioGroup: TRadioGroup;
    EqLabel: TLabel;
    EqLabelCrdate: TLabel;
    Field1Combo: TComboBox;
    Field2Combo: TComboBox;
    Field3Combo: TComboBox;
    FieldAdvancedSheet: TTabSheet;
    FieldBasicSheet: TTabSheet;
    FieldNameLabel: TLabel;
    FieldPageControl: TPageControl;
    FieldTypeLabel: TLabel;
    ForcePickListChkBox: TCheckBox;
    FromEdit: TEdit;
    GotoFieldLabel: TLabel;
    GotoResetBevel: TBevel;
    JumpGotoBevel: TBevel;
    JumpScrollBox: TScrollBox;
    JumpsGrpBox: TGroupBox;
    JumpSheet: TTabSheet;
    JumpValueLabel: TLabel;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LengthEdit: TEdit;
    LengthLabel: TLabel;
    ManageValueLabelsBtn: TButton;
    MidLabel: TLabel;
    MonthCombo: TComboBox;
    NameEdit: TEdit;
    NoCalcRadio: TRadioButton;
    NotesMemo: TMemo;
    NotesSheet: TTabSheet;
    PlusLabel1: TLabel;
    PlusLabel2: TLabel;
    PlusLabelCrDate: TLabel;
    PlusLabelCrDate2: TLabel;
    QuestionEdit: TEdit;
    QuestionLabel: TLabel;
    OriginalStateRadio: TRadioButton;
    RangesGrpBox: TGroupBox;
    RemoveJumpBtn: TSpeedButton;
    RepeatValueChkBox: TCheckBox;
    ResetAddBevel: TBevel;
    ResetLabel: TLabel;
    ShowValueLabelChkBox: TCheckBox;
    StartDateCombo: TComboBox;
    StartTimeCombo: TComboBox;
    StringResultCombo: TComboBox;
    TimeCalcRadio: TRadioButton;
    TimeDiffGrpBox: TGroupBox;
    TimeResultCombo: TComboBox;
    ToEdit: TEdit;
    TopBevel: TBevel;
    UpdateModeRadioGrp: TRadioGroup;
    ValueLabelComboBox: TComboBox;
    ValueLabelGrpBox: TGroupBox;
    ValueLabelSettingGrpBox: TGroupBox;
    ValueLabelWriteToComboBox: TComboBox;
    ValueLabelWriteToLabel: TLabel;
    YearCombo: TComboBox;
  private
    // Fields Access Functions:
    function ManyFields: boolean;
    function FieldsMustHaveFieldTypes(FieldTypes: TEpiFieldTypes): boolean;
    function FieldsHaveFieldTypes(FieldTypes: TEpiFieldTypes): boolean;
    function FieldsHaveSameFieldType: boolean;

    // Single field access.
    function FieldCount: integer;
    function Field: TEpiField;
    function GetField(const Index: integer): TEpiField;
  protected
    property Fields[const Index: integer]: TEpiField read GetField;
  private
    { Calculation }
    function  CalcFieldComboIsNil(Const Combo: TComboBox): boolean;
  private
    { private declarations }
    FFields: TEpiCustomControlItemArray;
    procedure UpdateVisibility;
    procedure UpdateContent;
    function  ValidateChanges: boolean;
    procedure InternalApplyChanges;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    procedure   SetEpiControls(EpiControls: TEpiCustomControlItemArray);
    function ApplyChanges: boolean;
  end;

implementation

{$R *.lfm}

uses
  epimiscutils, typinfo, epiranges, epiconvertutils;

resourcestring
  rsNotAValidType = 'Not a valid %s: %s';

{ TFieldPropertiesFrame }

function TFieldPropertiesFrame.GetField(const Index: integer): TEpiField;
begin
  result := TEpiField(FFields[Index]);
end;

function TFieldPropertiesFrame.CalcFieldComboIsNil(const Combo: TComboBox
  ): boolean;
begin
  result := Combo.ItemIndex = Combo.Items.IndexOfObject(nil);
end;

function TFieldPropertiesFrame.ManyFields: boolean;
begin
  result := Length(FFields) > 1;
end;

function TFieldPropertiesFrame.FieldsMustHaveFieldTypes(
  FieldTypes: TEpiFieldTypes): boolean;
var
  i: Integer;
begin
  result := false;

  for i := 0 to FieldCount - 1 do
    if not (Fields[i].FieldType in FieldTypes) then exit;

  result := true;
end;

function TFieldPropertiesFrame.FieldsHaveFieldTypes(FieldTypes: TEpiFieldTypes
  ): boolean;
var
  i: Integer;
begin
  result := false;

  for i := 0 to FieldCount - 1 do
    if (Fields[i].FieldType in FieldTypes) then
      Exit(true);
end;

function TFieldPropertiesFrame.FieldsHaveSameFieldType: boolean;
var
  Ft: TEpiFieldType;
  i: Integer;
begin
  result := false;

  Ft := Fields[0].FieldType;
  for i := 1 to FieldCount - 1 do
    if Fields[i].FieldType <> Ft then exit;

  result := true;
end;

function TFieldPropertiesFrame.FieldCount: integer;
begin
  result := Length(FFields);
end;

function TFieldPropertiesFrame.Field: TEpiField;
begin
  result := Fields[0];
end;

procedure TFieldPropertiesFrame.UpdateVisibility;
begin
  // Visiblity
  // - basic
  if ManyFields then
    NameEdit.Enabled              := False
  else
    NameEdit.Enabled              := (not IsReservedEpiFieldName(Field.Name));

  // TODO : Length edit must not be visible if both float and int/string are present.
  LengthEdit.Visible              := FieldsMustHaveFieldTypes(IntFieldTypes + FloatFieldTypes + StringFieldTypes);
  if FieldsHaveFieldTypes(FloatFieldTypes) and FieldsHaveFieldTypes(IntFieldTypes + StringFieldTypes)
  then
    LengthEdit.Visible := false;
  LengthLabel.Visible             := LengthEdit.Visible;

  DecimalsEdit.Visible            := FieldsMustHaveFieldTypes(FloatFieldTypes);
  DecimalsLabel.Visible           := DecimalsEdit.Visible;
  if not DecimalsEdit.Visible then
    Bevel1.Left := QuestionEdit.Left + QuestionEdit.Width
  else
    Bevel1.Left := QuestionEdit.Left + ((QuestionEdit.Width - Bevel1.Width) div 2);
  ValueLabelGrpBox.Visible        := FieldsMustHaveFieldTypes(ValueLabelFieldTypes);
  UpdateModeRadioGrp.Visible      := FieldsMustHaveFieldTypes(AutoUpdateFieldTypes);
  RangesGrpBox.Visible            := FieldsMustHaveFieldTypes(RangeFieldTypes) and FieldsHaveSameFieldType;

  // - extended
  EntryRadioGroup.Visible         := FieldsMustHaveFieldTypes(EntryModeFieldTypes);
  ConfirmEntryChkBox.Visible      := FieldsMustHaveFieldTypes(ConfirmEntryFieldTypes);
  AutoValuesGrpBox.Visible        := FieldsMustHaveFieldTypes(RepeatValueFieldTypes + DefaultValueFieldTypes);
  DefaultValueEdit.Visible        := FieldsHaveSameFieldType;
  DefaulValueLabel.Visible        := DefaultValueEdit.Visible;
  ValueLabelSettingGrpBox.Visible := FieldsMustHaveFieldTypes(ValueLabelFieldTypes);
  CompareGroupBox.Visible         := FieldsMustHaveFieldTypes(CompareFieldTypes);
  FieldAdvancedSheet.TabVisible   := not FieldsMustHaveFieldTypes(AutoFieldTypes);

  // - jumps
  JumpSheet.TabVisible            := FieldsMustHaveFieldTypes(JumpsFieldTypes) and FieldsHaveSameFieldType;

  // - calc
  CalcTabSheet.TabVisible         := (not FieldsMustHaveFieldTypes(AutoFieldTypes));

  // - notes
  NotesSheet.Visible              := FieldsMustHaveFieldTypes(NotesFieldTypes);
end;

procedure TFieldPropertiesFrame.UpdateContent;
var
  F: TEpiField;
  i: Integer;

  function ClearOrLeaveEdit(Edit: TEdit; Const NewText: string): boolean;
  begin
    result := false;
    if NewText <> Edit.Text then
    begin
      Edit.Clear;
      Result := true;
    end;
  end;

  function ClearOrLeaveRadioGrp(RGrp: TRadioGroup; Val: PtrUInt): boolean;
  begin
    result := false;
    if RGrp.ItemIndex <> RGrp.Items.IndexOfObject(TObject(Val)) then
    begin
      RGrp.ItemIndex := -1;
      Result := true;
    end;
  end;

  function ClearOrLeaveChkBox(Box: TCheckBox; Val: boolean): boolean;
  begin
    result := false;
    if (Box.Checked xor Val) then
    begin
      Box.State := cbGrayed;
      Result := true;
    end;
  end;

begin
  // ---------
  // BASIC
  // --------
  // FieldType:
  FieldTypeLabel.Caption := EpiTypeNames[Field.FieldType];

  // NAME
  NameEdit.Text := Field.Name;

  // QUESTON
  QuestionEdit.Text     := Field.Question.Text;
  for i := 1 to FieldCount - 1 do
    if ClearOrLeaveEdit(QuestionEdit, Fields[i].Question.Text)
    then break;

  // LENGTH
  if Field.FieldType = ftFloat then
    LengthEdit.Text     := IntToStr(Field.Length - (Field.Decimals + 1))
  else
    LengthEdit.Text     := IntToStr(Field.Length);
  for i := 1 to FieldCount - 1 do
    if (Fields[i].FieldType = ftFloat) then
    begin
      if ClearOrLeaveEdit(LengthEdit, IntToStr(Fields[i].Length - (Fields[i].Decimals + 1)))
      then break;
    end else begin
      if ClearOrLeaveEdit(LengthEdit, IntToStr(Fields[i].Length))
      then break;
    end;

  // DECIMAL
  DecimalsEdit.Text     := IntToStr(Field.Decimals);
  for i := 1 to FieldCount - 1 do
    if ClearOrLeaveEdit(DecimalsEdit, IntToStr(Fields[i].Decimals))
    then break;

  // VALUELABEL
//  ValueLabelComboBox.ItemIndex := ValueLabelComboBox.Items.IndexOfObject(nil);
//  UpdateValueLabels;

  // RANGE
  if Assigned(Field.Ranges) and (Field.Ranges.Count > 0) then
  begin
    FromEdit.Text := TEpiRange(Field.Ranges[0]).AsString[true];
    ToEdit.Text   := TEpiRange(Field.Ranges[0]).AsString[false];
  end else begin
    FromEdit.Text := '';
    ToEdit.Text   := '';
  end;
  for i := 1 to FieldCount - 1 do
    if Assigned(Field.Ranges) and (Field.Ranges.Count > 0) then
    begin
      if ClearOrLeaveEdit(FromEdit, TEpiRange(Field.Ranges[0]).AsString[true])
      then break;
      if ClearOrLeaveEdit(ToEdit, TEpiRange(Field.Ranges[0]).AsString[false])
      then break;
    end else begin
      // Clear if text already exists, then fields differ anyway.
      FromEdit.Text := '';
      ToEdit.Text   := '';
      break;
    end;

  // UPDATEMODE
  if Field is TEpiCustomAutoField then
    UpdateModeRadioGrp.ItemIndex  := UpdateModeRadioGrp.Items.IndexOfObject(TObject(PtrUInt(TEpiCustomAutoField(Field).AutoMode)));
  for i := 1 to FieldCount - 1 do
    if Fields[i] is TEpiCustomAutoField then
      if ClearOrLeaveRadioGrp(UpdateModeRadioGrp, PtrUInt(TEpiCustomAutoField(Field).AutoMode)) then
      break;


  // ---------
  // Extended
  // --------
  // ENTRYMODE
  EntryRadioGroup.ItemIndex       := EntryRadioGroup.Items.IndexOfObject(TObject(PtrUInt(Field.EntryMode)));
  for i := 1 to FieldCount - 1 do
    if ClearOrLeaveRadioGrp(EntryRadioGroup, PtrUInt(Fields[i].EntryMode))
    then break;

  // CONFIRMENTRY
  ConfirmEntryChkBox.Checked      := Field.ConfirmEntry;
  for i := 1 to FieldCount - 1 do
    if ClearOrLeaveChkBox(ConfirmEntryChkBox, Fields[i].ConfirmEntry)
    then break;

  RepeatValueChkBox.Checked       := Field.RepeatValue;
  for i := 1 to FieldCount - 1 do
    if ClearOrLeaveChkBox(RepeatValueChkBox, Fields[i].RepeatValue)
    then break;

  DefaultValueEdit.Text           := Field.DefaultValueAsString;
  for i := 1 to FieldCount - 1 do
    if ClearOrLeaveEdit(DefaultValueEdit, Fields[i].DefaultValueAsString)
    then break;

  ValueLabelSettingGrpBox.Enabled := Assigned(Field.ValueLabelSet);
  for i := 1 to FieldCount - 1 do
    if Assigned(Fields[i].ValueLabelSet) xor ValueLabelSettingGrpBox.Enabled
    then
      begin
        ValueLabelSettingGrpBox.Enabled := false;
        break;
      end;

  ShowValueLabelChkBox.Checked := Field.ShowValueLabel;
  for i := 1 to FieldCount - 1 do
    if ClearOrLeaveChkBox(ShowValueLabelChkBox, Fields[i].ShowValueLabel)
    then break;

  ForcePickListChkBox.Checked := Field.ForcePickList;
  for i := 1 to FieldCount - 1 do
    if ClearOrLeaveChkBox(ForcePickListChkBox, Fields[i].ForcePickList)
    then break;

//  UpdateValueLabelWriteTo;
//  UpdateComparison;

end;

function TFieldPropertiesFrame.ValidateChanges: boolean;

  function DoError(Const Msg: string; Ctrl: TWinControl): boolean;
  var
    P: TWinControl;
  begin
    P := Ctrl.Parent;
    while not (P is TTabSheet) do
      P := P.Parent;
    TPageControl(P.Parent).ActivePage := TTabSheet(P);
    Ctrl.SetFocus;
//    ShowHintMsg(Msg, Ctrl);
    Result := false;
  end;

var
  I: integer;
  I64: int64;
  F: Extended;
  W1, W2, W3: Word;
  S: string;
begin
  result := false;

  // Safely assume only one field preset if NameEdit.modified.
  if NameEdit.Modified and
     (not Field.ValidateRename(NameEdit.Text, false)) then
  begin
    DoError('Name already exists or invalid identifier', NameEdit);
    Exit;
  end;

  if LengthEdit.Modified then
    if (not TryStrToInt(LengthEdit.Text, I)) or
       (FieldsHaveFieldTypes([ftInteger]) and (I >= 19)) or
       (I <= 0)
    then
    begin
      // TODO : ErrorMessage
      Exit;
    end;

  // Safely assume decimal edit only visible with ftFloat's only.
  if DecimalsEdit.Modified then
    if (not TryStrToInt(DecimalsEdit.Text, I)) or
       (I <= 0) then
    begin
      DoError('Invalid decimals', DecimalsEdit);
      Exit;
    end;

  // Safely assume that From/To Edits are only visible if all fields have same type.
  if ((FromEdit.Text <> '') and (ToEdit.Text = '')) then
  begin
    DoError('No "To" entered', ToEdit);
    Exit;
  end;

  if ((FromEdit.Text = '') and (ToEdit.Text <> '')) then
  begin
    DoError('No "From" entered', FromEdit);
    Exit;
  end;

  if ((FromEdit.Text <> '') and (ToEdit.Text <> '')) then
  begin
    Case Field.FieldType of
      ftInteger:
        begin
          if not TryStrToInt64(FromEdit.Text, I64) then
          begin
            DoError(Format('Not a valid integer: %s', [FromEdit.Text]), FromEdit);
            Exit;
          end;

          if not TryStrToInt64(ToEdit.Text, I64) then
          begin
            DoError(Format('Not a valid integer: %s', [ToEdit.Text]), ToEdit);
            Exit;
          end;
        end;
      ftFloat:
        begin
          if not TryStrToFloat(FromEdit.Text, F) then
          begin
            DoError(Format('Not a valid float: %s', [FromEdit.Text]), FromEdit);
            Exit;
          end;

          if not TryStrToFloat(ToEdit.Text, F) then
          begin
            DoError(Format('Not a valid float: %s', [ToEdit.Text]), ToEdit);
            Exit;
          end;
        end;
      ftTime:
        begin
          if not EpiStrToTime(FromEdit.Text, TimeSeparator, W1, W2, W3, S) then
          begin
            DoError(S, FromEdit);
            Exit;
          end;

          if not EpiStrToTime(ToEdit.Text, TimeSeparator, W1, W2, W3, S) then
          begin
            DoError(S, ToEdit);
            Exit;
          end;
        end;
      ftDMYDate,
      ftMDYDate,
      ftYMDDate:
        begin
          if not EpiStrToDate(FromEdit.Text, DateSeparator, Field.FieldType, W1, W2, W3, S) then
          begin
            DoError(S, FromEdit);
            Exit;
          end;

          if not EpiStrToDate(ToEdit.Text, DateSeparator, Field.FieldType, W1, W2, W3, S) then
          begin
            DoError(S, ToEdit);
            Exit;
          end;
        end;
    end;
  end;

  if DefaultValueEdit.Modified and
     (DefaultValueEdit.Text <> '')
  then
    with DefaultValueEdit do
    begin
      if (Field.FieldType in BoolFieldTypes) and (not TryStrToInt64(Text, I64)) then
        Exit(DoError(Format(rsNotAValidType, ['boolean', Text]), DefaultValueEdit));

      if (Field.FieldType in IntFieldTypes) and (not TryStrToInt64(Text, I64)) then
        Exit(DoError(Format(rsNotAValidType, ['integer', Text]), DefaultValueEdit));

      if (Field.FieldType in FloatFieldTypes) and (not TryStrToFloat(Text, F)) then
        Exit(DoError(Format(rsNotAValidType, ['float', Text]), DefaultValueEdit));

      if (Field.FieldType in DateFieldTypes) and (not EpiStrToDate(Text, DateSeparator, Field.FieldType, W1, W2, W3, S)) then
        Exit(DoError(S, DefaultValueEdit));

      if (Field.FieldType in TimeFieldTypes) and (not EpiStrToTime(Text, TimeSeparator, W1, W2, W3, S)) then
        Exit(DoError(S, DefaultValueEdit));
    end;

  // Jumps
{  for I := 0 to FJumpComponentsList.Count - 1 do
  begin
    with PJumpComponents(FJumpComponentsList[I])^ do
    begin
      if TEdit(ValueEdit).Text = '' then
        Exit(DoError('Empty jump value!', TEdit(ValueEdit)));

      with TEdit(ValueEdit) do
      case Field.FieldType of
        ftBoolean:      if not TryStrToInt64(Text, I64) then Exit(DoError(Format(rsNotAValidType, ['boolean', Text]), TEdit(ValueEdit)));
        ftInteger:      if not TryStrToInt64(Text, I64) then Exit(DoError(Format(rsNotAValidType, ['integer', Text]), TEdit(ValueEdit)));
        ftFloat:        if not TryStrToFloat(Text, F)   then Exit(DoError(Format(rsNotAValidType, ['float', Text]), TEdit(ValueEdit)));
        ftString,
        ftUpperString:  ;
      end;

      if TComboBox(GotoCombo).ItemIndex = -1 then
        Exit(DoError('Invalid "Go To" selection"', TComboBox(GotoCombo)));
    end;
  end;       }

  // Calculate
  if not (
      (NoCalcRadio.Checked) or
      (OriginalStateRadio.Checked)
     )
  then
  begin
    if TimeCalcRadio.Checked then
    begin
      if CalcFieldComboIsNil(TimeResultCombo) then
        Exit(DoError('No result field assigned!', TimeResultCombo));

      if CalcFieldComboIsNil(StartDateCombo) and
         CalcFieldComboIsNil(StartTimeCombo) then
        Exit(DoError('No start date or time assigned!', StartDateCombo));

      if CalcFieldComboIsNil(EndDateCombo) and
         CalcFieldComboIsNil(EndTimeCombo) then
        Exit(DoError('No end date or time assigned!', EndDateCombo));
    end;

    if CombineDateRadio.Checked then
    begin
      if CalcFieldComboIsNil(DateResultCombo) then
        Exit(DoError('No result field assigned!', DateResultCombo));

      if CalcFieldComboIsNil(DayCombo) then
        Exit(DoError('No day field assigned!', DayCombo));

      if CalcFieldComboIsNil(MonthCombo) then
        Exit(DoError('No month field assigned!', MonthCombo));

      if CalcFieldComboIsNil(YearCombo) then
        Exit(DoError('No year field assigned!', YearCombo));
    end;

    if CombineStringRadio.Checked then
    begin
      if CalcFieldComboIsNil(StringResultCombo) then
        Exit(DoError('No result field assigned!', StringResultCombo));

      if CalcFieldComboIsNil(Field1Combo) and
         CalcFieldComboIsNil(Field2Combo) and
         CalcFieldComboIsNil(Field3Combo) then
        Exit(DoError('At least one field must be assigned!', Field1Combo));
    end;
  end;
  result := true;
end;

procedure TFieldPropertiesFrame.InternalApplyChanges;
var
  i: Integer;
  L: Cardinal;
begin
  // ---------
  // BASIC
  // ---------


  // Only one field can ever edit the name!
  if NameEdit.Modified then
    Field.Name := NameEdit.Text;

  if QuestionEdit.Modified then
    for i := 0 to FieldCount - 1 do
      Fields[i].Question.Text := QuestionEdit.Text;

  if LengthEdit.Modified then
    for i := 0 to FieldCount - 1 do
      if Fields[i].FieldType = ftFloat then
        Fields[i].Length := StrToInt(LengthEdit.Text) + TEpiField(FFields[i]).Decimals + 1
      else
        Fields[i].Length := StrToInt(LengthEdit.Text);

  if DecimalsEdit.Modified then
    for i := 0 to FieldCount - 1 do
    begin
      L := Fields[i].Length - Fields[i].Decimals - 1;
      Fields[i].Decimals := StrToInt(DecimalsEdit.Text);
      Fields[i].Length := L + Fields[i].Decimals + 1;
    end;

  // ---------
  // EXTENDED
  // ---------
  for i := 0 to FieldCount - 1 do
    Fields[i].ConfirmEntry := ConfirmEntryChkBox.Checked;
end;

constructor TFieldPropertiesFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  with EntryRadioGroup.Items do
  begin
    BeginUpdate;
    AddObject('Default', TObject(emDefault));
    AddObject('Must Enter', TObject(emMustEnter));
    AddObject('No Enter', TObject(emNoEnter));
    EndUpdate;
  end;

  with UpdateModeRadioGrp.Items do
  begin
    BeginUpdate;
    AddObject('On new record', TObject(umCreated));
    AddObject('On save/update record', TObject(umUpdated));
    EndUpdate;
  end;
end;

procedure TFieldPropertiesFrame.SetEpiControls(
  EpiControls: TEpiCustomControlItemArray);
begin
  FFields := EpiControls;

  if not Assigned(FFields[0]) then exit;

  UpdateVisibility;
  UpdateContent;
end;

function TFieldPropertiesFrame.ApplyChanges: boolean;
begin
  result := ValidateChanges;
  if Result then
    InternalApplyChanges;
end;

end.

