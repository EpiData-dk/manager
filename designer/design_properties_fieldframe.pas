unit design_properties_fieldframe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, StdCtrls, ExtCtrls,
  Buttons, JvDesignSurface, design_types, epidatafilestypes,
  epicustombase, epidatafiles, epivaluelabels, LCLType;

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
    procedure AddEditValueLabelBtnClick(Sender: TObject);
    procedure AddJumpBtnClick(Sender: TObject);
    procedure ManageValueLabelsBtnClick(Sender: TObject);
    procedure RemoveJumpBtnClick(Sender: TObject);
  private
    { Hooks }
    procedure RegisterValueLabelHook;
    procedure UnRegisterValueLabelHook;
    procedure ValueLabelsHook(Sender: TObject; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
  private
    FDataFile: TEpiDataFile;
    FValueLabels: TEpiValueLabelSets;

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
    property DataFile: TEpiDataFile read FDataFile;
    property ValueLabels: TEpiValueLabelSets read FValueLabels;
  private
    { Common combo handling }
    FNoneObject: TObject;
    FIgnoreObject: TObject;
    procedure InitCombo(Combo: TComboBox);
    procedure AddFieldToCombo(AField: TEpiField; FieldTypes: TEpiFieldTypes;
      Combo: TComboBox; AddSelf: boolean = false);
    procedure FinishCombo(Combo: TComboBox; IndexObject: TObject);
  private
    { ValueLabels }
    procedure AddValueLabels;
    procedure UpdateValueLabels;
    procedure UpdateValueLabelWriteTo;
  private
    { Comparison }
    procedure UpdateComparison;
  private
    { Jumps }
    FJumpComponentsList: TList;
    procedure UpdateJumps;
    function  DoAddNewJump: pointer;
    procedure AddFieldsToCombo(Combo: TComboBox);
  private
    { Calculation }
    procedure UpdateCalcFields;
    function  CalcFieldComboIsNil(Const Combo: TComboBox): boolean;
  private
    { private declarations }
    FFields: TEpiCustomControlItemArray;
    procedure UpdateVisibility;
    procedure UpdateContent;
    function  ValidateChanges: boolean;
    procedure InternalApplyChanges;
    procedure EditUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure   SetEpiControls(EpiControls: TEpiCustomControlItemArray);
    function    ApplyChanges: boolean;
  end;

implementation

{$R *.lfm}

uses
  epimiscutils, typinfo, epiranges, epiconvertutils,
  epistringutils, LazUTF8, field_valuelabelseditor_form,
  valuelabelseditor_form2;

resourcestring
  rsNotAValidType = 'Not a valid %s: %s';

type
  TJumpComponents = record
    ValueEdit: PtrUInt;
    GotoCombo: PtrUInt;
    ResetCombo: PtrUInt;
  end;
  PJumpComponents = ^TJumpComponents;

{ TFieldPropertiesFrame }

function TFieldPropertiesFrame.GetField(const Index: integer): TEpiField;
begin
  result := TEpiField(FFields[Index]);
end;

procedure TFieldPropertiesFrame.InitCombo(Combo: TComboBox);
begin
  Combo.Items.BeginUpdate;
  Combo.Clear;
  Combo.Items.AddObject('(none)', FNoneObject);
  if ManyFields then
    Combo.Items.AddObject('(ignore)', FIgnoreObject);
end;

procedure TFieldPropertiesFrame.AddFieldToCombo(AField: TEpiField;
  FieldTypes: TEpiFieldTypes; Combo: TComboBox; AddSelf: boolean);
begin
  if (not AddSelf) and (AField = Field) then exit;
  if (not (AField.FieldType in FieldTypes)) then exit;

  // Else...
  Combo.Items.AddObject(
    AField.Name + BoolToStr(AField.Question.Text <> '', ': ' + EpiCutString(AField.Question.Text, 40 - UTF8Length(AField.Name)), ''),
    AField);
end;

procedure TFieldPropertiesFrame.FinishCombo(Combo: TComboBox;
  IndexObject: TObject);
begin
  Combo.Items.EndUpdate;
  Combo.ItemIndex := Combo.Items.IndexOfObject(IndexObject);
end;

procedure TFieldPropertiesFrame.AddValueLabels;
var
  VL: TEpiValueLabelSet;
  i: Integer;
begin
  if not ValueLabelComboBox.Visible then exit;

  InitCombo(ValueLabelComboBox);

  for i := 0 to ValueLabels.Count - 1 do
  begin
    VL := ValueLabels[i];

    // Only support same types.
    if VL.LabelType <> Field.FieldType then continue;
    ValueLabelComboBox.Items.AddObject(VL.Name, VL);
  end;
  FinishCombo(ValueLabelComboBox, FNoneObject);
end;

procedure TFieldPropertiesFrame.UpdateValueLabels;
var
  VL: TEpiValueLabelSet;
  i: Integer;
begin
  AddValueLabels;

  VL := Field.ValueLabelSet;
  for i := 1 to FieldCount - 1 do
    if Fields[i].ValueLabelSet <> VL then
      VL := TEpiValueLabelSet(FIgnoreObject);
  ValueLabelComboBox.ItemIndex := ValueLabelComboBox.Items.IndexOfObject(VL);
end;

procedure TFieldPropertiesFrame.UpdateValueLabelWriteTo;
var
  i: Integer;
  F: TEpiField;
begin
  InitCombo(ValueLabelWriteToComboBox);
  for i := 0 to DataFile.Fields.Count - 1 do
    AddFieldToCombo(DataFile.Field[i], StringFieldTypes, ValueLabelWriteToComboBox);
  FinishCombo(ValueLabelWriteToComboBox, FNoneObject);

  F := Field.ValueLabelWriteField;
  for i := 1 to FieldCount - 1 do
    if Fields[i].ValueLabelWriteField <> F then
      F := TEpiField(FIgnoreObject);
  ValueLabelWriteToComboBox.ItemIndex := ValueLabelWriteToComboBox.Items.IndexOfObject(F);
end;

procedure TFieldPropertiesFrame.UpdateComparison;
var
  i: Integer;
  F: TEpiField;
begin
  InitCombo(CompareToCombo);
  for i := 0 to DataFile.Fields.Count -1 do
    AddFieldToCombo(DataFile.Field[i], NativeFieldTypeSetFromFieldType(Field.FieldType) - AutoFieldTypes, CompareToCombo);
  FinishCombo(CompareToCombo, FNoneObject);

  F := TEpiField(FNoneObject);
  if Assigned(Field.Comparison) then
    F := Field.Comparison.CompareField;

  for i := 1 to FieldCount - 1 do
  begin
    if (Assigned(Fields[i].Comparison) and
       (not Assigned(F)))
       or
       ((not Assigned(Fields[i].Comparison)) and
        (Assigned(F)))
       or
       ((Assigned(Fields[i].Comparison)) and
        (Fields[i].Comparison.CompareField <> F))
    then
    begin
      F := TEpiField(FIgnoreObject);
      Break;
    end;
  end;
  CompareToCombo.ItemIndex := CompareToCombo.Items.IndexOfObject(F);
  if Assigned(F) and (F <> FIgnoreObject) then
    CompareTypeCombo.ItemIndex := Integer(Field.Comparison.CompareType)
  else
    CompareTypeCombo.ItemIndex := -1;
end;

procedure TFieldPropertiesFrame.UpdateJumps;
var
  F: TEpiField;
  Jmp: TEpiJumps;
  i: Integer;
  j: Integer;
begin
  // Clear all previous visual controls
  AddJumpBtn.AnchorToNeighbour(akBottom, 3, TopBevel);
  RemoveJumpBtn.Enabled := Assigned(Field.Jumps);
  while FJumpComponentsList.Count > 0 do
  with PJumpComponents(FJumpComponentsList.Last)^ do
  begin
    TObject(ValueEdit).Free;
    TObject(GotoCombo).Free;
    TObject(ResetCombo).Free;
    FJumpComponentsList.Delete(FJumpComponentsList.Count-1);
  end;

  Jmp := Field.Jumps;
  for i := 1 to FieldCount - 1 do
  begin
    F := Fields[i];

    if (Assigned(Jmp) and (not Assigned(F.Jumps)))
       or
       ((not Assigned(Jmp)) and Assigned(F.Jumps))
    then
      // TODO : Set a check in Original state
      Exit;

    if (Assigned(Jmp) and Assigned(F.Jumps)) then
    begin
      if (Jmp.Count <> F.Jumps.Count) then
        // TODO : Set a check in Original state
        Exit;

      for j := 0 to Jmp.Count - 1 do
      begin
        if (Jmp[j].JumpType <> F.Jumps[j].JumpType)
           or
           (Jmp[j].JumpValueAsString <> F.Jumps[j].JumpValueAsString)
           or
           (Jmp[j].ResetType <> F.Jumps[j].ResetType)
           or
           (Jmp[j].JumpToField <> F.Jumps[j].JumpToField)
        then
          // TODO : Set a check in Original state
          Exit;
      end;
    end;
  end;

  if not Assigned(Jmp) then exit;

  for i := 0 to Field.Jumps.Count -1 do
    with PJumpComponents(DoAddNewJump)^ do
      with Field.Jumps[i] do
      begin
        TEdit(ValueEdit).Text            := JumpValueAsString;
        if JumpType = jtToField then
          TComboBox(GotoCombo).ItemIndex := TComboBox(GotoCombo).Items.IndexOfObject(JumpToField)
        else
          TComboBox(GotoCombo).ItemIndex := TComboBox(GotoCombo).Items.IndexOfObject(TObject(PtrInt(JumpType)));
        TComboBox(ResetCombo).ItemIndex  := TComboBox(ResetCombo).Items.IndexOfObject(TObject(PtrInt(ResetType)));
      end;
end;

function TFieldPropertiesFrame.DoAddNewJump: pointer;
var
  JVE: TEdit;     // Jump-to value edit.
  GFC: TComboBox; // Goto field combo
  RVC: TComboBox; // Reset value combo
  JRec: PJumpComponents;
begin
  JVE := TEdit.Create(JumpScrollBox);
  GFC := TComboBox.Create(JumpScrollBox);
  RVC := TComboBox.Create(JumpScrollBox);

  with GFC do
  begin
    if FJumpComponentsList.Count = 0 then
      AnchorToNeighbour(akTop, 3, TopBevel)
    else
      AnchorToNeighbour(akTop, 3, TControl(PJumpComponents(FJumpComponentsList[FJumpComponentsList.Count-1])^.GotoCombo));
    AnchorToNeighbour(akLeft, 5, JumpGotoBevel);
    AnchorToNeighbour(akRight, 5, GotoResetBevel);
    AddFieldsToCombo(GFC);
    Style := csDropDownList;
    Parent := JumpScrollBox;
  end;

  with JVE do
  begin
    AnchorParallel(akLeft, 10, JumpScrollBox);
    AnchorToNeighbour(akRight, 5, JumpGotoBevel);
    AnchorVerticalCenterTo(GFC);
    OnUTF8KeyPress := @EditUTF8KeyPress;
    Parent := JumpScrollBox;
  end;

  with RVC do
  begin
    AnchorToNeighbour(akLeft, 5, GotoResetBevel);
    AnchorToNeighbour(akRight, 5, ResetAddBevel);
    AnchorVerticalCenterTo(GFC);
    Style := csDropDownList;
    with Items do
    begin
      AddObject('Leave as is', TObject(jrLeaveAsIs));
      AddObject('System missing (.)', TObject(jrSystemMissing));
      AddObject('Max defined missingvalue', TObject(jrMaxMissing));
      AddObject('Second max defined missing value', TObject(jr2ndMissing));
    end;
    ItemIndex := 0;
    Parent := JumpScrollBox;
  end;

  AddJumpBtn.AnchorVerticalCenterTo(GFC);
  RemoveJumpBtn.Enabled := true;

  JVE.Tag      := FJumpComponentsList.Count;
  JVE.TabOrder := (FJumpComponentsList.Count * 3);
  GFC.TabOrder := (FJumpComponentsList.Count * 3) + 1;
  RVC.TabOrder := (FJumpComponentsList.Count * 3) + 2;

  Jrec := New(PJumpComponents);
  with Jrec^ do
  begin
    ValueEdit := PtrUInt(JVE);
    GotoCombo := PtrUInt(GFC);
    ResetCombo := PtrUInt(RVC);
  end;
  FJumpComponentsList.Add(JRec);
  Result := JRec;
end;

procedure TFieldPropertiesFrame.AddFieldsToCombo(Combo: TComboBox);
var
  i: Integer;
begin
  Combo.Items.BeginUpdate;
  Combo.Clear;
  Combo.Items.AddObject('(Skip Next Field)', TObject(jtSkipNextField));
  Combo.Items.AddObject('(Exit Section)', TObject(jtExitSection));
  Combo.Items.AddObject('(Save Record)', TObject(jtSaveRecord));
  for i := 0 to FDataFile.Fields.Count - 1 do
    AddFieldToCombo(FDataFile.Field[i], AllFieldTypes - AutoFieldTypes, Combo);
  Combo.ItemIndex := 0;
  Combo.Items.EndUpdate;
end;

procedure TFieldPropertiesFrame.UpdateCalcFields;
var
  i: Integer;
  F: TEpiField;
  C: TEpiCalculation;

  procedure UpdateFieldCombo(Combo: TComboBox; AField: TEpiField);
  var
    CurrIdx: Integer;
    NoneIdx: Integer;
    IgnIdx: Integer;
    NewIdx: Integer;
  begin
    CurrIdx := Combo.ItemIndex;
    NoneIdx := Combo.Items.IndexOfObject(FNoneObject);
    IgnIdx := Combo.Items.IndexOfObject(FIgnoreObject);
    NewIdx  := Combo.Items.IndexOfObject(AField);

    if (CurrIdx <> NoneIdx) and
       (CurrIdx <> IgnIdx) and
       (CurrIdx <> NewIdx)
    then
      Combo.ItemIndex := IgnIdx;

    if (CurrIdx = NoneIdx) then
      Combo.ItemIndex := NewIdx;
  end;

  procedure UpdateTimeCalc(Calculation: TEpiTimeCalc);
  begin
    UpdateFieldCombo(TimeResultCombo, Calculation.ResultField);
    UpdateFieldCombo(StartDateCombo,  Calculation.StartDate);
    UpdateFieldCombo(EndDateCombo,    Calculation.EndDate);
    UpdateFieldCombo(StartTimeCombo,  Calculation.StartTime);
    UpdateFieldCombo(EndTimeCombo,    Calculation.EndTime);
    Case Calculation.TimeCalcType of
      ctAsYear:        AsYearRadio.Checked := true;
      ctAsMonths:      AsMonthRadio.Checked := true;
      ctAsWeeks:       AsWeeksRadio.Checked := true;
      ctAsDays:        AsDaysRadio.Checked := true;
      ctAsDayFraction: AsTimeRadio.Checked := true;
    end;
    TimeCalcRadio.Checked := true;
  end;
  procedure UpdateDateCalc(Calculation: TEpiCombineDateCalc);
  begin
    UpdateFieldCombo(DateResultCombo, Calculation.ResultField);
    UpdateFieldCombo(DayCombo,        Calculation.Day);
    UpdateFieldCombo(MonthCombo,      Calculation.Month);
    UpdateFieldCombo(YearCombo,       Calculation.Year);
    CombineDateRadio.Checked := true;
  end;
  procedure UpdateStringCalc(Calculation: TEpiCombineStringCalc);
  begin
    UpdateFieldCombo(StringResultCombo, Calculation.ResultField);
    UpdateFieldCombo(Field1Combo,       Calculation.Field1);
    UpdateFieldCombo(Field2Combo,       Calculation.Field2);
    UpdateFieldCombo(Field3Combo,       Calculation.Field3);
    Delim1Edit.Text := Calculation.Delim1;
    Delim2Edit.Text := Calculation.Delim2;
    CombineStringRadio.Checked := true;
  end;

begin
  InitCombo(TimeResultCombo);
  InitCombo(StartDateCombo);
  InitCombo(EndDateCombo);
  InitCombo(StartTimeCombo);
  InitCombo(EndTimeCombo);

  InitCombo(DateResultCombo);
  InitCombo(DayCombo);
  InitCombo(MonthCombo);
  InitCombo(YearCombo);

  InitCombo(StringResultCombo);
  InitCombo(Field1Combo);
  InitCombo(Field2Combo);
  InitCombo(Field3Combo);

  for i := 0 to DataFile.Fields.Count -1 do
  begin
    F := DataFile.Field[i];

    // Time difference:
    // - active field cannot also be result field.
    AddFieldToCombo(F, [ftInteger, ftFloat], TimeResultCombo);
    AddFieldToCombo(F, DateFieldTypes, StartDateCombo, true);
    AddFieldToCombo(F, DateFieldTypes, EndDateCombo, true);
    AddFieldToCombo(F, TimeFieldTypes, StartTimeCombo, true);
    AddFieldToCombo(F, TimeFieldTypes, EndTimeCombo, true);

    // Create Date:
    // - active field cannot also be result field.
    AddFieldToCombo(F, DateFieldTypes-AutoFieldTypes, DateResultCombo);
    AddFieldToCombo(F, [ftInteger], DayCombo, true);
    AddFieldToCombo(F, [ftInteger], MonthCombo, true);
    AddFieldToCombo(F, [ftInteger], YearCombo, true);

    // Combine String:
    // - active field cannot also be result field.
    AddFieldToCombo(F, StringFieldTypes, StringResultCombo);
    AddFieldToCombo(F, AllFieldTypes, Field1Combo, true);
    AddFieldToCombo(F, AllFieldTypes, Field2Combo, true);
    AddFieldToCombo(F, AllFieldTypes, Field3Combo, true);
  end;

  FinishCombo(TimeResultCombo, FNoneObject);
  FinishCombo(StartDateCombo, FNoneObject);
  FinishCombo(EndDateCombo, FNoneObject);
  FinishCombo(StartTimeCombo, FNoneObject);
  FinishCombo(EndTimeCombo, FNoneObject);

  FinishCombo(DateResultCombo, FNoneObject);
  FinishCombo(DayCombo, FNoneObject);
  FinishCombo(MonthCombo, FNoneObject);
  FinishCombo(YearCombo, FNoneObject);

  FinishCombo(StringResultCombo, FNoneObject);
  FinishCombo(Field1Combo, FNoneObject);
  FinishCombo(Field2Combo, FNoneObject);
  FinishCombo(Field3Combo, FNoneObject);

  C := Field.Calculation;
  NoCalcRadio.Checked := true;

  // First check "equalness"
  for i := 1 to FieldCount -1 do
  begin
    if (Assigned(C) and (not Assigned(Fields[i].Calculation)))
       or
       ((not Assigned(C)) and Assigned(Fields[i].Calculation))
       or
       (Assigned(C) and Assigned(Fields[i].Calculation) and
        (C.ClassType <> Fields[i].Calculation.ClassType))
    then
    begin
      OriginalStateRadio.Checked := true;
      Break;
    end;
  end;

  // Then fill content!
  if (not OriginalStateRadio.Checked) and
     (Assigned(C))
  then
    for i := 0 to FieldCount -1 do
    begin
      case C.CalcType of
        ctTimeDiff:
          UpdateTimeCalc(TEpiTimeCalc(Fields[i].Calculation));
        ctCombineDate:
          UpdateDateCalc(TEpiCombineDateCalc(Fields[i].Calculation));
        ctCombineString:
          UpdateStringCalc(TEpiCombineStringCalc(Fields[i].Calculation));
      end;
    end;
end;

function TFieldPropertiesFrame.CalcFieldComboIsNil(const Combo: TComboBox
  ): boolean;
begin
  result := Combo.ItemIndex = Combo.Items.IndexOfObject(nil);
end;

procedure TFieldPropertiesFrame.AddJumpBtnClick(Sender: TObject);
begin
  TEdit(PJumpComponents(DoAddNewJump)^.ValueEdit).SetFocus;
end;

procedure TFieldPropertiesFrame.ManageValueLabelsBtnClick(Sender: TObject);
begin
  ShowValueLabelEditor2(ValueLabels);
end;

procedure TFieldPropertiesFrame.AddEditValueLabelBtnClick(Sender: TObject);
var
  VLEdit: TFieldValueLabelEditor;
  NewVL: Boolean;
  VLSet: TEpiValueLabelSet;

  function HasSelectedValueLabel(out ValueLabelSet: TEpiValueLabelSet): boolean;
  begin
    ValueLabelSet := TEpiValueLabelSet(ValueLabelComboBox.Items.Objects[ValueLabelComboBox.ItemIndex]);
    result := ValueLabelSet <> FNoneObject;
  end;

begin
  NewVL := false;

  VLEdit := TFieldValueLabelEditor.Create(Self, ValueLabels);
  if HasSelectedValueLabel(VLSet) then
  begin
    VLEdit.ValueLabelSet := VLSet
  end else begin
    VLEdit.ValueLabelSet := ValueLabels.NewValueLabelSet(Field.FieldType);
    NewVL := true;
  end;

  if VLEdit.ShowModal = mrOK then
  begin
//    UpdateValueLabels;
    ValueLabelComboBox.ItemIndex := ValueLabelComboBox.Items.IndexOfObject(VLEdit.ValueLabelSet);
  end else begin
    if NewVL then
      VLEdit.ValueLabelSet.Free;
  end;

  VLEDit.Free;
end;

procedure TFieldPropertiesFrame.RemoveJumpBtnClick(Sender: TObject);
begin
  with PJumpComponents(FJumpComponentsList.Last)^ do
  begin
    TObject(ValueEdit).Free;
    TObject(GotoCombo).Free;
    TObject(ResetCombo).Free;
    FJumpComponentsList.Delete(FJumpComponentsList.Count - 1);
  end;
  if FJumpComponentsList.Count = 0  then
  begin
    AddJumpBtn.AnchorToNeighbour(akBottom, 3, TopBevel);
    RemoveJumpBtn.Enabled := false;
  end else
    AddJumpBtn.AnchorVerticalCenterTo(TControl(PJumpComponents(FJumpComponentsList.Last)^.GotoCombo));
end;

procedure TFieldPropertiesFrame.RegisterValueLabelHook;
var
  i: Integer;
begin
  if not Assigned(ValueLabels) then exit;

  ValueLabels.RegisterOnChangeHook(@ValueLabelsHook, true);
  for i := 0 to ValueLabels.Count -1 do
    ValueLabels[i].RegisterOnChangeHook(@ValueLabelsHook, true);
end;

procedure TFieldPropertiesFrame.UnRegisterValueLabelHook;
var
  i: Integer;
begin
  if not Assigned(ValueLabels) then exit;

  for i := 0 to ValueLabels.Count -1 do
    ValueLabels[i].UnRegisterOnChangeHook(@ValueLabelsHook);
  ValueLabels.UnRegisterOnChangeHook(@ValueLabelsHook);
end;

procedure TFieldPropertiesFrame.ValueLabelsHook(Sender: TObject;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
var
  S: String;
  VL: TEpiValueLabelSet;
  Idx: Integer;
begin
  if EventGroup <> eegCustomBase then exit;

  if (Sender is TEpiValueLabelSets) then
  case TEpiCustomChangeEventType(EventType) of
    ecceDestroy: ;
    ecceUpdate: ;
    ecceName: ;
    ecceAddItem:
      begin
        VL := TEpiValueLabelSet(Data);
        VL.RegisterOnChangeHook(@ValueLabelsHook, true);

        if VL.LabelType = Field.FieldType then
          ValueLabelComboBox.Items.AddObject(VL.Name, VL);
      end;
    ecceDelItem:
      begin
        VL := TEpiValueLabelSet(Data);
        VL.UnRegisterOnChangeHook(@ValueLabelsHook);

        ValueLabelComboBox.Items.BeginUpdate;
        Idx := ValueLabelComboBox.Items.IndexOfObject(VL);
        if Idx > -1 then
        begin
          if Idx = ValueLabelComboBox.ItemIndex then
          ValueLabelComboBox.ItemIndex := ValueLabelComboBox.Items.IndexOfObject(FNoneObject);
          ValueLabelComboBox.Items.Delete(Idx);

{          ShowHintMsg(
            Format('Warning: Valuelabels changed for field "%s"', [TEpiField(EpiControl).Name]),
            GetValueLabelsEditor(TEpiDocument(FValueLabelSets.RootOwner)).ToolBar1);  }
        end;
        ValueLabelComboBox.Items.EndUpdate;
      end;
    ecceSetItem: ;
    ecceSetTop: ;
    ecceSetLeft: ;
    ecceText: ;
  end;

  if (Sender is TEpiValueLabelSet) then
    case TEpiCustomChangeEventType(EventType) of
      ecceDestroy: ;
      ecceUpdate: ;
      ecceName:
        begin
          S := string(data^);
          Idx := ValueLabelComboBox.Items.IndexOf(S);
          ValueLabelComboBox.Items.Strings[Idx] := TEpiValueLabelSet(Sender).Name;
        end;
      ecceAddItem: ;
      ecceDelItem: ;
      ecceSetItem: ;
      ecceSetTop: ;
      ecceSetLeft: ;
      ecceText: ;
    end;
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
  ValueLabelGrpBox.Visible        := FieldsMustHaveFieldTypes(ValueLabelFieldTypes) and FieldsHaveSameFieldType;
  UpdateModeRadioGrp.Visible      := FieldsMustHaveFieldTypes(AutoUpdateFieldTypes);
  RangesGrpBox.Visible            := FieldsMustHaveFieldTypes(RangeFieldTypes) and FieldsHaveSameFieldType;

  // - extended
  EntryRadioGroup.Visible         := FieldsMustHaveFieldTypes(EntryModeFieldTypes);
  ConfirmEntryChkBox.Visible      := FieldsMustHaveFieldTypes(ConfirmEntryFieldTypes);
  AutoValuesGrpBox.Visible        := FieldsMustHaveFieldTypes(RepeatValueFieldTypes + DefaultValueFieldTypes);
  DefaultValueEdit.Visible        := FieldsHaveSameFieldType;
  DefaulValueLabel.Visible        := DefaultValueEdit.Visible;
  ValueLabelSettingGrpBox.Visible := FieldsMustHaveFieldTypes(ValueLabelFieldTypes) and FieldsHaveSameFieldType;
  CompareGroupBox.Visible         := FieldsMustHaveFieldTypes(CompareFieldTypes);
  FieldAdvancedSheet.TabVisible   := not FieldsMustHaveFieldTypes(AutoFieldTypes);

  // - jumps
  JumpSheet.TabVisible            := FieldsMustHaveFieldTypes(JumpsFieldTypes) and FieldsHaveSameFieldType;

  // - calc
  OriginalStateRadio.Visible      := ManyFields;
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
  UpdateValueLabels;

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

  UpdateValueLabelWriteTo;

  UpdateComparison;

  // ---------
  // Jumps
  // --------
  UpdateJumps;

  // ---------
  // Calculations
  // --------
  UpdateCalcFields;
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

procedure TFieldPropertiesFrame.EditUTF8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
var
  I: integer;
  Ch: LongWord;
begin
  Ch := UTF8CharacterToUnicode(@UTF8Key[1], I);
  if (not (Field.FieldType in StringFieldTypes)) and
     (not (Char(Ch) in [VK_0..VK_9, VK_RETURN, Char(VK_BACK)] + ['.',','] + ['-', ':', '.'] + ['/', '-', '\', '.'])) then
    UTF8Key := '';
  case Field.FieldType of
    ftFloat:   if (Char(Ch) in ['.',',']) then UTF8Key := DecimalSeparator;
    ftTime:    if (Char(Ch) in ['-', ':', '.']) then UTF8Key := TimeSeparator;
    ftDMYDate,
    ftMDYDate,
    ftYMDDate: if (Char(Ch) in ['/', '-', '\', '.']) then UTF8Key := DateSeparator;
  end;
end;

constructor TFieldPropertiesFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FieldPageControl.ActivePage := FieldBasicSheet;

  FNoneObject   := nil; //TObject.Create;
  FIgnoreObject := TObject.Create;
  FJumpComponentsList := TList.Create;

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

  FromEdit.OnUTF8KeyPress         := @EditUTF8KeyPress;
  ToEdit.OnUTF8KeyPress           := @EditUTF8KeyPress;
  DefaultValueEdit.OnUTF8KeyPress := @EditUTF8KeyPress;
end;

destructor TFieldPropertiesFrame.Destroy;
begin
  UnRegisterValueLabelHook;
  inherited Destroy;
end;

procedure TFieldPropertiesFrame.SetEpiControls(
  EpiControls: TEpiCustomControlItemArray);
begin
  UnRegisterValueLabelHook;

  FFields := EpiControls;
  FDataFile := Field.DataFile;
  FValueLabels := FDataFile.ValueLabels;

  if not Assigned(FFields[0]) then exit;

  RegisterValueLabelHook;

  UpdateVisibility;
  UpdateContent;
end;

function TFieldPropertiesFrame.ApplyChanges: boolean;
begin
{  result := ValidateChanges;
  if Result then
    InternalApplyChanges;}
  Result := true;
end;

end.

