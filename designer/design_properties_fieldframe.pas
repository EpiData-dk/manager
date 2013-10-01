unit design_properties_fieldframe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, StdCtrls, ExtCtrls,
  Buttons, JvDesignSurface, design_types, epidatafilestypes,
  epicustombase, epidatafiles, epivaluelabels, LCLType, ActnList,
  design_properties_baseframe;

type

  { TFieldPropertiesFrame }

  TFieldPropertiesFrame = class(TDesignPropertiesFrame, IDesignPropertiesFrame)
    AddValueLabelPlusAction: TAction;
    ActionList1: TActionList;
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
    ValueLabelAsNoteChkBox: TCheckBox;
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
    CalcUnchangedRadioBtn: TRadioButton;
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
    UseJumpsCombo: TComboBox;
    UseJumpsLabel: TLabel;
    ValueLabelComboBox: TComboBox;
    ValueLabelGrpBox: TGroupBox;
    ValueLabelSettingGrpBox: TGroupBox;
    ValueLabelWriteToComboBox: TComboBox;
    ValueLabelWriteToLabel: TLabel;
    YearCombo: TComboBox;
    procedure AddEditValueLabelBtnClick(Sender: TObject);
    procedure AddJumpBtnClick(Sender: TObject);
    procedure AddValueLabelPlusActionExecute(Sender: TObject);
    procedure CalcRadioChange(Sender: TObject);
    procedure LengthEditingDone(Sender: TObject);
    procedure RemoveJumpBtnClick(Sender: TObject);
    procedure UseJumpsComboSelect(Sender: TObject);
    procedure ValueLabelComboBoxChange(Sender: TObject);
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
    function ComboIgnoreSelected(Combo: TComboBox): boolean;
    function ComboNoneSelected(Const Combo: TComboBox): boolean;
    function ComboSelectedObject(Combo: TComboBox): TObject;
    // Other common
    function SelectedEnum(ItemObject: TObject): PtrInt;
  private
    { ValueLabels }
    FVLIncompatibleItemIndex: integer;
    procedure AddValueLabels;
    procedure UpdateValueLabels;
    procedure UpdateValueLabelWriteTo;
  private
    { Comparison }
    procedure UpdateComparison;
  private
    { Jumps }
    FJumpComponentsList: TList;
    procedure ClearJumps;
    procedure UpdateJumps; overload;
    procedure UpdateJumps(Jumps: TEpiJumps); overload;
    function  DoAddNewJump: pointer;
    procedure AddFieldsToCombo(Combo: TComboBox);

  private
    { Calculation }
    procedure UpdateCalcFields;
  private
    { private declarations }
    FFields: TEpiCustomControlItemArray;
    function DoError(Const Msg: string; Ctrl: TWinControl): boolean;
    procedure DoWarning(Const Msg: string; Ctrl: TWinControl);
    procedure UpdateVisibility;
    procedure UpdateContent;
    procedure DoUpdateCaption;
    function  ValidateChanges: boolean;
    procedure InternalApplyChanges;
    procedure EditUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure JumpEditUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure FocusOnNewControl;
    procedure ResetControls;
    procedure SetEpiControls(EpiControls: TEpiCustomControlItemArray);
    function ApplyChanges: boolean;
  end;

implementation

{$R *.lfm}

uses
  epimiscutils, typinfo, epiranges, epiconvertutils,
  epistringutils, LazUTF8, field_valuelabelseditor_form,
  valuelabelseditor_form2, math;

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
    Combo.Items.AddObject('(leave as is)', FIgnoreObject);
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
  if Combo.ItemIndex = -1 then
    Combo.ItemIndex := Combo.Items.IndexOfObject(FNoneObject);
end;

function TFieldPropertiesFrame.ComboIgnoreSelected(Combo: TComboBox): boolean;
begin
  result := (Combo.ItemIndex = Combo.Items.IndexOfObject(FIgnoreObject));
end;

function TFieldPropertiesFrame.ComboNoneSelected(const Combo: TComboBox
  ): boolean;
begin
  result := (Combo.ItemIndex = Combo.Items.IndexOfObject(FNoneObject));
end;

function TFieldPropertiesFrame.ComboSelectedObject(Combo: TComboBox): TObject;
begin
  Result := nil;
  if Combo.ItemIndex > -1 then
    result := Combo.Items.Objects[Combo.ItemIndex];
end;

function TFieldPropertiesFrame.SelectedEnum(ItemObject: TObject): PtrInt;
var
  Items: TStrings;
  Idx: Int64;
begin
  Items := TStrings(GetObjectProp(ItemObject, 'Items', TStrings));
  if not Assigned(Items) then exit;

  Idx := GetOrdProp(ItemObject, 'ItemIndex');
  if Idx > -1 then
    result := PtrInt(Items.Objects[Idx]);
end;

procedure TFieldPropertiesFrame.AddValueLabels;
var
  VL: TEpiValueLabelSet;
  i: Integer;
  DoAdd: Boolean;
  IntL: Integer;
  DecL: Integer;
  S: String;
  l: SizeInt;
  j: Integer;
  IncompatibleList: TList;
begin
  if not ValueLabelComboBox.Visible then exit;

  IncompatibleList := TList.Create;

  InitCombo(ValueLabelComboBox);

  for i := 0 to ValueLabels.Count - 1 do
  begin
    VL := ValueLabels[i];

    case VL.LabelType of
      ftInteger:
        begin
          DoAdd := Field.FieldType in [ftInteger, ftFloat];
          if Field.FieldType = ftFloat then
            DoAdd := DoAdd and (VL.MaxValueLength <= StrToIntDef(LengthEdit.Text, (Field.Length - Field.Decimals - 1)))
          else
            DoAdd := DoAdd and (VL.MaxValueLength <= StrToIntDef(LengthEdit.Text, Field.Length));
        end;
      ftFloat:
        begin
          DoAdd := Field.FieldType = ftFloat;

          if DoAdd then
          begin
            IntL := 0;
            DecL := 0;
            for j := 0 to VL.Count - 1 do
            begin
              S := VL[j].ValueAsString;
              l := Pos(DecimalSeparator, S);
              if l = 0 then
                IntL := Length(S)
              else
                IntL := Max(IntL, l - 1);
              DecL := Max(DecL, Length(S) - (IntL+1));
            end;
          end;
          DoAdd := DoAdd and
            (IntL <= StrToIntDef(LengthEdit.Text, Field.Length - Field.Decimals - 1)) and
            (DecL <= StrToIntDef(DecimalsEdit.Text, Field.Decimals));
        end;
      ftString:  DoAdd := Field.FieldType in [ftString, ftUpperString];
    end;

    if DoAdd then
      ValueLabelComboBox.AddItem(VL.Name, VL)
    else
      IncompatibleList.Add(VL);
  end;

  FVLIncompatibleItemIndex := -1;
  if IncompatibleList.Count > 0 then
  begin
    FVLIncompatibleItemIndex := ValueLabelComboBox.Items.Count;
    ValueLabelComboBox.AddItem('--- incompatible value label set ---', FIgnoreObject);

    for i := 0 to IncompatibleList.Count - 1 do
    begin
      VL := TEpiValueLabelSet(IncompatibleList[i]);
      ValueLabelComboBox.AddItem(VL.Name, VL)
    end;
  end;

  FinishCombo(ValueLabelComboBox, FNoneObject);
  IncompatibleList.Free;
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
    begin
      VL := TEpiValueLabelSet(FIgnoreObject);
      Break;
    end;
  ValueLabelComboBox.ItemIndex := ValueLabelComboBox.Items.IndexOfObject(VL);
  ValueLabelComboBoxChange(ValueLabelComboBox);
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
    AddFieldToCombo(DataFile.Field[i], NativeFieldTypeSetFromFieldType(Field.FieldType), CompareToCombo);
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

procedure TFieldPropertiesFrame.ClearJumps;
begin
  // Clear all previous visual controls
  // - remove akTop, since AnchorVertical uses akTop and not akBottom.
  AddJumpBtn.Anchors := AddJumpBtn.Anchors - [akTop];
  AddJumpBtn.AnchorToNeighbour(akBottom, 3, TopBevel);
  RemoveJumpBtn.Enabled := False;
  while FJumpComponentsList.Count > 0 do
  with PJumpComponents(FJumpComponentsList.Last)^ do
  begin
    TObject(ValueEdit).Free;
    TObject(GotoCombo).Free;
    TObject(ResetCombo).Free;
    FJumpComponentsList.Delete(FJumpComponentsList.Count-1);
  end;
end;

procedure TFieldPropertiesFrame.UpdateJumps;
var
  F: TEpiField;
  Jmp: TEpiJumps;
  i: Integer;
  j: Integer;
  SelectIgnoreCombo: Boolean;
begin
  InitCombo(UseJumpsCombo);
  SelectIgnoreCombo := false;
  Jmp := Field.Jumps;

  if Assigned(Jmp) then
    AddFieldToCombo(Field, AllFieldTypes, UseJumpsCombo, True);

  for i := 1 to FieldCount - 1 do
  begin
    F := Fields[i];

    if Assigned(F.Jumps) then
      AddFieldToCombo(F, AllFieldTypes, UseJumpsCombo, True);

    if SelectIgnoreCombo then
      Continue;

    if (Assigned(Jmp) and (not Assigned(F.Jumps)))
       or
       ((not Assigned(Jmp)) and Assigned(F.Jumps))
    then
    begin
      SelectIgnoreCombo := true;
      Continue;
    end;

    if (Assigned(Jmp) and Assigned(F.Jumps)) then
    begin
      if (Jmp.Count <> F.Jumps.Count) then
      begin
        SelectIgnoreCombo := true;
        Continue;
      end;

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
        begin
          SelectIgnoreCombo := true;
          Break;
        end;
      end;
    end;
  end;

  if SelectIgnoreCombo then
  begin
    FinishCombo(UseJumpsCombo, FIgnoreObject);
    Jmp := nil;
  end else
    FinishCombo(UseJumpsCombo, Field);

  UpdateJumps(Jmp);
end;

procedure TFieldPropertiesFrame.UpdateJumps(Jumps: TEpiJumps);
var
  i: Integer;
begin
  ClearJumps;

  if Assigned(Jumps) then
    for i := 0 to Jumps.Count -1 do
      with PJumpComponents(DoAddNewJump)^ do
        with Jumps[i] do
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
    OnUTF8KeyPress := @JumpEditUTF8KeyPress;
    Hint := 'Specify value or use "." to indicate all other values';
    ShowHint := true;
    ParentShowHint := false;
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
    if (FJumpComponentsList.Count > 0) then
      ItemIndex := TComboBox(PJumpComponents(FJumpComponentsList[FJumpComponentsList.Count - 1])^.ResetCombo).ItemIndex
    else
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
      CalcUnchangedRadioBtn.Checked := true;
      Break;
    end;
  end;

  // Then fill content!
  if (not CalcUnchangedRadioBtn.Checked) and
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

function TFieldPropertiesFrame.DoError(const Msg: string; Ctrl: TWinControl
  ): boolean;
begin
  DoWarning(Msg, Ctrl);
  Ctrl.SetFocus;
  Result := false;
end;

procedure TFieldPropertiesFrame.DoWarning(const Msg: string; Ctrl: TWinControl);
var
  P: TWinControl;
begin
  P := Ctrl.Parent;
  while not (P is TTabSheet) do
    P := P.Parent;
  TPageControl(P.Parent).ActivePage := TTabSheet(P);
  ShowHintMsg(Msg, Ctrl);
end;

procedure TFieldPropertiesFrame.AddJumpBtnClick(Sender: TObject);
begin
  TEdit(PJumpComponents(DoAddNewJump)^.ValueEdit).SetFocus;

  if ManyFields and
     (ComboIgnoreSelected(UseJumpsCombo))
  then
    UseJumpsCombo.ItemIndex := UseJumpsCombo.Items.IndexOfObject(FNoneObject);
end;

procedure TFieldPropertiesFrame.AddValueLabelPlusActionExecute(Sender: TObject);
begin
  AddEditValueLabelBtn.Click;
end;

procedure TFieldPropertiesFrame.CalcRadioChange(Sender: TObject);
begin
  TimeDiffGrpBox.Enabled      := TimeCalcRadio.Checked;
  CombineDateGrpBox.Enabled   := CombineDateRadio.Checked;
  CombineStringGrpBox.Enabled := CombineStringRadio.Checked;
end;

procedure TFieldPropertiesFrame.LengthEditingDone(Sender: TObject);
begin
  UpdateValueLabels;
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
    VLSet := ValueLabels.NewValueLabelSet(Field.FieldType);
    VLSet.Name := 'valuelabel_' + Field.Name;
    VLEdit.ValueLabelSet := VLSet;
    NewVL := true;
  end;

  if VLEdit.ShowModal = mrOK then
  begin
    ValueLabelComboBox.ItemIndex := ValueLabelComboBox.Items.IndexOfObject(VLEdit.ValueLabelSet);
    ValueLabelComboBoxChange(ValueLabelComboBox);
  end
  else if NewVL then
    VLEdit.ValueLabelSet.Free;

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

procedure TFieldPropertiesFrame.UseJumpsComboSelect(Sender: TObject);
var
  F: TEpiField;
begin
  if ComboIgnoreSelected(UseJumpsCombo) or
     ComboNoneSelected(UseJumpsCombo)
  then
  begin
    ClearJumps;
    Exit;
  end;

  F := TEpiField(ComboSelectedObject(UseJumpsCombo));
  UpdateJumps(F.Jumps);
end;

procedure TFieldPropertiesFrame.ValueLabelComboBoxChange(Sender: TObject);
begin
  ValueLabelSettingGrpBox.Enabled :=
    (not ComboIgnoreSelected(ValueLabelComboBox)) and
    (not ComboNoneSelected(ValueLabelComboBox));

  if (FVLIncompatibleItemIndex > - 1) and
     (ValueLabelComboBox.ItemIndex > FVLIncompatibleItemIndex)
  then
    ShowHintMsg('Warning: Selected value label set is no compatible with all fields',
      ValueLabelComboBox);

  If ComboNoneSelected(ValueLabelComboBox) then
    AddEditValueLabelBtn.Caption := 'New'
  else
    AddEditValueLabelBtn.Caption := 'Edit';
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
  if (Sender is TEpiValueLabelSets) and
     (EventGroup = eegCustomBase)
  then
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

          DoWarning('Warning: Valuelabels changed!', ValueLabelComboBox);
        end;
        ValueLabelComboBox.Items.EndUpdate;
      end;
    ecceSetItem: ;
    ecceSetTop: ;
    ecceSetLeft: ;
    ecceText: ;
  end;

  if (Sender is TEpiValueLabelSet) then
  begin
    if EventGroup = eegCustomBase then
      case TEpiCustomChangeEventType(EventType) of
        ecceDestroy: ;
        ecceUpdate: ;
        ecceName:
          begin
            S := string(data^);
            Idx := ValueLabelComboBox.Items.IndexOf(S);
            if Idx > -1 then
              ValueLabelComboBox.Items.Strings[Idx] := TEpiValueLabelSet(Sender).Name;
          end;
        ecceAddItem: ;
        ecceDelItem: ;
        ecceSetItem: ;
        ecceSetTop: ;
        ecceSetLeft: ;
        ecceText: ;
      end;

    if EventGroup = eegValueLabelSet then
      case TEpiValueLabelSetChangeEvent(EventType) of
        evlsMaxValueLength: UpdateValueLabels;
      end;
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
  UseJumpsCombo.Visible           := ManyFields;
  UseJumpsLabel.Visible           := ManyFields;

  // - calc
  CalcUnchangedRadioBtn.Visible      := ManyFields;
  CalcTabSheet.TabVisible         := (not FieldsMustHaveFieldTypes(AutoFieldTypes));

  // - notes
  NotesSheet.Visible              := FieldsMustHaveFieldTypes(NotesFieldTypes);
end;

procedure TFieldPropertiesFrame.UpdateContent;
var
  F: TEpiField;
  i: Integer;

  function ClearOrLeaveEdit(Edit: TCustomEdit; Const NewText: string): boolean;
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

  ValueLabelAsNoteChkBox.Checked := Field.ShowValueLabelNotes;
  for i := 1 to FieldCount - 1 do
    if ClearOrLeaveChkBox(ValueLabelAsNoteChkBox, Fields[i].ShowValueLabelNotes)
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

  // ---------
  // Notes
  // --------
  NotesMemo.Clear;
  NotesMemo.Text := Field.Notes.Text;
  for i := 1 to FieldCount - 1 do
    if ClearOrLeaveEdit(NotesMemo, Fields[i].Notes.Text)
    then break;
end;

procedure TFieldPropertiesFrame.DoUpdateCaption;
var
  S: String;
  i: Integer;
begin
  S := Field.Name;
  for i := 1 to FieldCount - 1 do
    S := S + ', ' + Fields[i].Name;

  S := EpiCutString(S, 20);
  UpdateCaption('Field Properties: ' + S);
end;

function TFieldPropertiesFrame.ValidateChanges: boolean;
var
  I: integer;
  I64: int64;
  F: Extended;
  W1, W2, W3: Word;
  S: string;
begin
  result := false;

  // *******
  // BASIC
  // *******

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
      DoError('Invalid length', LengthEdit);
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

  // Check for invalid chosen valuelabelset
  if (FVLIncompatibleItemIndex > -1) and
     (ValueLabelComboBox.ItemIndex >= FVLIncompatibleItemIndex)
  then
    Exit(DoError('Incompatible value label set selected', ValueLabelComboBox));

  // Safely assume that From/To Edits are only visible if all fields have same type.
  if FromEdit.Modified or ToEdit.Modified then
  begin
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
  end;

  // *******
  // Extended
  // *******

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

  // *******
  // Jumps
  // *******
  for I := 0 to FJumpComponentsList.Count - 1 do
  begin
    with PJumpComponents(FJumpComponentsList[I])^ do
    begin
      if TEdit(ValueEdit).Text = '' then
        Exit(DoError('Empty jump value!', TEdit(ValueEdit)));

      // This allow for using '.' as an "On any value" specifier.
      if TEdit(ValueEdit).Text <> TEpiJump.DefaultOnAnyValue then
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
  end;

  // *******
  // Calculate
  // *******
  if not (
      (NoCalcRadio.Checked) or
      (CalcUnchangedRadioBtn.Checked)
     )
  then
  begin
    if TimeCalcRadio.Checked then
    begin
      if ComboNoneSelected(TimeResultCombo) then
        Exit(DoError('No result field assigned!', TimeResultCombo));

      if ComboNoneSelected(StartDateCombo) and
         ComboNoneSelected(StartTimeCombo) then
        Exit(DoError('No start date or time assigned!', StartDateCombo));

      if ComboNoneSelected(EndDateCombo) and
         ComboNoneSelected(EndTimeCombo) then
        Exit(DoError('No end date or time assigned!', EndDateCombo));
    end;

    if CombineDateRadio.Checked then
    begin
      if ComboNoneSelected(DateResultCombo) then
        Exit(DoError('No result field assigned!', DateResultCombo));

      if ComboNoneSelected(DayCombo) then
        Exit(DoError('No day field assigned!', DayCombo));

      if ComboNoneSelected(MonthCombo) then
        Exit(DoError('No month field assigned!', MonthCombo));

      if ComboNoneSelected(YearCombo) then
        Exit(DoError('No year field assigned!', YearCombo));
    end;

    if CombineStringRadio.Checked then
    begin
      if ComboNoneSelected(StringResultCombo) then
        Exit(DoError('No result field assigned!', StringResultCombo));

      if ComboNoneSelected(Field1Combo) and
         ComboNoneSelected(Field2Combo) and
         ComboNoneSelected(Field3Combo) then
        Exit(DoError('At least one field must be assigned!', Field1Combo));
    end;
  end;

  ShowHintMsg('', nil);
  result := true;
end;

procedure TFieldPropertiesFrame.InternalApplyChanges;
var
  i, j: Integer;
  L: Cardinal;
  R: TEpiRange;
  S: string;
  NJump: TEpiJump;
  Calc: TEpiCalculation;
  I1, I2: EpiInteger;
  F1, F2: Extended;

  procedure SwapInt(var LowI, HighI: EpiInteger);
  var
    Tmp: EpiInteger;
  begin
    if LowI <= HighI then exit;
    Tmp := LowI;
    LowI := HighI;
    HighI := Tmp;
  end;

  procedure SwapFloat(var LowF, HighF: EpiFloat);
  var
    Tmp: EpiFloat;
  begin
    if LowF <= HighF then exit;
    Tmp := LowF;
    LowF := HighF;
    HighF := Tmp;
  end;

begin
  // ---------
  // BASIC
  // ---------

  // Name
  if NameEdit.Modified then
    Field.Name := NameEdit.Text;

  // Question
  if QuestionEdit.Modified then
    for i := 0 to FieldCount - 1 do
      Fields[i].Question.Text := QuestionEdit.Text;

  // Length
  if LengthEdit.Modified then
    if Field.FieldType = ftFloat then
      for i := 0 to FieldCount - 1 do
        Fields[i].Length := StrToInt(LengthEdit.Text) + TEpiField(FFields[i]).Decimals + 1
    else
      for i := 0 to FieldCount - 1 do
        Fields[i].Length := StrToInt(LengthEdit.Text);

  // Decimal
  if DecimalsEdit.Modified then
    for i := 0 to FieldCount - 1 do
    begin
      L := Fields[i].Length - Fields[i].Decimals - 1;
      Fields[i].Decimals := StrToInt(DecimalsEdit.Text);
      Fields[i].Length := L + Fields[i].Decimals + 1;
    end;

  // Valuelabels
  if not ComboIgnoreSelected(ValueLabelComboBox) then
    for i := 0 to FieldCount -1 do
      Fields[i].ValueLabelSet := TEpiValueLabelSet(ComboSelectedObject(ValueLabelComboBox));

  // Range
  if FromEdit.Modified or ToEdit.Modified then
  begin
    for i := 0 to FieldCount -1 do
    begin
      if FromEdit.Text = '' then
      begin
        if Assigned(Fields[i].Ranges) then
        begin
          Fields[i].Ranges.Free;
          Fields[i].Ranges := nil;
        end;
      end else begin
        if Assigned(Fields[i].Ranges) then
          R := Fields[i].Ranges[0]
        else begin
          Fields[i].Ranges := TEpiRanges.Create(Fields[i]);
          Fields[i].Ranges.ItemOwner := true;
          R := Fields[i].Ranges.NewRange;
        end;
        Case Field.FieldType of
          ftInteger:
            begin
              I1 := StrToInt64(FromEdit.Text);
              I2 := StrToInt64(ToEdit.Text);
              SwapInt(I1, I2);
              R.AsInteger[true]  := I1;
              R.AsInteger[false] := I2;
            end;
          ftFloat:
            begin
              F1 := StrToFloat(FromEdit.Text);
              F2 := StrToFloat(ToEdit.Text);
              SwapFloat(F1, F2);
              R.AsFloat[true]  := F1;
              R.AsFloat[false] := F2;
            end;
          ftTime:
            begin
              F1 := EpiStrToTime(FromEdit.Text, TimeSeparator, S);
              F2 := EpiStrToTime(ToEdit.Text, TimeSeparator, S);
              SwapFloat(F1, F2);
              R.AsTime[true]  := F1;
              R.AsTime[false] := F2;
            end;
          ftDMYDate,
          ftMDYDate,
          ftYMDDate:
            begin
              I1 := Trunc(EpiStrToDate(FromEdit.Text, DateSeparator, Field.FieldType, S));
              I2 := Trunc(EpiStrToDate(ToEdit.Text, DateSeparator, Field.FieldType, S));
              SwapInt(I1, I2);
              R.AsDate[true]  := I1;
              R.AsDate[false] := I2;
            end;
        end; // Case Field.FieldType
      end; // if FromEdit.text = ''
    end; // for i := 0 to fieldCount - 1
  end; // FromEdit.Modified or ...

  // Updatemode
  if (Field is TEpiCustomAutoField) and
     (UpdateModeRadioGrp.ItemIndex <> -1)
  then
    for i := 0 to FieldCount - 1 do
      TEpiCustomAutoField(Fields[i]).AutoMode := TEpiAutoUpdateMode(SelectedEnum(UpdateModeRadioGrp));

  // ---------
  // EXTENDED
  // ---------
  // Entrymode
  if (EntryRadioGroup.ItemIndex <> -1)
  then
    for i := 0 to FieldCount - 1 do
      Fields[i].EntryMode := TEpiEntryMode(SelectedEnum(EntryRadioGroup));

  // Confirm Entry
  if ConfirmEntryChkBox.State <> cbGrayed then
    for i := 0 to FieldCount - 1 do
      Fields[i].ConfirmEntry := ConfirmEntryChkBox.Checked;

  // Repeat
  if RepeatValueChkBox.State <> cbGrayed then
    for i := 0 to FieldCount - 1 do
      Fields[i].RepeatValue := RepeatValueChkBox.Checked;

  // Defaultvalue
  if DefaultValueEdit.Modified then
    for i := 0 to FieldCount - 1 do
      Fields[i].DefaultValueAsString := DefaultValueEdit.Text;

  // Show valuelabel
  if ShowValueLabelChkBox.State <> cbGrayed then
    for i := 0 to FieldCount - 1 do
      Fields[i].ShowValueLabel := ShowValueLabelChkBox.Checked;

  // Show valuelabel as notes
  if ValueLabelAsNoteChkBox.State <> cbGrayed then
    for i := 0 to FieldCount - 1 do
      Fields[i].ShowValueLabelNotes := ValueLabelAsNoteChkBox.Checked;

  // Forcepicklist
  if ForcePickListChkBox.State <> cbGrayed then
    for i := 0 to FieldCount - 1 do
      Fields[i].ForcePickList := ForcePickListChkBox.Checked;

  // WriteValueLabels
  if not ComboIgnoreSelected(ValueLabelWriteToComboBox) then
    for i := 0 to FieldCount -1 do
      Fields[i].ValueLabelWriteField := TEpiField(ComboSelectedObject(ValueLabelWriteToComboBox));

  // Comparison
  if CompareGroupBox.Visible and
     (not ComboIgnoreSelected(CompareToCombo))
  then
  begin
    if ComboNoneSelected(CompareToCombo) then
    begin
      for i := 0 to FieldCount - 1 do
      begin
        // Just call Free, if Obj.=nil nothing happens anyway.
        Fields[i].Comparison.Free;
        Fields[i].Comparison := nil;
      end;
    end else begin
      for i := 0 to FieldCount -1 do
      begin
        Fields[i].Comparison.Free;
        Fields[i].Comparison := TEpiComparison.Create(Fields[i]);
        Fields[i].Comparison.CompareType := TEpiComparisonType(SelectedEnum(CompareTypeCombo));
        Fields[i].Comparison.CompareField := TEpiField(ComboSelectedObject(CompareToCombo));
      end;
    end;
  end;

  // Jumps
  if (not ComboIgnoreSelected(UseJumpsCombo)) then
  begin
    if (FJumpComponentsList.Count = 0) then
    begin
      for i := 0 to FieldCount -1  do
      begin
        Fields[i].Jumps.Free;
        Fields[i].Jumps := nil;
      end;
    end else begin
      for i := 0 to FieldCount -1  do
      begin
        Fields[i].Jumps.Free;
        Fields[i].Jumps := TEpiJumps.Create(Fields[i]);
        Fields[i].Jumps.ItemOwner := true;

        for j := 0 to FJumpComponentsList.Count - 1 do
        with Fields[i].Jumps do
        begin
          NJump := NewJump;
          with PJumpComponents(FJumpComponentsList[j])^ do
          begin
            if TComboBox(GotoCombo).ItemIndex <= 2 then
              NJump.JumpType := TEpiJumpType(SelectedEnum(TComboBox(GotoCombo)))
            else begin
              NJump.JumpType := jtToField;
              NJump.JumpToField := TEpiField(ComboSelectedObject(TComboBox(GotoCombo)))
            end;
            NJump.ResetType := TEpiJumpResetType(SelectedEnum(TComboBox(ResetCombo)));
            if TEdit(ValueEdit).Text = TEpiJump.DefaultOnAnyValue then
            begin
              Case Field.FieldType of
                ftBoolean:     TEpiBoolJump(NJump).JumpValue   := TEpiBoolField.DefaultMissing;
                ftInteger:     TEpiIntJump(NJump).JumpValue    := TEpiIntField.DefaultMissing;
                ftFloat:       TEpiFloatJump(NJump).JumpValue  := TEpiFloatField.DefaultMissing;
                ftString,
                ftUpperString: TEpiStringJump(NJump).JumpValue := TEpiStringField.DefaultMissing;
              end;
            end else begin
              Case Field.FieldType of
                ftBoolean:     TEpiBoolJump(NJump).JumpValue   := StrToInt(TEdit(ValueEdit).Text);
                ftInteger:     TEpiIntJump(NJump).JumpValue    := StrToInt(TEdit(ValueEdit).Text);
                ftFloat:       TEpiFloatJump(NJump).JumpValue  := StrToFloat(TEdit(ValueEdit).Text);
                ftString,
                ftUpperString: TEpiStringJump(NJump).JumpValue := TEdit(ValueEdit).Text;
              end; // Case Field.Fieldtype
            end; // if TEdit(ValueEdit).Text = TEpiJump.DefaultOnAnyValue
          end; // with PJumpComponents(FJumpComponentsList[i])^ do
        end; // with Field.Jumps do
      end; // for i := 0 to FieldCount -1  do
    end; // if ComboNoneSelected(UseJumpsCombo) then > ELSE
  end; // if (not ComboIgnoreSelected(UseJumpsCombo)) then

  // Calculate
  if not (CalcUnchangedRadioBtn.Checked) then
  begin
    if NoCalcRadio.Checked then
    begin
      for i := 0 to FieldCount - 1 do
      begin
        Fields[i].Calculation.Free;
        Fields[i].Calculation := nil;
      end;
    end else begin
      for i := 0 to FieldCount - 1 do
      begin
        if TimeCalcRadio.Checked then
        begin
          Calc := TEpiTimeCalc.Create(Field);
          with TEpiTimeCalc(Calc) do
          begin
            ResultField := TEpiField(ComboSelectedObject(TimeResultCombo));

            if not ComboNoneSelected(StartDateCombo) then
              StartDate := TEpiDateField(ComboSelectedObject(StartDateCombo));
            if not ComboNoneSelected(StartTimeCombo) then
              StartTime := TEpiDateTimeField(ComboSelectedObject(StartTimeCombo));

            if not ComboNoneSelected(EndDateCombo) then
              EndDate := TEpiDateField(ComboSelectedObject(EndDateCombo));
            if not ComboNoneSelected(EndTimeCombo) then
              EndTime := TEpiDateTimeField(ComboSelectedObject(EndTimeCombo));

            if AsYearRadio.Checked  then TimeCalcType := ctAsYear;
            if AsMonthRadio.Checked then TimeCalcType := ctAsMonths;
            if AsWeeksRadio.Checked then TimeCalcType := ctAsWeeks;
            if AsDaysRadio.Checked  then TimeCalcType := ctAsDays;
            if AsTimeRadio.Checked  then TimeCalcType := ctAsDayFraction;
          end;
        end;
        if CombineDateRadio.Checked then
        begin
          Calc := TEpiCombineDateCalc.Create(Field);
          with TEpiCombineDateCalc(Calc) do
          begin
            ResultField := TEpiField(ComboSelectedObject(DateResultCombo));

            Day   := TEpiIntField(ComboSelectedObject(DayCombo));
            Month := TEpiIntField(ComboSelectedObject(MonthCombo));
            Year  := TEpiIntField(ComboSelectedObject(YearCombo));
          end;
        end;
        if CombineStringRadio.Checked then
        begin
          Calc := TEpiCombineStringCalc.Create(Field);
          with TEpiCombineStringCalc(Calc) do
          begin
            ResultField := TEpiField(ComboSelectedObject(StringResultCombo));

            if not ComboNoneSelected(Field1Combo) then
              Field1 := TEpiField(ComboSelectedObject(Field1Combo));
            if not ComboNoneSelected(Field2Combo) then
              Field2 := TEpiField(ComboSelectedObject(Field2Combo));
            if not ComboNoneSelected(Field3Combo) then
              Field3 := TEpiField(ComboSelectedObject(Field3Combo));
            Delim1 := Delim1Edit.Text;
            Delim2 := Delim2Edit.Text;
          end;
        end;
        Fields[i].Calculation := Calc;
      end;
    end;
  end;

  // Notes
  if NotesMemo.Modified then
    for i := 0 to FieldCount - 1 do
      Fields[i].Notes.Text := NotesMemo.Text;
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

procedure TFieldPropertiesFrame.JumpEditUTF8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
var
  S: String;
begin
  S := String(UTF8Key);

  if S = TEpiJump.DefaultOnAnyValue then
    exit
  else
    EditUTF8KeyPress(Sender, UTF8Key);
end;

constructor TFieldPropertiesFrame.Create(TheOwner: TComponent);
var
  CmpType: TEpiComparisonType;
begin
  inherited Create(TheOwner);
  FieldPageControl.ActivePage := FieldBasicSheet;
  QuestionEdit.SetFocus;

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
    AddObject('On first save', TObject(umFirstSave));
    AddObject('On save/update record', TObject(umUpdated));
    EndUpdate;
  end;

  with CompareTypeCombo.Items do
  begin
    BeginUpdate;
    for CmpType in TEpiComparisonType do
      AddObject(ComparisonTypeToString(CmpType), TObject(PtrInt(CmpType)));
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

procedure TFieldPropertiesFrame.FocusOnNewControl;
begin
  FieldPageControl.ActivePage := FieldBasicSheet;
  QuestionEdit.SetFocus;
end;

procedure TFieldPropertiesFrame.ResetControls;
begin
  UpdateContent;
end;

procedure TFieldPropertiesFrame.SetEpiControls(
  EpiControls: TEpiCustomControlItemArray);
begin
  UnRegisterValueLabelHook;

  FFields := EpiControls;
  if not Assigned(FFields[0]) then exit;

  FDataFile := Field.DataFile;
  FValueLabels := FDataFile.ValueLabels;

  RegisterValueLabelHook;

  UpdateVisibility;
  UpdateContent;
  DoUpdateCaption;
end;

function TFieldPropertiesFrame.ApplyChanges: boolean;
begin
  if (Length(FFields) > 0) and
     (not Assigned(FFields[0]))
  then
    Exit(true);

  result := ValidateChanges;
  if Result then
    InternalApplyChanges;

  DoUpdateCaption;
end;

end.

