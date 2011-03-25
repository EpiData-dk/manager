unit design_fieldproperties_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, StdCtrls, ExtCtrls,
  Buttons, LCLType, epicustombase, design_controls, epivaluelabels, epidatafiles,
  design_propertiesbase_frame;

type

  { TFieldPropertiesFrame }

  TFieldPropertiesFrame = class(TDesignPropertiesFrame)
    AddJumpBtn: TSpeedButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    ComboBox1: TComboBox;
    ComboBox10: TComboBox;
    ComboBox11: TComboBox;
    ComboBox12: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    ComboBox4: TComboBox;
    ComboBox5: TComboBox;
    ComboBox6: TComboBox;
    ComboBox7: TComboBox;
    ComboBox8: TComboBox;
    CombineDateResultCombo: TComboBox;
    ComboBox9: TComboBox;
    DefaultValueEdit: TEdit;
    AutoValuesGrpBox: TGroupBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Label13: TLabel;
    EqLabelCrdate: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    PlusLabelCrDate: TLabel;
    PlusLabelCrDate2: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    EqLabel: TLabel;
    EndLabel: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    MidLabel: TLabel;
    PlusLabel1: TLabel;
    PlusLabel2: TLabel;
    TimeDiffGrpBox: TGroupBox;
    CombineDateGrpBox: TGroupBox;
    CombineStringGrpBox: TGroupBox;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    ValueLabelSettingGrpBox: TGroupBox;
    CalcTabSheet: TTabSheet;
    ValueLabelGrpBox: TGroupBox;
    Label1: TLabel;
    DefaulValueLabel: TLabel;
    JumpSheet: TTabSheet;
    UpdateModeRadioGrp: TRadioGroup;
    ValueLabelWriteToLabel: TLabel;
    NotesMemo: TMemo;
    RepeatValueChkBox: TCheckBox;
    DecimalsEdit: TEdit;
    DecimalsLabel: TLabel;
    EntryRadioGroup: TRadioGroup;
    FieldAdvancedSheet: TTabSheet;
    FieldBasicSheet: TTabSheet;
    FieldNameLabel: TLabel;
    FieldPageControl: TPageControl;
    FieldTypeLabel: TLabel;
    FromEdit: TEdit;
    GotoFieldLabel: TLabel;
    GotoResetBevel: TBevel;
    JumpGotoBevel: TBevel;
    JumpScrollBox: TScrollBox;
    JumpsGrpBox: TGroupBox;
    JumpValueLabel: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    LengthEdit: TEdit;
    LengthLabel: TLabel;
    ManageValueLabelsButton: TButton;
    NameEdit: TEdit;
    QuestionEdit: TEdit;
    QuestionLabel: TLabel;
    RangesGrpBox: TGroupBox;
    RemoveJumpBtn: TSpeedButton;
    ResetAddBevel: TBevel;
    ResetLabel: TLabel;
    ConfirmEntryChkBox: TCheckBox;
    ShowValueLabelChkBox: TCheckBox;
    NotesSheet: TTabSheet;
    ToEdit: TEdit;
    TopBevel: TBevel;
    ValueLabelComboBox: TComboBox;
    ValueLabelWriteToComboBox: TComboBox;
    procedure AddJumpBtnClick(Sender: TObject);
    procedure LengthEditEditingDone(Sender: TObject);
    procedure ManageValueLabelsButtonClick(Sender: TObject);
    procedure RangeEditUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure RemoveJumpBtnClick(Sender: TObject);
    procedure ValueLabelComboBoxChange(Sender: TObject);
  private
    { private declarations }
    FDataFile: TEpiDataFile;
    FValueLabelSets: TEpiValueLabelSets;
    FNilValueLabel: TObject;
    function  GetField: TEpiField;
    function  UpdateValueLabels: boolean;
    procedure UpdateValueLabelWriteTo;
    procedure ValueLabelSetHook(Sender: TObject; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
    procedure ValueLabelSetsHook(Sender: TObject; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
  private
    { Jumps section }
    FJumpComponentsList: TList;
    procedure UpdateFieldComboBox(Combo: TComboBox);
    procedure AddFieldsToCombo(Combo: TComboBox);
    procedure FieldHook(Sender: TObject; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
    procedure JumpValueEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    function  DoAddNewJump: pointer;
    function  UpdateJumps: boolean;
  protected
    { Inheritance overrides }
    procedure SetEpiControl(const AValue: TEpiCustomControlItem); override;
    procedure ShiftToTabSheet(const SheetNo: Byte); override;
    procedure UpdateCaption(const S: String); override;
  private
    { Validation }
    function    InternalValidate: boolean;
    procedure   InternalApply;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; Const AValueLabelSets: TEpiValueLabelSets; Const DataFile: TEpiDataFile);
    destructor  Destroy; override;
    function    ValidateControl: boolean; override;
    procedure   UpdateFormContent; override;
    procedure   ForceShow; override;
    property    ValueLabelSets: TEpiValueLabelSets read FValueLabelSets;
    property    Field: TEpiField read GetField;
  end;

implementation

{$R *.lfm}

uses
  LCLProc, epidatafilestypes, valuelabelseditor_form, epimiscutils, epiranges,
  math, epidocument, epiconvertutils, main;

resourcestring
  rsVLWarning = 'Warning: Valuelabels have changed...';
  rsNotAValidType = 'Not a valid %s: %s';

type
  TJumpComponents = record
    ValueEdit: PtrInt;
    GotoCombo: PtrInt;
    ResetCombo: PtrInt;
  end;
  PJumpComponents = ^TJumpComponents;

{ TFieldPropertiesFrame }

procedure TFieldPropertiesFrame.RangeEditUTF8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
var
  I: integer;
  Ch: LongWord;
begin
  Ch := UTF8CharacterToUnicode(@UTF8Key[1], I);
  if (not (Field.FieldType in StringFieldTypes)) and
     (not (Char(Ch) in [VK_0..VK_9, Char(VK_BACK)] + ['.',','] + ['-', ':', '.'] + ['/', '-', '\', '.'])) then
    UTF8Key := '';
  case Field.FieldType of
    ftFloat:   if (Char(Ch) in ['.',',']) then UTF8Key := DecimalSeparator;
    ftTime:    if (Char(Ch) in ['-', ':', '.']) then UTF8Key := TimeSeparator;
    ftDMYDate,
    ftMDYDate,
    ftYMDDate: if (Char(Ch) in ['/', '-', '\', '.']) then UTF8Key := DateSeparator;
  end;
end;

procedure TFieldPropertiesFrame.RemoveJumpBtnClick(Sender: TObject);
var
  Sibling: TControl;
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

procedure TFieldPropertiesFrame.ValueLabelComboBoxChange(Sender: TObject);
begin
  ValueLabelSettingGrpBox.Visible :=
    ValueLabelComboBox.ItemIndex <> ValueLabelComboBox.Items.IndexOfObject(FNilValueLabel);
  UpdateValueLabelWriteTo;
end;

function TFieldPropertiesFrame.UpdateValueLabels: boolean;
var
  i: Integer;
  DoAdd: boolean;
  FList: TStringList;
  Idx: LongInt;
  l: Integer;
  PreSelectedVLSet: TEpiValueLabelSet;
  OIdx: LongInt;
  CurrentVLSet: TEpiValueLabelSet;
  IntL: Integer;
  DecL: Integer;
  j: Integer;
  S: String;
begin
  PreSelectedVLSet := nil;
  if ValueLabelComboBox.ItemIndex >= 0 then
    PreSelectedVLSet := TEpiValueLabelSet(ValueLabelComboBox.Items.Objects[ValueLabelComboBox.ItemIndex]);
  Idx := -1;

  ValueLabelComboBox.Items.BeginUpdate;
  ValueLabelComboBox.Clear;
  ValueLabelComboBox.Sorted := true;
  if (FValueLabelSets.Count = 0) or
     (not (Field.FieldType in [ftInteger, ftFloat, ftString, ftUpperString])) then
  begin
    OIdx := ValueLabelComboBox.Items.AddObject('(none)', FNilValueLabel);
    if not (Field.FieldType in [ftInteger, ftFloat, ftString, ftUpperString]) then
      ValueLabelComboBox.Hint := 'ValueLabels not support for this field type!'
    else
      ValueLabelComboBox.Hint := 'No Valuelabels Defined.' + LineEnding + 'Press "Manage" to create a ValueLabel set.';
  end else begin
   for i := 0 to FValueLabelSets.Count - 1 do
   begin
     CurrentVLSet := FValueLabelSets[i];
     case CurrentVLSet.LabelType of
       ftInteger:
         begin
           DoAdd := Field.FieldType in [ftInteger, ftFloat];
           if Field.FieldType = ftFloat then
             DoAdd := DoAdd and (CurrentVLSet.MaxValueLength <= StrToIntDef(LengthEdit.Text, (Field.Length - Field.Decimals - 1)))
           else
             DoAdd := DoAdd and (CurrentVLSet.MaxValueLength <= StrToIntDef(LengthEdit.Text, Field.Length));
         end;
       ftFloat:
         begin
           DoAdd := Field.FieldType = ftFloat;

           if DoAdd then
           begin
             IntL := 0;
             DecL := 0;
             for j := 0 to CurrentVLSet.Count - 1 do
             begin
               S := CurrentVLSet[j].ValueAsString;
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
       ValueLabelComboBox.AddItem(CurrentVLSet.Name, CurrentVLSet);
   end;
   S := 'Support ValueLabel types:' + LineEnding;
   case Field.FieldType of
     ftInteger: S := S + EpiTypeNames[ftInteger];
     ftFloat:   S := S + EpiTypeNames[ftInteger] + LineEnding + EpiTypeNames[ftFloat];
     ftString,
     ftUpperString: S := S + EpiTypeNames[ftString] + LineEnding + EpiTypeNames[ftUpperString];
   end;
   ValueLabelComboBox.Hint := S;

   OIdx := ValueLabelComboBox.Items.AddObject('(none)', FNilValueLabel);
   if Assigned(PreSelectedVLSet) then
     Idx := ValueLabelComboBox.Items.IndexOfObject(PreSelectedVLSet)
   else if Assigned(Field.ValueLabelSet) then
     Idx := ValueLabelComboBox.Items.IndexOfObject(Field.ValueLabelSet);
  end;
  if Idx = -1 then
    Idx := OIdx;
  ValueLabelComboBox.Items.EndUpdate;
  ValueLabelComboBox.ItemIndex := Idx;

  result := (PreSelectedVLSet <> ValueLabelComboBox.Items.Objects[Idx]);
end;

procedure TFieldPropertiesFrame.UpdateValueLabelWriteTo;
var
  i: Integer;
  Idx: LongInt;
begin
  with ValueLabelWriteToComboBox do
  begin
    Items.BeginUpdate;
    Clear;
    Sorted := true;

    for i := 0 to FDataFile.Fields.Count -1 do
    with FDataFile.Field[i] do
    begin
      if not (FieldType in StringFieldTypes) then continue;
      if FDataFile.Field[i] = Field then continue;
      Items.AddObject(Name, FDataFile.Field[i]);
    end;
    Idx := Items.AddObject('(none)', nil);
    if Assigned(Field.ValueLabelWriteField) then
      Idx := Items.IndexOfObject(Field.ValueLabelWriteField);

    Items.EndUpdate;
    ItemIndex := Idx;
  end;
end;

function TFieldPropertiesFrame.GetField: TEpiField;
begin
  result := TEpiField(EpiControl);
end;

procedure TFieldPropertiesFrame.ValueLabelSetHook(Sender: TObject;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  if (EventGroup = eegValueLabels) and (EventType = Word(evceName)) then
    UpdateValueLabels;
end;

procedure TFieldPropertiesFrame.ValueLabelSetsHook(Sender: TObject;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  if (EventGroup = eegCustomBase) and (EventType = Word(ecceAddItem)) then
    TEpiValueLabelSet(Data).RegisterOnChangeHook(@ValueLabelSetHook, true);

  if (EventGroup = eegCustomBase) and (EventType = Word(ecceDelItem)) then
  begin
    TEpiValueLabelSet(Data).UnRegisterOnChangeHook(@ValueLabelSetHook);
    if UpdateValueLabels then
      ShowHintMsg(
        Format('Warning: Valuelabels changed for field "%s"', [TEpiField(EpiControl).Name]),
        GetValueLabelsEditor(TEpiDocument(FValueLabelSets.RootOwner)).ToolBar1);
  end;

  if (EventGroup = eegCustomBase) and (EventType = Word(ecceUpdate)) then
  begin
    if UpdateValueLabels then
      ShowHintMsg(rsVLWarning, Self);
  end;
end;


procedure TFieldPropertiesFrame.UpdateFieldComboBox(Combo: TComboBox);
var
  CurrentSelect: TObject;
begin
  CurrentSelect := nil;
  if Combo.ItemIndex > -1 then
    CurrentSelect := Combo.Items.Objects[Combo.ItemIndex];
  AddFieldsToCombo(Combo);
  Combo.ItemIndex := Combo.Items.IndexOfObject(CurrentSelect);
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
  with FDataFile do
  begin
     if (not (EpiControl = Field[i])) and
        (not (Field[i].FieldType in AutoFieldTypes)) then
       Combo.Items.AddObject(Field[i].Name, Field[i]);
  end;
  Combo.ItemIndex := 0;
  Combo.Items.EndUpdate;
end;

procedure TFieldPropertiesFrame.FieldHook(Sender: TObject;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  if (EventGroup = eegCustomBase) and
     (TEpiCustomChangeEventType(EventType) in [ecceUpdate, ecceName]) then
  begin
    UpdateJumps;
  end;

  if (EventGroup = eegCustomBase) and
     (EventType = Word(ecceDestroy)) then
  begin
    FEpiControl := nil;
  end;
end;

procedure TFieldPropertiesFrame.JumpValueEditKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  JVE: TEdit absolute Sender;
begin
  if Key <> VK_RETURN then exit;

  TComboBox(PJumpComponents(FJumpComponentsList[JVE.Tag])^.GotoCombo).SetFocus;
  Key := VK_UNKNOWN;
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
    OnUTF8KeyPress := @RangeEditUTF8KeyPress;
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
    ValueEdit := PtrInt(JVE);
    GotoCombo := PtrInt(GFC);
    ResetCombo := PtrInt(RVC);
  end;
  FJumpComponentsList.Add(JRec);
  Result := JRec;
end;

function TFieldPropertiesFrame.UpdateJumps: boolean;
var
  i: Integer;
begin
  while FJumpComponentsList.Count > 0 do
  with PJumpComponents(FJumpComponentsList.Last)^ do
  begin
    TObject(ValueEdit).Free;
    TObject(GotoCombo).Free;
    TObject(ResetCombo).Free;
    FJumpComponentsList.Delete(FJumpComponentsList.Count-1);
  end;
  AddJumpBtn.AnchorToNeighbour(akBottom, 3, TopBevel);
  RemoveJumpBtn.Enabled := Assigned(Field.Jumps);
  if not Assigned(Field.Jumps) then exit;

  for i := 0 to Field.Jumps.Count -1 do
    with PJumpComponents(DoAddNewJump)^ do
      with Field.Jumps[i] do
      begin
        TEdit(ValueEdit).Text          := JumpValueAsString;
        if JumpType = jtToField then
          TComboBox(GotoCombo).ItemIndex := TComboBox(GotoCombo).Items.IndexOfObject(JumpToField)
        else
          TComboBox(GotoCombo).ItemIndex := TComboBox(GotoCombo).Items.IndexOfObject(TObject(PtrInt(JumpType)));
        TComboBox(ResetCombo).ItemIndex := TComboBox(ResetCombo).Items.IndexOfObject(TObject(PtrInt(ResetType)));
      end;
end;

procedure TFieldPropertiesFrame.LengthEditEditingDone(Sender: TObject);
begin
  if ValueLabelComboBox.ItemIndex = -1 then exit;

  if UpdateValueLabels then
    ShowHintMsg(rsVLWarning, TControl(Sender))
  else
    ShowHintMsg('', nil);
end;

procedure TFieldPropertiesFrame.AddJumpBtnClick(Sender: TObject);
begin
  TEdit(PJumpComponents(DoAddNewJump)^.ValueEdit).SetFocus;
end;

procedure TFieldPropertiesFrame.ManageValueLabelsButtonClick(Sender: TObject);
begin
  GetValueLabelsEditor(TEpiDocument(FValueLabelSets.RootOwner)).Show;
end;

procedure TFieldPropertiesFrame.SetEpiControl(
  const AValue: TEpiCustomControlItem);
begin
  if Assigned(Field) then
    Field.UnRegisterOnChangeHook(@FieldHook);
  inherited SetEpiControl(AValue);
  UpdateFormContent;
  AValue.RegisterOnChangeHook(@FieldHook);
end;

procedure TFieldPropertiesFrame.ShiftToTabSheet(const SheetNo: Byte);
begin
  if SheetNo = 0 then exit;
  if SheetNo > FieldPageControl.PageCount then exit;
  FieldPageControl.ActivePageIndex := SheetNo - 1;
end;

procedure TFieldPropertiesFrame.UpdateCaption(const S: String);
var
  T: String;
begin
  T := 'Field Properties: ' + Field.Name;
  inherited UpdateCaption(T);
end;

function TFieldPropertiesFrame.InternalValidate: boolean;

function DoError(Const Msg: string; Ctrl: TWinControl): boolean;
  var
    P: TWinControl;
  begin
    P := Ctrl.Parent;
    while not (P is TTabSheet) do
      P := P.Parent;
    TPageControl(P.Parent).ActivePage := TTabSheet(P);
    Ctrl.SetFocus;
    ShowHintMsg(Msg, Ctrl);
    Result := false;
  end;

var
  I: integer;
  I64: int64;
  F: Extended;
  S: string;
  W1, W2, W3: Word;
begin
  Result := false;

  if LengthEdit.Visible then
  begin
    if (not TryStrToInt(LengthEdit.Text, I)) or
       ((Field.FieldType = ftInteger) and (I >= 19)) or
       (I <= 0)
    then
    begin
      DoError('Invalid length', LengthEdit);
      exit;
    end;
  end;

  if DecimalsEdit.Visible then
  begin
    if (not TryStrToInt(DecimalsEdit.Text, I)) or
       (I <= 0) then
    begin
      DoError('Invalid decimals', DecimalsEdit);
      exit;
    end;
  end;

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

  if (not Field.ValidateRename(NameEdit.Text, false)) then
  begin
    DoError('Name already exists or invalid identifier', NameEdit);
    Exit;
  end;

  if DefaultValueEdit.Text <> '' then
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

  for I := 0 to FJumpComponentsList.Count - 1 do
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
  end;

  ShowHintMsg('', nil);
  Result := true;
end;

procedure TFieldPropertiesFrame.InternalApply;
var
  R: TEpiRange;
  S: string;
  NJump: TEpiJump;
  i: Integer;
begin
  Field.BeginUpdate;
  Field.Name := NameEdit.Text;
  Field.Length := StrToInt(LengthEdit.Text);
  Field.Decimals := StrToInt(DecimalsEdit.Text);
  if Field.Decimals > 0 then
    Field.Length := Field.Length + Field.Decimals + 1;
  Field.Question.Text := QuestionEdit.Text;

  if UpdateValueLabels then
    ShowHintMsg(rsVLWarning, MainForm.ActiveControl);

  if ValueLabelComboBox.ItemIndex >= 0 then
    if ValueLabelComboBox.Items.Objects[ValueLabelComboBox.ItemIndex] = FNilValueLabel then
      Field.ValueLabelSet := nil
    else
      Field.ValueLabelSet := TEpiValueLabelSet(ValueLabelComboBox.Items.Objects[ValueLabelComboBox.ItemIndex]);
  Field.ShowValueLabel := ShowValueLabelChkBox.Checked;

  if ValueLabelWriteToComboBox.Enabled and
     (ValueLabelWriteToComboBox.ItemIndex >= 0) then
    Field.ValueLabelWriteField := TEpiField(ValueLabelWriteToComboBox.Items.Objects[ValueLabelWriteToComboBox.ItemIndex]);

  if FromEdit.Text <> '' then
  begin
    if Assigned(Field.Ranges) then
      R := Field.Ranges[0]
    else begin
      Field.Ranges := TEpiRanges.Create(Field);
      Field.Ranges.ItemOwner := true;
      R := Field.Ranges.NewRange;
    end;
    Case Field.FieldType of
      ftInteger:
        begin
          R.AsInteger[true]  := StrToInt64(FromEdit.Text);
          R.AsInteger[false] := StrToInt64(ToEdit.Text);
        end;
      ftFloat:
        begin
          R.AsFloat[true]  := StrToFloat(FromEdit.Text);
          R.AsFloat[false] := StrToFloat(ToEdit.Text);
        end;
      ftTime:
        begin
          R.AsTime[true]  := EpiStrToTime(FromEdit.Text, TimeSeparator, S);
          R.AsTime[false] := EpiStrToTime(ToEdit.Text, TimeSeparator, S);
        end;
      ftDMYDate,
      ftMDYDate,
      ftYMDDate:
        begin
          R.AsDate[true]  := Trunc(EpiStrToDate(FromEdit.Text, DateSeparator, Field.FieldType, S));
          R.AsDate[false] := Trunc(EpiStrToDate(ToEdit.Text, DateSeparator, Field.FieldType, S));
        end;
    end;
  end;

  // Extended page
  Field.EntryMode := TEpiEntryMode(PtrUInt(EntryRadioGroup.Items.Objects[EntryRadioGroup.ItemIndex]));
  Field.ConfirmEntry := ConfirmEntryChkBox.Checked;
  Field.RepeatValue := RepeatValueChkBox.Checked;
  if Field is TEpiCustomAutoField then
    TEpiCustomAutoField(Field).AutoMode := TEpiAutoUpdateMode(PtrUInt(UpdateModeRadioGrp.Items.Objects[UpdateModeRadioGrp.ItemIndex]));

  Field.Jumps.Free;
  if (FJumpComponentsList.Count > 0) then
  begin
    Field.Jumps := TEpiJumps.Create(Field);
    Field.Jumps.ItemOwner := true;
    for i := 0 to FJumpComponentsList.Count - 1 do
    with Field.Jumps do
    begin
      NJump := NewJump;
      with PJumpComponents(FJumpComponentsList[i])^ do
      begin
        if TComboBox(GotoCombo).ItemIndex <= 2 then
          NJump.JumpType := TEpiJumpType(PtrInt(TComboBox(GotoCombo).Items.Objects[TComboBox(GotoCombo).ItemIndex]))
        else begin
          NJump.JumpType := jtToField;
          NJump.JumpToField := TEpiField(TComboBox(GotoCombo).Items.Objects[TComboBox(GotoCombo).ItemIndex]);
        end;
        NJump.ResetType := TEpiJumpResetType(PtrInt(TComboBox(ResetCombo).Items.Objects[TComboBox(ResetCombo).ItemIndex]));
        Case Field.FieldType of
          ftBoolean:     TEpiBoolJump(NJump).JumpValue  := StrToInt(TEdit(ValueEdit).Text);
          ftInteger:     TEpiIntJump(NJump).JumpValue  := StrToInt(TEdit(ValueEdit).Text);
          ftFloat:       TEpiFloatJump(NJump).JumpValue  := StrToFloat(TEdit(ValueEdit).Text);
          ftString,
          ftUpperString: TEpiStringJump(NJump).JumpValue  := TEdit(ValueEdit).Text;
        end;
      end;
    end;
  end;

  if DefaultValueEdit.Text <> '' then
    Field.DefaultValueAsString := DefaultValueEdit.Text
  else
    Field.HasDefaultValue := false;

  // Notes
  Field.Notes.Text := NotesMemo.Text;

  Field.EndUpdate;
  UpdateCaption('');
end;

constructor TFieldPropertiesFrame.Create(TheOwner: TComponent;
  const AValueLabelSets: TEpiValueLabelSets; const DataFile: TEpiDataFile);
var
  i: Integer;
begin
  inherited Create(TheOwner);
  FValueLabelSets := AValueLabelSets;
  FValueLabelSets.RegisterOnChangeHook(@ValueLabelSetsHook, true);

  for i := 0 to FValueLabelSets.Count - 1 do
    FValueLabelSets[i].RegisterOnChangeHook(@ValueLabelSetHook, true);

  FDataFile := DataFile;
  FJumpComponentsList := TList.Create;

  FNilValueLabel := TObject.Create;

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

destructor TFieldPropertiesFrame.Destroy;
var
  i: Integer;
begin
  for i := 0 to FValueLabelSets.Count - 1 do
    FValueLabelSets[i].UnRegisterOnChangeHook(@ValueLabelSetHook);
  FValueLabelSets.UnRegisterOnChangeHook(@ValueLabelSetsHook);
  inherited Destroy;
end;

function TFieldPropertiesFrame.ValidateControl: boolean;
begin
  result := InternalValidate;
  if Result then InternalApply;
end;

procedure TFieldPropertiesFrame.UpdateFormContent;
begin
  Caption := 'Field Properties';

  // Visiblity
  // - basic
  LengthEdit.Visible              := Field.FieldType in (IntFieldTypes + FloatFieldTypes + StringFieldTypes);
  LengthLabel.Visible             := LengthEdit.Visible;
  DecimalsEdit.Visible            := Field.FieldType in FloatFieldTypes;
  DecimalsLabel.Visible           := DecimalsEdit.Visible;
  ValueLabelGrpBox.Visible        := Field.FieldType in ValueLabelFieldTypes;
  RangesGrpBox.Visible            := field.FieldType in RangeFieldTypes;
  UpdateModeRadioGrp.Visible      := Field.FieldType in AutoUpdateFieldTypes;

  // - extended
  EntryRadioGroup.Visible         := Field.FieldType in EntryModeFieldTypes;
  ConfirmEntryChkBox.Visible      := Field.FieldType in ConfirmEntryFieldTypes;
  AutoValuesGrpBox.Visible        := Field.FieldType in (RepeatValueFieldTypes + DefaultValueFieldTypes);
  ValueLabelSettingGrpBox.Visible := Field.FieldType in ValueLabelFieldTypes;
  FieldAdvancedSheet.TabVisible   := not (Field.FieldType in AutoFieldTypes);

  // - jumps
  JumpSheet.TabVisible            := Field.FieldType in JumpsFieldTypes;

  // - calc
  CalcTabSheet.TabVisible         := not (Field.FieldType in AutoFieldTypes);

  // - notes
  NotesSheet.Visible              := Field.FieldType in NotesFieldTypes;

  // Setup
  // - basic
  NameEdit.Text         := Field.Name;
  FieldTypeLabel.Caption := EpiTypeNames[Field.FieldType];
  QuestionEdit.Text     := Field.Question.Text;
  if Field.FieldType = ftFloat then
    LengthEdit.Text     := IntToStr(Field.Length - (Field.Decimals + 1))
  else
    LengthEdit.Text     := IntToStr(Field.Length);
  DecimalsEdit.Text     := IntToStr(Field.Decimals);
  ValueLabelComboBox.ItemIndex := ValueLabelComboBox.Items.IndexOfObject(nil);
  UpdateValueLabels;
  if Assigned(Field.Ranges) and (Field.Ranges.Count > 0) then
  begin
    FromEdit.Text := TEpiRange(Field.Ranges[0]).AsString[true];
    ToEdit.Text   := TEpiRange(Field.Ranges[0]).AsString[false];
  end else begin
    FromEdit.Text := '';
    ToEdit.Text   := '';
  end;
  if Field is TEpiCustomAutoField then
    UpdateModeRadioGrp.ItemIndex := UpdateModeRadioGrp.Items.IndexOfObject(TObject(PtrUInt(TEpiCustomAutoField(Field).AutoMode)));

  // - extended
  EntryRadioGroup.ItemIndex := EntryRadioGroup.Items.IndexOfObject(TObject(PtrUInt(Field.EntryMode)));
  ConfirmEntryChkBox.Checked := Field.ConfirmEntry;
  RepeatValueChkBox.Checked := Field.RepeatValue;
  DefaultValueEdit.Text := Field.DefaultValueAsString;

  ValueLabelSettingGrpBox.Visible := ValueLabelSettingGrpBox.Visible and Assigned(Field.ValueLabelSet);
  ShowValueLabelChkBox.Checked := Field.ShowValueLabel;
  UpdateValueLabelWriteTo;

  // - jumps
  UpdateJumps;

  // - notes
  NotesMemo.Clear;
  NotesMemo.Text := Field.Notes.Text;

  UpdateCaption('');
end;

procedure TFieldPropertiesFrame.ForceShow;
begin
  inherited ForceShow;
  QuestionEdit.SetFocus;
end;

end.

