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
    DecimalsEdit: TEdit;
    DecimalsLabel: TLabel;
    DefaultEnterRadioBtn: TRadioButton;
    EntryRadioGroup: TRadioGroup;
    FieldAdvancedSheet: TTabSheet;
    FieldBasicSheet: TTabSheet;
    FieldNameLabel: TLabel;
    FieldPageControl: TPageControl;
    FieldTypeLabel: TLabel;
    FromEdit: TEdit;
    GotoFieldComboBox1: TComboBox;
    GotoFieldLabel: TLabel;
    GotoResetBevel: TBevel;
    JumpGotoBevel: TBevel;
    JumpScrollBox: TScrollBox;
    JumpsGrpBox: TGroupBox;
    JumpValueEdit: TEdit;
    JumpValueLabel: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    LengthEdit: TEdit;
    LengthLabel: TLabel;
    ManageValueLabelsButton: TButton;
    MustEnterRadioBtn: TRadioButton;
    NameEdit: TEdit;
    NoEnterRadioBtn: TRadioButton;
    QuestionEdit: TEdit;
    QuestionLabel: TLabel;
    RangesGrpBox: TGroupBox;
    RemoveJumpBtn: TSpeedButton;
    ResetAddBevel: TBevel;
    ResetLabel: TLabel;
    ResetValueComboBox: TComboBox;
    ShowValueLabelChkBox: TCheckBox;
    ToEdit: TEdit;
    TopBevel: TBevel;
    ValueLabelComboBox: TComboBox;
    ValueLabelLabel: TLabel;
    procedure AddJumpBtnClick(Sender: TObject);
    procedure LengthEditEditingDone(Sender: TObject);
    procedure ManageValueLabelsButtonClick(Sender: TObject);
    procedure RangeEditUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure RemoveJumpBtnClick(Sender: TObject);
  private
    { private declarations }
    FDataFile: TEpiDataFile;
    FValueLabelSets: TEpiValueLabelSets;
    function  GetField: TEpiField;
    function  UpdateValueLabels: boolean;
    procedure ValueLabelSetHook(Sender: TObject; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
    procedure ValueLabelSetsHook(Sender: TObject; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
  private
    { Jumps section }
    FJumpComponentsList: TList;
    procedure UpdateFieldComboBox(Combo: TComboBox);
    procedure AddFieldsToCombo(Combo: TComboBox);
    function  DoAddNewJump: pointer;
    function  UpdateJumps: boolean;
  protected
    procedure SetEpiControl(const AValue: TEpiCustomControlItem); override;
    procedure ShiftToTabSheet(const SheetNo: Byte); override;
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

const
  rsVLWarning = 'Warning: Valuelabels have changed...';

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
  if not (Char(Ch) in [VK_0..VK_9, Char(VK_BACK)] + ['.',','] + ['-', ':', '.'] + ['/', '-', '\', '.']) then
    UTF8Key := '';
  case TEpiField(EpiControl).FieldType of
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
  if FJumpComponentsList.Count <= 1 then exit;
  with PJumpComponents(FJumpComponentsList.Last)^ do
  begin
    TObject(ValueEdit).Free;
    TObject(GotoCombo).Free;
    TObject(ResetCombo).Free;
    FJumpComponentsList.Delete(FJumpComponentsList.Count - 1);
  end;
  Sibling := TControl(PJumpComponents(FJumpComponentsList.Last)^.GotoCombo);
  AddJumpBtn.AnchorVerticalCenterTo(Sibling);
  RemoveJumpBtn.AnchorVerticalCenterTo(Sibling);
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
    OIdx := ValueLabelComboBox.Items.AddObject('(none)', nil);
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

   OIdx := ValueLabelComboBox.Items.AddObject('(none)', nil);
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
  Combo.Items.AddObject('(none)', nil);
  for i := 0 to FDataFile.Fields.Count - 1 do
  with FDataFile do
  begin
     if not (Field[i].FieldType in AutoFieldTypes) then
       Combo.Items.AddObject(Field[i].Name, Field[i]);
  end;
  Combo.Items.EndUpdate;
end;

function TFieldPropertiesFrame.DoAddNewJump: pointer;
var
  JVE: TEdit;     // Jump-to value edit.
  GFC: TComboBox; // Goto field combo
  RVC: TComboBox; // Reset value combo
  JRec: PJumpComponents;
begin
  GFC := TComboBox.Create(JumpScrollBox);
  with GFC do
  begin
    AnchorToNeighbour(akTop, 3, TControl(PJumpComponents(FJumpComponentsList[FJumpComponentsList.Count-1])^.GotoCombo));
    AnchorToNeighbour(akLeft, 5, JumpGotoBevel);
    AnchorToNeighbour(akRight, 5, GotoResetBevel);
    AddFieldsToCombo(GFC);
    Style := csDropDownList;
    Parent := JumpScrollBox;
  end;

  JVE := TEdit.Create(JumpScrollBox);
  with JVE do
  begin
    AnchorParallel(akLeft, 10, JumpScrollBox);
    AnchorToNeighbour(akRight, 5, JumpGotoBevel);
    AnchorVerticalCenterTo(GFC);
    Parent := JumpScrollBox;
  end;

  RVC := TComboBox.Create(JumpScrollBox);
  with RVC do
  begin
    AnchorToNeighbour(akLeft, 5, GotoResetBevel);
    AnchorToNeighbour(akRight, 5, ResetAddBevel);
    AnchorVerticalCenterTo(GFC);
    Parent := JumpScrollBox;
  end;

  RemoveJumpBtn.AnchorVerticalCenterTo(GFC);
  AddJumpBtn.AnchorVerticalCenterTo(GFC);

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
  while FJumpComponentsList.Count > 1 do
  with PJumpComponents(FJumpComponentsList.Last)^ do
  begin
    TObject(ValueEdit).Free;
    TObject(GotoCombo).Free;
    TObject(ResetCombo).Free;
    FJumpComponentsList.Delete(FJumpComponentsList.Count-1);
  end;

  if not Assigned(Field.Jumps) then exit;

  with Field do
  begin
    with Jumps[0] do
    begin
      JumpValueEdit.Text           := JumpValueAsString;
      GotoFieldComboBox1.ItemIndex := GotoFieldComboBox1.Items.IndexOfObject(JumpToField);
      if ResetOnJump then
        ResetValueComboBox.Text    := ResetValueAsString;
    end;

    for i := 1 to Jumps.Count -1 do
    with PJumpComponents(DoAddNewJump)^ do
    with Jumps[i] do
    begin
      TEdit(ValueEdit).Text          := JumpValueAsString;
      TComboBox(GotoCombo).ItemIndex := TComboBox(GotoCombo).Items.IndexOfObject(JumpToField);
      if ResetOnJump then
        TComboBox(ResetCombo).Text   := ResetValueAsString;
    end;
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
  DoAddNewJump;
end;

procedure TFieldPropertiesFrame.ManageValueLabelsButtonClick(Sender: TObject);
begin
  GetValueLabelsEditor(TEpiDocument(FValueLabelSets.RootOwner)).Show;
end;

procedure TFieldPropertiesFrame.SetEpiControl(
  const AValue: TEpiCustomControlItem);
begin
  inherited SetEpiControl(AValue);
  UpdateFormContent;
end;

procedure TFieldPropertiesFrame.ShiftToTabSheet(const SheetNo: Byte);
begin
  if SheetNo = 0 then exit;
  if SheetNo > FieldPageControl.PageCount then exit;
  FieldPageControl.ActivePageIndex := SheetNo - 1;
end;

function TFieldPropertiesFrame.InternalValidate: boolean;

procedure DoError(Const Msg: string; Ctrl: TWinControl);
  var
    P: TWinControl;
  begin
    P := Ctrl.Parent;
    while not (P is TTabSheet) do
      P := P.Parent;
    TPageControl(P.Parent).ActivePage := TTabSheet(P);
    Ctrl.SetFocus;
    ShowHintMsg(Msg, Ctrl);
  end;

var
  I: integer;
  I64: int64;
  F: Extended;
  S: string;
  w: Word;
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
          if not EpiStrToTime(FromEdit.Text, TimeSeparator, w, W, W, S) then
          begin
            DoError(S, FromEdit);
            Exit;
          end;

          if not EpiStrToTime(ToEdit.Text, TimeSeparator, W, W, W, S) then
          begin
            DoError(S, ToEdit);
            Exit;
          end;
        end;
      ftDMYDate,
      ftMDYDate,
      ftYMDDate:
        begin
          if not EpiStrToDate(FromEdit.Text, DateSeparator, Field.FieldType, W, W, W, S) then
          begin
            DoError(S, FromEdit);
            Exit;
          end;

          if not EpiStrToDate(ToEdit.Text, DateSeparator, Field.FieldType, W, W, W, S) then
          begin
            DoError(S, ToEdit);
            Exit;
          end;
        end;
    end;
  end;

  if (NameEdit.Text <> Field.Name) and
     (not FDataFile.ValidateFieldRename(Field, NameEdit.Text)) then
  begin
    DoError('Name already exists or invalid identifier', NameEdit);
    Exit;
  end;

  Result := true;
end;

procedure TFieldPropertiesFrame.InternalApply;
var
  R: TEpiRange;
  S: string;
begin
  Field.BeginUpdate;
  Field.Name := NameEdit.Text;
  Field.Length := StrToInt(LengthEdit.Text);
  Field.Decimals := StrToInt(DecimalsEdit.Text);
  if Field.Decimals > 0 then
    Field.Length := Field.Length + Field.Decimals + 1;
  Field.Question.Caption.Text := QuestionEdit.Text;

  if UpdateValueLabels then
    ShowHintMsg(rsVLWarning, MainForm.ActiveControl);

  if ValueLabelComboBox.ItemIndex >= 0 then
    Field.ValueLabelSet := TEpiValueLabelSet(ValueLabelComboBox.Items.Objects[ValueLabelComboBox.ItemIndex]);

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

  // "Advanced" page

  Field.EndUpdate;
end;

constructor TFieldPropertiesFrame.Create(TheOwner: TComponent;
  const AValueLabelSets: TEpiValueLabelSets; const DataFile: TEpiDataFile);
var
  i: Integer;
  Jrec: PJumpComponents;
begin
  inherited Create(TheOwner);
  FValueLabelSets := AValueLabelSets;
  FValueLabelSets.RegisterOnChangeHook(@ValueLabelSetsHook, true);

  for i := 0 to FValueLabelSets.Count - 1 do
    FValueLabelSets[i].RegisterOnChangeHook(@ValueLabelSetHook, true);

  FDataFile := DataFile;


  Jrec := New(PJumpComponents);
  with JRec^ do
  begin
    ValueEdit := PtrInt(JumpValueEdit);
    GotoCombo := PtrInt(GotoFieldComboBox1);
    ResetCombo := PtrInt(ResetValueComboBox);
  end;
  FJumpComponentsList := TList.Create;
  FJumpComponentsList.Add(Jrec);
  AddFieldsToCombo(GotoFieldComboBox1);
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
var
  FField: TEpiField;
  NewLen: LongInt;
  NewDecLen: LongInt;
  i: Integer;
  // Ranges vars.
  Ranges: TEpiRanges;
  R: TEpiRange;
  Int1, Int2: EpiInteger;
  Flt1, Flt2: EpiFloat;
  D1, D2, M1, M2, Y1, Y2,
  H1, H2, S1, S2: Word;
  S: string;


begin
  result := InternalValidate;
  if not Result then exit;

  InternalApply;
end;

procedure TFieldPropertiesFrame.UpdateFormContent;
begin
  Caption := 'Field Properties';

  // Visiblity
  LengthEdit.Visible              := Field.FieldType in (IntFieldTypes + FloatFieldTypes + StringFieldTypes);
  LengthLabel.Visible             := LengthEdit.Visible;
  DecimalsEdit.Visible            := Field.FieldType in FloatFieldTypes;
  DecimalsLabel.Visible           := DecimalsEdit.Visible;
  RangesGrpBox.Visible            := field.FieldType in RangeFieldTypes;
  ValueLabelComboBox.Visible      := Field.FieldType in ValueLabelFieldTypes;
  ValueLabelLabel.Visible         := ValueLabelComboBox.Visible;
  ManageValueLabelsButton.Visible := ValueLabelComboBox.Visible;

  // Setup Basic page
  NameEdit.Text         := Field.Name;
  FieldTypeLabel.Caption := EpiTypeNames[Field.FieldType];
  QuestionEdit.Text     := Field.Question.Caption.Text;
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

  // Setup "advanced" page.
  UpdateJumps;
end;

procedure TFieldPropertiesFrame.ForceShow;
begin
  inherited ForceShow;
  QuestionEdit.SetFocus;
end;

end.

