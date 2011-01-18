unit design_fieldproperties_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, StdCtrls, ExtCtrls,
  Buttons, LCLType, epicustombase, design_controls, epivaluelabels, epidatafiles;

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
    procedure LengthEditEditingDone(Sender: TObject);
    procedure ManageValueLabelsButtonClick(Sender: TObject);
    procedure RangeEditUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
  private
    { private declarations }
    FValueLabelSets: TEpiValueLabelSets;
    function GetField: TEpiField;
    function  UpdateValueLabels: boolean;
    procedure ValueLabelSetHook(Sender: TObject; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
    procedure ValueLabelSetsHook(Sender: TObject; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
  protected
    procedure SetEpiControl(const AValue: TEpiCustomControlItem); override;
    procedure ShiftToTabSheet(const SheetNo: Byte); override;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; Const AValueLabelSets: TEpiValueLabelSets);
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

procedure TFieldPropertiesFrame.LengthEditEditingDone(Sender: TObject);
begin
  if ValueLabelComboBox.ItemIndex = -1 then exit;

  if UpdateValueLabels then
    ShowHintMsg(rsVLWarning, TControl(Sender))
  else
    ShowHintMsg('', nil);
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

constructor TFieldPropertiesFrame.Create(TheOwner: TComponent;
  const AValueLabelSets: TEpiValueLabelSets);
var
  i: Integer;
begin
  inherited Create(TheOwner);
  FValueLabelSets := AValueLabelSets;
  FValueLabelSets.RegisterOnChangeHook(@ValueLabelSetsHook, true);

  for i := 0 to FValueLabelSets.Count - 1 do
    FValueLabelSets[i].RegisterOnChangeHook(@ValueLabelSetHook, true);
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

begin
  Result := false;
  FField := TEpiField(EpiControl);

  // "Standard" page
  // Rules for field creation!
  NewLen := FField.Length;
  if LengthEdit.Visible then
  begin
    if (not TryStrToInt(LengthEdit.Text, NewLen)) or
       ((FField.FieldType = ftInteger) and (NewLen >= 19)) or
       (NewLen <= 0)
    then
    begin
      DoError('Invalid length', LengthEdit);
      exit;
    end;
  end;

  NewDecLen := FField.Decimals;
  if DecimalsEdit.Visible then
  begin
    if (not TryStrToInt(DecimalsEdit.Text, NewDecLen)) or
       (NewDecLen <= 0) then
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

  R := nil;
  if ((FromEdit.Text <> '') and (ToEdit.Text <> '')) then
  begin
    if Assigned(FField.Ranges) then
    begin
      Ranges := FField.Ranges;
      R := TEpiRange(Ranges[0]);
    end else begin
      Ranges := TEpiRanges.Create(FField);
      Ranges.ItemOwner := true;
      R := Ranges.NewRange;
    end;
    Case Ranges.FieldType of
      ftInteger:
        begin
          if not TryStrToInt64(FromEdit.Text, Int1) then
          begin
            DoError(Format('Not a valid integer: %s', [FromEdit.Text]), FromEdit);
            Exit;
          end;

          if not TryStrToInt64(ToEdit.Text, Int2) then
          begin
            DoError(Format('Not a valid integer: %s', [ToEdit.Text]), ToEdit);
            Exit;
          end;
        end;
      ftFloat:
        begin
          if not TryStrToFloat(FromEdit.Text, Flt1) then
          begin
            DoError(Format('Not a valid float: %s', [FromEdit.Text]), FromEdit);
            Exit;
          end;

          if not TryStrToFloat(ToEdit.Text, Flt2) then
          begin
            DoError(Format('Not a valid float: %s', [ToEdit.Text]), ToEdit);
            Exit;
          end;
        end;
      ftTime:
        begin
          if not EpiStrToTime(FromEdit.Text, TimeSeparator, H1, M1, S1, S) then
          begin
            DoError(S, FromEdit);
            Exit;
          end;

          if not EpiStrToTime(ToEdit.Text, TimeSeparator, H2, M2, S2, S) then
          begin
            DoError(S, ToEdit);
            Exit;
          end;
        end;
      ftDMYDate,
      ftMDYDate,
      ftYMDDate:
        begin
          if not EpiStrToDate(FromEdit.Text, DateSeparator, Ranges.FieldType, D1, M1, Y1, S) then
          begin
            DoError(S, FromEdit);
            Exit;
          end;

          if not EpiStrToDate(ToEdit.Text, DateSeparator, Ranges.FieldType, D2, M2, Y2, S) then
          begin
            DoError(S, ToEdit);
            Exit;
          end;
        end;
    end;
  end;

  if (NameEdit.Text <> FField.Name) and
     (not FField.DataFile.ValidateFieldRename(FField, NameEdit.Text)) then
  begin
    DoError('Name already exists or invalid identifier', NameEdit);
    Exit;
  end;

  FField.BeginUpdate;
  FField.Name := NameEdit.Text;
  FField.Length := NewLen;
  FField.Decimals := NewDecLen;
  if NewDecLen > 0 then
    FField.Length := FField.Length + FField.Decimals + 1;
  FField.Question.Caption.Text := QuestionEdit.Text;

  // "Advanced" page
  if UpdateValueLabels then
    ShowHintMsg(rsVLWarning, MainForm.ActiveControl);

  if ValueLabelComboBox.ItemIndex >= 0 then
    FField.ValueLabelSet := TEpiValueLabelSet(ValueLabelComboBox.Items.Objects[ValueLabelComboBox.ItemIndex]);

  if Assigned(R) then
  begin
    Case Ranges.FieldType of
      ftInteger:
        begin
          R.AsInteger[true]  := Int1;
          R.AsInteger[false] := Int2;
        end;
      ftFloat:
        begin
          R.AsFloat[true]  := Flt1;
          R.AsFloat[false] := Flt2;
        end;
      ftTime:
        begin
          R.AsTime[true]  := EncodeTime(H1, M1, S1, 0);
          R.AsTime[false] := EncodeTime(H2, M2, S2, 0);
        end;
      ftDMYDate,
      ftMDYDate,
      ftYMDDate:
        begin
          R.AsDate[true]  := Trunc(EncodeDate(Y1, M1, D1));
          R.AsDate[false] := Trunc(EncodeDate(Y2, M2, D2));
        end;
    end;
    FField.Ranges := Ranges;
  end;

  FField.EndUpdate;
  Result := true;
end;

procedure TFieldPropertiesFrame.UpdateFormContent;
begin
  Caption := 'Field Properties';
//  EpiControlPageControl.ActivePage := FieldTabSheet;

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

  // Visiblity
  LengthEdit.Visible              := Field.FieldType in (IntFieldTypes + FloatFieldTypes + StringFieldTypes);
  LengthLabel.Visible             := LengthEdit.Visible;
  DecimalsEdit.Visible            := Field.FieldType in FloatFieldTypes;
  DecimalsLabel.Visible           := DecimalsEdit.Visible;
  RangesGrpBox.Visible            := field.FieldType in RangeFieldTypes;
  ValueLabelComboBox.Visible      := Field.FieldType in ValueLabelFieldTypes;
  ValueLabelLabel.Visible         := ValueLabelComboBox.Visible;
  ManageValueLabelsButton.Visible := ValueLabelComboBox.Visible;

  // Setup "advanced" page.
  if Assigned(Field.Ranges) and (Field.Ranges.Count > 0) then
  begin
    FromEdit.Text := TEpiRange(Field.Ranges[0]).AsString[true];
    ToEdit.Text   := TEpiRange(Field.Ranges[0]).AsString[false];
  end else begin
    FromEdit.Text := '';
    ToEdit.Text   := '';
  end;

  UpdateValueLabels;
end;

procedure TFieldPropertiesFrame.ForceShow;
begin
  inherited ForceShow;
  QuestionEdit.SetFocus;
end;

end.

