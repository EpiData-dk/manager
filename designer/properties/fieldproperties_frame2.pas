unit fieldproperties_frame2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, StdCtrls, ExtCtrls,
  Buttons, epidatafilestypes, epidatafiles;

type

  { TFieldPropertiesFrame2 }

  TFieldPropertiesFrame2 = class(TFrame)
    AutomaticGrpBox: TGroupBox;
    AddLeadingZeroChkBox: TCheckBox;
    ConfirmEntryChkBox: TCheckBox;
    ComparisonGrpBox: TGroupBox;
    DecimalLabel: TLabel;
    DecimalEdit: TEdit;
    ValueLabelSettingsGrpBox: TGroupBox;
    NameEdit: TEdit;
    QuestionEdit: TEdit;
    LengthEdit: TEdit;
    EntryModeRadioGrp: TRadioGroup;
    ValueLabelGrpBox: TGroupBox;
    RangeGrpBox: TGroupBox;
    NameLabel: TLabel;
    QuestionLabel: TLabel;
    LengthLabel: TLabel;
    PageControl1: TPageControl;
    BasicSheet: TTabSheet;
    ExtendedSheet: TTabSheet;
    UpdateModeRadioGrp: TRadioGroup;
  private
    FField: TEpiField;
    // Fields Access Functions:
    function ManyFields: boolean;
    function IsKeyField: boolean;
    function IsRelatedKeyField: boolean;
    function FieldsMustHaveFieldTypes(FieldTypes: TEpiFieldTypes): boolean;
    function FieldsHaveFieldTypes(FieldTypes: TEpiFieldTypes): boolean;
    function FieldsHaveSameFieldType: boolean;

    // Single field access.
    function FieldCount: integer;
    function GetField(const Index: integer): TEpiField;
  private
    procedure UpdateVisibility;
  public
    { public declarations }
    procedure Initialize;
    property  Field: TEpiField read FField write FField;
  end;

implementation

{$R *.lfm}

{ TFieldPropertiesFrame2 }

function TFieldPropertiesFrame2.ManyFields: boolean;
begin
  result := false;
end;

function TFieldPropertiesFrame2.IsKeyField: boolean;
begin
  result := false;
end;

function TFieldPropertiesFrame2.IsRelatedKeyField: boolean;
begin
  result := false;
end;

function TFieldPropertiesFrame2.FieldsMustHaveFieldTypes(
  FieldTypes: TEpiFieldTypes): boolean;
begin
  result := field.FieldType in FieldTypes;
end;

function TFieldPropertiesFrame2.FieldsHaveFieldTypes(FieldTypes: TEpiFieldTypes
  ): boolean;
begin
  result := field.FieldType in FieldTypes;
end;

function TFieldPropertiesFrame2.FieldsHaveSameFieldType: boolean;
begin
  result := true;
end;

function TFieldPropertiesFrame2.FieldCount: integer;
begin
  result := 1;
end;

function TFieldPropertiesFrame2.GetField(const Index: integer): TEpiField;
begin
  result := Field;
end;

procedure TFieldPropertiesFrame2.UpdateVisibility;
begin
  BasicSheet.Enabled := not IsRelatedKeyField;
  NameEdit.Enabled := not (ManyFields or
//                           IsReservedEpiFieldName(Field.Name) or
                           IsKeyField);

  LengthEdit.Visible              := FieldsMustHaveFieldTypes(IntFieldTypes + FloatFieldTypes + StringFieldTypes);
  if FieldsHaveFieldTypes(FloatFieldTypes) and FieldsHaveFieldTypes(IntFieldTypes + StringFieldTypes)
  then
    LengthEdit.Visible := false;
  LengthLabel.Visible             := LengthEdit.Visible;

  DecimalEdit.Visible            := FieldsMustHaveFieldTypes(FloatFieldTypes);
  DecimalLabel.Visible           := DecimalEdit.Visible;
{  if not DecimalsEdit.Visible then
    Bevel1.Left := QuestionEdit.Left + QuestionEdit.Width
  else
    Bevel1.Left := QuestionEdit.Left + ((QuestionEdit.Width - Bevel1.Width) div 2);     }

  ValueLabelGrpBox.Visible        := FieldsMustHaveFieldTypes(ValueLabelFieldTypes) and FieldsHaveSameFieldType;
  UpdateModeRadioGrp.Visible      := FieldsMustHaveFieldTypes(AutoUpdateFieldTypes);
  RangeGrpBox.Visible             := FieldsMustHaveFieldTypes(RangeFieldTypes) and FieldsHaveSameFieldType;

  // - extended
  ExtendedSheet.TabVisible        := not FieldsMustHaveFieldTypes(AutoFieldTypes);
  ExtendedSheet.Enabled           := not IsRelatedKeyField;
  EntryModeRadioGrp.Visible         := FieldsMustHaveFieldTypes(EntryModeFieldTypes);
  ConfirmEntryChkBox.Visible      := FieldsMustHaveFieldTypes(ConfirmEntryFieldTypes);
  AutomaticGrpBox.Visible        := FieldsMustHaveFieldTypes(RepeatValueFieldTypes + DefaultValueFieldTypes);
//  DefaultValueEdit.Visible        := FieldsHaveSameFieldType;
//  DefaulValueLabel.Visible        := DefaultValueEdit.Visible;
  ValueLabelSettingsGrpBox.Visible := FieldsMustHaveFieldTypes(ValueLabelFieldTypes) and FieldsHaveSameFieldType;
  ComparisonGrpBox.Visible         := FieldsMustHaveFieldTypes(CompareFieldTypes);
  AddLeadingZeroChkBox.Visible        := FieldsMustHaveFieldTypes(IntFieldTypes);
  AddLeadingZeroChkBox.AllowGrayed    := ManyFields;
end;

procedure TFieldPropertiesFrame2.Initialize;
begin
  UpdateVisibility;
end;

end.

