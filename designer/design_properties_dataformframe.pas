unit design_properties_dataformframe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, Buttons, Spin,
  MaskEdit, ComCtrls, ExtCtrls, design_properties_baseframe, design_types,
  epicustombase, epidatafiles, epirelates;

type

  { TDataformPropertiesFrame }

  TDataformPropertiesFrame = class(TDesignPropertiesFrame, IDesignPropertiesFrame)
    AddRelateBtn: TSpeedButton;
    Bevel5: TBevel;
    Bevel7: TBevel;
    CaptionEdit: TEdit;
    GotoDataFormBevel: TBevel;
    GotoDataformLabel: TLabel;
    GroupAssignedListBox: TListBox;
    GroupAvailableListBox: TListBox;
    ChildRecGrpBox: TGroupBox;
    GrpRightsMoveLeft: TSpeedButton;
    GrpRightsMoveRight: TSpeedButton;
    Label1: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label9: TLabel;
    MaskEdit1: TMaskEdit;
    NameEdit: TEdit;
    PageControl1: TPageControl;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    AfterRecordGrpBox: TRadioGroup;
    RelateScrollBox: TScrollBox;
    RelatesGrpBox: TGroupBox;
    RelateTopBevel: TBevel;
    RelateValueBevel: TBevel;
    RelateOrderLabel: TLabel;
    RemoveRelateBtn: TSpeedButton;
    SectionGroupAccessGroupBox: TGroupBox;
    BasicSheet: TTabSheet;
    AfterRecordSheet: TTabSheet;
    procedure AddRelateBtnClick(Sender: TObject);
    procedure AllowedRecordsEditKeyPress(Sender: TObject; var Key: char);
    procedure RadioButton1Click(Sender: TObject);
    procedure RemoveRelateBtnClick(Sender: TObject);
  private
    { Relate }
    FRelatesComponentsList: TList;
    procedure ClearRelates;
    procedure UpdateRelate;
    function  DoAddNewRelate: Pointer;
    procedure AddRelationsToCombo(Combo: TComboBox);
  private
    procedure DataFileCaptionHook(const Sender: TEpiCustomBase;
      const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
    procedure DoUpdateCaption;
    procedure UpdateVisibility;
    procedure UpdateContent;
    procedure RegisterDataFileCaptionHook;
    procedure UnRegisterDataFileCaptionHook;
  private
    { Helpers }
    function ComboSelectedObject(Combo: TComboBox): TObject;
  private
    { Validate and Apply }
    function  InternalValidate: Boolean;
    procedure InternalApply;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure FocusOnNewControl;
    procedure SetEpiControls(EpiControls: TEpiCustomControlItemArray);
    procedure ResetControls;
    function ApplyChanges: boolean;
  end;

implementation

{$R *.lfm}

uses
  epirelations;

type

  TRelateComponents = record
    ValueEdit: TEdit;
    GotoCombo: TComboBox;
  end;
  PRelateComponents = ^TRelateComponents;


{ TDataformPropertiesFrame }

procedure TDataformPropertiesFrame.AllowedRecordsEditKeyPress(Sender: TObject; var Key: char
  );
begin
  if not (Key in ['0'..'9', #8]) then Key := #0;
end;

procedure TDataformPropertiesFrame.AddRelateBtnClick(Sender: TObject);
begin
  PRelateComponents(DoAddNewRelate)^.GotoCombo.SetFocus;
end;

procedure TDataformPropertiesFrame.RadioButton1Click(Sender: TObject);
begin
  MaskEdit1.Enabled := (Sender = RadioButton2);
end;

procedure TDataformPropertiesFrame.RemoveRelateBtnClick(Sender: TObject);
begin
  with PRelateComponents(FRelatesComponentsList.Last)^ do
  begin
    ValueEdit.Free;
    GotoCombo.Free;
    FRelatesComponentsList.Delete(FRelatesComponentsList.Count - 1);
  end;
  if FRelatesComponentsList.Count = 0  then
  begin
    AddRelateBtn.AnchorToNeighbour(akBottom, 3, RelateTopBevel);
    RemoveRelateBtn.Enabled := false;
  end else
    AddRelateBtn.AnchorVerticalCenterTo(PRelateComponents(FRelatesComponentsList.Last)^.GotoCombo);
end;

procedure TDataformPropertiesFrame.ClearRelates;
begin
  // Clear all previous visual controls
  // - remove akTop, since AnchorVertical uses akTop and not akBottom.
  AddRelateBtn.Anchors := AddRelateBtn.Anchors - [akTop];
  AddRelateBtn.AnchorToNeighbour(akBottom, 3, RelateTopBevel);
  RemoveRelateBtn.Enabled := False;
  while FRelatesComponentsList.Count > 0 do
    with PRelateComponents(FRelatesComponentsList.Last)^ do
    begin
      ValueEdit.Free;
      GotoCombo.Free;
      FRelatesComponentsList.Delete(FRelatesComponentsList.Count-1);
    end;
end;

procedure TDataformPropertiesFrame.UpdateRelate;
var
  Relate: TEpiRelate;
begin
  ClearRelates;

  for Relate in DataFile.Relates do
  begin
    with PRelateComponents(DoAddNewRelate)^ do
      GotoCombo.ItemIndex := GotoCombo.Items.IndexOfObject(Relate.DetailRelation);
  end;
end;

function TDataformPropertiesFrame.DoAddNewRelate: Pointer;
var
  RVE: TEdit;
  GDC: TComboBox;
  RRec: PRelateComponents;
begin
  RVE := TEdit.Create(RelateScrollBox);
  GDC := TComboBox.Create(RelateScrollBox);

  with GDC do
  begin
    if FRelatesComponentsList.Count = 0 then
      AnchorToNeighbour(akTop, 3, RelateTopBevel)
    else
      AnchorToNeighbour(akTop, 3, PRelateComponents(FRelatesComponentsList[FRelatesComponentsList.Count-1])^.GotoCombo);
    AnchorToNeighbour(akLeft, 5, RelateValueBevel);
    AnchorToNeighbour(akRight, 5, GotoDataFormBevel);
    AddRelationsToCombo(GDC);
    Style := csDropDownList;
    Parent := RelateScrollBox;
  end;

  with RVE do
  begin
    Alignment := taRightJustify;
    Text := IntToStr(FRelatesComponentsList.Count + 1);
    AnchorParallel(akLeft, 10, RelateScrollBox);
    AnchorToNeighbour(akRight, 5, RelateValueBevel);
    AnchorVerticalCenterTo(GDC);
    Parent := RelateScrollBox;
    Enabled := False;
  end;

  AddRelateBtn.AnchorVerticalCenterTo(GDC);
  RemoveRelateBtn.Enabled := true;

  RVE.Tag      := FRelatesComponentsList.Count;
  RVE.TabOrder := (FRelatesComponentsList.Count * 3);
  GDC.TabOrder := (FRelatesComponentsList.Count * 3) + 1;

  RRec := New(PRelateComponents);
  with RRec^ do
  begin
    ValueEdit := RVE;
    GotoCombo := GDC;
  end;
  FRelatesComponentsList.Add(RRec);
  Result := RRec;
end;

procedure TDataformPropertiesFrame.AddRelationsToCombo(Combo: TComboBox);
var
  i: Integer;
begin
  Combo.Items.BeginUpdate;
  Combo.Clear;
  for i := 0 to Relation.DetailRelations.Count - 1 do
    Combo.AddItem(Relation.DetailRelation[i].Datafile.Caption.Text, Relation.DetailRelation[i]);
  Combo.ItemIndex := 0;
  Combo.Items.EndUpdate;
end;

procedure TDataformPropertiesFrame.DataFileCaptionHook(
  const Sender: TEpiCustomBase; const Initiator: TEpiCustomBase;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  if (EventGroup <> eegCustomBase) then exit;
  if (Word(ecceText) <> EventType) then exit;

  CaptionEdit.Text := DataFile.Caption.Text;
end;

procedure TDataformPropertiesFrame.DoUpdateCaption;
begin
  UpdateCaption('DataForm Properties: ' + DataFile.Caption.Text);
end;

procedure TDataformPropertiesFrame.UpdateVisibility;
var
  MasterDF: TEpiDataFile;
begin
  SectionGroupAccessGroupBox.Visible := false;
  ChildRecGrpBox.Visible := Relation.InheritsFrom(TEpiDetailRelation);

  if ChildRecGrpBox.Visible then
  begin
    MasterDF := TEpiDetailRelation(Relation).MasterRelation.Datafile;
    ChildRecGrpBox.Enabled := (MasterDF.KeyFields.Count < DataFile.KeyFields.Count);
  end;

  RelatesGrpBox.Enabled     := (Relation.DetailRelations.Count > 0);
  AfterRecordGrpBox.Visible := Relation.InheritsFrom(TEpiDetailRelation);
end;

procedure TDataformPropertiesFrame.UpdateContent;
begin
  // ********************
  //  BASIC
  // ********************
  NameEdit.Text    := DataFile.Name;
  CaptionEdit.Text := DataFile.Caption.Text;

  if ChildRecGrpBox.Visible then
    if ChildRecGrpBox.Enabled then
      begin
        if (TEpiDetailRelation(Relation).MaxRecordCount = 0) then
          RadioButton1.Checked := true
        else
          RadioButton2.Checked := true;
        MaskEdit1.Text := IntToStr(TEpiDetailRelation(Relation).MaxRecordCount);
      end
    else
      begin
        RadioButton2.Checked := true;
        MaskEdit1.Text := '1';
      end;

  // ********************
  //  AfterRecord
  // ********************
  UpdateRelate;

  case DataFile.AfterRecordState of
    arsNewRecord:      AfterRecordGrpBox.ItemIndex := 0;
    arsReturnToParent: AfterRecordGrpBox.ItemIndex := 1;
  end;
end;

procedure TDataformPropertiesFrame.RegisterDataFileCaptionHook;
begin
  DataFile.Caption.RegisterOnChangeHook(@DataFileCaptionHook, true);
end;

procedure TDataformPropertiesFrame.UnRegisterDataFileCaptionHook;
begin
  DataFile.Caption.UnRegisterOnChangeHook(@DataFileCaptionHook);
end;

function TDataformPropertiesFrame.ComboSelectedObject(Combo: TComboBox
  ): TObject;
begin
  Result := nil;
  if Combo.ItemIndex > -1 then
    result := Combo.Items.Objects[Combo.ItemIndex];
end;

function TDataformPropertiesFrame.InternalValidate: Boolean;
var
  S: String;
begin
  Result := true;

  // ********************
  //  BASIC
  // ********************
  if NameEdit.Modified then
  begin
    if NameEdit.Text = '' then
    begin
      ShowHintMsg('A dataform name cannot be empty!', NameEdit);
      Exit(false);
    end;

    if (NameEdit.Text <> DataFile.Name) then
    begin
      if not TEpiCustomList(DataFile.Owner).ValidateRename(NameEdit.Text, false) then
      begin
        ShowHintMsg('Name already exists or invalid identifier', NameEdit);
        Exit(false);
      end;
    end;
  end;

  if CaptionEdit.Modified then
  begin
    if CaptionEdit.Text = '' then
    begin
      ShowHintMsg('A dataform caption cannot be empty!', CaptionEdit);
      Exit(false);
    end;
  end;

  S := Trim(MaskEdit1.Text);
  if (ChildRecGrpBox.Visible) and
     (RadioButton2.Checked) and
     (
      (S = '') or
      (StrToInt(S) = 0)
     )
  then
  begin
    ShowHintMsg('Number of child records cannot be empty or 0!', MaskEdit1);
    Exit(false);
  end;

  // ********************
  //  AfterRecord
  // ********************
end;

procedure TDataformPropertiesFrame.InternalApply;
var
  NRelate: TEpiRelate;
  i: Integer;
begin
  // ********************
  //  BASIC
  // ********************
  if NameEdit.Modified then
    DataFile.Name := NameEdit.Text;

  if CaptionEdit.Modified then
    DataFile.Caption.Text := CaptionEdit.Text;

  if ChildRecGrpBox.Visible then
  with TEpiDetailRelation(Relation) do
  begin
    if RadioButton1.Checked then
      MaxRecordCount := 0
    else
      MaxRecordCount := StrToInt(Trim(MaskEdit1.Text));
  end;

  // ********************
  //  AfterRecord
  // ********************
  if FRelatesComponentsList.Count > 0 then
    DataFile.Relates.ClearAndFree;

  for i := 0 to FRelatesComponentsList.Count - 1 do
  with Datafile.Relates do
  begin
    NRelate := NewRelate;
    with PRelateComponents(FRelatesComponentsList[i])^ do
      NRelate.DetailRelation := TEpiDetailRelation(ComboSelectedObject(GotoCombo));
  end;

  Case AfterRecordGrpBox.ItemIndex of
    0: DataFile.AfterRecordState := arsNewRecord;
    1: DataFile.AfterRecordState := arsReturnToParent;
  end;
end;

constructor TDataformPropertiesFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  PageControl1.ActivePage := BasicSheet;

  FRelatesComponentsList := TList.Create;
end;

destructor TDataformPropertiesFrame.Destroy;
begin
  if Assigned(DataFile) then
    UnRegisterDataFileCaptionHook;
  inherited Destroy;
end;

procedure TDataformPropertiesFrame.FocusOnNewControl;
begin
  CaptionEdit.SetFocus;
end;

procedure TDataformPropertiesFrame.SetEpiControls(EpiControls: TEpiCustomControlItemArray);
begin
  if not Assigned(DataFile) then exit;
  if not Assigned(Relation) then exit;

  RegisterDataFileCaptionHook;

  UpdateVisibility;
  UpdateContent;
  DoUpdateCaption;
end;

procedure TDataformPropertiesFrame.ResetControls;
begin
  UpdateVisibility;
  UpdateContent;
end;

function TDataformPropertiesFrame.ApplyChanges: boolean;
var
  S: String;
begin
  result := true;

  if not Assigned(DataFile) then exit;
  if not Assigned(Relation) then exit;

  Result := InternalValidate;
  if not Result then exit;

  InternalApply;

  ShowHintMsg('', nil);
  DoUpdateCaption;
  Result := true;
end;

end.

