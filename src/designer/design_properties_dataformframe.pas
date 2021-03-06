unit design_properties_dataformframe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, Buttons, Spin,
  MaskEdit, ComCtrls, ExtCtrls, design_properties_baseframe, design_types,
  epicustombase, epidatafiles, epirelates, admin_users_accum_rights_frame, types,
  design_properties_dataform_statusbarframe, project_keyfields_form;

type

  { TDataformPropertiesFrame }

  TDataformPropertiesFrame = class(TDesignPropertiesFrame, IDesignPropertiesFrame)
    AddRelateBtn: TSpeedButton;
    Bevel5: TBevel;
    Bevel7: TBevel;
    CaptionEdit: TEdit;
    GotoDataFormBevel: TBevel;
    GotoDataformLabel: TLabel;
    ChildRecGrpBox: TGroupBox;
    Label1: TLabel;
    Label9: TLabel;
    MaskEdit1: TMaskEdit;
    NameEdit: TEdit;
    PageControl1: TPageControl;
    NoLimitRadioBtn: TRadioButton;
    FixedLimitRadioBtn: TRadioButton;
    AfterRecordGrpBox: TRadioGroup;
    RelateScrollBox: TScrollBox;
    RelatesGrpBox: TGroupBox;
    RelateTopBevel: TBevel;
    RelateValueBevel: TBevel;
    RelateOrderLabel: TLabel;
    RemoveRelateBtn: TSpeedButton;
    BasicSheet: TTabSheet;
    AfterRecordSheet: TTabSheet;
    RightsTabSheet: TTabSheet;
    StatusbarContentSheet: TTabSheet;
    KeySheet: TTabSheet;
    procedure AddRelateBtnClick(Sender: TObject);
    procedure MaskEdit1EditingDone(Sender: TObject);
    procedure NoLimitRadioBtnClick(Sender: TObject);
    procedure RemoveRelateBtnClick(Sender: TObject);
  private
    { Relate }
    FRelatesComponentsList: TList;
    procedure ClearRelates;
    procedure UpdateRelate;
    function  DoAddNewRelate: Pointer;
    procedure AddRelationsToCombo(Combo: TComboBox);
  private
    { AfterRecord }
    FAfterRecordIndex: Integer;
    procedure UpdateAfterRecordRadioBoxVisibility;
    procedure UpdateAfterRecordRadioBoxContent;
  private
    { Accumulated User Rights }
    FUserRightsFrame: TUsersAccumulatedRightsFrame;
    procedure UpdateUserRightsFrameVisibility;
    procedure UpdateUserRightsFrameContent;
  private
    { Statusbar }
    FStatusbarContentFrame: TStatusbarContentFrame;
    procedure UpdateStatusbarFrameContent;
  private
    { KeySheet fields }
    FKeyFieldsFrame: TKeyFieldsFrame;
    procedure UpdateKeyFieldFrameContent;
  private
    procedure DataFileCaptionHook(const Sender: TEpiCustomBase;
      const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
    procedure DataFileHook(const Sender: TEpiCustomBase;
      const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
    procedure DoUpdateCaption;
    procedure UpdateVisibility;
    procedure UpdateContent;
    procedure RegisterDataFileHooks;
    procedure UnRegisterDataFileHooks;
  private
    { Helpers }
    function ComboSelectedObject(Combo: TComboBox): TObject;
  private
    { Validate and Apply }
    function  InternalValidate: Boolean;
    procedure InternalApply;
  protected
    procedure SetReadOnly(AValue: Boolean); override;
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
  epidatafilerelations, admin_authenticator, epiadmin;

const
  AfterRecordStateCaption: array[TEpiDataFileAfterRecordState] of string =
    ('New Record',
     'Return to parent',
     'Return to parent on Max records',
     'Stay on record'
    );

type
  TAfterRecordStates = set of TEpiDataFileAfterRecordState;

  TRelateComponents = record
    ValueEdit: TEdit;
    GotoCombo: TComboBox;
  end;
  PRelateComponents = ^TRelateComponents;


{ TDataformPropertiesFrame }

procedure TDataformPropertiesFrame.AddRelateBtnClick(Sender: TObject);
begin
  PRelateComponents(DoAddNewRelate)^.GotoCombo.SetFocus;
end;

procedure TDataformPropertiesFrame.MaskEdit1EditingDone(Sender: TObject);
begin
  NoLimitRadioBtnClick(FixedLimitRadioBtn);
end;

procedure TDataformPropertiesFrame.NoLimitRadioBtnClick(Sender: TObject);
var
  Fixed: Boolean;

  procedure AddToGrpBox(States: TAfterRecordStates);
  var
    S: TEpiDataFileAfterRecordState;
  begin
    for S in States do
      AfterRecordGrpBox.Items.AddObject(
        AfterRecordStateCaption[S],
        TObject(PtrInt(S))
      );
  end;

  procedure SelectItem(State: TEpiDataFileAfterRecordState);
  var
    Obj: TObject;
    Idx: Integer;
  begin
    Obj := TObject(PtrInt(State));
    Idx := AfterRecordGrpBox.Items.IndexOfObject(Obj);
    AfterRecordGrpBox.ItemIndex := Idx;
  end;

  procedure LocalUpdateAfterRecordGroup;
  var
    DefaultState: TEpiDataFileAfterRecordState;
    Count: Integer;
    S: String;
  begin
    AfterRecordGrpBox.Items.Clear;

    S := Trim(MaskEdit1.EditText);
    if S <> '' then
      Count := StrToInt(S)
    else
      Count := 1;

    // 1:1
    if (Fixed) and (Count <= 1)
    then
      begin
        AddToGrpBox([arsStayOnRecord, arsReturnToParent]);
        DefaultState := arsReturnToParent;
      end;

    // 1:X
    if (Fixed) and (Count > 1)
    then
      begin
        AddToGrpBox([arsReturnToParent, arsReturnToParentOnMax]);
        DefaultState := arsReturnToParentOnMax;
      end;

    // 1:inf
    if (not Fixed)
    then
      begin
        AddToGrpBox([arsNewRecord, arsReturnToParent]);
        DefaultState := arsNewRecord;
      end;

    SelectItem(DefaultState);
  end;

begin
  Fixed := (Sender = FixedLimitRadioBtn);
  MaskEdit1.Enabled := Fixed;

  LocalUpdateAfterRecordGroup;
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

procedure TDataformPropertiesFrame.DataFileHook(const Sender: TEpiCustomBase;
  const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
begin
  if (Initiator <> DataFile) then exit;
  if (EventGroup <> eegCustomBase) then exit;
  if (Word(ecceDestroy) <> EventType) then exit;

  UnRegisterDataFileHooks;
  SetDataFile(nil);
end;

procedure TDataformPropertiesFrame.DoUpdateCaption;
begin
  UpdateCaption('Dataset Properties: ' + DataFile.Caption.Text);
end;

procedure TDataformPropertiesFrame.UpdateAfterRecordRadioBoxVisibility;
var
  DetailRelation: TEpiDetailRelation;

  procedure AddToGrpBox(States: TAfterRecordStates);
  var
    S: TEpiDataFileAfterRecordState;
  begin
    for S in States do
      AfterRecordGrpBox.Items.AddObject(
        AfterRecordStateCaption[S],
        TObject(PtrInt(S))
      );
  end;

begin
  if Relation.InheritsFrom(TEpiDetailRelation) then
  begin
    DetailRelation := TEpiDetailRelation(Relation);

    AfterRecordGrpBox.Items.Clear;

    // 1:1
    if DetailRelation.MaxRecordCount = 1 then AddToGrpBox([arsStayOnRecord, arsReturnToParent]);

    // 1:X
    if DetailRelation.MaxRecordCount > 1 then AddToGrpBox([arsReturnToParent, arsReturnToParentOnMax]);

    // 1:inf
    if DetailRelation.MaxRecordCount = 0 then AddToGrpBox([arsNewRecord, arsReturnToParent]);

    AfterRecordGrpBox.Visible := true;
  end else
    AfterRecordGrpBox.Visible := false;
end;

procedure TDataformPropertiesFrame.UpdateAfterRecordRadioBoxContent;
var
  Obj: TObject;
  Idx: Integer;
begin
  if not Relation.InheritsFrom(TEpiDetailRelation)
  then
    Exit;

  Obj := TObject(PtrInt(DataFile.AfterRecordState));
  Idx := AfterRecordGrpBox.Items.IndexOfObject(Obj);
  if (Idx < 0) then Idx := 0;
  AfterRecordGrpBox.ItemIndex := Idx;
  FAfterRecordIndex := Idx;
end;

procedure TDataformPropertiesFrame.UpdateUserRightsFrameVisibility;
begin
  RightsTabSheet.TabVisible := Assigned(Authenticator.AuthedUser);
end;

procedure TDataformPropertiesFrame.UpdateUserRightsFrameContent;
begin
  FUserRightsFrame.Admin    := Authenticator.Admin;
  FUserRightsFrame.DataFile := DataFile;
end;

procedure TDataformPropertiesFrame.UpdateStatusbarFrameContent;
begin
  FStatusbarContentFrame.Relation := Relation;
end;

procedure TDataformPropertiesFrame.UpdateKeyFieldFrameContent;
begin
  FKeyFieldsFrame.Relation  := Relation;
end;

procedure TDataformPropertiesFrame.UpdateVisibility;
begin
  BasicSheet.Enabled     := IsAuthorized(earDefineProject) and
                            (not Relation.ProtectedItem);

  ChildRecGrpBox.Visible := Relation.InheritsFrom(TEpiDetailRelation);

  UpdateAfterRecordRadioBoxVisibility;

  RelatesGrpBox.Visible     := (Relation.DetailRelations.Count > 0);

  AfterRecordSheet.TabVisible :=
    (Relation.DetailRelations.Count > 0) OR
    (Relation.InheritsFrom(TEpiDetailRelation));
  AfterRecordSheet.Enabled := IsAuthorized(earDefineProject) and
                              (not Relation.ProtectedItem);

  UpdateUserRightsFrameVisibility;

  StatusbarContentSheet.Enabled := IsAuthorized(earDefineProject) and
                                   (not Relation.ProtectedItem);

  KeySheet.Enabled              := IsAuthorized(earDefineProject) and
                                   (not Relation.ProtectedItem);
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
        MaskEdit1.Text := IntToStr(TEpiDetailRelation(Relation).MaxRecordCount);
        if (TEpiDetailRelation(Relation).MaxRecordCount = 0) then
          NoLimitRadioBtn.Checked := true
        else
          FixedLimitRadioBtn.Checked := true;
      end
    else
      begin
        FixedLimitRadioBtn.Checked := true;
        MaskEdit1.Text := '1';
      end;

  // ********************
  //  AfterRecord
  // ********************
  UpdateRelate;
  UpdateAfterRecordRadioBoxContent;
  UpdateUserRightsFrameContent;
  UpdateStatusbarFrameContent;
  UpdateKeyFieldFrameContent;
end;

procedure TDataformPropertiesFrame.RegisterDataFileHooks;
begin
  DataFile.RegisterOnChangeHook(@DataFileHook, true);
  DataFile.Caption.RegisterOnChangeHook(@DataFileCaptionHook, true);
end;

procedure TDataformPropertiesFrame.UnRegisterDataFileHooks;
begin
  DataFile.Caption.UnRegisterOnChangeHook(@DataFileCaptionHook);
  DataFile.UnRegisterOnChangeHook(@DataFileHook);
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
//      if not TEpiCustomList(DataFile.Owner).ValidateRename(NameEdit.Text, false) then
      if not TEpiCustomItem(DataFile).ValidateRename(NameEdit.Text, false) then
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
      ShowHintMsg('A dataform label cannot be empty!', CaptionEdit);
      Exit(false);
    end;
  end;

  S := Trim(MaskEdit1.Text);
  if (ChildRecGrpBox.Visible) and
     (FixedLimitRadioBtn.Checked) and
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

  Result := Result and
            FStatusbarContentFrame.ValidateContent and
            FKeyFieldsFrame.ValidateContent;
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
    if NoLimitRadioBtn.Checked then
      MaxRecordCount := 0
    else
      MaxRecordCount := StrToInt(Trim(MaskEdit1.Text));
  end;

  // ********************
  //  AfterRecord
  // ********************
  DataFile.Relates.ClearAndFree;

  for i := 0 to FRelatesComponentsList.Count - 1 do
  with Datafile.Relates do
  begin
    NRelate := NewRelate;
    with PRelateComponents(FRelatesComponentsList[i])^ do
      NRelate.DetailRelation := TEpiDetailRelation(ComboSelectedObject(GotoCombo));
  end;

  if Relation.InheritsFrom(TEpiDetailRelation) then
    DataFile.AfterRecordState :=
      TEpiDataFileAfterRecordState(
        PtrInt(
            AfterRecordGrpBox.Items.Objects[
              AfterRecordGrpBox.ItemIndex
            ]
        )
      )
  else
    // "Master" datafile is ALWAYS new record state.
    DataFile.AfterRecordState := arsNewRecord;


  FStatusbarContentFrame.ApplyContent;
  FKeyFieldsFrame.ApplyContent;
end;

procedure TDataformPropertiesFrame.SetReadOnly(AValue: Boolean);
var
  i: Integer;
begin
  {inherited SetReadOnly(AValue);

  for i := 0 to PageControl1.PageCount - 1 do
    PageControl1.Pages[i].Enabled := (not ReadOnly);    }
end;

constructor TDataformPropertiesFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  PageControl1.ActivePage := BasicSheet;

  FRelatesComponentsList := TList.Create;

  FUserRightsFrame := TUsersAccumulatedRightsFrame.Create(Self);
  FUserRightsFrame.Align := alClient;
  FUserRightsFrame.BorderSpacing.Around := 10;
  FUserRightsFrame.Parent := RightsTabSheet;

  FStatusbarContentFrame := TStatusbarContentFrame.Create(Self);
  FStatusbarContentFrame.Align := alClient;
  FStatusbarContentFrame.Parent := StatusbarContentSheet;

  FKeyFieldsFrame := TKeyFieldsFrame.Create(Self);
  FKeyFieldsFrame.Align := alClient;
  FKeyFieldsFrame.Parent := KeySheet;
end;

destructor TDataformPropertiesFrame.Destroy;
begin
  if Assigned(DataFile) then
    UnRegisterDataFileHooks;
  inherited Destroy;
end;

procedure TDataformPropertiesFrame.FocusOnNewControl;
begin
  PageControl1.ActivePage := BasicSheet;
  if CaptionEdit.CanFocus then
    CaptionEdit.SetFocus;
end;

procedure TDataformPropertiesFrame.SetEpiControls(EpiControls: TEpiCustomControlItemArray);
begin
  if not Assigned(DataFile) then exit;
  if not Assigned(Relation) then exit;

  RegisterDataFileHooks;

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

