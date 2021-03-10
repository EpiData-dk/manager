unit project_keyfields_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, ActnList, epitools_integritycheck, epidatafiles,
  epivaluelabels, epidatafilerelations;

type

  { TKeyFieldsFrame }

  TKeyFieldsFrame = class(TFrame)
    DeleteIndexAction: TAction;
    AddNewIndexAction: TAction;
    AddIndexFieldAction: TAction;
    ActionList1: TActionList;
    AddIndexComboBtn: TSpeedButton;
    Bevel3: TBevel;
    Label1: TLabel;
    RealTimeStatusChkBox: TCheckBox;
    RemoveIndexBtn: TSpeedButton;
    RightBevel: TBevel;
    ScrollBox1: TScrollBox;
    ShowRecordsBtn: TButton;
    Panel1: TPanel;
    TopBevel: TBevel;
    procedure AddIndexFieldActionExecute(Sender: TObject);
    procedure AddIndexFieldActionUpdate(Sender: TObject);
    procedure AddNewIndexActionExecute(Sender: TObject);
    procedure AddNewIndexActionUpdate(Sender: TObject);
    procedure DeleteIndexActionExecute(Sender: TObject);
    procedure DeleteIndexActionUpdate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
    procedure RealTimeStatusChkBoxChange(Sender: TObject);
    procedure ShowRecordsBtnClick(Sender: TObject);
  private
    { private declarations }
    FIndexChecker: TEpiIntegrityChecker;
    FFixedKeyList: TList;
    FDynamicKeyList: TList;
    FHintWindow: THintWindow;
    FReadOnly: boolean;
    FRelation: TEpiMasterRelation;
    FValueLabelSets: TEpiValueLabelSets;
    function  DoAddNewKey: TComboBox;
    function  DoDeleteKey: boolean;
    function  GetDataFile: TEpiDataFile;
    procedure SetItemIndexOnField(Combo: TComboBox; Field: TEpiField);
    procedure AddFieldsToCombo(Combo: TComboBox);
    procedure ComboSelect(Sender: TObject);
    procedure SetReadOnly(AValue: boolean);
    procedure UpdateComboContent(Const Combo: TComboBox);
    function  GetFieldList: TEpiFields;
    function  FieldSelected(Field: TEpiField): Boolean;
    procedure IndexCheckError;
    function  PerformIndexCheck(Out FailedRecords: TBoundArray;
      out FailedValues: TBoundArray): Boolean;
    function  ShowError(Const Msg: string; Const Ctrl: TControl): boolean;
    procedure LoadGlyphs;
    procedure SetRelation(AValue: TEpiMasterRelation);
  protected
    property  DataFile: TEpiDataFile read GetDataFile;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property Relation: TEpiMasterRelation read FRelation write SetRelation;
    property ReadOnly: boolean read FReadOnly write SetReadOnly;
    procedure UpdateContent;
    procedure ApplyContent;
    function  ValidateContent: Boolean;
  end;

implementation

{$R *.lfm}

uses
  types, dataset_form, settings2, settings2_var,
  epiglobals, epidatafilestypes, LCLIntf, manager_messages,
  main, lcltype, epiv_datamodule;

const
  FormName = 'KeyFieldsForm';

{ TKeyFieldsFrame }

procedure TKeyFieldsFrame.AddIndexFieldActionExecute(Sender: TObject);
var
  i: Integer;
  F: TEpiField;
  FailedRecords: TBoundArray;
  FailedValues: TBoundArray;
  VL: TEpiValueLabelSet;
  V: TEpiCustomValueLabel;
begin
  F := DataFile.Fields.FieldByName[EpiIndexIntegrityFieldName];
  if not Assigned(F) then
  begin
    F := DataFile.NewField(ftInteger);
    F.Name := EpiIndexIntegrityFieldName;
    F.Question.Text := 'Unique index status';
    F.ShowValueLabel := ManagerSettings.ShowValuelabelText;

    VL := FValueLabelSets.GetValueLabelSetByName(EpiIndexIntegrityValueLabelSetName);
    if not Assigned(VL) then
    begin
      VL := FValueLabelSets.NewValueLabelSet(ftInteger);
      VL.Name := EpiIndexIntegrityValueLabelSetName;

      V := Vl.NewValueLabel;
      TEpiIntValueLabel(V).Value := 0;
      V.TheLabel.Text := 'Index OK';

      V := Vl.NewValueLabel;
      TEpiIntValueLabel(V).Value := 1;
      V.TheLabel.Text := 'Index fail';

      V := Vl.NewValueLabel;
      TEpiIntValueLabel(V).Value := 2;
      V.TheLabel.Text := 'Index fail (missing value in key)';
    end;
    F.ValueLabelSet := VL;

    PostMessage(MainForm.Handle, LM_DESIGNER_ADD, WParam(F), 0);
  end;

  // TODO: A SetAll method is missing... :(
  for i := 0 to F.Size -1 do
    F.AsInteger[i] := 0;

  if not PerformIndexCheck(FailedRecords, FailedValues) then
  begin

    // Set only failed records.
    for i := 0 to Length(FailedRecords) - 1 do
      F.AsInteger[FailedRecords[i]] := FailedValues[i];
  end;

  AddIndexFieldAction.Update;
end;

procedure TKeyFieldsFrame.AddIndexFieldActionUpdate(Sender: TObject);
var
  S: String;
begin
  S := 'Add ';
  if DataFile.Fields.ItemExistsByName(EpiIndexIntegrityFieldName) then
    S := 'Update ';
  AddIndexFieldAction.Caption := S + 'Status Variable';
end;

procedure TKeyFieldsFrame.AddNewIndexActionExecute(Sender: TObject);
begin
  FDynamicKeyList.Add(DoAddNewKey);
end;

procedure TKeyFieldsFrame.AddNewIndexActionUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Relation.DetailRelations.Count = 0;
end;

procedure TKeyFieldsFrame.DeleteIndexActionExecute(Sender: TObject);
begin
  DoDeleteKey;
end;

procedure TKeyFieldsFrame.DeleteIndexActionUpdate(Sender: TObject);
var
  Cmb: TComboBox;
begin
  Cmb := TComboBox(FDynamicKeyList.Last);

  TAction(Sender).Enabled :=
    Assigned(Cmb) and
    Cmb.Enabled;
end;

procedure TKeyFieldsFrame.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  FailedRecords: TBoundArray;
  Res: Integer;
  FL: TEpiFields;
  i: Integer;
  FailedValues: TBoundArray;
  IndexCheck: Boolean;
begin
  IndexCheck := PerformIndexCheck(FailedRecords, FailedValues);

{  if (ModalResult = mrOK) and (not IndexCheck) then
  begin
    Res := MessageDlg('Index Error',
                        'Observations with Non-Unique key' + LineEnding +
                        'or missing values in key variables exist.' + LineEnding +
                        BoolToStr(DataFile.Fields.ItemExistsByName(EpiIndexIntegrityFieldName),
                          'Index status saved in variable: '+ EpiIndexIntegrityFieldName + LineEnding, '') +
                        LineEnding +
                        'Apply Key Variables?',
                        mtWarning, mbYesNoCancel, 0, mbCancel);

    // On cancel do nothing, not even close the form.
    CanClose := false;
    if Res = mrCancel then Exit;

    // On no close the form but do not apply index.
    CanClose := true;
    if Res = mrNo then Exit;
  end;

  if ModalResult = mrOk then
  begin
    FL := GetFieldList;
    DataFile.KeyFields.Clear;
    for i := 0 to Fl.Count - 1 do
      DataFile.KeyFields.AddItem(Fl[i]);
    Fl.Free;

    if DataFile.Fields.ItemExistsByName(EpiIndexIntegrityFieldName) then
      AddIndexFieldAction.Execute;
  end;

  if ManagerSettings.SaveWindowPositions then
    SaveFormPosition(Self, FormName); }
end;

procedure TKeyFieldsFrame.FormShow(Sender: TObject);
begin
{  if ManagerSettings.SaveWindowPositions then
    LoadFormPosition(Self, FormName);  }
end;

procedure TKeyFieldsFrame.RealTimeStatusChkBoxChange(Sender: TObject);
begin
  IndexCheckError;
end;

procedure TKeyFieldsFrame.ComboSelect(Sender: TObject);
var
  Cmb: TComboBox;
  i: Integer;
const
  // "Selecting" is needed in MAC, because setting the ItemIndex in UpdateComboContent
  // fires additional OnSelect events, hence creating a loop. This is not the case
  // on other platforms apparently...
  Selecting: boolean = false;
begin
  if Selecting then exit;

  IndexCheckError;

  Selecting := true;
  for i := 0 to FDynamicKeyList.Count -1 do
  begin
    Cmb := TComboBox(FDynamicKeyList[i]);
    if Cmb = Sender then Continue;

    UpdateComboContent(Cmb);
  end;
  Selecting := false;
end;

procedure TKeyFieldsFrame.SetReadOnly(AValue: boolean);
begin
  if FReadOnly = AValue then Exit;
  FReadOnly := AValue;

  ScrollBox1.Enabled := (not ReadOnly);
end;

procedure TKeyFieldsFrame.UpdateComboContent(const Combo: TComboBox);
var
  SelectedField: TEpiField;
  Idx: Integer;
begin
  SelectedField := nil;
  Idx := Combo.ItemIndex;

  if (Idx > -1) then
    SelectedField := TEpiField(Combo.Items.Objects[Idx]);

  Combo.Items.BeginUpdate;

  AddFieldsToCombo(Combo);

  if Assigned(SelectedField) then
  begin
    Idx := Combo.Items.IndexOfObject(SelectedField);
    Combo.ItemIndex := Idx;
  end;

  Combo.Items.EndUpdate;
end;

function TKeyFieldsFrame.GetFieldList: TEpiFields;
var
  i: Integer;
  Cmb: TComboBox;
begin
  Result := TEpiFields.Create(nil);

  for i := 0 to FFixedKeyList.Count - 1 do
  begin
    Cmb := TComboBox(FFixedKeyList[i]);
    if Cmb.ItemIndex = -1 then  continue;

    Result.AddItem(TEpiField(Cmb.Items.Objects[Cmb.ItemIndex]));
  end;

  for i := 0 to FDynamicKeyList.Count - 1 do
  begin
    Cmb := TComboBox(FDynamicKeyList[i]);
    if Cmb.ItemIndex = -1 then  continue;

    Result.AddItem(TEpiField(Cmb.Items.Objects[Cmb.ItemIndex]));
  end;
end;

function TKeyFieldsFrame.FieldSelected(Field: TEpiField): Boolean;
var
  FieldList: TEpiFields;
begin
  FieldList := GetFieldList;
  Result := FieldList.FieldExists(Field);
  FieldList.Free;
end;

procedure TKeyFieldsFrame.IndexCheckError;
var
  FailedRecords: TBoundArray;
  FailedValues: TBoundArray;
  Cmb: TComboBox;
begin
  if not RealTimeStatusChkBox.Checked then exit;

  if (FDynamicKeyList.Count > 0) then
    Cmb := TComboBox(FDynamicKeyList.Last)
  else
    Cmb := TComboBox(FFixedKeyList.Last);

  if not PerformIndexCheck(FailedRecords, FailedValues) then
  begin
    ShowError('Warning: Index not uniquely defined!' + LineEnding +
              'Use "List Observations" to get a list of affected observations (' + IntToStr(Length(FailedRecords)) + ').',
              Cmb);
    ShowRecordsBtn.Enabled := true;
  end else
    ShowRecordsBtn.Enabled := false;
end;

function TKeyFieldsFrame.PerformIndexCheck(out FailedRecords: TBoundArray; out
  FailedValues: TBoundArray): Boolean;
var
  FieldList: TEpiFields;
begin
  FieldList := GetFieldList;

  Result :=
    FIndexChecker.IndexIntegrity(DataFile,
      FailedRecords,
      FailedValues,
      false,
      FieldList);

  FieldList.Free;
end;

procedure TKeyFieldsFrame.ShowRecordsBtnClick(Sender: TObject);
var
  FieldList: TEpiFields;
  i: Integer;
  Cmb: TComboBox;
  FailedRecords: TBoundArray;
  FailedValues: TBoundArray;
begin
  FieldList := TEpiFields.Create(nil);

  for i := 0 to FFixedKeyList.Count - 1 do
  begin
    Cmb := TComboBox(FFixedKeyList[i]);
    if Cmb.ItemIndex = -1 then  continue;

    FieldList.AddItem(TEpiField(Cmb.Items.Objects[Cmb.ItemIndex]));
  end;

  for i := 0 to FDynamicKeyList.Count - 1 do
  begin
    Cmb := TComboBox(FDynamicKeyList[i]);
    if Cmb.ItemIndex = -1 then  continue;

    FieldList.AddItem(TEpiField(Cmb.Items.Objects[Cmb.ItemIndex]));
  end;

  if not FIndexChecker.IndexIntegrity(DataFile, FailedRecords, FailedValues,
           false, FieldList) then
    ShowDataSetViewerForm(Self,
      'List of non-unique observations:',
      DataFile,
      FailedRecords,
      FieldList,
      nil,
      -1,
      true);

  FieldList.Free;
end;

function TKeyFieldsFrame.DoAddNewKey: TComboBox;
var
  TopCtrl: TControl;
begin
  result := TComboBox.Create(ScrollBox1);

  with result do
  begin
    if FDynamicKeyList.Count = 0 then
      if FFixedKeyList.Count = 0 then
        TopCtrl := TopBevel
      else
        TopCtrl := TControl(FFixedKeyList.Last)
    else
      TopCtrl := TControl(FDynamicKeyList.Last);

    AnchorToNeighbour(akTop, 3, TopCtrl);
    AnchorParallel(akLeft, 0, TopBevel);
    AnchorToNeighbour(akRight, 5, RightBevel);
    AddFieldsToCombo(result);
    Style := csDropDownList;
    OnSelect  := @ComboSelect;

    Parent := ScrollBox1;
  end;
  AddIndexComboBtn.AnchorVerticalCenterTo(result);
end;

function TKeyFieldsFrame.DoDeleteKey: boolean;
var
  Cmb: TComboBox;
begin
  if (FDynamicKeyList.Count = 0) then
    Exit(false);

  Cmb := TComboBox(FDynamicKeyList.Last);
  FDynamicKeyList.Delete(FDynamicKeyList.Count - 1);

  if FDynamicKeyList.Count = 0 then
    begin
      if FFixedKeyList.Count = 0 then
        begin
          AddIndexComboBtn.Anchors := AddIndexComboBtn.Anchors - [akTop];
          AddIndexComboBtn.AnchorToNeighbour(akBottom, 3, TopBevel);
        end
      else
        AddIndexComboBtn.AnchorVerticalCenterTo(TControl(FFixedKeyList.Last));
    end
  else
    AddIndexComboBtn.AnchorVerticalCenterTo(TControl(FDynamicKeyList.Last));

  ComboSelect(Cmb);
  Cmb.Free;

  IndexCheckError;

  Result := (FDynamicKeyList.Count > 0);
end;

function TKeyFieldsFrame.GetDataFile: TEpiDataFile;
begin
  Result := nil;
  if Assigned(Relation) then
    Result := Relation.Datafile;
end;

procedure TKeyFieldsFrame.SetItemIndexOnField(Combo: TComboBox; Field: TEpiField
  );
var
  Idx: Integer;
  MR: TEpiMasterRelation;
  MasterDF: TEpiDataFile;
begin
  Idx := Combo.Items.IndexOfObject(Field);
  if Idx <> -1 then
  begin
    Combo.ItemIndex := Idx;
    MR := Relation;

    Combo.Enabled := not (MR.DetailRelations.Count > 0);

    if (MR is TEpiDetailRelation) and
       (Combo.Enabled)
    then
    begin
      MasterDF := TEpiDetailRelation(MR).MasterRelation.Datafile;
      Combo.Enabled := not (MasterDF.KeyFields.ItemExistsByName(Field.Name));
    end;

    if Combo.Enabled then
      FDynamicKeyList.Add(Combo)
    else
      FFixedKeyList.Add(Combo);
  end;
end;

procedure TKeyFieldsFrame.AddFieldsToCombo(Combo: TComboBox);
var
  Flds: TEpiFields;
  F: TEpiField;
begin
  Combo.Items.BeginUpdate;
  Combo.Clear;

  Flds := DataFile.Fields;
  for F in Flds do
  begin
    if (
        (not DataFile.KeyFields.FieldExists(F)) and
        (
         (F.FieldType in AutoUpdateFieldTypes) or
         (F.EntryMode = emNoEnter)
        )
       ) or
       (FieldSelected(F))
    then
      Continue;

    Combo.AddItem(F.Name + BoolToStr(F.Question.Text <> '', ': ' + F.Question.Text, ''), F);
  end;
  Combo.Items.EndUpdate;
end;


function TKeyFieldsFrame.ShowError(const Msg: string; const Ctrl: TControl
  ): boolean;
var
  R: TRect;
  P: TPoint;
begin
  result := false;

  if (Msg = '') and (Ctrl = nil) then
  begin
    FHintWindow.Hide;
    exit;
  end;

  R := FHintWindow.CalcHintRect(0, Msg, nil);
  P := Ctrl.ClientToScreen(Point(0,0));
  OffsetRect(R, P.X, P.Y + Ctrl.Height + 2);
  FHintWindow.ActivateHint(R, Msg);
end;

procedure TKeyFieldsFrame.LoadGlyphs;
begin
  DM.Icons16.GetBitmap(31, AddIndexComboBtn.Glyph);
  DM.Icons16.GetBitmap(32, RemoveIndexBtn.Glyph);
end;

procedure TKeyFieldsFrame.SetRelation(AValue: TEpiMasterRelation);
var
  i: Integer;
begin
  FRelation := AValue;
  FValueLabelSets := FRelation.Datafile.ValueLabels;
  UpdateContent;
end;

constructor TKeyFieldsFrame.Create(TheOwner: TComponent);
var
  i: Integer;
begin
  inherited Create(TheOwner);

  FIndexChecker := TEpiIntegrityChecker.Create;
  FFixedKeyList := TList.Create;
  FDynamicKeyList := TList.Create;

  FHintWindow := THintWindow.Create(Self);
  FHintWindow.HideInterval := 5 * 1000;
  FHintWindow.AutoHide := true;

  LoadGlyphs;
end;

destructor TKeyFieldsFrame.Destroy;
begin
  FFixedKeyList.Free;
  FDynamicKeyList.Free;
  FHintWindow.Free;
  FIndexChecker.Free;
  inherited Destroy;
end;

procedure TKeyFieldsFrame.UpdateContent;
var
  i: Integer;
  Cmb: TComboBox;
begin
   // Put the add/delete keys back to top.
  AddIndexComboBtn.Anchors := AddIndexComboBtn.Anchors - [akTop];
  AddIndexComboBtn.AnchorToNeighbour(akBottom, 3, TopBevel);

  while (FDynamicKeyList.Count > 0) do
    begin
      Cmb := TComboBox(FDynamicKeyList.Last);
      FDynamicKeyList.Remove(Cmb);
      Cmb.Free;
    end;

  while (FFixedKeyList.Count > 0) do
    begin
      Cmb := TComboBox(FFixedKeyList.Last);
      FFixedKeyList.Remove(Cmb);
      Cmb.Free;
    end;

  with DataFile do
    for i := 0 to KeyFields.Count - 1 do
      SetItemIndexOnField(DoAddNewKey, KeyFields[i]);

  RealTimeStatusChkBox.Checked := false;
  ShowRecordsBtn.Enabled := false;
end;

procedure TKeyFieldsFrame.ApplyContent;
var
  FL: TEpiFields;
  F: TEpiField;
begin
  FL := GetFieldList;
  DataFile.KeyFields.Clear;
  for F in FL do
    DataFile.KeyFields.AddItem(F);
  Fl.Free;
end;

function TKeyFieldsFrame.ValidateContent: Boolean;
var
  IndexCheck: Boolean;
  FailedRecords, FailedValues: TBoundArray;
begin
  Result := true;
  IndexCheck := PerformIndexCheck(FailedRecords, FailedValues);

  if (not IndexCheck) then
    begin
      ShowError('Observations with Non-Unique key' + LineEnding +
                  'or missing values in key variables exist.' + LineEnding +
                  LineEnding +
                  'Use "List Observations" to see the conflicting observations',
                ShowRecordsBtn);
    end;
  ShowRecordsBtn.Enabled := not IndexCheck;
end;

end.
