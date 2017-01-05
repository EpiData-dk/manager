unit project_keyfields_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, ActnList, epitools_integritycheck, epidatafiles, epidocument,
  epivaluelabels, project_types, epidatafilerelations;

type

  { TKeyFieldsForm }

  TKeyFieldsForm = class(TForm)
    DeleteIndexAction: TAction;
    AddNewIndexAction: TAction;
    AddIndexFieldAction: TAction;
    ActionList1: TActionList;
    AddIndexComboBtn: TSpeedButton;
    Bevel3: TBevel;
    AddIndexFieldBtn: TButton;
    Label1: TLabel;
    RealTimeStatusChkBox: TCheckBox;
    RemoveIndexBtn: TSpeedButton;
    RightBevel: TBevel;
    ScrollBox1: TScrollBox;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
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
  protected
    property  DataFile: TEpiDataFile read GetDataFile;
    property  Relation: TEpiMasterRelation read FRelation;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent;
      ARelation: TEpiMasterRelation);
    destructor Destroy; override;
    property ReadOnly: boolean read FReadOnly write SetReadOnly;
    class procedure RestoreDefaultPos;
  end;

implementation

{$R *.lfm}

uses
  types, dataset_form, settings2, settings2_var,
  epiglobals, epidatafilestypes, LCLIntf, manager_messages,
  main, lcltype, epiv_datamodule;

const
  FormName = 'KeyFieldsForm';

{ TKeyFieldsForm }

procedure TKeyFieldsForm.AddIndexFieldActionExecute(Sender: TObject);
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

procedure TKeyFieldsForm.AddIndexFieldActionUpdate(Sender: TObject);
var
  S: String;
begin
  S := 'Add ';
  if DataFile.Fields.ItemExistsByName(EpiIndexIntegrityFieldName) then
    S := 'Update ';
  AddIndexFieldAction.Caption := S + 'Status Variable';
end;

procedure TKeyFieldsForm.AddNewIndexActionExecute(Sender: TObject);
begin
  FDynamicKeyList.Add(DoAddNewKey);
end;

procedure TKeyFieldsForm.AddNewIndexActionUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Relation.DetailRelations.Count = 0;
end;

procedure TKeyFieldsForm.DeleteIndexActionExecute(Sender: TObject);
begin
  DoDeleteKey;
end;

procedure TKeyFieldsForm.DeleteIndexActionUpdate(Sender: TObject);
var
  Cmb: TComboBox;
begin
  Cmb := TComboBox(FDynamicKeyList.Last);

  TAction(Sender).Enabled :=
    Assigned(Cmb) and
    Cmb.Enabled;
end;

procedure TKeyFieldsForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  FailedRecords: TBoundArray;
  Res: Integer;
  FL: TEpiFields;
  i: Integer;
  FailedValues: TBoundArray;
  IndexCheck: Boolean;
begin
  IndexCheck := PerformIndexCheck(FailedRecords, FailedValues);

  if (ModalResult = mrOK) and (not IndexCheck) then
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
    SaveFormPosition(Self, FormName);
end;

procedure TKeyFieldsForm.FormShow(Sender: TObject);
begin
  if ManagerSettings.SaveWindowPositions then
    LoadFormPosition(Self, FormName);
end;

procedure TKeyFieldsForm.RealTimeStatusChkBoxChange(Sender: TObject);
begin
  IndexCheckError;
end;

procedure TKeyFieldsForm.ComboSelect(Sender: TObject);
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

procedure TKeyFieldsForm.SetReadOnly(AValue: boolean);
begin
  if FReadOnly = AValue then Exit;
  FReadOnly := AValue;

  ScrollBox1.Enabled := (not ReadOnly);
end;

procedure TKeyFieldsForm.UpdateComboContent(const Combo: TComboBox);
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

function TKeyFieldsForm.GetFieldList: TEpiFields;
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

function TKeyFieldsForm.FieldSelected(Field: TEpiField): Boolean;
var
  FieldList: TEpiFields;
begin
  FieldList := GetFieldList;
  Result := FieldList.FieldExists(Field);
  FieldList.Free;
end;

procedure TKeyFieldsForm.IndexCheckError;
var
  FailedRecords: TBoundArray;
  FailedValues: TBoundArray;
begin
  if not RealTimeStatusChkBox.Checked then exit;

  if not PerformIndexCheck(FailedRecords, FailedValues) then
  begin
    ShowError('Warning: Index not uniquely defined!' + LineEnding +
              'Use "List Observations" to get a list of affected observations (' + IntToStr(Length(FailedRecords)) + ').',
              TComboBox(FDynamicKeyList.Last));
    ShowRecordsBtn.Enabled := true;
  end else
    ShowRecordsBtn.Enabled := false;
end;

function TKeyFieldsForm.PerformIndexCheck(out FailedRecords: TBoundArray; out
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

procedure TKeyFieldsForm.ShowRecordsBtnClick(Sender: TObject);
var
  FieldList: TEpiFields;
  i: Integer;
  Cmb: TComboBox;
  FailedRecords: TBoundArray;
  FailedValues: TBoundArray;
begin
  FieldList := TEpiFields.Create(nil);
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

function TKeyFieldsForm.DoAddNewKey: TComboBox;
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

function TKeyFieldsForm.DoDeleteKey: boolean;
var
  Cmb: TComboBox;
begin
  Result := true;

  Cmb := TComboBox(FDynamicKeyList.Last);
  FDynamicKeyList.Delete(FDynamicKeyList.Count - 1);

  if FDynamicKeyList.Count = 0  then
  begin
    if FFixedKeyList.Count = 0 then
    begin
      AddIndexComboBtn.Anchors := AddIndexComboBtn.Anchors - [akTop];
      AddIndexComboBtn.AnchorToNeighbour(akBottom, 3, TopBevel);
    end else
    AddIndexComboBtn.AnchorVerticalCenterTo(TControl(FFixedKeyList.Last));
  end else
    AddIndexComboBtn.AnchorVerticalCenterTo(TControl(FDynamicKeyList.Last));

  ComboSelect(Cmb);
  Cmb.Free;

  IndexCheckError;
end;

function TKeyFieldsForm.GetDataFile: TEpiDataFile;
begin
  Result := nil;
  if Assigned(Relation) then
    Result := Relation.Datafile;
end;

procedure TKeyFieldsForm.SetItemIndexOnField(Combo: TComboBox; Field: TEpiField
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

procedure TKeyFieldsForm.AddFieldsToCombo(Combo: TComboBox);
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


function TKeyFieldsForm.ShowError(const Msg: string; const Ctrl: TControl
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

procedure TKeyFieldsForm.LoadGlyphs;
begin
  DM.Icons16.GetBitmap(31, AddIndexComboBtn.Glyph);
  DM.Icons16.GetBitmap(32, RemoveIndexBtn.Glyph);
end;

constructor TKeyFieldsForm.Create(TheOwner: TComponent;
  ARelation: TEpiMasterRelation);
var
  i: Integer;
begin
  inherited Create(TheOwner);

  FRelation := ARelation;
  FValueLabelSets := ARelation.Datafile.ValueLabels;

  FIndexChecker := TEpiIntegrityChecker.Create;
  FFixedKeyList := TList.Create;
  FDynamicKeyList := TList.Create;

  FHintWindow := THintWindow.Create(Self);
  FHintWindow.HideInterval := 5 * 1000;
  FHintWindow.AutoHide := true;

  LoadGlyphs;

  with DataFile do
    for i := 0 to KeyFields.Count - 1 do
      SetItemIndexOnField(DoAddNewKey, KeyFields[i]);

  Caption := 'Define Key (' + Datafile.Caption.Text + ')';
end;

destructor TKeyFieldsForm.Destroy;
begin
  FFixedKeyList.Free;
  FDynamicKeyList.Free;
  FHintWindow.Free;
  FIndexChecker.Free;
  inherited Destroy;
end;

class procedure TKeyFieldsForm.RestoreDefaultPos;
var
  Aform: TForm;
begin
  Aform := TForm.Create(nil);
  Aform.Width := 600;
  Aform.Height := 480;
  Aform.top := (Screen.Monitors[0].Height - Aform.Height) div 2;
  Aform.Left := (Screen.Monitors[0].Width - Aform.Width) div 2;
  SaveFormPosition(Aform, FormName);
  AForm.free;
end;

end.
