unit project_keyfields_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, ActnList, epitools_integritycheck, epidatafiles, epidocument,
  epivaluelabels, project_types, epirelations;

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
    procedure DeleteIndexActionExecute(Sender: TObject);
    procedure DeleteIndexActionUpdate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
    procedure RealTimeStatusChkBoxChange(Sender: TObject);
    procedure ShowRecordsBtnClick(Sender: TObject);
  private
    { private declarations }
    FIndexChecker: TEpiIntegrityChecker;
    FKeyList: TList;
    FHintWindow: THintWindow;
    FDataFile: TEpiDataFile;
    FValueLabelSets: TEpiValueLabelSets;
    function  DoAddNewKey: TComboBox;
    function  DoDeleteKey: boolean;
    procedure SetItemIndexOnField(Combo: TComboBox; Field: TEpiField);
    procedure AddFieldsToCombo(Combo: TComboBox);
    procedure ComboSelect(Sender: TObject);
    function  GetFieldList: TEpiFields;
    procedure IndexCheckError;
    function  PerformIndexCheck(Out FailedRecords: TBoundArray;
      out FailedValues: TBoundArray): Boolean;
    function  ShowError(Const Msg: string; Const Ctrl: TControl): boolean;
    function  GetRelation: TEpiMasterRelation;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; Datafile: TEpiDataFile;
      ValueLabelSets: TEpiValueLabelSets);
    destructor Destroy; override;
    class procedure RestoreDefaultPos;
  end;

implementation

{$R *.lfm}

uses
  types, dataset_form, settings2, settings2_var,
  epiglobals, epidatafilestypes, LCLIntf, manager_messages,
  main, lcltype;

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
  F := FDataFile.Fields.FieldByName[EpiIndexIntegrityFieldName];
  if not Assigned(F) then
  begin
    F := FDataFile.NewField(ftInteger);
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
  if FDataFile.Fields.ItemExistsByName(EpiIndexIntegrityFieldName) then
    S := 'Update ';
  AddIndexFieldAction.Caption := S + 'Status Field';
end;

procedure TKeyFieldsForm.AddNewIndexActionExecute(Sender: TObject);
begin
  DoAddNewKey;
end;

procedure TKeyFieldsForm.DeleteIndexActionExecute(Sender: TObject);
begin
  DoDeleteKey;
end;

procedure TKeyFieldsForm.DeleteIndexActionUpdate(Sender: TObject);
var
  Cmb: TComboBox;
begin
  Cmb := TComboBox(FKeyList.Last);
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
                        'Records with Non-Unique key' + LineEnding +
                        'or missing values in key fields exist.' + LineEnding +
                        BoolToStr(FDataFile.Fields.ItemExistsByName(EpiIndexIntegrityFieldName),
                          'Index status saved in field: '+ EpiIndexIntegrityFieldName + LineEnding, '') +
                        LineEnding +
                        'Apply Key Fields?',
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
    FDataFile.KeyFields.Clear;
    for i := 0 to Fl.Count - 1 do
      FDataFile.KeyFields.AddItem(Fl[i]);
    Fl.Free;

    if FDataFile.Fields.ItemExistsByName(EpiIndexIntegrityFieldName) then
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
begin
  IndexCheckError;
end;

function TKeyFieldsForm.GetFieldList: TEpiFields;
var
  i: Integer;
  Cmb: TComboBox;
begin
  Result := TEpiFields.Create(nil);
  for i := 0 to FKeyList.Count - 1 do
  begin
    Cmb := TComboBox(FKeyList[i]);
    if Cmb.ItemIndex = -1 then  continue;

    Result.AddItem(TEpiField(Cmb.Items.Objects[Cmb.ItemIndex]));
  end;
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
              'Use "List Records" to get a list of affected records (' + IntToStr(Length(FailedRecords)) + ').',
              TComboBox(FKeyList.Last));
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
    FIndexChecker.IndexIntegrity(FDataFile,
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
  for i := 0 to FKeyList.Count - 1 do
  begin
    Cmb := TComboBox(FKeyList[i]);
    if Cmb.ItemIndex = -1 then  continue;

    FieldList.AddItem(TEpiField(Cmb.Items.Objects[Cmb.ItemIndex]));
  end;

  if not FIndexChecker.IndexIntegrity(FDataFile, FailedRecords, FailedValues,
           false, FieldList) then
    ShowDataSetViewerForm(Self,
      'List of non-unique records:',
      FDataFile,
      FailedRecords,
      FieldList,
      nil,
      -1,
      true);

  FieldList.Free;
end;

function TKeyFieldsForm.DoAddNewKey: TComboBox;
begin
  result := TComboBox.Create(ScrollBox1);

  with result do
  begin
    if FKeyList.Count = 0 then
      AnchorToNeighbour(akTop, 3, TopBevel)
    else
      AnchorToNeighbour(akTop, 3, TControl(FKeyList.Last));
    AnchorParallel(akLeft, 0, TopBevel);
    AnchorToNeighbour(akRight, 5, RightBevel);
    AddFieldsToCombo(result);
    Style := csDropDownList;
    OnSelect  := @ComboSelect;

    Parent := ScrollBox1;
  end;
  FKeyList.Add(Result);

  AddIndexComboBtn.AnchorVerticalCenterTo(result);
end;

function TKeyFieldsForm.DoDeleteKey: boolean;
var
  Cmb: TComboBox;
begin
  Cmb := TComboBox(FKeyList.Last);
  FKeyList.Delete(FKeyList.Count - 1);

  if FKeyList.Count = 0  then
  begin
    AddIndexComboBtn.Anchors := AddIndexComboBtn.Anchors - [akTop];
    AddIndexComboBtn.AnchorToNeighbour(akBottom, 3, TopBevel);
  end else
    AddIndexComboBtn.AnchorVerticalCenterTo(TControl(FKeyList.Last));
  Cmb.Free;

  IndexCheckError;
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
    MR := GetRelation;
    if (MR is TEpiDetailRelation) then
    begin
      MasterDF := TEpiDetailRelation(MR).MasterRelation.Datafile;
      Combo.Enabled := not (MasterDF.KeyFields.ItemExistsByName(Field.Name));
    end;
  end;
end;

procedure TKeyFieldsForm.AddFieldsToCombo(Combo: TComboBox);
var
  Flds: TEpiFields;
  i: Integer;
begin
  Combo.Clear;

  Flds := FDataFile.Fields;
  for i := 0 to Flds.Count - 1 do
  begin
    Combo.AddItem(
      Flds[i].Name + BoolToStr(Flds[i].Question.Text <> '', ': ' + Flds[i].Question.Text, ''),
      Flds[i]
    );
  end;
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

function TKeyFieldsForm.GetRelation: TEpiMasterRelation;
begin
  result := TEpiMasterRelation(FDataFile.FindCustomData(PROJECT_RELATION_KEY));
end;

constructor TKeyFieldsForm.Create(TheOwner: TComponent; Datafile: TEpiDataFile;
  ValueLabelSets: TEpiValueLabelSets);
var
  i: Integer;
begin
  inherited Create(TheOwner);

  FDataFile := Datafile;
  FValueLabelSets := ValueLabelSets;

  FIndexChecker := TEpiIntegrityChecker.Create;
  FKeyList := TList.Create;

  FHintWindow := THintWindow.Create(Self);
  FHintWindow.HideInterval := 5 * 1000;
  FHintWindow.AutoHide := true;

  with FDataFile do
    for i := 0 to KeyFields.Count - 1 do
      SetItemIndexOnField(DoAddNewKey, KeyFields[i]);
end;

destructor TKeyFieldsForm.Destroy;
begin
  FKeyList.Free;
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

