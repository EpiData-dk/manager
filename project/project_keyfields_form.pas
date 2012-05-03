unit project_keyfields_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, epiintegritycheck, epidatafiles, epidocument;

type

  { TKeyFieldsForm }

  TKeyFieldsForm = class(TForm)
    TopBevel: TBevel;
    RightBevel: TBevel;
    Bevel3: TBevel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    ShowRecordsBtn: TButton;
    RealTimeStatusChkBox: TCheckBox;
    Label1: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    AddIndexBtn: TSpeedButton;
    RemoveIndexBtn: TSpeedButton;
    procedure AddIndexBtnClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure RealTimeStatusChkBoxChange(Sender: TObject);
    procedure RemoveIndexBtnClick(Sender: TObject);
    procedure ShowRecordsBtnClick(Sender: TObject);
  private
    { private declarations }
    FIndexChecker: TEpiIntegrityChecker;
    FKeyList: TList;
    FHintWindow: THintWindow;
    FEpiDoc: TEpiDocument;
    function  DoAddNewKey: TComboBox;
    procedure SetItemIndexOnField(Combo: TComboBox; Field: TEpiField);
    procedure AddFieldsToCombo(Combo: TComboBox);
    procedure ComboSelect(Sender: TObject);
    function  GetFieldList: TEpiFields;
    procedure IndexCheckError;
    function  PerformIndexCheck(Out FailedRecords: TBoundArray): Boolean;
    function  ShowError(Const Msg: string; Const Ctrl: TControl): boolean;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; EpiDoc: TEpiDocument);
    destructor Destroy; override;
  end;

var
  KeyFieldsForm: TKeyFieldsForm;

implementation

{$R *.lfm}

uses
  types, datasetviewer_frame;

{ TKeyFieldsForm }

procedure TKeyFieldsForm.AddIndexBtnClick(Sender: TObject);
begin
  DoAddNewKey;
end;

procedure TKeyFieldsForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  FailedRecords: TBoundArray;
  Res: Integer;
  FL: TEpiFields;
  i: Integer;
begin
  if (ModalResult = mrOK) and (not PerformIndexCheck(FailedRecords)) then
  begin
    Res := MessageDlg('Index Error',
                        'Index integrity check failed.' + LineEnding +
                        'Are you sure you what to apply index which contains duplicates?',
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
    FEpiDoc.DataFiles[0].KeyFields.Clear;
    for i := 0 to Fl.Count - 1 do
      FEpiDoc.DataFiles[0].KeyFields.AddItem(Fl[i]);
    Fl.Free;
  end;
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
begin
  if not RealTimeStatusChkBox.Checked then exit;

  if not PerformIndexCheck(FailedRecords) then
  begin
    ShowError('Warning: Index not uniquely defined!' + LineEnding +
              'Use "List Records" to get a list of affected records (' + IntToStr(Length(FailedRecords)) + ').',
              TComboBox(FKeyList.Last));
    ShowRecordsBtn.Enabled := true;
  end else
    ShowRecordsBtn.Enabled := false;
end;

function TKeyFieldsForm.PerformIndexCheck(out FailedRecords: TBoundArray
  ): Boolean;
var
  FieldList: TEpiFields;
begin
  FieldList := GetFieldList;

  Result :=
    FIndexChecker.IndexIntegrity(FEpiDoc.DataFiles[0], FailedRecords,
      false, FieldList);

  FieldList.Free;
end;

procedure TKeyFieldsForm.RemoveIndexBtnClick(Sender: TObject);
var
  Cmb: TComboBox;
begin
  Cmb := TComboBox(FKeyList.Last);
  FKeyList.Delete(FKeyList.Count - 1);

  if FKeyList.Count = 0  then
  begin
    AddIndexBtn.Anchors := AddIndexBtn.Anchors - [akTop];
    AddIndexBtn.AnchorToNeighbour(akBottom, 3, TopBevel);
    RemoveIndexBtn.Enabled := false;
  end else
    AddIndexBtn.AnchorVerticalCenterTo(TControl(FKeyList.Last));
  Cmb.Free;

  IndexCheckError;
end;

procedure TKeyFieldsForm.ShowRecordsBtnClick(Sender: TObject);
var
  FieldList: TEpiFields;
  i: Integer;
  Cmb: TComboBox;
  F: TForm;
  V: TDataSetViewFrame;
  FailedRecords: TBoundArray;
begin
  FieldList := TEpiFields.Create(nil);
  for i := 0 to FKeyList.Count - 1 do
  begin
    Cmb := TComboBox(FKeyList[i]);
    if Cmb.ItemIndex = -1 then  continue;

    FieldList.AddItem(TEpiField(Cmb.Items.Objects[Cmb.ItemIndex]));
  end;

  if not FIndexChecker.IndexIntegrity(FEpiDoc.DataFiles[0], FailedRecords,
           false, FieldList) then
  begin
    F := TForm.Create(Self);
    V := TDataSetViewFrame.Create(F, FEpiDoc.DataFiles[0]);
    V.Align := alClient;
    V.Parent := F;
    V.ShowRecords(FailedRecords);
    F.ShowModal;
    F.Free;
  end;
end;

function TKeyFieldsForm.DoAddNewKey: TComboBox;
begin
  result := TComboBox.Create(Panel2);

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

    Parent := Panel2;
  end;
  FKeyList.Add(Result);

  AddIndexBtn.AnchorVerticalCenterTo(result);
  RemoveIndexBtn.Enabled := true;
end;

procedure TKeyFieldsForm.SetItemIndexOnField(Combo: TComboBox; Field: TEpiField
  );
var
  Idx: Integer;
begin
  Idx := Combo.Items.IndexOfObject(Field);
  if Idx <> -1 then
    Combo.ItemIndex := Idx;
end;

procedure TKeyFieldsForm.AddFieldsToCombo(Combo: TComboBox);
var
  Flds: TEpiFields;
  i: Integer;
begin
  Combo.Clear;

  Flds := FEpiDoc.DataFiles[0].Fields;
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

constructor TKeyFieldsForm.Create(TheOwner: TComponent; EpiDoc: TEpiDocument);
var
  i: Integer;
begin
  inherited Create(TheOwner);
  FEpiDoc := EpiDoc;
  FIndexChecker := TEpiIntegrityChecker.Create;
  FKeyList := TList.Create;
  FHintWindow := THintWindow.Create(Self);
  FHintWindow.HideInterval := 5 * 1000;
  FHintWindow.AutoHide := true;

  with FEpiDoc.DataFiles[0] do
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

end.

