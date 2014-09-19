unit report_project_validation_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, Buttons, CheckLst,
  ExtCtrls, ActnList, report_types, report_base, epidocument, epicustombase,
  types, projectfilelist_frame, manager_types;

type

  { TProjectValidationFrame }

  TProjectValidationFrame = class(TFrame, IReportOptionFrame, ICanCloseQuery)
    CFExcludeTxtAction: TAction;
    CmpFExcludeTextFBtn: TButton;
    CFSelectNoneAction: TAction;
    CFSelectAllNonKFAction: TAction;
    KFMoveFieldDown: TAction;
    KFMoveFieldUp: TAction;
    KFAutoIncAction: TAction;
    KFIndexAction: TAction;
    KFNoneAction: TAction;
    ActionList1: TActionList;
    Bevel1: TBevel;
    CmpFAllNonKeyFBtn: TButton;
    CmpFNoneBtn: TButton;
    CmpFCheckList: TCheckListBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    KFAutoIncBtn: TButton;
    KFCheckList: TCheckListBox;
    KFIndexBtn: TButton;
    KFNoneBtn: TButton;
    Label7: TLabel;
    Label8: TLabel;
    OptionsChkGrp: TCheckGroup;
    Panel1: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    procedure CFExcludeTxtActionExecute(Sender: TObject);
    procedure CFSelectAllNonKFActionExecute(Sender: TObject);
    procedure CFSelectNoneActionExecute(Sender: TObject);
    procedure KFAutoIncActionExecute(Sender: TObject);
    procedure KFIndexActionExecute(Sender: TObject);
    procedure KFMoveFieldUpExecute(Sender: TObject);
    procedure KFMoveFieldUpUpdate(Sender: TObject);
    procedure KFNoneActionExecute(Sender: TObject);
  private
    { private declarations }
    FDoc:     TEpiDocument;
    procedure UpdateKeyFields;
    procedure UpdateCompareFields;
    procedure UpdateOptions;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    function  GetFrameCaption: string;
    procedure UpdateFrame(Selection: TStrings);
    procedure ApplyReportOptions(Report: TReportBase);
    function OkToAdvance(ProjectList: TProjectFileListFrame): boolean;
    function OkToAdvanceText: string;
    function CanClose: boolean;
  end;

implementation

{$R *.lfm}

uses
  epidatafiles, report_project_validation, epitools_projectvalidate,
  epidatafilestypes, epiglobals, LCLIntf, manager_messages, LCLType,
  main, math, LazUTF8, strutils, Dialogs;

{ TProjectValidationFrame }

procedure TProjectValidationFrame.KFNoneActionExecute(Sender: TObject);
begin
  KFCheckList.CheckAll(cbUnchecked, false, false);
end;

procedure TProjectValidationFrame.KFIndexActionExecute(Sender: TObject);
var
  i: Integer;
begin
  KFCheckList.CheckAll(cbUnchecked, false, false);

  KFCheckList.Items.BeginUpdate;
  for i := 0 to KFCheckList.Count - 1 do
    if FDoc.DataFiles[0].KeyFields.FieldExists(TEpiField(KFCheckList.Items.Objects[i])) then
      KFCheckList.Checked[i] := true;
  KFCheckList.Items.EndUpdate;
end;

procedure TProjectValidationFrame.KFMoveFieldUpExecute(Sender: TObject);
var
  Idx1: Integer;
  Idx2: Integer;
  Chk1: Boolean;
  Chk2: Boolean;
begin
  KFCheckList.Items.BeginUpdate;

  Idx1 := KFCheckList.ItemIndex;
  if Sender = KFMoveFieldUp then
    Idx2 := Idx1 - 1
  else
    Idx2 := Idx1 + 1;

  Chk1 := KFCheckList.Checked[Idx1];
  Chk2 := KFCheckList.Checked[Idx2];

  KFCheckList.Items.Exchange(Idx1, Idx2);
  KFCheckList.ItemIndex := Idx2;

  KFCheckList.Checked[Idx1] := Chk2;
  KFCheckList.Checked[Idx2] := Chk1;
  KFCheckList.Items.EndUpdate;
end;

procedure TProjectValidationFrame.KFMoveFieldUpUpdate(Sender: TObject);
begin
  if Sender = KFMoveFieldUp then
    TAction(Sender).Enabled := KFCheckList.ItemIndex > 0
  else
    TAction(Sender).Enabled := KFCheckList.ItemIndex < (KFCheckList.Count - 1);
end;

procedure TProjectValidationFrame.KFAutoIncActionExecute(Sender: TObject);
var
  i: Integer;
begin
  KFCheckList.CheckAll(cbUnchecked, false, false);

  KFCheckList.Items.BeginUpdate;
  for i := 0 to KFCheckList.Count - 1 do
    if TEpiField(KFCheckList.Items.Objects[i]).FieldType = ftAutoInc then
      KFCheckList.Checked[i] := true;
  KFCheckList.Items.EndUpdate;
end;

procedure TProjectValidationFrame.CFSelectAllNonKFActionExecute(Sender: TObject
  );
var
  O: TObject;
  Idx: Integer;
  i: Integer;
begin
  CmpFCheckList.CheckAll(cbUnchecked, false, false);

  CmpFCheckList.Items.BeginUpdate;
  for i := 0 to CmpFCheckList.Count -1 do
  begin
    O := CmpFCheckList.Items.Objects[i];
    Idx := KFCheckList.Items.IndexOfObject(O);
    if not KFCheckList.Checked[Idx] then
      CmpFCheckList.Checked[i] := true;
  end;
  CmpFCheckList.Items.EndUpdate;
end;

procedure TProjectValidationFrame.CFExcludeTxtActionExecute(Sender: TObject);
var
  i: Integer;
  F: TEpiField;
begin
  CmpFCheckList.Items.BeginUpdate;
  for i := 0 to CmpFCheckList.Count -1 do
  begin
    F := TEpiField(CmpFCheckList.Items.Objects[i]);
    if F.FieldType in StringFieldTypes then
      CmpFCheckList.Checked[i] := false;
  end;
  CmpFCheckList.Items.EndUpdate;
end;

procedure TProjectValidationFrame.CFSelectNoneActionExecute(Sender: TObject);
begin
  CmpFCheckList.CheckAll(cbUnchecked, false, false);
end;

procedure TProjectValidationFrame.UpdateKeyFields;
var
  DF: TEpiDataFile;
  i: Integer;
  F: TEpiField;
  W: Integer;
begin
  KFCheckList.Clear;

  DF := FDoc.DataFiles[0];

  W := 0;
  for i := 0 to DF.Fields.Count - 1 do
    W := Max(W, UTF8Length(DF.Field[i].Name));

  KFCheckList.Items.BeginUpdate;
  for i := 0 to DF.Fields.Count - 1 do
  begin
    F := DF.Fields[i];
    KFCheckList.AddItem(F.Name + DupeString(' ', W - UTF8Length(F.Name)) + ' - ' + F.Question.Text,  F);
  end;
  KFCheckList.Items.EndUpdate;

  if (KFIndexAction.Update) and (KFIndexAction.Enabled) then
    KFIndexAction.Execute
  else if (KFAutoIncAction.Update) and (KFAutoIncAction.Enabled) then
    KFAutoIncAction.Execute
  else
    KFCheckList.Selected[0] := true;
end;

procedure TProjectValidationFrame.UpdateCompareFields;
var
  DF: TEpiDataFile;
  i: Integer;
  W: Integer;
  F: TEpiField;
begin
  CmpFCheckList.Clear;

  DF := FDoc.DataFiles[0];

  W := 0;
  for i := 0 to DF.Fields.Count - 1 do
    W := Max(W, UTF8Length(DF.Field[i].Name));


  CmpFCheckList.Items.BeginUpdate;
  for i := 0 to DF.Fields.Count - 1 do
  begin
    F := DF.Fields[i];
    CmpFCheckList.AddItem(F.Name + DupeString(' ', W - UTF8Length(F.Name)) + ' - ' + F.Question.Text, F);
  end;
  CmpFCheckList.CheckAll(cbChecked, false, false);
  CmpFCheckList.Items.EndUpdate;
end;

procedure TProjectValidationFrame.UpdateOptions;
var
  i: Integer;
begin
  for i := 0 to OptionsChkGrp.Items.Count - 1 do
    OptionsChkGrp.Checked[i] := true;
end;

constructor TProjectValidationFrame.Create(TheOwner: TComponent);
var
  Option: TEpiToolsProjectValidateOption;
begin
  inherited Create(TheOwner);


  With OptionsChkGrp.Items do
  begin
    BeginUpdate;
    Clear;

    for Option in EpiProjectValidationOptionsSelectable do
      AddObject(EpiToolProjectValidationOptionText[Option], TObject(PtrInt(Option)));

    EndUpdate;
  end;
end;

function TProjectValidationFrame.GetFrameCaption: string;
begin
  result := 'Select all fields identifying an observation';
end;

procedure TProjectValidationFrame.UpdateFrame(Selection: TStrings);
begin
  if Selection.Count <> 1 then
    // Raise some error?
    Exit;

  FDoc := TEpiDocument(Selection.Objects[0]);

  UpdateKeyFields;
  UpdateCompareFields;
  UpdateOptions;
end;

procedure TProjectValidationFrame.ApplyReportOptions(Report: TReportBase);
var
  KF: TEpiFields;
  VF: TEpiFields;
  i: Integer;
  Opts: TEpiToolsProjectValidateOptions;
begin
  KF := TEpiFields.Create(nil);
  for i := 0 to KFCheckList.Count - 1 do
    if KFCheckList.Checked[i] then
      KF.AddItem(TEpiField(KFCheckList.Items.Objects[i]));

  VF := TEpiFields.Create(nil);
  for i := 0 to CmpFCheckList.Count - 1 do
    if CmpFCheckList.Checked[i] then
      VF.AddItem(TEpiField(CmpFCheckList.Items.Objects[i]));

  Opts := EpiProjectValidationOptionsAll;
  for i := 0 to OptionsChkGrp.Items.Count -1 do
    if not OptionsChkGrp.Checked[i] then
      Exclude(Opts, TEpiToolsProjectValidateOption(PtrInt(OptionsChkGrp.Items.Objects[i])));

{  with TReportProjectValidation(Report) do
  begin
    KeyFields := KF;
    ValidationFields := VF;
    Options := Opts;
  end;    }
end;

function TProjectValidationFrame.OkToAdvance(
  ProjectList: TProjectFileListFrame): boolean;
begin
  Result := ProjectList.SelectedList.Count = 1;
end;

function TProjectValidationFrame.OkToAdvanceText: string;
begin
  result := 'Only one project may be validated at a time!';
end;

function TProjectValidationFrame.CanClose: boolean;
var
  KFChecked: Boolean;
  Res: TModalResult;
  i: Integer;
begin
  Result := true;
  KFChecked := false;

  for i := 0 to KFCheckList.Count - 1 do
  begin
    KFChecked := KFChecked or KFCheckList.Checked[i];
    if KFChecked then break;
  end;

  if not KFChecked then
  begin
    Res := MessageDlg('Warning!',
             'No Key Fields selected! Report will be displayed in order of record number!' + LineEnding +
             'Press Cancel to select Key Fields!',
             mtWarning,
             mbOKCancel,
             0,
             mbCancel
           );
    if Res = mrCancel then
      Result := false
    else
      Result := true;
  end;
end;

end.

