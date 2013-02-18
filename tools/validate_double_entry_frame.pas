unit validate_double_entry_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, Buttons, CheckLst,
  ExtCtrls, ActnList, report_types, report_base, epidocument, epicustombase, types;

type

  { TValideDoubleEntryFrame }

  TValideDoubleEntryFrame = class(TFrame, IReportOptionFrame)
    CFSelectNoneAction: TAction;
    CFSelectAllNonKFAction: TAction;
    KFMoveFieldDown: TAction;
    KFMoveFieldUp: TAction;
    KFAutoIncAction: TAction;
    KFIndexAction: TAction;
    KFNoneAction: TAction;
    ActionList1: TActionList;
    Bevel1: TBevel;
    CmpFAutoDateTimeBtn: TButton;
    CmpFAutoIncBtn: TButton;
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
    procedure CFSelectAllNonKFActionExecute(Sender: TObject);
    procedure CFSelectNoneActionExecute(Sender: TObject);
    procedure KFAutoIncActionExecute(Sender: TObject);
    procedure KFIndexActionExecute(Sender: TObject);
    procedure KFMoveFieldUpExecute(Sender: TObject);
    procedure KFMoveFieldUpUpdate(Sender: TObject);
    procedure KFNoneActionExecute(Sender: TObject);
  private
    { private declarations }
    FMainDoc: TEpiDocument;
    FDupDoc: TEpiDocument;
    procedure UpdateKeyFields;
    procedure UpdateCompareFields;
    procedure AddFieldHook(Sender: TObject; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
  public
    { public declarations }
    function  GetFrameCaption: string;
    procedure UpdateFrame(Selection: TStrings);
    procedure ApplyReportOptions(Report: TReportBase);
    function CanClose: boolean;
  end;

implementation

{$R *.lfm}

uses
  epidatafiles, report_double_entry_validation, epitools_val_dbl_entry,
  epidatafilestypes, epiglobals, LCLIntf, manager_messages, LCLType,
  main;

{ TValideDoubleEntryFrame }

procedure TValideDoubleEntryFrame.KFNoneActionExecute(Sender: TObject);
begin
  KFCheckList.CheckAll(cbUnchecked, false, false);
end;

procedure TValideDoubleEntryFrame.KFIndexActionExecute(Sender: TObject);
var
  i: Integer;
begin
  KFCheckList.CheckAll(cbUnchecked, false, false);

  KFCheckList.Items.BeginUpdate;
  for i := 0 to KFCheckList.Count - 1 do
    if FMainDoc.DataFiles[0].KeyFields.FieldExists(TEpiField(KFCheckList.Items.Objects[i])) then
      KFCheckList.Checked[i] := true;
  KFCheckList.Items.EndUpdate;
end;

procedure TValideDoubleEntryFrame.KFMoveFieldUpExecute(Sender: TObject);
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

procedure TValideDoubleEntryFrame.KFMoveFieldUpUpdate(Sender: TObject);
begin
  if Sender = KFMoveFieldUp then
    TAction(Sender).Enabled := KFCheckList.ItemIndex > 0
  else
    TAction(Sender).Enabled := KFCheckList.ItemIndex < (KFCheckList.Count - 1);
end;

procedure TValideDoubleEntryFrame.KFAutoIncActionExecute(Sender: TObject);
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

procedure TValideDoubleEntryFrame.CFSelectAllNonKFActionExecute(Sender: TObject
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

procedure TValideDoubleEntryFrame.CFSelectNoneActionExecute(Sender: TObject);
begin
  CmpFCheckList.CheckAll(cbUnchecked, false, false);
end;

procedure TValideDoubleEntryFrame.UpdateKeyFields;
var
  MainDF: TEpiDataFile;
  DupDF: TEpiDataFile;
  i: Integer;
begin
  KFCheckList.Clear;

  MainDF := FMainDoc.DataFiles[0];
  DupDF  := FDupDoc.DataFiles[0];

  KFCheckList.Items.BeginUpdate;
  for i := 0 to MainDF.Fields.Count - 1 do
  begin
    if DupDF.Fields.ItemExistsByName(MainDF.Fields[i].Name) then
      KFCheckList.AddItem(MainDF.Fields[i].Name, MainDF.Fields[i]);
  end;
  KFCheckList.Items.EndUpdate;

  if (KFIndexAction.Update) and (KFIndexAction.Enabled) then
    KFIndexAction.Execute
  else if (KFAutoIncAction.Update) and (KFAutoIncAction.Enabled) then
    KFAutoIncAction.Execute
  else
    KFCheckList.Selected[0] := true;
end;

procedure TValideDoubleEntryFrame.UpdateCompareFields;
var
  MainDF: TEpiDataFile;
  DupDF: TEpiDataFile;
  i: Integer;
begin
  CmpFCheckList.Clear;

  MainDF := FMainDoc.DataFiles[0];
  DupDF  := FDupDoc.DataFiles[0];

  CmpFCheckList.Items.BeginUpdate;
  for i := 0 to MainDF.Fields.Count - 1 do
  begin
    if DupDF.Fields.ItemExistsByName(MainDF.Fields[i].Name) then
      CmpFCheckList.AddItem(MainDF.Fields[i].Name, MainDF.Fields[i]);
  end;
  CmpFCheckList.Items.EndUpdate;
end;

procedure TValideDoubleEntryFrame.AddFieldHook(Sender: TObject;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  if (EventGroup <> eegCustomBase) then exit;

  case TEpiCustomChangeEventType(EventType) of
    ecceDestroy: ;
    ecceUpdate: ;
    ecceName:
      begin
        if TEpiField(Sender).Name = EpiDoubleEntryFieldName then
          PostMessage(MainForm.Handle, LM_DESIGNER_ADD, WPARAM(Sender), 0);
        TEpiField(Sender).UnRegisterOnChangeHook(@AddFieldHook);
      end;
    ecceAddItem:
      TEpiField(Data).RegisterOnChangeHook(@AddFieldHook, true);
    ecceDelItem:
      TEpiField(Data).UnRegisterOnChangeHook(@AddFieldHook);
    ecceSetItem: ;
    ecceSetTop: ;
    ecceSetLeft: ;
    ecceText: ;
  end;
end;

function TValideDoubleEntryFrame.GetFrameCaption: string;
begin
  result := 'Double Entry Validation Options';
end;

procedure TValideDoubleEntryFrame.UpdateFrame(Selection: TStrings);
begin
  if Selection.Count <> 2 then
    // Raise some error?
    Exit;

  FMainDoc := TEpiDocument(Selection.Objects[0]);
  FDupDoc  := TEpiDocument(Selection.Objects[1]);

  FMainDoc.DataFiles[0].Fields.RegisterOnChangeHook(@AddFieldHook, true);

  UpdateKeyFields;
  UpdateCompareFields;
end;

procedure TValideDoubleEntryFrame.ApplyReportOptions(Report: TReportBase);
var
  KF: TEpiFields;
  CF: TEpiFields;
  i: Integer;
  Options: TEpiToolsDblEntryValidateOptions;
begin
  KF := TEpiFields.Create(nil);
  for i := 0 to KFCheckList.Count - 1 do
    if KFCheckList.Checked[i] then
      KF.AddItem(TEpiField(KFCheckList.Items.Objects[i]));

  CF := TEpiFields.Create(nil);
  for i := 0 to CmpFCheckList.Count - 1 do
    if CmpFCheckList.Checked[i] then
      CF.AddItem(TEpiField(CmpFCheckList.Items.Objects[i]));

  Options := [];
  if OptionsChkGrp.Checked[0] then Include(Options, devIgnoreDeleted);
  if OptionsChkGrp.Checked[1] then Include(Options, devCaseSensitiveText);
  if OptionsChkGrp.Checked[2] then Include(Options, devIgnoreMissingRecords);
  if OptionsChkGrp.Checked[3] then Include(Options, devAddResultToField);

  with TReportDoubleEntryValidation(Report) do
  begin
    KeyFields := KF;
    CompareFields := CF;
    DblEntryValidateOptions := Options;
  end;
end;

function TValideDoubleEntryFrame.CanClose: boolean;
begin
  result := true;
end;

end.

