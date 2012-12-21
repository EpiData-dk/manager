unit validate_double_entry_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  CheckLst, ExtCtrls, Buttons, ActnList, ComCtrls, epidocument, epicustombase;

type

  { TValidateDoubleEntryForm }

  TValidateDoubleEntryForm = class(TForm)
    CFAllAction: TAction;
    CFTextAction: TAction;
    CFAutoIncAction: TAction;
    CFAutoTimeDateAction: TAction;
    KFAutoIncAction: TAction;
    KFIndexAction: TAction;
    KFNoneAction: TAction;
    ActionList1: TActionList;
    Bevel1: TBevel;
    OkBtn: TBitBtn;
    CancelBtn: TBitBtn;
    OptionsChkGrp: TCheckGroup;
    CmpFAllBtn: TButton;
    CmpFAutoIncBtn: TButton;
    CmpFTextBtn: TButton;
    KFNoneBtn: TButton;
    KFIndexBtn: TButton;
    KFAutoIncBtn: TButton;
    CmpFAutoDateTimeBtn: TButton;
    KFCheckList: TCheckListBox;
    CmpFCheckList: TCheckListBox;
    FileNameLabel1: TLabel;
    FileNameLabel2: TLabel;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    ProjectTitle1: TLabel;
    ProjectTitle2: TLabel;
    RecordCount1: TLabel;
    RecordCount2: TLabel;
    OpenDblEntryFileBtn: TSpeedButton;
    procedure CFAllActionExecute(Sender: TObject);
    procedure CFAutoIncActionExecute(Sender: TObject);
    procedure CFAutoTimeDateActionExecute(Sender: TObject);
    procedure CFTextActionExecute(Sender: TObject);
    procedure KFAutoIncActionExecute(Sender: TObject);
    procedure KFAutoIncActionUpdate(Sender: TObject);
    procedure KFIndexActionExecute(Sender: TObject);
    procedure KFIndexActionUpdate(Sender: TObject);
    procedure KFNoneActionExecute(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure OpenDblEntryFileBtnClick(Sender: TObject);
  private
    { private declarations }
    FMainDoc: TEpiDocument;
    FDupDoc: TEpiDocument;
    FCmpAutoTimeDateSelected: boolean;
    FCmpAutoIncSelected: boolean;
    FCmpTextSelected: boolean;
    procedure AddFieldHook(Sender: TObject; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
    procedure ShowMainEmpty(Data: PtrInt);
    procedure UpdateKeyFields;
    procedure UpdateCompareFields;
    procedure UpdateMainDocInfo(Const Fn: string);
    procedure UpdateDupDocInfo(Const Fn: string);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; MainDoc: TEpiDocument; Const FileName: string);
    destructor Destroy; override;
  end;

procedure ValidateDoubleEntry(Doc: TEpiDocument; Const Filename: string);

implementation

{$R *.lfm}

uses
  epidatafiles, epidatafilestypes, epimiscutils, settings2_var,
  epitools_val_dbl_entry, viewer_form, report_double_entry_validation,
  main, epireport_generator_txt, manager_messages, epiglobals,
  LCLIntf, LCLType, managerprocs;

procedure ValidateDoubleEntry(Doc: TEpiDocument; const Filename: string);
var
  F: TValidateDoubleEntryForm;
begin
  F := TValidateDoubleEntryForm.Create(nil, Doc, Filename);
  F.ShowModal;
  F.Free;
end;

{ TValidateDoubleEntryForm }

procedure TValidateDoubleEntryForm.OpenDblEntryFileBtnClick(Sender: TObject);
var
  Dlg: TOpenDialog;
begin
  Dlg := TOpenDialog.Create(Self);
  Dlg.Filter := GetEpiDialogFilter([dfEPX,dfEPZ]);
  Dlg.Options := [ofPathMustExist,   // shows an error message if selected path does not exist
                  ofFileMustExist,   // shows an error message if selected file does not exist
                  ofEnableSizing,    // dialog can be resized, e.g. via the mouse
                  ofViewDetail      // details are OS and interface dependent
                 ];
  Dlg.InitialDir := ManagerSettings.WorkingDirUTF8;

  try
    if not Dlg.Execute then exit;

    FDupDoc := TOpenEpiDoc.OpenDoc(Dlg.FileName, ManagerSettings.StudyLang);

    UpdateDupDocInfo(Dlg.FileName);
    UpdateKeyFields;
    UpdateCompareFields;
  finally
    Dlg.Free;
  end;
end;

procedure TValidateDoubleEntryForm.ShowMainEmpty(Data: PtrInt);
var
  S: String;
begin
  S := 'Main Project File ';
  if Data = 1 then
    S := 'Dublicate Entry File ';

  MessageDlg('Warning', S + 'contains NO DATA',
             mtWarning, [mbOK], 0, mbOK);
end;

procedure TValidateDoubleEntryForm.KFNoneActionExecute(Sender: TObject);
begin
  KFCheckList.CheckAll(cbUnchecked, false, false);
end;

procedure TValidateDoubleEntryForm.OkBtnClick(Sender: TObject);
var
  H: TReportViewerForm;
  S: TStringList;
  KeyFields: TEpiFields;
  R: TReportDoubleEntryValidation;
  CmpFields: TEpiFields;
  i: Integer;
  Options: TEpiToolsDblEntryValidateOptions;
begin
  S := TStringList.Create;
  S.AddObject(FileNameLabel1.Caption, FMainDoc);
  S.AddObject(FileNameLabel2.Caption, FDupDoc);
  R := TReportDoubleEntryValidation.Create(S, TEpiReportTXTGenerator);

  KeyFields := TEpiFields.Create(nil);
  for i := 0 to KFCheckList.Count - 1 do
    if KFCheckList.Checked[i] then
      KeyFields.AddItem(TEpiField(KFCheckList.Items.Objects[i]));
  R.KeyFields := KeyFields;

  CmpFields := TEpiFields.Create(nil);
  for i := 0 to CmpFCheckList.Count - 1 do
    if CmpFCheckList.Checked[i] then
      CmpFields.AddItem(TEpiField(CmpFCheckList.Items.Objects[i]));
  R.CompareFields := CmpFields;

  Options := [];
  if OptionsChkGrp.Checked[0] then Include(Options, devIgnoreDeleted);
  if OptionsChkGrp.Checked[1] then Include(Options, devCaseSensitiveText);
  if OptionsChkGrp.Checked[2] then Include(Options, devIgnoreMissingRecords);
  if OptionsChkGrp.Checked[3] then Include(Options, devAddResultToField);
  R.DblEntryValidateOptions := Options;

  ShowReportForm(MainForm,
    'Report of: ' + R.ReportTitle,
    R.RunReport);
end;

procedure TValidateDoubleEntryForm.KFIndexActionExecute(Sender: TObject);
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

procedure TValidateDoubleEntryForm.KFAutoIncActionExecute(Sender: TObject);
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

procedure TValidateDoubleEntryForm.CFAutoTimeDateActionExecute(Sender: TObject);
var
  i: Integer;
begin
  FCmpAutoTimeDateSelected := not FCmpAutoTimeDateSelected;
  for i := 0 to CmpFCheckList.Count - 1 do
    if TEpiField(CmpFCheckList.Items.Objects[i]).FieldType in AutoUpdateFieldTypes then
      CmpFCheckList.Checked[i] := FCmpAutoTimeDateSelected;
end;

procedure TValidateDoubleEntryForm.CFTextActionExecute(Sender: TObject);
var
  i: Integer;
begin
  FCmpTextSelected := not FCmpTextSelected;
  for i := 0 to CmpFCheckList.Count - 1 do
    if TEpiField(CmpFCheckList.Items.Objects[i]).FieldType in AutoUpdateFieldTypes then
      CmpFCheckList.Checked[i] := FCmpTextSelected;
end;

procedure TValidateDoubleEntryForm.CFAutoIncActionExecute(Sender: TObject);
var
  i: Integer;
begin
  FCmpAutoIncSelected := not FCmpAutoIncSelected;
  for i := 0 to CmpFCheckList.Count - 1 do
    if TEpiField(CmpFCheckList.Items.Objects[i]).FieldType in AutoUpdateFieldTypes then
      CmpFCheckList.Checked[i] := FCmpAutoIncSelected;
end;

procedure TValidateDoubleEntryForm.CFAllActionExecute(Sender: TObject);
begin
  CmpFCheckList.CheckAll(cbChecked, false, false);
end;

procedure TValidateDoubleEntryForm.AddFieldHook(Sender: TObject;
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

procedure TValidateDoubleEntryForm.KFAutoIncActionUpdate(Sender: TObject);
var
  Res: Boolean;
  i: Integer;
begin
  Res := false;
  for i := 0 to FMainDoc.DataFiles[0].Fields.Count - 1 do
    if FMainDoc.DataFiles[0].Fields[i].FieldType = ftAutoInc then
    begin
      Res := true;
      Break;
    end;
  TAction(Sender).Enabled := Res;
end;

procedure TValidateDoubleEntryForm.KFIndexActionUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := FMainDoc.DataFiles[0].KeyFields.Count > 0;
end;

procedure TValidateDoubleEntryForm.UpdateKeyFields;
var
  MainDF: TEpiDataFile;
  DupDF: TEpiDataFile;
  i: Integer;
begin
  KFCheckList.Clear;
  if not Assigned(FDupDoc) then exit;

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
    KFAutoIncAction.Execute;
end;

procedure TValidateDoubleEntryForm.UpdateCompareFields;
var
  MainDF: TEpiDataFile;
  DupDF: TEpiDataFile;
  i: Integer;
begin
  CmpFCheckList.Clear;
  if not Assigned(FDupDoc) then exit;

  MainDF := FMainDoc.DataFiles[0];
  DupDF  := FDupDoc.DataFiles[0];

  CmpFCheckList.Items.BeginUpdate;
  for i := 0 to MainDF.Fields.Count - 1 do
  begin
    if DupDF.Fields.ItemExistsByName(MainDF.Fields[i].Name) then
      CmpFCheckList.AddItem(MainDF.Fields[i].Name, MainDF.Fields[i]);
  end;
  CmpFCheckList.Items.EndUpdate;

  CFAllAction.Execute;
  CFAutoIncAction.Execute;
  CFAutoTimeDateAction.Execute;
  CFTextAction.Execute;
end;

procedure TValidateDoubleEntryForm.UpdateMainDocInfo(const Fn: string);
begin
  FileNameLabel1.Caption := Fn;
  if Assigned(FMainDoc) then
  begin
    ProjectTitle1.Caption := FMainDoc.Study.Title.Text;
    RecordCount1.Caption := IntToSTr(FMainDoc.DataFiles[0].Size);
  end;

  if FMainDoc.DataFiles[0].Size = 0 then
    Application.QueueAsyncCall(@ShowMainEmpty, 0);
end;

procedure TValidateDoubleEntryForm.UpdateDupDocInfo(const Fn: string);
begin
  if FN <> '' then
  begin
    FileNameLabel2.Font.Color := clDefault;
    FileNameLabel2.Caption := Fn;
    OkBtn.Enabled := True;
  end else begin
    FileNameLabel2.Font.Color := clRed;
    FileNameLabel2.Caption := '[Please open file. Click folder icon on right ->]';
    OkBtn.Enabled := False;
  end;

  if Assigned(FDupDoc) then
  begin
    ProjectTitle2.Caption := FDupDoc.Study.Title.Text;
    RecordCount2.Caption := IntToSTr(FDupDoc.DataFiles[0].Size);
    if FDupDoc.DataFiles[0].Size = 0 then
      Application.QueueAsyncCall(@ShowMainEmpty, 1);
  end else begin
    ProjectTitle2.Caption := '';
    RecordCount2.Caption := '';
  end;
end;

constructor TValidateDoubleEntryForm.Create(TheOwner: TComponent;
  MainDoc: TEpiDocument; const FileName: string);
begin
  inherited Create(TheOwner);
  FMainDoc := MainDoc;
  FCmpAutoTimeDateSelected := true;
  FCmpAutoIncSelected      := true;
  FCmpTextSelected         := true;

  UpdateMainDocInfo(FileName);
  UpdateDupDocInfo('');

  FMainDoc.DataFiles[0].Fields.RegisterOnChangeHook(@AddFieldHook, true);
end;

destructor TValidateDoubleEntryForm.Destroy;
begin
  FMainDoc.DataFiles[0].Fields.UnRegisterOnChangeHook(@AddFieldHook);
  inherited Destroy;
end;

end.

