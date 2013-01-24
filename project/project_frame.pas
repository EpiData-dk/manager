unit project_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, ExtCtrls, ComCtrls, ActnList,
  Controls, Dialogs, epidocument, epidatafiles, epicustombase,
  manager_messages, LMessages;

type

  { TProjectFrame }

  TProjectFrame = class(TFrame)
    StudyInformationAction: TAction;
    KeyFieldsAction: TAction;
    ProjectPasswordAction: TAction;
    OpenProjectAction: TAction;
    ValueLabelEditorAction: TAction;
    ProjectSettingsAction: TAction;
    DeleteDataFormAction: TAction;
    SaveProjectAsAction: TAction;
    SaveProjectAction: TAction;
    ProjectImageList: TImageList;
    NewDataFormAction: TAction;
    ActionList1: TActionList;
    ProjectPanel: TPanel;
    DataFilesTreeView: TTreeView;
    ToolBar1: TToolBar;
    OpenProjectToolBtn: TToolButton;
    ToolButton1: TToolButton;
    SaveProjectToolBtn: TToolButton;
    SaveProjectAsToolBtn: TToolButton;
    ToolButton4: TToolButton;
    AddDataFormToolBtn: TToolButton;
    DeleteDataFormToolBtn: TToolButton;
    ToolButton7: TToolButton;
    procedure KeyFieldsActionExecute(Sender: TObject);
    procedure NewDataFormActionExecute(Sender: TObject);
    procedure OpenProjectActionExecute(Sender: TObject);
    procedure ProjectPasswordActionExecute(Sender: TObject);
    procedure ProjectSettingsActionExecute(Sender: TObject);
    procedure SaveProjectActionExecute(Sender: TObject);
    procedure SaveProjectAsActionExecute(Sender: TObject);
    procedure StudyInformationActionExecute(Sender: TObject);
    procedure ValueLabelEditorActionExecute(Sender: TObject);
  private
    { private declarations }
    FFileName: string;
    FFileTimeStamp: longint;
    FActiveFrame: TFrame;
    FModified: Boolean;
    FOnModified: TNotifyEvent;
    FrameCount: integer;
    FEpiDocument: TEpiDocument;
    procedure OnDataFileChange(Sender: TObject; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
    procedure OnTitleChange(Sender: TObject; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
    function  DoCreateNewDocument: TEpiDocument;
    function  NewDataFileItem(Sender: TEpiCustomList; DefaultItemClass: TEpiCustomItemClass): TEpiCustomItemClass;
    procedure AddToRecent(Const AFileName: string);
    procedure DoSaveProject(AFileName: string);
    procedure DoOpenProject(Const AFileName: string);
    procedure DoNewDataForm(Df: TEpiDataFile);
    procedure DoCloseProject;
    {$IFDEF EPI_DEBUG_CONTENT}
    procedure AddDebugingContent;
    {$ENDIF}
    procedure EpiDocumentModified(Sender: TObject);
    procedure SaveDlgTypeChange(Sender: TObject);
    procedure SetModified(const AValue: Boolean);
    procedure SetOnModified(const AValue: TNotifyEvent);
    procedure UpdateCaption;
  private
    { Backup }
    FBackupTimer: TTimer;
    function  InitBackupTimer: boolean;
    procedure UpdateTimer;
    procedure TimedBackup(Sender: TObject);
    procedure UpdateShortCuts;
  private
    { Messages }
    // Message relaying...
    procedure LMDesignerAdd(var Msg: TLMessage); message LM_DESIGNER_ADD;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   CloseQuery(var CanClose: boolean);
    procedure   RestoreDefaultPos;
    procedure   UpdateFrame;
    procedure   OpenProject(Const AFileName: string);
    property   EpiDocument: TEpiDocument read FEpiDocument;
    property   ActiveFrame: TFrame read FActiveFrame;
    property   ProjectFileName: string read FFileName write FFileName;
    property   Modified: Boolean read FModified write SetModified;
    property   OnModified: TNotifyEvent read FOnModified write SetOnModified;
  end;

implementation

{$R *.lfm}

uses
  design_runtimedesigner, Clipbrd, epimiscutils,
  main, settings2, settings2_var, epistringutils,
  valuelabelseditor_form2,
  managerprocs, Menus, LCLType, LCLIntf, project_settings,
  shortcuts, project_keyfields_form, project_studyunit_form,
  align_form;

type

  { TEpiDataFileEx }

  TEpiDataFileEx = class(TEpiDataFile)
  private
    FTreeNode: TTreeNode;
  public
    property TreeNode: TTreeNode read FTreeNode write FTreeNode;
  end;

{ TProjectFrame }

procedure TProjectFrame.NewDataFormActionExecute(Sender: TObject);
var
  Df: TEpiDataFile;
begin
  inc(FrameCount);

  Df := EpiDocument.DataFiles.NewDataFile;
  Df.Caption.Text := 'Dataform ' + IntToStr(FrameCount);
  EpiDocument.Modified := false;

  DoNewDataForm(Df);
  UpdateCaption;
end;

procedure TProjectFrame.KeyFieldsActionExecute(Sender: TObject);
var
  F: TKeyFieldsForm;
begin
  F := TKeyFieldsForm.Create(Self, EpiDocument);
  F.ShowModal;
  F.Free;
end;

procedure TProjectFrame.OpenProjectActionExecute(Sender: TObject);
begin
  PostMessage(MainForm.Handle, LM_MAIN_OPENPROJECT, 0, 0);
end;

procedure TProjectFrame.ProjectPasswordActionExecute(Sender: TObject);
var
  PW: String;
  PW2: String;
  Header: String;
begin
  PW := '';
  PW2 := '';
  Header := 'Set Project Password';

  if not InputQuery(Header, 'Enter New Project Password' + LineEnding + '(Press enter to clear):', True, PW)
  then
    Exit;
  if PW <> '' then
    PW2 := PasswordBox(Header, 'Re-enter Password:');

  if PW = PW2 then
  begin
    EpiDocument.PassWord := PW;
    if PW <> '' then
      MessageDlg(Header, 'Password successfully set!', mtInformation, [mbOK], 0)
    else
      MessageDlg(Header, 'Password successfully reset!', mtInformation, [mbOK], 0);
  end else
    MessageDlg(Header, 'The two passwords are not identical!' + LineEnding + 'Password NOT set.', mtError, [mbOK], 0);
end;

procedure TProjectFrame.ProjectSettingsActionExecute(Sender: TObject);
var
  ProjectSettings: TProjectSettingsForm;
begin
  ProjectSettings := TProjectSettingsForm.Create(self, EpiDocument);
  ProjectSettings.ShowModal;
  ProjectSettings.Free;

  TRuntimeDesignFrame(ActiveFrame).UpdateFrame;
  UpdateTimer;
end;

procedure TProjectFrame.SaveProjectActionExecute(Sender: TObject);
var
  Res: LongInt;
begin
  if ProjectFileName = '' then
    SaveProjectAsAction.Execute
  else
    try
      if (FileExistsUTF8(ProjectFileName)) and
         (FileAgeUTF8(ProjectFileName) <> FFileTimeStamp) then
      begin
        Res := MessageDlg('WARNING', 'Project file: ' + ProjectFileName + ' has been modified by another program since last save.' + LineEnding+
                          'Backup existing file?', mtWarning, mbYesNoCancel, 0, mbCancel);
        case Res of
          mrYes:    CopyAndBackup(ProjectFileName);
          mrCancel: Exit;
          mrNo:     ; // do nothing.
        end;
      end;
      DoSaveProject(ProjectFileName);
    except
      on E: EFCreateError do
        begin
          MessageDlg('Error',
            'Unable to save project to:' + LineEnding +
            ProjectFileName + LineEnding +
            'Error message: ' + E.Message,
            mtError, [mbOK], 0);
          Exit;
        end;
    end;
  EpiDocument.Modified := false;
  UpdateCaption;
end;

procedure TProjectFrame.SaveDlgTypeChange(Sender: TObject);
var
  Dlg: TSaveDialog absolute Sender;
  S: String;
begin
  case Dlg.FilterIndex of
    1: Dlg.DefaultExt := 'epx';
    2: Dlg.DefaultExt := 'epz';
  end;

  // TODO : Must be changed when supporting multiple desinger frames (take only the topmost/first datafile).
  if (Dlg.FileName = '') and
     (ProjectFileName = '') and
     (TRuntimeDesignFrame(ActiveFrame).ImportedFileName <> '') then
    Dlg.FileName := ChangeFileExt(TRuntimeDesignFrame(ActiveFrame).ImportedFileName, Dlg.DefaultExt);
end;

procedure TProjectFrame.SaveProjectAsActionExecute(Sender: TObject);
var
  Dlg: TSaveDialog;
  Fn: String;
begin
  Dlg := TSaveDialog.Create(Self);
  Dlg.Filter := GetEpiDialogFilter([dfEPX, dfEPZ]);
  Dlg.InitialDir := ManagerSettings.WorkingDirUTF8;
  Dlg.FilterIndex := ManagerSettings.SaveType + 1;
  SaveDlgTypeChange(Dlg);
  Dlg.OnTypeChange := @SaveDlgTypeChange;
  Dlg.Options := Dlg.Options + [ofOverwritePrompt, ofExtensionDifferent];

  if not Dlg.Execute then exit;
  Fn := Dlg.FileName;
  Dlg.Free;

  try
    DoSaveProject(Fn);
  except
    on E: EFCreateError do
      begin
        MessageDlg('Error',
          'Unable to save project to:' + LineEnding +
          Fn + LineEnding +
          'Error message: ' + E.Message,
          mtError, [mbOK], 0);
        Exit;
      end;
  end;

  if Fn <> ProjectFileName then
    FFileName := Fn;

  EpiDocument.Modified := false;
  UpdateCaption;
end;

procedure TProjectFrame.StudyInformationActionExecute(Sender: TObject);
var
  F: TStudyUnitForm;
begin
  F := TStudyUnitForm.Create(Self, EpiDocument.Study);
  F.ShowModal;
  F.Free;
end;

procedure TProjectFrame.ValueLabelEditorActionExecute(Sender: TObject);
begin
  ShowValueLabelEditor2(EpiDocument.ValueLabelSets);
end;

procedure TProjectFrame.OnDataFileChange(Sender: TObject;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
var
  Df: TEpiDataFileEx;
begin
  if (EventGroup = eegCustomBase) and (EventType = Word(ecceText)) then
  begin
    Df := TEpiDataFileEx(TEpiCustomItem(Sender).Owner);
    Df.TreeNode.Text := Df.Caption.Text;
  end;
end;

procedure TProjectFrame.OnTitleChange(Sender: TObject;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  UpdateCaption;
end;

function TProjectFrame.DoCreateNewDocument: TEpiDocument;
begin
  Result := TEpiDocument.Create(ManagerSettings.StudyLang);

  with Result.ProjectSettings, ManagerSettings do
  begin
    BackupInterval    := TimedRecoveryInterval;
    BackupOnShutdown  := SaveBackup;
    AutoIncStartValue := AutoIncStart;
    // - Fields:
    ShowFieldNames    := ShowNames;
    ShowFieldBorders  := ShowBorders;
  end;

  with Result.Study, ManagerSettings do
  begin
    Title.RegisterOnChangeHook(@OnTitleChange);

    // - Study:
    Title.Text                := StudyTitle;
    Identifier                := StudyIndent;
    Version                   := StudyVersion;

    // - Content Desc:
    Purpose.Text              := ContPurpose;
    AbstractText.Text         := ContAbstract;
    Citations.Text            := ContCitation;
    GeographicalCoverage.Text := ContGeoCover;
    TimeCoverage.Text         := ContTimeCover;

    // - Ownership;
    Author                    := OwnAuthers;
    Rights.Text               := OwnRights;
    Publisher.Text            := OwnPublisher;
    Funding.Text              := OwnFunding;
  end;

  Result.DataFiles.OnNewItemClass := @NewDataFileItem;
  Result.OnModified := @EpiDocumentModified;
end;

function TProjectFrame.NewDataFileItem(Sender: TEpiCustomList;
  DefaultItemClass: TEpiCustomItemClass): TEpiCustomItemClass;
begin
  result := TEpiDataFileEx;
end;

procedure TProjectFrame.AddToRecent(const AFileName: string);
begin
  settings2.AddToRecent(AFileName);
  MainForm.UpdateRecentFiles;
end;

procedure TProjectFrame.DoSaveProject(AFileName: string);
var
  Fs: TFileStream;
  Ms: TMemoryStream;
begin
  // If project haven't been saved before.
  InitBackupTimer;

  ActiveFrame.Cursor := crHourGlass;
  Application.ProcessMessages;
  Fs := nil;
  Ms := nil;
  try
    Ms := TMemoryStream.Create;
    EpiDocument.Study.ModifiedDate := Now;
    EpiDocument.SaveToStream(Ms);
    Ms.Position := 0;

    if ExtractFileExt(UTF8ToSys(AFileName)) = '.epz' then
      StreamToZipFile(Ms, AFileName)
    else begin
      Fs := TFileStream.Create(UTF8ToSys(AFileName), fmCreate);
      Fs.CopyFrom(Ms, Ms.Size);
    end;
    FFileTimeStamp := FileAgeUTF8(AFileName);
    // Do not add backupfiles to Recent list!
    if ExtractFileExt(AFileName) <> '.bak' then
      AddToRecent(AFileName);
  finally
    ActiveFrame.Cursor := crDefault;
    Application.ProcessMessages;
    if Assigned(Ms) then FreeAndNil(Ms);
    if Assigned(Fs) then FreeAndNil(Fs);
  end;
end;

procedure TProjectFrame.DoOpenProject(Const AFileName: string);
var
  St: TMemoryStream;
  Fn: String;
  Res: Integer;
begin
  Fn := aFilename;
  Res := mrNone;
  if FileExistsUTF8(Fn + '.bak') then
  begin
    Res := MessageDlg('Information',
             'A timed backup file exists. (loading of this overwrites previous project file)' + LineEnding + LineEnding +
             'File: ' +  #9  + #9  + SysToUTF8(ExtractFileName(UTF8ToSys(Fn)))          +
               ' (' + FormatDateTime('YYYY/MM/DD HH:NN:SS', FileDateToDateTime(FileAgeUTF8(Fn))) + ')' + LineEnding +
             'Recovery: ' + #9 + SysToUTF8(ExtractFileName(UTF8ToSys(Fn + '.bak'))) +
               ' (' + FormatDateTime('YYYY/MM/DD HH:NN:SS', FileDateToDateTime(FileAgeUTF8(Fn + '.bak'))) + ')' + LineEnding +  LineEnding +
             'Load the backup instead?',
             mtInformation, mbYesNoCancel, 0, mbYes);
    case Res of
      mrYes:    Fn := aFilename + '.bak';
      mrNo:     begin
                  Res := MessageDlg('Warning',
                           'Loading ' + SysToUTF8(ExtractFileName(UTF8ToSys(Fn))) + ' will delete recovery file.' + LineEnding +
                           'Continue?',
                           mtWarning, mbYesNo, 0, mbNo);
                  case Res of
                    mrNo:  Exit;
                    mrYes: Res := mrNo;  // Res used later to check for modification state.
                  end;
                end;
      mrCancel: Exit;
    end;
  end;

  DoCloseProject;

  try
    Screen.Cursor := crHourGlass;
    Application.ProcessMessages;

    St := nil;
    try
      FEpiDocument := TOpenEpiDoc.OpenDoc(DoCreateNewDocument, Fn);
      FFileName := AFileName;
      DoNewDataForm(FEpiDocument.DataFiles[0]);
      St.Free;
    except
      if Assigned(St) then FreeAndNil(St);
      if Assigned(FEpiDocument) then FreeAndNil(FEpiDocument);
      if Assigned(FActiveFrame) then FreeAndNil(FActiveFrame);
      raise
    end;
    FFileTimeStamp := FileAgeUTF8(fn);

    MainForm.AssignActionLinks;

    // Create backup process.
    InitBackupTimer;

    if Res = mrYes then
      EpiDocument.Modified := true;

    AddToRecent(Fn);
    UpdateCaption;
  finally
    Screen.Cursor := crDefault;
    Application.ProcessMessages;
  end;
end;

procedure TProjectFrame.DoNewDataForm(Df: TEpiDataFile);
var
  Frame: TRuntimeDesignFrame;
begin
  Frame := TRuntimeDesignFrame.Create(Self);
  Frame.Align := alClient;
  Frame.Parent := Self;
  Frame.DataFile := Df;
  FActiveFrame := Frame;

  Frame.OpenProjectToolBtn.Action := OpenProjectAction;
  Frame.SaveProjectToolBtn.Action := SaveProjectAction;
  Frame.SaveProjectAsToolBtn.Action := SaveProjectAsAction;
  Frame.ProjectToolBar.Images := ProjectImageList;

  {$IFDEF EPI_DEBUG}
//  AddDebugingContent;
  {$ENDIF}

  DataFilesTreeView.Selected := DataFilesTreeView.Items.AddObject(nil, Df.Caption.Text, Frame);
  TEpiDataFileEx(Df).TreeNode := DataFilesTreeView.Selected;
  Df.Caption.RegisterOnChangeHook(@OnDataFileChange);
end;

procedure TProjectFrame.DoCloseProject;
begin
  // Close VAlueLabel Editor - else the reference to
  // ValueLabelSEts is incomplete!
  CloseValueLabelEditor2;

  // Close Alignment Form.
  CloseAlignmentForm;

  if not Assigned(FEpiDocument) then exit;

  // TODO : Delete ALL dataforms!
  FreeAndNil(FActiveFrame);

  FreeAndNil(FBackupTimer);
  FreeAndNil(FEpiDocument);

  if FileExistsUTF8(ProjectFileName + '.bak') then
    DeleteFileUTF8(ProjectFileName + '.bak');
  DataFilesTreeView.Items.Clear;

  Modified := false;
end;

{$IFDEF EPI_DEBUG_CONTENT}
procedure TProjectFrame.AddDebugingContent;
var
  i: Integer;
  LocalAdm: TEpiAdmin;
  Grp: TEpiGroup;
  LocalVLSets: TEpiValueLabelSets;
  VLSet: TEpiValueLabelSet;
begin
  // DEBUGGING!!!!
  LocalAdm := FEpiDocument.Admin;
  Grp := LocalAdm.NewGroup;
  GRp.Name := 'G1';
  Grp.Caption.Text := 'Group 1';
  Grp.Rights := [earCreate, earRead, earUpdate, earDelete, earVerify,
    earStructure, earTranslate, earUsers, earPassword];
  Grp := LocalAdm.NewGroup;
  Grp.Name := 'G2';
  Grp.Caption.Text := 'Group 2';
  Grp.Rights := [earCreate, earRead, earUpdate, earDelete, earVerify,
    earStructure, earTranslate, earUsers, earPassword];
  Grp := LocalAdm.NewGroup;
  Grp.Name := 'G3';
  Grp.Caption.Text := 'Group 3';
  Grp.Rights := [earCreate, earRead, earUpdate, earDelete, earVerify,
    earStructure, earTranslate, earUsers, earPassword];
  FEpiDocument.DataFiles[0].MainSection.Groups.AddItem(Grp);
{
  LocalVLSets := FEpiDocument.ValueLabelSets;
  VLSet := LocalVLSets.NewValueLabelSet(ftInteger);
  VLSet.Name := 'The Set';
  for i := 1 to 10 do
    with TEpiIntValueLabel(VLSet.NewValueLabel) do
    begin
      Value := i;
      TheLabel.Text := DupeString(IntToStr(i), 4);
      if (i mod 2) = 0 then
        IsMissingValue := true;
    end;

  VLSet := LocalVLSets.NewValueLabelSet(ftInteger);
  VLSet.Name := 'Whatever';
  for i := 1 to 10 do
    with TEpiIntValueLabel(VLSet.NewValueLabel) do
    begin
      Value := i;
      TheLabel.Text := DupeString(IntToStr(i), 4);
      if (i mod 2) = 0 then
        IsMissingValue := true;
    end;

  VLSet := LocalVLSets.NewValueLabelSet(ftFloat);
  VLSet.Name := 'ZZ Top';
  for i := 1 to 10 do
    with TEpiFloatValueLabel(VLSet.NewValueLabel) do
    begin
      Value := i / 10;
      TheLabel.Text := DupeString(IntToStr(i), 4);
      if (i mod 2) = 0 then
        IsMissingValue := true;
    end;          }
end;
{$ENDIF}

procedure TProjectFrame.EpiDocumentModified(Sender: TObject);
begin
  Modified := TEpiDocument(Sender).Modified;

  // Activates/Deactivates timed backup.
  if Assigned(FBackupTimer) and Assigned(EpiDocument) then
    FBackupTimer.Enabled := EpiDocument.Modified;
end;

procedure TProjectFrame.SetModified(const AValue: Boolean);
begin
  if FModified = AValue then exit;
  FModified := AValue;
  UpdateCaption;
  if Assigned(FOnModified) then
    FOnModified(Self);
end;

procedure TProjectFrame.SetOnModified(const AValue: TNotifyEvent);
begin
  if FOnModified = AValue then exit;
  FOnModified := AValue;
end;

procedure TProjectFrame.UpdateCaption;
var
  S: String;
  T: String;
begin
  S := 'EpiData Manager (v' + GetManagerVersion + ')'
    {$IFDEF EPIDATA_TEST_RELEASE}
    + 'test version'
    {$ENDIF}
    ;

  if Assigned(EpiDocument) then
  begin
    if FFileName <> '' then
      S := S + ' - ' + ExtractFileName(FFileName);
    if EpiDocument.Modified then
      S := S + '*';

    T := EpiDocument.Study.Version;
    if (T <> '') then
      S := S + ' Version: ' + T;

    T := EpiDocument.Study.Title.Text;
    if (T <> '') then
      S := S + ' [' + EpiCutString(T, 20) + ']';
  end;
  MainForm.Caption := S;
end;

function TProjectFrame.InitBackupTimer: boolean;
begin
  if Assigned(FBackupTimer) then
  begin
    if not FBackupTimer.Enabled then
      FBackupTimer.Enabled := true;
    exit(true);
  end;

  Result := false;
  // Create backup process.
  if EpiDocument.ProjectSettings.BackupInterval > 0 then
  begin
    FBackupTimer := TTimer.Create(Self);
    FBackupTimer.Enabled := false;
    FBackupTimer.OnTimer := @TimedBackup;
    FBackupTimer.Interval := EpiDocument.ProjectSettings.BackupInterval * 60000;
    Result := true;
  end;
end;

procedure TProjectFrame.UpdateTimer;
begin
  if not Assigned(FBackupTimer) then exit;

  // Only update interval time if new interval is smaller   (Milliseconds * 60 sec/min.)
  if (FBackupTimer.Interval = 0) or
     (FBackupTimer.Interval > EpiDocument.ProjectSettings.BackupInterval * 60000) then
    FBackupTimer.Interval := EpiDocument.ProjectSettings.BackupInterval * 60000;
end;

procedure TProjectFrame.TimedBackup(Sender: TObject);
var
  Res: LongInt;
begin
  try
    FBackupTimer.Enabled := false;

    {$IFNDEF EPI_DEBUG}
    // Warn user that project has not yet been saved...
    if ProjectFileName = '' then
    begin
      Res := MessageDlg('Warning',
               'Your project have not yet been saved' + LineEnding +
               'Save now?' + LineEnding +
               '(NO = you will NOT be asked again)', mtWarning, mbYesNo, 0, mbYes);
      if Res = mrNo then exit;
      SaveProjectAsAction.Execute;
    end;
    {$ENDIF}
    try
      DoSaveProject(ProjectFileName + '.bak');
    except
      // TODO : Message on bad backup?
      Exit;
    end;
    UpdateTimer;
    FBackupTimer.Enabled := true;
  except
    //
  end;
end;

procedure TProjectFrame.UpdateShortCuts;
begin
  // Project Frame
  SaveProjectAction.ShortCut      := P_SaveProject;
  SaveProjectAsAction.ShortCut    := P_SaveProjectAs;
  NewDataFormAction.ShortCut      := P_NewDataForm;
  DeleteDataFormAction.ShortCut   := P_DelDataForm;
  ProjectSettingsAction.ShortCut  := P_ProjectSettings;
  ValueLabelEditorAction.ShortCut := P_StartValueLabelEditor;
  OpenProjectAction.ShortCut      := P_OpenProject;
  KeyFieldsAction.ShortCut        := P_KeyFields;
end;

procedure TProjectFrame.LMDesignerAdd(var Msg: TLMessage);
begin
  if Assigned(FActiveFrame) then
  with Msg do
    Result := SendMessage(FActiveFrame.Handle, Msg, WParam, LParam);
end;

constructor TProjectFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FrameCount := 0;

  FrameCount := 0;
  FActiveFrame := nil;
  FFileName := '';

  FEpiDocument := DoCreateNewDocument;
  UpdateCaption;
  UpdateShortCuts;
  InitBackupTimer;

  {$IFDEF EPI_DEBUG}

  {$ELSE}
  ProjectPanel.Enabled := false;
  ProjectPanel.Visible := false;
  {$ENDIF}
end;

destructor TProjectFrame.Destroy;
begin
  DoCloseProject;
  inherited Destroy;
end;

procedure TProjectFrame.CloseQuery(var CanClose: boolean);
var
  res: LongInt;
begin
  {$IFNDEF EPI_DEBUG}
  if Modified or
    (Assigned(EpiDocument) and (EpiDocument.Modified)) then
  begin
    res := MessageDlg('Warning', 'Content has been modified since last save.' + LineEnding +
             'Save before close?', mtWarning, mbYesNoCancel, 0, mbCancel);
    case res of
      mrYes:    SaveProjectAction.Execute;
      mrNo:     exit;
      mrCancel: CanClose := false;
    end;
  end;
  {$ENDIF}
end;

procedure TProjectFrame.RestoreDefaultPos;
begin
//  GetValueLabelsEditor(EpiDocument).RestoreDefaultPos;
  TProjectSettingsForm.RestoreDefaultPos;
  TStudyUnitForm.RestoreDefaultPos;

  if Assigned(FActiveFrame) then
    TRuntimeDesignFrame(FActiveFrame).RestoreDefaultPos;
end;

procedure TProjectFrame.UpdateFrame;
begin
  UpdateShortCuts;
//  if ValueLabelsEditorCreated then
//    GetValueLabelsEditor(EpiDocument).UpdateSettings;

  // TODO : Update all frames.
  if Assigned(FActiveFrame) then
    TRuntimeDesignFrame(FActiveFrame).UpdateFrame;
end;

procedure TProjectFrame.OpenProject(const AFileName: string);
begin
  DoOpenProject(AFileName);
end;

end.

