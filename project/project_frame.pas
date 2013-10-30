unit project_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, ExtCtrls, ComCtrls, ActnList,
  Controls, Dialogs, epidocument, epidatafiles, epicustombase,
  manager_messages, LMessages, epiv_documentfile;

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
    FDocumentFile: TDocumentFile;
    FActiveFrame: TFrame;
    FModified: Boolean;
    FOnModified: TNotifyEvent;
    FrameCount: integer;
    procedure OnDataFileChange(Const Sender, Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
    procedure OnTitleChange(Const Sender, Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
    function  NewDataFileItem(Sender: TEpiCustomList; DefaultItemClass: TEpiCustomItemClass): TEpiCustomItemClass;
    procedure AddToRecent(Const AFileName: string);
    // Common for open/create
    procedure CommonProjectInit;
    procedure DoNewDataForm(Df: TEpiDataFile);
    // open existing
    function  DoSaveProject(AFileName: string): boolean;
    function  DoOpenProject(Const AFileName: string): boolean;
    // create new
    function  DoCreateNewDocument: TEpiDocument;
    procedure DoCreateNewProject;
    procedure DoCloseProject;
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
//    FFileName: string;
    function GetEpiDocument: TEpiDocument;
    { Messages }
    // Message relaying...
    procedure LMDesignerAdd(var Msg: TLMessage); message LM_DESIGNER_ADD;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   CloseQuery(var CanClose: boolean);
    procedure   UpdateFrame;
    function    OpenProject(Const AFileName: string): boolean;
    function    SaveProject(Const ForceSaveAs: boolean): boolean;
    procedure   CreateNewProject;
    property   DocumentFile: TDocumentFile read FDocumentFile;
    property   EpiDocument: TEpiDocument read GetEpiDocument;
    property   ActiveFrame: TFrame read FActiveFrame;
//    property   ProjectFileName: string read FFileName write FFileName;
    property   Modified: Boolean read FModified write SetModified;
    property   OnModified: TNotifyEvent read FOnModified write SetOnModified;
  public
    class procedure   RestoreDefaultPos(F: TProjectFrame);
  end;

implementation

{$R *.lfm}

uses
  design_runtimedesigner, Clipbrd, epimiscutils,
  main, settings2, settings2_var, epistringutils,
  valuelabelseditor_form2,
  managerprocs, Menus, LCLType, LCLIntf, project_settings,
  shortcuts, project_keyfields_form, project_studyunit_form,
  align_form, RegExpr;

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
  SaveProject(False);
end;

procedure TProjectFrame.SaveDlgTypeChange(Sender: TObject);
var
  Dlg: TSaveDialog absolute Sender;
begin
  case Dlg.FilterIndex of
    1: Dlg.DefaultExt := 'epx';
    2: Dlg.DefaultExt := 'epz';
  end;

  // TODO : Must be changed when supporting multiple desinger frames (take only the topmost/first datafile).
  if (Dlg.FileName = '') and
     (Assigned(DocumentFile)) and
     (TRuntimeDesignFrame(ActiveFrame).ImportedFileName <> '') then
    Dlg.FileName := ChangeFileExt(TRuntimeDesignFrame(ActiveFrame).ImportedFileName, Dlg.DefaultExt);
end;

procedure TProjectFrame.SaveProjectAsActionExecute(Sender: TObject);
begin
  SaveProject(True);
end;

procedure TProjectFrame.StudyInformationActionExecute(Sender: TObject);
var
  F: TStudyUnitForm;
begin
  F := TStudyUnitForm.Create(Self, EpiDocument.Study);
  F.ShowModal;
  F.Free;
  UpdateCaption;
end;

procedure TProjectFrame.ValueLabelEditorActionExecute(Sender: TObject);
begin
  ShowValueLabelEditor2(EpiDocument.ValueLabelSets);
end;

procedure TProjectFrame.OnDataFileChange(const Sender,
  Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
var
  Df: TEpiDataFileEx;
begin
  // TODO : Handle change event when implementing multiple dataforms.
{  if (EventGroup = eegCustomBase) and (EventType = Word(ecceText)) then
  begin
    Df := TEpiDataFileEx(TEpiCustomItem(Sender).Owner);
    Df.TreeNode.Text := Df.Caption.Text;
  end;}
end;

procedure TProjectFrame.OnTitleChange(const Sender, Initiator: TEpiCustomBase;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  UpdateCaption;
end;

function TProjectFrame.DoCreateNewDocument: TEpiDocument;
begin
  FDocumentFile := TDocumentFile.Create;
  Result := FDocumentFile.CreateNewDocument(ManagerSettings.StudyLang);

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
//    TimeCoverage.Text         := ContTimeCover;

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

procedure TProjectFrame.CommonProjectInit;
begin
  UpdateCaption;
  UpdateShortCuts;
  InitBackupTimer;
end;

function TProjectFrame.DoSaveProject(AFileName: string): boolean;
begin
  // If project haven't been saved before.
  InitBackupTimer;

  ActiveFrame.Cursor := crHourGlass;
  Application.ProcessMessages;

  try
    EpiDocument.IncCycleNo;
    Result := DocumentFile.SaveFile(AFileName);
    AddToRecent(AFileName);
  finally
    ActiveFrame.Cursor := crDefault;
    Application.ProcessMessages;
  end;
end;

function TProjectFrame.DoOpenProject(const AFileName: string): boolean;
var
  TmpDoc: TEpiDocument;
begin
  Result := false;
  try
    FDocumentFile := TDocumentFile.Create;
    if not FDocumentFile.OpenFile(AFileName) then
    begin
      FreeAndNil(FDocumentFile);
      Exit;
    end;
  except
    FreeAndNil(FDocumentFile);
    // If ever this happens then it is because something not right happened
    // during OpenFile(...) and we need to notify the user.
    raise;
  end;

  try
    Screen.Cursor := crHourGlass;
    Application.ProcessMessages;

    try
      DoNewDataForm(EpiDocument.DataFiles[0]);
    except
      if Assigned(FDocumentFile) then
        FreeAndNil(FDocumentFile);
      if Assigned(FActiveFrame) then FreeAndNil(FActiveFrame);
      raise
    end;

    CommonProjectInit;

    EpiDocument.Modified := false;
    Result := true;

    AddToRecent(DocumentFile.FileName);
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

  DataFilesTreeView.Selected := DataFilesTreeView.Items.AddObject(nil, Df.Caption.Text, Frame);
  TEpiDataFileEx(Df).TreeNode := DataFilesTreeView.Selected;
  Df.Caption.RegisterOnChangeHook(@OnDataFileChange);
end;

procedure TProjectFrame.DoCreateNewProject;
begin
  DoCreateNewDocument;
  CommonProjectInit;
  NewDataFormAction.Execute;
end;

procedure TProjectFrame.DoCloseProject;
begin
  // Close VAlueLabel Editor - else the reference to
  // ValueLabelSEts is incomplete!
  CloseValueLabelEditor2;

  // Close Alignment Form.
  CloseAlignmentForm;

//  if not Assigned(EpiDocument) then exit;

  // TODO : Delete ALL dataforms!
  FreeAndNil(FActiveFrame);

  FreeAndNil(FBackupTimer);
  FreeAndNil(FDocumentFile);

//  if FileExistsUTF8(ProjectFileName + '.bak') then
//    DeleteFileUTF8(ProjectFileName + '.bak');

  DataFilesTreeView.Items.Clear;

  Modified := false;
end;

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
    if DocumentFile.FileName <> '' then
      S := S + ' - ' + ExtractFileName(DocumentFile.FileName);
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
    if DocumentFile.FileName = '' then
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
      DocumentFile.SaveBackupFile;
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

function TProjectFrame.GetEpiDocument: TEpiDocument;
begin
  result := nil;
  if Assigned(DocumentFile) and
     Assigned(DocumentFile.Document)
  then
    Result := DocumentFile.Document;
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
    Res := MessageDlg('Warning',
      'Project data content modified.' + LineEnding +
      'Store project permanently on disk before exit?',
      mtWarning, mbYesNoCancel, 0, mbCancel);

    if Res = mrNo then
    begin
      Res := MessageDlg('Warning',
        'Project content is NOT saved to disk.' + LineEnding +
        'Choose YES to permanently store data on disk!' + LineEnding +
        LineEnding +
        'Save project to disk before exit?',
        mtWarning, mbYesNoCancel, 0, mbCancel);
    end;

    case res of
      mrYes:    CanClose := SaveProject(False);
      mrCancel: CanClose := false;
    end;
  end;
  {$ENDIF}
end;

class procedure TProjectFrame.RestoreDefaultPos(F: TProjectFrame);
begin
  RestoreDefaultPosValueLabelEditor2;
  TProjectSettingsForm.RestoreDefaultPos;
  TStudyUnitForm.RestoreDefaultPos;
  TKeyFieldsForm.RestoreDefaultPos;

   if Assigned(F) then
    TRuntimeDesignFrame.RestoreDefaultPos(TRuntimeDesignFrame(F.ActiveFrame))
  else
    TRuntimeDesignFrame.RestoreDefaultPos(nil);
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

function TProjectFrame.OpenProject(const AFileName: string): boolean;
begin
  Result := DoOpenProject(AFileName);
end;

function TProjectFrame.SaveProject(const ForceSaveAs: boolean): boolean;
var
  Dlg: TSaveDialog;
  Fn: String;
begin
  if (not DocumentFile.IsSaved) or
     ForceSaveAs
  then
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
  end else
    Fn := DocumentFile.FileName;

  Result := DoSaveProject(Fn);
  if Result then
    EpiDocument.Modified := false;
  UpdateCaption;
end;

procedure TProjectFrame.CreateNewProject;
begin
  DoCreateNewProject;
end;

end.

