unit project_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, ExtCtrls, ComCtrls, ActnList, Controls,
  Dialogs, epidocument, epidatafiles, epicustombase, epidatafilerelations,
  epirelates, epiadmin, epidatafilestypes, manager_messages, LMessages, Menus,
  StdCtrls, epiv_documentfile, types, design_runtimedesigner, project_types,
  epiv_projecttreeview_frame, manager_types, core_logger, project_statusbar,
  epiv_custom_statusbar, admin_logviewer_frame;

type

  { TProjectFrame }

  TProjectFrame = class(TFrame)
    ExportLogAction: TAction;
    ViewLogAction: TAction;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    MenuItem1: TMenuItem;
    Panel1: TPanel;
    ProjectRecentFilesDropDownMenu: TPopupMenu;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    StudyInformationAction: TAction;
    KeyFieldsAction: TAction;
    SetProjectPasswordAction: TAction;
    OpenProjectAction: TAction;
    NewProjectToolBtn: TToolButton;
    Divider1: TToolButton;
    Divider4: TToolButton;
    ProjectTestToolBtn: TToolButton;
    ValueLabelEditorAction: TAction;
    ProjectSettingsAction: TAction;
    DeleteDataFormAction: TAction;
    SaveProjectAsAction: TAction;
    SaveProjectAction: TAction;
    NewDataFormAction: TAction;
    ActionList1: TActionList;
    ProjectPanel: TPanel;
    ToolBar1: TToolBar;
    OpenProjectToolBtn: TToolButton;
    Divider2: TToolButton;
    SaveProjectToolBtn: TToolButton;
    SaveProjectAsToolBtn: TToolButton;
    Divider3: TToolButton;
    AddDataFormToolBtn: TToolButton;
    DeleteDataFormToolBtn: TToolButton;
    DefineGroupsAction: TAction;
    DefineUsersAction: TAction;
    DefineEntryRightsAction: TAction;
    DefineExtendedAccessAction: TAction;
    RemoveProjectPassword: TAction;
    procedure DeleteDataFormActionExecute(Sender: TObject);
    procedure DeleteDataFormActionUpdate(Sender: TObject);
    procedure ExportLogActionExecute(Sender: TObject);
    procedure ExportLogActionUpdate(Sender: TObject);
    procedure NewDataFormActionExecute(Sender: TObject);
    procedure NewDataFormActionUpdate(Sender: TObject);
    procedure OpenProjectActionExecute(Sender: TObject);
    procedure SetProjectPasswordActionExecute(Sender: TObject);
    procedure SetProjectPasswordActionUpdate(Sender: TObject);
    procedure ProjectSettingsActionExecute(Sender: TObject);
    procedure ProjectSettingsActionUpdate(Sender: TObject);
    procedure SaveProjectActionExecute(Sender: TObject);
    procedure SaveProjectActionUpdate(Sender: TObject);
    procedure SaveProjectAsActionExecute(Sender: TObject);
    procedure StudyInformationActionExecute(Sender: TObject);
    procedure ProjectTestToolBtnClick(Sender: TObject);
    procedure ValueLabelEditorActionExecute(Sender: TObject);
    procedure DefineGroupsActionUpdate(Sender: TObject);
    procedure DefineGroupsActionExecute(Sender: TObject);
    procedure DefineUsersActionExecute(Sender: TObject);
    procedure DefineUsersActionUpdate(Sender: TObject);
    procedure DefineEntryRightsActionExecute(Sender: TObject);
    procedure DefineEntryRightsActionUpdate(Sender: TObject);
    procedure DefineExtendedAccessActionExecute(Sender: TObject);
    procedure DefineExtendedAccessActionUpdate(Sender: TObject);
    procedure RemoveProjectPasswordExecute(Sender: TObject);
    procedure RemoveProjectPasswordUpdate(Sender: TObject);
    procedure ViewLogActionExecute(Sender: TObject);
  private
    FSaveWarningRes: Integer;
    { Core Logger }
    FCoreLoggerForm: TCoreLogger;
    procedure DocumentHook(const Sender: TEpiCustomBase;
      const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
    procedure ShowCoreLogger;
    procedure CreateCoreLogger;
    procedure AfterDocumentCreated(const Sender: TObject;
      const ADocument: TEpiDocument);
    procedure SaveThreadError(const FatalErrorObject: Exception);
  private
    { private declarations }
    FDocumentFile: TDocumentFile;
    FActiveFrame: IProjectFrame;
    FModified: Boolean;
    FOnModified: TNotifyEvent;
    FrameCount: integer;
    FDataFileTreeViewCaptionUpdating: boolean;
    procedure AddToRecent(Const AFileName: string);
    // Common for open/create
    procedure CommonProjectInit;
    function  DoNewDataForm(ParentRelation: TEpiMasterRelation): TEpiMasterRelation;
    function  DoNewRuntimeFrame(Relation: TEpiMasterRelation): TRuntimeDesignFrame;
    // open existing
    function  DoOpenProject(Const AFileName: string): boolean;
    function  DoSaveProject(AFileName: string): boolean;
    procedure OpenProjectOrderedWalkCallBack(
      const Relation: TEpiMasterRelation; const Depth: Cardinal;
      const Index: Cardinal; var aContinue: boolean;
      Data: Pointer = nil);
    // close
    procedure DoCloseProject;
    procedure CloseProjectOrderedWalkCallBack(
      const Relation: TEpiMasterRelation; const Depth: Cardinal;
      const Index: Cardinal; var aContinue: boolean;
      Data: Pointer = nil);
    // create new
    function  DoCreateNewDocumentFile: TDocumentFile;
    function  DoCreateNewDocument: TEpiDocument;
    procedure DoCreateNewProject;
    procedure EpiDocumentModified(Sender: TObject);
    procedure UpdateDefaultExtension(Const Dlg: TOpenDialog);
    procedure SaveDlgTypeChange(Sender: TObject);
    procedure SetModified(const AValue: Boolean);
    procedure SetOnModified(const AValue: TNotifyEvent);
    procedure UpdateCaption;
    procedure RuntimeFrameUpdateFrameOrderedWalkCallBack(
      const Relation: TEpiMasterRelation; const Depth: Cardinal;
      const Index: Cardinal; var aContinue: boolean;
      Data: Pointer = nil);
  private
    { Project Tree View }
    FProjectTreeView: TEpiVProjectTreeViewFrame;
    procedure ProjectTreeDelete(const Relation: TEpiMasterRelation);
    procedure ProjectTreeEdited(Sender: TObject;
      const AObject: TEpiCustomBase; ObjectType: TEpiVTreeNodeObjectType);
    procedure ProjectTreeEditing(Sender: TObject;
      const AObject: TEpiCustomBase; ObjectType: TEpiVTreeNodeObjectType;
      var Allowed: Boolean);
    procedure ProjectTreeError(const Msg: String);
    procedure ProjectTreeGetHint(Sender: TObject;
      const AObject: TEpiCustomBase; ObjectType: TEpiVTreeNodeObjectType;
      var HintText: string);
    procedure ProjectTreeNewRelation(const Relation: TEpiMasterRelation);
    procedure ProjectTreeSelected(Sender: TObject;
      const AObject: TEpiCustomBase; ObjectType: TEpiVTreeNodeObjectType);
    procedure ProjectTreeSelecting(Sender: TObject; const OldObject,
      NewObject: TEpiCustomBase; OldObjectType,
      NewObjectType: TEpiVTreeNodeObjectType; var Allowed: Boolean);
    procedure ProjectTreeShowPopupMenu(Sender: TObject;
      const AObject: TEpiCustomBase; ObjectType: TEpiVTreeNodeObjectType);
  private
    { Relation Handling }
    procedure KeyFieldEvent(const Sender: TEpiCustomBase;
      const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
    procedure BindKeyFields(DR: TEpiDetailRelation);
    procedure UnBindKeyFields(DR: TEpiDetailRelation);
  private
    { Hint }
    FHintWindow: THintWindow;
    procedure ShowHintMsg(Sender: TObject; Ctrl: TControl; const Msg: string);
  private
    { Backup }
    FBackupTimer: TTimer;
    function  InitBackupTimer: boolean;
    procedure UpdateTimer;
    procedure TimedBackup(Sender: TObject);
    procedure UpdateShortCuts;
  private
    function GetEditingProjectTree: boolean;
//    FFileName: string;
    procedure LoadError(const Sender: TEpiCustomBase; ErrorType: Word;
      Data: Pointer; out Continue: boolean);
    function GetEpiDocument: TEpiDocument;
    { Messages }
    // Message relaying...
    procedure LMDesignerAdd(var Msg: TLMessage); message LM_DESIGNER_ADD;
    procedure OpenRecentMenuItemClick(Sender: TObject);
    procedure UpdateRecentFilesDropDown;

  { StatusBar }
  private
    FStatusBar: TManagerStatusBar;
    procedure DesignerUpdateStatusbar(Sender: TObject;
      SelectionList: TEpiCustomList);

  { Authorization }
  private
    function GetAuthorizedUser(Sender: TObject): TEpiUser;
    function IsProjectSetupForAdmin: Boolean;
  public
    { Access/Helper methods }
    function SelectDataformIfNotSelected: Boolean;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   CloseQuery(var CanClose: boolean);
    procedure   UpdateFrame;
    procedure   UpdateStatusBar(Condition: TEpiVCustomStatusbarUpdateCondition = sucDefault);
    function    OpenProject(Const AFileName: string): boolean;
    function    SaveProject(Const ForceSaveAs: boolean): boolean;
    function    Import(Const FromCB: boolean): boolean;
    procedure   CreateNewProject;
    procedure   AssignActionLinks;
    procedure   StopEditingProjectTree;
    property   DocumentFile: TDocumentFile read FDocumentFile;
    property   EpiDocument: TEpiDocument read GetEpiDocument;
    property   ActiveFrame: IProjectFrame read FActiveFrame;
    property   Modified: Boolean read FModified write SetModified;
    property   OnModified: TNotifyEvent read FOnModified write SetOnModified;
    property   EditingProjectTree: boolean read GetEditingProjectTree;
  public
    class procedure   RestoreDefaultPos(F: TProjectFrame);
  end;

implementation

{$R *.lfm}

uses
  Clipbrd, epimiscutils, admin_authenticator,
  main, settings2, settings2_var, epistringutils,
  valuelabelseditor_form2, LazFileUtils,
  managerprocs, LCLType, LCLIntf, project_settings,
  shortcuts, project_keyfields_form,
  align_form, RegExpr, project_studyunit_frame,
  design_properties_form, admin_form, epidatafilerelations_helper,
  admin_user_form, admin_groups_form, admin_users_form, admin_entryrights_form,
  epiranges, empty_form, episervice_asynchandler, epiopenfile, export_securitylog_form,
  epitools_export_seclog
  {$IFDEF LINUX},gtk2{$ENDIF}
  ;

{ TProjectFrame }

procedure TProjectFrame.NewDataFormActionExecute(Sender: TObject);
var
  MR: TEpiMasterRelation;
begin
  MR := nil;
  if FProjectTreeView.SelectedObjectType = otRelation then
    MR := TEpiMasterRelation(FProjectTreeView.SelectedObject);

  MR := DoNewDataForm(MR);
  if Assigned(MR) then
    FProjectTreeView.SelectedObject := MR;
end;

procedure TProjectFrame.NewDataFormActionUpdate(Sender: TObject);
var
  ActionEnabled: Boolean;
begin
  ActionEnabled := Authenticator.IsAuthorized([earDefineProject]);

  if (FProjectTreeView.SelectedObjectType = otRelation) then
    ActionEnabled := ActionEnabled and
                     (not TEpiMasterRelation(FProjectTreeView.SelectedObject).ProtectedItem);

  NewDataFormAction.Enabled := ActionEnabled;
end;

procedure TProjectFrame.LoadError(const Sender: TEpiCustomBase;
  ErrorType: Word; Data: Pointer; out Continue: boolean);
var
  Fn: String;
  Res: TModalResult;
begin
  Continue := false;
  if ErrorType <> 0 then exit;
  if Sender <> EpiDocument then exit;
  if not Assigned(FDocumentFile) then exit;

  Fn := string(data^);
  Res :=
    MessageDlg('Warning',
      'External file "' + Fn + '" not found.' + LineEnding +
      'Please restore "' + Fn + '" to folder: ' + ExtractFilePath(FDocumentFile.FileName) + LineEnding +
      LineEnding +
      'If you continue all variables referecing the valuelabelset will be lost!' + LineEnding +
      LineEnding +
      'Continue loading the project?',
      mtWarning,
      mbYesNo,
      0,
      mbNo
    );
  Continue := Res = mrYes;
end;


procedure TProjectFrame.DeleteDataFormActionExecute(Sender: TObject);
var
  DF: TEpiDataFile;
  Res: TModalResult;
  Relation: TEpiMasterRelation;
begin
  if FProjectTreeView.SelectedObjectType <> otRelation then exit;

  Relation := TEpiMasterRelation(FProjectTreeView.SelectedObject);

  DF := Relation.DataFile;

  Res :=
    MessageDlg('Warning!',
      'Are you sure you want to delete the dataform "' + Df.Caption.Text + '" ?',
      mtWarning,
      mbYesNo,
      0,
      mbNo
  );
  if Res = mrNo then exit;

  Res :=
    MessageDlg('Last Warning!',
      'Are you absolutely sure you want to delete the dataform "' + Df.Caption.Text + '" ?' + LineEnding +
      'All strutures and data will be permanently lost.' + LineEnding +
      LineEnding +
      'No additional warnings are given!',
      mtWarning,
      mbYesNo,
      0,
      mbNo
  );

  FProjectTreeView.DeleteRelation(Relation);
  PropertiesForm.UpdateSelection(nil, nil);
end;

procedure TProjectFrame.DefineEntryRightsActionUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled :=
    Assigned(Authenticator.AuthedUser){ and
    Authenticator.IsAuthorized([earGroups])};
end;

procedure TProjectFrame.DeleteDataFormActionUpdate(Sender: TObject);
begin
  DeleteDataFormAction.Enabled :=
    (Authenticator.IsAuthorized([earDefineProject])) and
    (FProjectTreeView.SelectedObjectType = otRelation) and
    (not TEpiMasterRelation(FProjectTreeView.SelectedObject).ProtectedItem);
end;

procedure TProjectFrame.ExportLogActionExecute(Sender: TObject);
var
  F: TExportSecurityLogForm;
  Tool: TEpiTool_ExportSecurityLog;
begin
  F := TExportSecurityLogForm.Create(Self);
  F.DocFile := DocumentFile;

  if (F.ShowModal = mrOK) then
    begin
      Tool := TEpiTool_ExportSecurityLog.Create;
      Tool.DocumentFileClass := TDocumentFile;

      if (not Tool.ExportSecLog(EpiDocument, F.ExportAfterNoDays, F.DeleteLog, F.Filename)) then
        ShowMessage('Security log export failed:' + LineEnding +
          Tool.ErrorMessage
        );

      Tool.Free;
    end;

  F.Free;
end;

procedure TProjectFrame.ExportLogActionUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled :=
    Assigned(Authenticator.AuthedUser) and
    Authenticator.IsAuthorized([earExport]);
end;

procedure TProjectFrame.OpenProjectActionExecute(Sender: TObject);
begin
  PostMessage(MainForm.Handle, LM_MAIN_OPENPROJECT, 0, 0);
end;

procedure TProjectFrame.OpenRecentMenuItemClick(Sender: TObject);
begin
  PostMessage(MainForm.Handle, LM_MAIN_OPENRECENT, WParam(Sender), 0);
end;

procedure TProjectFrame.SetProjectPasswordActionExecute(Sender: TObject);
var
  PW: String;
  PW2: String;
  Header: String;
begin
  PW := '';
  PW2 := '';
  Header := 'Set Project Password';

  if (EpiDocument.PassWord <> '') then
  begin
    if not InputQuery(Header, 'Enter Current Project Password:', True, PW)
    then
      Exit;

    if PW <> EpiDocument.PassWord then
    begin
      MessageDlg(Header, 'Incorrect Password', mtInformation, [mbOK], 0);
      Exit;
    end;
  end;

  PW := '';
  if not InputQuery(Header, 'Enter New Project Password:', True, PW)
  then
    Exit;

  if PW = '' then
  begin
    MessageDlg(Header, 'Empty passwords are not allowed!', mtInformation, [mbOK], 0);
    Exit;
  end;

  PW2 := PasswordBox(Header, 'Re-enter Password:');
  if PW = PW2 then
  begin
    EpiDocument.PassWord := PW;
    MessageDlg(Header, 'Password successfully set!', mtInformation, [mbOK], 0);
  end else
    MessageDlg(Header, 'The two passwords are not identical!' + LineEnding + 'Password NOT set.', mtError, [mbOK], 0);
end;

procedure TProjectFrame.SetProjectPasswordActionUpdate(Sender: TObject);
begin
  SetProjectPasswordAction.Enabled :=
    (EpiDocument.Admin.Users.Count = 0);
end;

procedure TProjectFrame.ProjectSettingsActionExecute(Sender: TObject);
var
  ProjectSettings: TProjectSettingsForm;
  Res: Integer;
begin
  ProjectSettings := TProjectSettingsForm.Create(self, EpiDocument);
  Res := ProjectSettings.ShowModal;
  ProjectSettings.Free;

  if Res <> mrOK then Exit;

  TStudyUnitFrame(EpiDocument.FindCustomData(PROJECT_RUNTIMEFRAME_KEY)).UpdateFrame;
  EpiDocument.Relations.OrderedWalk(@RuntimeFrameUpdateFrameOrderedWalkCallBack);
  UpdateTimer;
end;

procedure TProjectFrame.ProjectSettingsActionUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Authenticator.IsAuthorized([earDefineProject]);
end;

procedure TProjectFrame.SaveProjectActionExecute(Sender: TObject);
begin
  SaveProject(False);
end;

procedure TProjectFrame.SaveProjectActionUpdate(Sender: TObject);
begin
  SaveProjectAction.Enabled :=
    (not FDocumentFile.ReadOnly);
end;

procedure TProjectFrame.SaveDlgTypeChange(Sender: TObject);
var
  Dlg: TSaveDialog absolute Sender;
  Fn: String;
begin
  UpdateDefaultExtension(Dlg);

  Dlg.FileName := ChangeFileExt(Dlg.FileName, Dlg.DefaultExt);

  {$IFDEF LINUX}
  Fn := ExtractFileName(Dlg.FileName);
  gtk_file_chooser_set_current_name(
    PGtkFileChooser(Dlg.Handle),
    PChar(FN));
  {$ENDIF}
end;

procedure TProjectFrame.SaveProjectAsActionExecute(Sender: TObject);
begin
  SaveProject(True);
end;

procedure TProjectFrame.StudyInformationActionExecute(Sender: TObject);
begin
  FProjectTreeView.SelectedObject := EpiDocument;
end;

procedure TProjectFrame.ProjectTestToolBtnClick(Sender: TObject);
begin
  ShowCoreLogger;
end;

procedure TProjectFrame.ValueLabelEditorActionExecute(Sender: TObject);
begin
  ShowValueLabelEditor2(EpiDocument.ValueLabelSets);
end;

procedure TProjectFrame.DefineGroupsActionUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled :=
    Assigned(Authenticator.AuthedUser){ and
    Authenticator.IsAuthorized([earGroups])};
end;

procedure TProjectFrame.DefineGroupsActionExecute(Sender: TObject);
begin
  if IsProjectSetupForAdmin then
    ShowDefineGroupsForm(Self, EpiDocument.Admin);
end;

procedure TProjectFrame.DefineUsersActionExecute(Sender: TObject);
begin
  if IsProjectSetupForAdmin then
    ShowDefineUsersForm(Self, EpiDocument.Admin);
end;

procedure TProjectFrame.DefineUsersActionUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled :=
    Assigned(Authenticator.AuthedUser) and
    Authenticator.IsAuthorized([earUsers]);
end;

procedure TProjectFrame.DefineEntryRightsActionExecute(Sender: TObject);
begin
  if IsProjectSetupForAdmin then
    ShowDefineEntryRightsForm(Self, EpiDocument);
end;

procedure TProjectFrame.DefineExtendedAccessActionExecute(Sender: TObject);
begin
  IsProjectSetupForAdmin;
end;

procedure TProjectFrame.DefineExtendedAccessActionUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (Authenticator.Admin.Users.Count = 0);
end;

procedure TProjectFrame.RemoveProjectPasswordExecute(Sender: TObject);
var
  Header: String;
  PW: String;
begin
  Header := 'Remove Project Password';

  if not InputQuery(Header, 'Enter Current Project Password:', True, PW)
  then
    Exit;

  if PW = EpiDocument.PassWord then
  begin
    EpiDocument.PassWord := '';
    MessageDlg(Header, 'Password successfully removed!', mtInformation, [mbOK], 0);
  end
  else
    MessageDlg(Header, 'Incorrect Password', mtInformation, [mbOK], 0)
end;

procedure TProjectFrame.RemoveProjectPasswordUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (EpiDocument.PassWord <> '') and (not Assigned(Authenticator.AuthedUser));
end;

procedure TProjectFrame.ViewLogActionExecute(Sender: TObject);
var
  LV: TLogViewerFrame;
  F: TPositionForm;
begin
  F := TPositionForm.Create(Self);
  F.StorageName := 'LogViewer';
  LV := TLogViewerFrame.Create(F);
  LV.Align := alClient;
  LV.Parent := F;
  LV.Document := EpiDocument;
  F.ShowModal;
  F.Free;
end;

procedure TProjectFrame.ShowCoreLogger;
begin
  if Assigned(FCoreLoggerForm) then
    FCoreLoggerForm.Show;
end;

procedure TProjectFrame.DocumentHook(const Sender: TEpiCustomBase;
  const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
var
  PData: PEpiIdCaseErrorRecord;
  S: String;
begin
  FCoreLoggerForm.DocumentHook(Sender, Initiator, EventGroup, EventType, Data);

  if (EventGroup <> eegCustomBase) then exit;
  if (TEpiCustomChangeEventType(EventType) <> ecceIdCaseOnLoad) then exit;

  PData := PEpiIdCaseErrorRecord(Data);

  PData^.ReturnState := crsRename;
  S := PData^.CurrentName;

  if InputQuery('Naming conflict',
                'This is already an item named "' + PData^.CurrentName + '"' + LineEnding +
                'Please suggest a new name:',
                S)
  then
    PData^.NewName := S
  else
    PData^.ReturnState := crsCancel;
end;

procedure TProjectFrame.CreateCoreLogger;
begin
  FCoreLoggerForm := TCoreLogger.Create(Self);
end;

procedure TProjectFrame.AfterDocumentCreated(const Sender: TObject;
  const ADocument: TEpiDocument);
begin
  ADocument.RegisterOnChangeHook(@DocumentHook, true);
  EpiAsyncHandlerGlobal.AddDocument(ADocument);
end;

procedure TProjectFrame.SaveThreadError(const FatalErrorObject: Exception);
var
  S: UTF8String;
begin
  S := 'A fatal error has happened during the saving process' + LineEnding +
       'and the project has not been save.' + LineEnding +
       LineEnding +
       EEpiThreadSaveExecption(FatalErrorObject).FileName +
       'In order to ensure futher functionality, use the Save As... option' + LineEnding +
       'to save in another location';

  ShowMessage(S);
end;

function TProjectFrame.DoCreateNewDocument: TEpiDocument;
begin
  Result := DoCreateNewDocumentFile.CreateNewDocument(ManagerSettings.StudyLang);

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

  Result.OnModified := @EpiDocumentModified;
end;

procedure TProjectFrame.AddToRecent(const AFileName: string);
begin
  settings2.AddToRecent(AFileName);
  MainForm.UpdateRecentFiles;
end;

procedure TProjectFrame.CommonProjectInit;
var
  Frame: TStudyUnitFrame;
begin
  Authenticator := TAuthenticator.Create(FDocumentFile);

  // No need to do undo's if the usage does not require such a thing.
  if (not Assigned(Authenticator.AuthedUser))
  then
    FDocumentFile.UndoCopy := false;

  UpdateCaption;
  UpdateShortCuts;
  InitBackupTimer;
//  FStatusBar.DocFile := DocumentFile;

  Splitter1.Visible    := true;
  ProjectPanel.Visible := true;

  Frame := TStudyUnitFrame.Create(self, EpiDocument.Study, (not DocumentFile.IsSaved));
  Frame.Align := alClient;
  Frame.Parent := self;
  EpiDocument.AddCustomData(PROJECT_RUNTIMEFRAME_KEY, Frame);

  Frame.DeActivate(true);
end;

function TProjectFrame.DoSaveProject(AFileName: string): boolean;
begin
  // If project haven't been saved before.
  InitBackupTimer;

  Screen.Cursor := crHourGlass;
  Application.ProcessMessages;

  try
    Result := DocumentFile.SaveFile(AFileName);
    if Result then
    begin
      EpiDocument.IncCycleNo;
      AddToRecent(AFileName);
      UpdateStatusBar(sucSave);
    end;
  finally
    Screen.Cursor := crDefault;
    Application.ProcessMessages;
  end;
end;

procedure TProjectFrame.OpenProjectOrderedWalkCallBack(
  const Relation: TEpiMasterRelation; const Depth: Cardinal;
  const Index: Cardinal; var aContinue: boolean; Data: Pointer);
var
  Frame: TRuntimeDesignFrame;
begin
  Frame := DoNewRuntimeFrame(Relation);
  Relation.AddCustomData(PROJECT_RUNTIMEFRAME_KEY, Frame);
  if Depth > 0 then
    BindKeyFields(TEpiDetailRelation(Relation));
end;

procedure TProjectFrame.CloseProjectOrderedWalkCallBack(
  const Relation: TEpiMasterRelation; const Depth: Cardinal;
  const Index: Cardinal; var aContinue: boolean; Data: Pointer);
begin
  if (Depth > 0) then
    UnBindKeyFields(TEpiDetailRelation(Relation));
end;

function TProjectFrame.DoOpenProject(const AFileName: string): boolean;
var
  i: Integer;
  T1: TDateTime;
  T2: TDateTime;
  G: TEpiGroup;
  Rights: TEpiManagerRights;
  R: TEpiManagerRight;
begin
  Result := false;
  try
    DoCreateNewDocumentFile;

    T1 := Now;
    if not FDocumentFile.OpenFile(AFileName) then
    begin
      FreeAndNil(FDocumentFile);
      Exit;
    end;
    T2 := Now;
    if IsConsole then
      WriteLn('LoadProject: ', FormatDateTime('NN:SS:ZZZ', T2-T1));
  except
    FreeAndNil(FDocumentFile);
    // If ever this happens then it is because something not right happened
    // during OpenFile(...) and we need to notify the user.
    raise;
  end;

  try
    CommonProjectInit;

    FProjectTreeView.AddDocument(EpiDocument);

    try
      EpiDocument.Relations.OrderedWalk(@OpenProjectOrderedWalkCallBack);
    except
      if Assigned(FDocumentFile) then
        FreeAndNil(FDocumentFile);
      if Assigned(FActiveFrame) then FreeAndNil(FActiveFrame);
      raise
    end;

    if Assigned(FDocumentFile.AuthedUser) then
    begin
      Label1.Caption := 'User: ' + FDocumentFile.AuthedUser.FullName;
      Label2.Caption := 'Groups: ';
      Rights := [];
      for G in FDocumentFile.AuthedUser.Groups do
      begin
        Label2.Caption := Label2.Caption + G.Caption.Text + ',';
        Rights := Rights + G.ManageRights;
      end;
      Label3.Caption := 'Combined rights: ' + LineEnding;
      for R in Rights do
        Label3.Caption := Label3.Caption + ' *' + EpiManagerRightCaptions[R] + LineEnding;
    end
    else
      Label1.Caption := 'No auth';

    if EpiDocument.Relations.Count > 0 then
      FProjectTreeView.SelectedObject := EpiDocument.Relations[0];

    EpiDocument.Modified := false;
    Result := true;

    AddToRecent(DocumentFile.FileName);
    UpdateCaption;
  finally
  end;
end;

function TProjectFrame.DoCreateNewDocumentFile: TDocumentFile;
begin
  FDocumentFile := TDocumentFile.Create;
  FDocumentFile.OnAfterDocumentCreated := @AfterDocumentCreated;
  FDocumentFile.OnLoadError            := @LoadError;
  FDocumentFile.OnSaveThreadError      := @SaveThreadError;
  FDocumentFile.DataDirectory          := ManagerSettings.WorkingDirUTF8;
  FDocumentFile.UndoCopy               := true;

  Result := FDocumentFile;
  FStatusBar.Visible := true;
  FStatusBar.DocFile := result;
end;

function TProjectFrame.DoNewDataForm(ParentRelation: TEpiMasterRelation
  ): TEpiMasterRelation;
var
  MR: TEpiMasterRelation;
  Df: TEpiDataFile;
  Rel: TEpiRelate;
begin
  Result := nil;
  Df := nil;

  if Assigned(ParentRelation) then
  begin
    DF := ParentRelation.Datafile;
    if DF.KeyFields.Count = 0 then
    begin
      ShowMessage(
        'You must define a key with at least 1 variable' + LineEnding +
        'before you can create a related dataform'
      );
      Exit;
    end;

    if (ParentRelation is TEpiDetailRelation) then
    begin
      MR := TEpiDetailRelation(ParentRelation).MasterRelation;
      if MR.Datafile.KeyFields.Count = Df.KeyFields.Count then
      begin
        ShowMessage(
          'You must define a key with at least 1 variable more than' + LineEnding +
          'in the parent dataform!'
        );
        Exit;
      end;
    end;
  end;

  Result := FProjectTreeView.CreateRelation(ParentRelation);

  if Assigned(ParentRelation) then
  begin
    Rel := ParentRelation.Datafile.Relates.NewRelate;
    Rel.DetailRelation := Result;
  end;
end;

function TProjectFrame.DoNewRuntimeFrame(Relation: TEpiMasterRelation
  ): TRuntimeDesignFrame;
begin
  Inc(FrameCount);

  Result := TRuntimeDesignFrame.Create(Self);
  Result.Name := GetRandomComponentName;
  Result.Align := alClient;
  Result.Parent := Self;
  Result.Relation := Relation;
  Result.DeActivate(true);
  Result.OnGetUser := @GetAuthorizedUser;
  Result.OnUpdateStatusBarContent := @DesignerUpdateStatusbar;
end;

procedure TProjectFrame.DoCreateNewProject;
var
  Doc: TEpiDocument;
begin
  MainForm.BeginUpdatingForm;

  Doc := DoCreateNewDocument;
  CommonProjectInit;

  FProjectTreeView.AddDocument(Doc);

  DoNewDataForm(nil);
  FProjectTreeView.SelectedObject := EpiDocument;
  EpiDocument.Modified := false;

  MainForm.EndUpdatingForm;
end;

procedure TProjectFrame.DoCloseProject;
begin
  // Close ValueLabel Editor - else the reference to
  // ValueLabelSets is incomplete!
  CloseValueLabelEditor2;

  // Close Alignment Form.
  AlignForm.DesignFrame := nil;

  FreeAndNil(AlignForm);
  FreeAndNil(PropertiesForm);


  FActiveFrame := nil;
  FreeAndNil(FBackupTimer);

  if (FSaveWarningRes = mrYes) and
     (Assigned(FDocumentFile)) and
     (FDocumentFile.UndoCopy)
  then
    FDocumentFile.UndoCopy := false;

  if Assigned(FDocumentFile) then
    FDocumentFile.Document.Relations.OrderedWalk(@CloseProjectOrderedWalkCallBack, nil);

  FreeAndNil(FDocumentFile);

  Modified := false;
end;

procedure TProjectFrame.EpiDocumentModified(Sender: TObject);
begin
  Modified := TEpiDocument(Sender).Modified;

  // Activates/Deactivates timed backup.
  if Assigned(FBackupTimer) and Assigned(EpiDocument) then
    FBackupTimer.Enabled := EpiDocument.Modified;
end;

procedure TProjectFrame.UpdateDefaultExtension(const Dlg: TOpenDialog);
begin
  case Dlg.FilterIndex of
    1: Dlg.DefaultExt := 'epx';
    2: Dlg.DefaultExt := 'epz';
  end;
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
    if DocumentFile.IsSaved then
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

procedure TProjectFrame.RuntimeFrameUpdateFrameOrderedWalkCallBack(
  const Relation: TEpiMasterRelation; const Depth: Cardinal;
  const Index: Cardinal; var aContinue: boolean; Data: Pointer);
begin
  TRuntimeDesignFrame(Relation.FindCustomData(PROJECT_RUNTIMEFRAME_KEY)).UpdateFrame;
end;

procedure TProjectFrame.ProjectTreeDelete(const Relation: TEpiMasterRelation);
var
  Frame: TCustomFrame;
begin
  if (Relation.InheritsFrom(TEpiDetailRelation)) then
    UnBindKeyFields(TEpiDetailRelation(Relation));

  Frame := TCustomFrame(Relation.FindCustomData(PROJECT_RUNTIMEFRAME_KEY));
  Frame.Free;
end;

procedure TProjectFrame.ProjectTreeEdited(Sender: TObject;
  const AObject: TEpiCustomBase; ObjectType: TEpiVTreeNodeObjectType);
begin
  (AObject.FindCustomData(PROJECT_RUNTIMEFRAME_KEY) as IProjectFrame).Activate;
end;

procedure TProjectFrame.ProjectTreeEditing(Sender: TObject;
  const AObject: TEpiCustomBase; ObjectType: TEpiVTreeNodeObjectType;
  var Allowed: Boolean);
begin
  Allowed := Authenticator.IsAuthorized([earDefineProject]);

  if (ObjectType = otRelation) then
    Allowed := Allowed and
               (not (TEpiCustomItem(AObject).ProtectedItem));

  if Allowed then
    (AObject.FindCustomData(PROJECT_RUNTIMEFRAME_KEY) as IProjectFrame).DeActivate(false);
end;

procedure TProjectFrame.ProjectTreeError(const Msg: String);
begin
  ShowMessage(Msg);
end;

procedure TProjectFrame.ProjectTreeGetHint(Sender: TObject;
  const AObject: TEpiCustomBase; ObjectType: TEpiVTreeNodeObjectType;
  var HintText: string);
begin
  //  No hints - the ones produces are sufficient.
end;

procedure TProjectFrame.ProjectTreeNewRelation(
  const Relation: TEpiMasterRelation);
var
  Frame: TRuntimeDesignFrame;
begin
  Relation.Datafile.Caption.Text := 'Dataset ' + IntToStr(FrameCount);

  if Relation.InheritsFrom(TEpiDetailRelation)
  then
    begin
      Relation.Datafile.AfterRecordState := arsReturnToParent;
      BindKeyFields(TEpiDetailRelation(Relation));
    end;

  Frame := DoNewRuntimeFrame(Relation);
  Frame.Activate;
  Frame.DeActivate(true);
  Relation.AddCustomData(PROJECT_RUNTIMEFRAME_KEY, Frame);
end;

procedure TProjectFrame.ProjectTreeSelected(Sender: TObject;
  const AObject: TEpiCustomBase; ObjectType: TEpiVTreeNodeObjectType);
begin
  if not Supports(AObject.FindCustomData(PROJECT_RUNTIMEFRAME_KEY), IProjectFrame, FActiveFrame)
  then
    Exit;

  if ObjectType = otRelation then
    begin
      AlignForm.DesignFrame := TRuntimeDesignFrame(AObject.FindCustomData(PROJECT_RUNTIMEFRAME_KEY));
      FStatusBar.Datafile := TEpiMasterRelation(AObject).Datafile;
    end;

  FActiveFrame.Activate;
  FActiveFrame.AssignActionLinks;

  MainForm.DataFormBtn.Enabled := ObjectType = otRelation;
end;

procedure TProjectFrame.ProjectTreeSelecting(Sender: TObject; const OldObject,
  NewObject: TEpiCustomBase; OldObjectType,
  NewObjectType: TEpiVTreeNodeObjectType; var Allowed: Boolean);
var
  IFrame: IProjectFrame;
begin
  Allowed :=
    // Cannot select empty tree node
    (NewObjectType <> otEmpty) and
    // Selecting the same node again should do nothing.
    (OldObject <> NewObject);

  if Allowed and
     Assigned(OldObject) and
     Supports(OldObject.FindCustomData(PROJECT_RUNTIMEFRAME_KEY), IProjectFrame, IFrame)
  then
    Allowed := IFrame.DeActivate(true);
end;

procedure TProjectFrame.ProjectTreeShowPopupMenu(Sender: TObject;
  const AObject: TEpiCustomBase; ObjectType: TEpiVTreeNodeObjectType);
begin
  case ObjectType of
    otEmpty,
    otFake:
      Exit;
    otRelation:
      MainForm.DataformPopupMenu.PopUp;
    otProject:
      MainForm.ProjectPopupMenu.PopUp;
  end;
end;

procedure TProjectFrame.KeyFieldEvent(const Sender: TEpiCustomBase;
  const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
var
  MasterField: TEpiField absolute Sender;
  DetailFields: TEpiFields;
  DetailField: TEpiField;

begin
  DetailFields := TEPiFields(MasterField.FindCustomData(PROJECT_RELATION_KEYFIELD_CHILD_KEY));

  case EventGroup of
    eegCustomBase:
      case TEpiCustomChangeEventType(EventType) of
        ecceDestroy:
          begin
            // I am being destroyed, remember to destroy the detailfields list too.
            MasterField.RemoveCustomData(PROJECT_RELATION_KEYFIELD_CHILD_KEY);
            DetailFields.Free;
          end;
        ecceUpdate,
        ecceName,
        ecceAddItem,
        ecceDelItem,
        ecceSetItem,
        ecceSetTop,
        ecceSetLeft:
          exit;   // We don't care about being updated in these ways.

        ecceText:
          // Text can be from Question or Notes.
          // Only update question -> notes is "personal" to the field.
          if Initiator = MasterField.Question then
            for DetailField in DetailFields do
              DetailField.Question.Text := MasterField.Question.Text;

        ecceReferenceDestroyed:
          Exit;   // Just ignore.
      end;

    eegFields:
      case TEpiFieldsChangeEventType(EventType) of
        efceSetDecimal:
          for DetailField in DetailFields do
            DetailField.Decimals := MasterField.Decimals;

        efceSetLeft:
          Exit;     // Ignore.

        efceSetLength:
          for DetailField in DetailFields do
            DetailField.Length := MasterField.Length;

        efceSetTop,
        efceSetSize,
        efceData,
        efceResetData,
        efceEntryMode,
        efceConfirmEntry:
          Exit;   // ignore

        efceShowValueLabel:
          for DetailField in DetailFields do
            DetailField.ShowValueLabel := MasterField.ShowValueLabel;

        efceRepeatValue:
          Exit;  // Ignore

        efceDefaultValue:
          Exit;  // Ignore

        efceValueLabelWriteTo:
          Exit;  // Ignore

        efceForcePickList:
          Exit;  // Ignore

        efceValueLabelSet:
          for DetailField in DetailFields do
            DetailField.ValueLabelSet := MasterField.ValueLabelSet;

        efceZeroFilled:
          for DetailField in DetailFields do
            TEpiIntField(DetailField).ZeroFilled := TEpiIntField(MasterField).ZeroFilled;
      end;

    eegRange:
      case TEpiRangeChangeEventType(EventType) of
        erceSetStart:
          for DetailField in DetailFields do
            begin
              if (not Assigned(DetailField.Ranges)) then
              begin
                DetailField.Ranges := TEpiRanges.Create(DetailField);
                DetailField.Ranges.NewRange;
              end;
              DetailField.Ranges[0].AsFloat[true] := MasterField.Ranges[0].AsFloat[true];
            end;
        erceSetEnd:
          for DetailField in DetailFields do
          begin
            if (not Assigned(DetailField.Ranges)) then
            begin
              DetailField.Ranges := TEpiRanges.Create(DetailField);
              DetailField.Ranges.NewRange;
            end;
            DetailField.Ranges[0].AsFloat[false] := MasterField.Ranges[0].AsFloat[false];
          end;
      end;
  end;
end;

procedure TProjectFrame.BindKeyFields(DR: TEpiDetailRelation);
var
  MasterKeyFields: TEpiFields;
  DetailKeyFields: TEpiFields;
  MasterField: TEpiField;
  DetailField: TEpiField;
  ChildFields: TEpiFields;
  i: Integer;
begin
  if not Assigned(DR) then exit;

  MasterKeyFields := DR.MasterRelation.Datafile.KeyFields;
  DetailKeyFields := DR.Datafile.KeyFields;

  for i := 0 to MasterKeyFields.Count - 1 do
  begin
    MasterField := MasterKeyFields[i];
    DetailField := DetailKeyFields.FieldByName[MasterField.Name];

    // It does not matter if we register the same event multiple times, since
    // the underlying structure does not allow duplicates anyway...
    MasterField.RegisterOnChangeHook(@KeyFieldEvent, true);

    // Link to "child" field...
    ChildFields := TEpiFields(MasterField.FindCustomData(PROJECT_RELATION_KEYFIELD_CHILD_KEY));
    if (not Assigned(ChildFields)) then
    begin
      ChildFields := TEpiFields.Create(nil);
      ChildFields.Sorted := false;
      ChildFields.ItemOwner := false;
      ChildFields.UniqueNames := false;
      MasterField.AddCustomData(PROJECT_RELATION_KEYFIELD_CHILD_KEY, ChildFields);
    end;
    ChildFields.AddItem(DetailField);
  end;
end;

procedure TProjectFrame.UnBindKeyFields(DR: TEpiDetailRelation);
var
  MasterKeyFields: TEpiFields;
  DetailKeyFields: TEpiFields;
  MasterField: TEpiField;
  DetailField: TEpiField;
  ChildFields: TEpiFields;
  i: Integer;
begin
  if not Assigned(DR) then exit;

  MasterKeyFields := DR.MasterRelation.Datafile.KeyFields;
  DetailKeyFields := DR.Datafile.KeyFields;

  for i := 0 to MasterKeyFields.Count - 1 do
  begin
    MasterField := MasterKeyFields[i];
    DetailField := DetailKeyFields.FieldByName[MasterField.Name];

    MasterField.UnRegisterOnChangeHook(@KeyFieldEvent);

    // Link to "child" field...
    ChildFields := TEpiFields(MasterField.FindCustomData(PROJECT_RELATION_KEYFIELD_CHILD_KEY));
    if (Assigned(ChildFields)) then
    begin
      ChildFields.Free;
      MasterField.RemoveCustomData(PROJECT_RELATION_KEYFIELD_CHILD_KEY);
    end;
  end;
end;

procedure TProjectFrame.ShowHintMsg(Sender: TObject; Ctrl: TControl;
  const Msg: string);
var
  R: TRect;
  P: TPoint;
begin
  if (Msg = '') and (Ctrl = nil) then
  begin
    FHintWindow.Hide;
    Exit;
  end;

  R := FHintWindow.CalcHintRect(0, Msg, nil);
  P := Ctrl.ClientToScreen(Point(0, Ctrl.Height + 2));
  OffsetRect(R, P.X, P.Y);
  FHintWindow.ActivateHint(R, Msg);
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
    if not DocumentFile.IsSaved then
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
  SaveProjectAction.ShortCut       := P_SaveProject;
  SaveProjectAsAction.ShortCut     := P_SaveProjectAs;
  NewDataFormAction.ShortCut       := P_NewDataForm;
  DeleteDataFormAction.ShortCut    := P_DelDataForm;
  ProjectSettingsAction.ShortCut   := P_ProjectSettings;
  ValueLabelEditorAction.ShortCut  := P_StartValueLabelEditor;
  OpenProjectAction.ShortCut       := P_OpenProject;
  KeyFieldsAction.ShortCut         := P_KeyFields;
  ViewLogAction.ShortCut           := P_ViewLog;
  DefineEntryRightsAction.ShortCut := P_EntryRight;
  DefineGroupsAction.ShortCut      := P_Groups;
  DefineUsersAction.ShortCut       := P_Users;
end;

function TProjectFrame.GetEditingProjectTree: boolean;
begin
  result := FProjectTreeView.EditingCaption;
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
{  if Assigned(FActiveFrame) then
  with Msg do
    Result := SendMessage(FActiveFrame.Handle, Msg, WParam, LParam);   }
end;

procedure TProjectFrame.UpdateRecentFilesDropDown;
var
  Mi: TMenuItem;
  K: Word;
  Shift: TShiftState;
  i: Integer;
begin
  ShortCutToKey(M_OpenRecent, K, Shift);

  LoadRecentFilesIni(GetRecentIniFileName);

  ProjectRecentFilesDropDownMenu.Items.Clear;

  for i := 0 to RecentFiles.Count - 1 do
  begin
    // Main menu
    Mi := TMenuItem.Create(ProjectRecentFilesDropDownMenu);
    Mi.Name := 'project_frame_recent' + inttostr(i);
    Mi.Caption := RecentFiles[i];
    Mi.OnClick := @OpenRecentMenuItemClick;
    if i < 9 then
      Mi.ShortCut := KeyToShortCut(VK_1 + i, Shift);
    ProjectRecentFilesDropDownMenu.Items.Add(Mi);
  end;
end;

procedure TProjectFrame.DesignerUpdateStatusbar(Sender: TObject;
  SelectionList: TEpiCustomList);
begin
  FStatusBar.Selection := SelectionList;
  FStatusBar.Update();
end;

function TProjectFrame.GetAuthorizedUser(Sender: TObject): TEpiUser;
begin
  result := FDocumentFile.AuthedUser;
end;

function TProjectFrame.IsProjectSetupForAdmin: Boolean;
var
  S: String;
  MsgDlgType: TMsgDlgType;
  Res: TModalResult;
  User: TEpiUser;

  function ShowUserForm: TModalResult;
  var
    F: TAdminUserForm;
  begin
    F := TAdminUserForm.Create(Self);
    F.User  := User;
    F.Admin := EpiDocument.Admin;
    F.NewUser := true;
    Result  := F.ShowModal;
    F.Free;
  end;

begin
  Result := true;

  if (not EpiDocument.Admin.Initialized) then
    begin
      Result := false;

      if (EpiDocument.PassWord <> '') then
        begin
          S := 'It is not possible to have a single-password project AND user administration active at the same time!' + LineEnding +
               LineEnding +
               'If you choose to continue the current password is reset and you will be asked to create a new user, which will automatically be added to the Administrators group!' + LineEnding +
               LineEnding +
               'Afterwards the project will save and re-open.' + LineEnding +
               'Then login with the new user!' + LineEnding +
               LineEnding +
               'Continue?';
          MsgDlgType := mtWarning;
        end
      else
        begin
          S :=
            'User/Group Administration adds complex access control to the project.' + LineEnding +
            'Press Cancel if you only wished to encrypt data' + LineEnding +
            LineEnding +
            'These steps are applied when You add User/Group Administration:' + LineEnding +
            '1. Define the "main admin" (current step)' + LineEnding +
            '   (login, name, password etc)' + LineEnding +
            '2. When You Press "OK" the project file is saved' + LineEnding +
            '3. The project is re-opened when you login as "main admin"' + LineEnding +
            '4. You may then define groups and users.';
          MsgDlgType := mtInformation;
        end;

      Res := MessageDlg('Warning',
                          S,
                          MsgDlgType,
                          mbOKCancel, 0,
                          mbCancel
                        );
      if (Res <> mrOK) then
        Exit;

      EpiDocument.PassWord := '';

      User := EpiDocument.Admin.NewUser;
      if ShowUserForm <> mrOK then
        begin
          User.Free;
          Exit;
        end;

      EpiDocument.Admin.InitAdmin;
      User.Groups.AddItem(EpiDocument.Admin.Admins);
      EpiDocument.Admin.Created := Now;
      EpiDocument.Admin.DaysBetweenPasswordChange := ManagerSettings.DaysBetweenPassword;

      if (not SaveProject(false)) then
      begin
        User.Free;
        Exit;
      end;

      Res := MessageDlg('Information',
                        'User/Group Administration successfully added.' + LineEnding +
                        'Re-open project as "' + User.Login + '": ' + LineEnding +
                        DocumentFile.FileName,
                        mtInformation,
                        mbOKCancel, 0,
                        mbOK
                       );

      PostMessage(MainForm.Handle, LM_MAIN_CLOSEPROJECT, 1, 0);
      if (Res = mrOK) then
        PostMessage(MainForm.Handle, LM_MAIN_OPENRECENT, 0, 0);
      Exit;
    end;
end;

function TProjectFrame.SelectDataformIfNotSelected: Boolean;
begin
  Result := true;
  case FProjectTreeView.SelectedObjectType of
    otRelation:
      // Do nothing - a Dataform IS selected!
      Exit;

    otEmpty,
    otFake,
    otProject:
      // Either the Project is selected OR nothing is selected.
      // Get First Dataform.
      begin
        if EpiDocument.Relations.Count = 0 then
        begin
          Result := false;
          Exit;
        end;

        FProjectTreeView.SelectedObject := EpiDocument.Relations[0];
      end;
  end;
end;

constructor TProjectFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FStatusBar := TManagerStatusBar.Create(Self);
  FStatusBar.Parent := self;
  FStatusBar.Align := alBottom;
  FStatusBar.Visible := false;
  FStatusBar.LoadSettings;

  FrameCount := 1;
  FActiveFrame := nil;
  FDataFileTreeViewCaptionUpdating := false;

  AlignForm := TAlignmentForm.Create(self);

  PropertiesForm := TPropertiesForm.Create(self);
  PropertiesForm.OnShowHintMsg := @ShowHintMsg;

  FHintWindow := THintWindow.Create(Self);
  FHintWindow.AutoHide := true;
  FHintWindow.HideInterval := 5 * 1000;

  FProjectTreeView := TEpiVProjectTreeViewFrame.Create(Self);
  with FProjectTreeView do
  begin
    MinDocumentCount    := 1;
    MaxDocumentCount    := 1;
    AllowSelectProject  := true;
    DisplayMode         := pdmCommon;
    EditCaption         := true;
    EditStructure       := true;
    ShowHint            := true;
    ShowProject         := true;
    ShowProtected       := true;

    OnDelete            := @ProjectTreeDelete;
    OnEdited            := @ProjectTreeEdited;
    OnEditing           := @ProjectTreeEditing;
    OnError             := @ProjectTreeError;
    OnGetHint           := @ProjectTreeGetHint;
    OnNewRelation       := @ProjectTreeNewRelation;
    OnShowPopupMenu     := @ProjectTreeShowPopupMenu;
    OnTreeNodeSelected  := @ProjectTreeSelected;
    OnTreeNodeSelecting := @ProjectTreeSelecting;
  end;
  FProjectTreeView.Align  := alClient;
  FProjectTreeView.Parent := ProjectPanel;
  {$IFDEF LCLCocoa}
     FProjectTreeView.BorderSpacing.Left := 10;
     ToolBar1.BorderSpacing.Left := 10;
  {$ENDIF}
  CreateCoreLogger;

  {$IFNDEF EPI_DEBUG}
  Splitter2.Visible := False;
  Panel1.Visible := false;
  Divider4.Visible := false;
  ProjectTestToolBtn.Visible := false;
  {$ENDIF}

  UpdateRecentFilesDropDown;
  LoadSplitterPosition(Splitter1, 'ProjectSplitter');
end;

destructor TProjectFrame.Destroy;
begin
  if ManagerSettings.SaveWindowPositions then
    SaveSplitterPosition(Splitter1, 'ProjectSplitter');
  DoCloseProject;
  inherited Destroy;
end;

procedure TProjectFrame.CloseQuery(var CanClose: boolean);
var
  res: LongInt;
begin
  FSaveWarningRes := mrYes;

  // If Seleted Page is StudyUnit, then do a "silent" deactive in order to
  // check if content of page can actually be saved.
  if (Assigned(FActiveFrame)) and
     (not FActiveFrame.DeActivate(false)) then
  begin
    FActiveFrame.Activate;
    CanClose := false;
    Exit;
  end;

  if Modified or
    (Assigned(EpiDocument) and (EpiDocument.Modified)) then
  begin
    FSaveWarningRes := MessageDlg('Warning',
      'Project data content modified.' + LineEnding +
      'Store project permanently on disk before exit?',
      mtWarning, mbYesNoCancel, 0, mbCancel);

    if FSaveWarningRes = mrNo then
    begin
      Res := MessageDlg('Warning',
        'Project content is NOT saved to disk.' + LineEnding +
        'Choose YES to permanently store data on disk!' + LineEnding +
        LineEnding +
        'Save project to disk before exit?',
        mtWarning, mbYesNoCancel, 0, mbCancel);
    end;

    case FSaveWarningRes of
      mrYes:    CanClose := SaveProject(False);
      mrCancel: CanClose := false;
    end;

    FActiveFrame.Activate;
  end;
end;

class procedure TProjectFrame.RestoreDefaultPos(F: TProjectFrame);
var
  Splitter: TSplitter;
begin
  if Assigned(F) then
  begin
    Splitter := F.Splitter1;
    Splitter.SetSplitterPosition(180);
  end else begin
    Splitter := TSplitter.Create(nil);
    Splitter.Left := 180;
  end;
  SaveSplitterPosition(Splitter, 'ProjectSplitter');
  if not Assigned(F) then
    Splitter.Free;

  RestoreDefaultPosValueLabelEditor2;
  RestoreDefaultPosEntryRightsForm;
  RestoreDefaultPosDefineUsersForm;
  RestoreDefaultPosDefineGroupsForm;

  TProjectSettingsForm.RestoreDefaultPos;
  //TKeyFieldsForm.RestoreDefaultPos;
  TAlignmentForm.RestoreDefaultPos;
  TPropertiesForm.RestoreDefaultPos(PropertiesForm);
  TRuntimeDesignFrame.RestoreDefaultPos(nil);
end;

procedure TProjectFrame.UpdateFrame;
begin
  UpdateShortCuts;

  TStudyUnitFrame(EpiDocument.FindCustomData(PROJECT_RUNTIMEFRAME_KEY)).UpdateFrame;

  FStatusBar.LoadSettings;
  FStatusBar.DocFile := FDocumentFile;

  EpiDocument.Relations.OrderedWalk(@RuntimeFrameUpdateFrameOrderedWalkCallBack);
  if (FProjectTreeView.SelectedObjectType = otRelation) then
    FStatusBar.Datafile := TEpiMasterRelation(FProjectTreeView.SelectedObject).Datafile;
end;

procedure TProjectFrame.UpdateStatusBar(
  Condition: TEpiVCustomStatusbarUpdateCondition);
begin
  FStatusBar.Update(Condition);
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
    Dlg.FilterIndex := ManagerSettings.SaveType + 1;
    UpdateDefaultExtension(Dlg);

    if DocumentFile.IsSaved then
    begin
      Dlg.InitialDir := ExtractFilePath(DocumentFile.FileName);
      Dlg.FileName := DocumentFile.FileName
    end else
      Dlg.InitialDir := ManagerSettings.WorkingDirUTF8;

    Dlg.OnTypeChange := @SaveDlgTypeChange;
    Dlg.Options := Dlg.Options + [ofOverwritePrompt, ofExtensionDifferent];

    if not Dlg.Execute then exit;
    Fn := Dlg.FileName;
    Dlg.Free;
  end else
    Fn := DocumentFile.FileName;

  try
    result := DoSaveProject(Fn);
  except
    on E: Exception do
      begin
        MessageDlg('Error',
          'Unable to save project to:' + LineEnding +
          DocumentFile.FileName + LineEnding +
          'Error message: ' + E.Message,
          mtError, [mbOK], 0);
        Exit;
      end;
  end;
  if Result then
    EpiDocument.Modified := false;
  UpdateCaption;
end;

function TProjectFrame.Import(const FromCB: boolean): boolean;
var
  Frame: TRuntimeDesignFrame;
  MR: TEpiMasterRelation;
begin
  // Assumes the user want to open new project with direct import.

  // Relation[0] will always exists!
  MR := EpiDocument.Relations[0];
  FProjectTreeView.SelectedObject := MR;
  Frame := TRuntimeDesignFrame(MR.FindCustomData(PROJECT_RUNTIMEFRAME_KEY));
  if FromCB then
    Result := Frame.ImportCBAction.Execute
  else
    Result := Frame.ImportAction.Execute;
end;

procedure TProjectFrame.CreateNewProject;
begin
  DoCreateNewProject;
end;

procedure TProjectFrame.AssignActionLinks;
begin
  // File
  with MainForm do
  begin
    SaveProjectMenuItem.Action   := SaveProjectAction;
    SaveProjectAsMenuItem.Action := SaveProjectAsAction;

    // Project
    ProjectPropertiesMenuItem.Action := ProjectSettingsAction;
    ValueLabelsMenuItem.Action       := ValueLabelEditorAction;
    StudyInfoMenuItem.Action         := StudyInformationAction;

    // --project details popup-menu
    ProjectPropertiesPopupMenuItem.Action := ProjectSettingsAction;
    ValueLabelEditorPopupMenuItem.Action  := ValueLabelEditorAction;
    SetPasswordPopupMenuItem.Action       := SetProjectPasswordAction;
    StudyInfoPopupMenuItem.Action         := StudyInformationAction;


    // User Access
    // - Simple Password
    SetSimplePasswordMenuItem.Action      := SetProjectPasswordAction;
    RemoveSimplePasswordMenuItem.Action   := RemoveProjectPassword;

    // - Extended Access
    DefineExtendedAccessMenuItem.Action   := DefineExtendedAccessAction;
    DefineGroupsMenuItem.Action           := DefineGroupsAction;
    DefineUsersMenuItem.Action            := DefineUsersAction;
    DefineEntryRightsMenuItem.Action      := DefineEntryRightsAction;
    ExportSecurityLogMenuItem.Action      := ExportLogAction;
//    ViewLogMenuItem.Action                := ViewLogAction;
  end;

  NewProjectToolBtn.Action := MainForm.NewProjectAction;
  FActiveFrame.AssignActionLinks;
end;

procedure TProjectFrame.StopEditingProjectTree;
begin
  if EditingProjectTree then
    FProjectTreeView.StopEditing;
end;

end.


