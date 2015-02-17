unit project_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, ExtCtrls, ComCtrls, ActnList, Controls,
  Dialogs, epidocument, epidatafiles, epicustombase, epirelations,
  manager_messages, LMessages, Menus, epiv_documentfile, types,
  design_runtimedesigner, project_types, epiv_projecttreeview_frame;

type

  { TProjectFrame }

  TProjectFrame = class(TFrame)
    AdminAction: TAction;
    MenuItem1: TMenuItem;
    ProjectRecentFilesDropDownMenu: TPopupMenu;
    ProgressBar1: TProgressBar;
    Splitter1: TSplitter;
    StudyInformationAction: TAction;
    KeyFieldsAction: TAction;
    ProjectPasswordAction: TAction;
    OpenProjectAction: TAction;
    NewProjectToolBtn: TToolButton;
    ToolButton2: TToolButton;
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
    ToolButton1: TToolButton;
    SaveProjectToolBtn: TToolButton;
    SaveProjectAsToolBtn: TToolButton;
    ToolButton4: TToolButton;
    AddDataFormToolBtn: TToolButton;
    DeleteDataFormToolBtn: TToolButton;
    ToolButton7: TToolButton;
    procedure AdminActionExecute(Sender: TObject);
    procedure DeleteDataFormActionExecute(Sender: TObject);
    procedure DeleteDataFormActionUpdate(Sender: TObject);
    procedure DocumentProgress(const Sender: TEpiCustomBase;
      ProgressType: TEpiProgressType; CurrentPos, MaxPos: Cardinal;
      var Canceled: Boolean);
    procedure NewDataFormActionExecute(Sender: TObject);
    procedure OpenProjectActionExecute(Sender: TObject);
    procedure ProjectPasswordActionExecute(Sender: TObject);
    procedure ProjectSettingsActionExecute(Sender: TObject);
    procedure SaveProjectActionExecute(Sender: TObject);
    procedure SaveProjectActionUpdate(Sender: TObject);
    procedure SaveProjectAsActionExecute(Sender: TObject);
    procedure StudyInformationActionExecute(Sender: TObject);
    procedure ValueLabelEditorActionExecute(Sender: TObject);
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
    function  DoSaveProject(AFileName: string): boolean;
    procedure OpenProjectOrderedWalkCallBack(
      const Relation: TEpiMasterRelation; const Depth: Cardinal;
      const Index: Cardinal; var aContinue: boolean;
      Data: Pointer = nil);
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
//    FFileName: string;
    procedure LoadError(const Sender: TEpiCustomBase; ErrorType: Word;
      Data: Pointer; out Continue: boolean);
    function GetEpiDocument: TEpiDocument;
    { Messages }
    // Message relaying...
    procedure LMDesignerAdd(var Msg: TLMessage); message LM_DESIGNER_ADD;
    procedure OpenRecentMenuItemClick(Sender: TObject);
    procedure UpdateRecentFilesDropDown;
  public
    { Access/Helper methods }
    function SelectDataformIfNotSelected: Boolean;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   CloseQuery(var CanClose: boolean);
    procedure   UpdateFrame;
    procedure   UpdateStatusBar;
    function    OpenProject(Const AFileName: string): boolean;
    function    SaveProject(Const ForceSaveAs: boolean): boolean;
    function    Import(Const FromCB: boolean): boolean;
    procedure   CreateNewProject;
    procedure   AssignActionLinks;
    property   DocumentFile: TDocumentFile read FDocumentFile;
    property   EpiDocument: TEpiDocument read GetEpiDocument;
    property   ActiveFrame: IProjectFrame read FActiveFrame;
    property   Modified: Boolean read FModified write SetModified;
    property   OnModified: TNotifyEvent read FOnModified write SetOnModified;
  public
    class procedure   RestoreDefaultPos(F: TProjectFrame);
  end;

implementation

{$R *.lfm}

uses
  Clipbrd, epimiscutils,
  main, settings2, settings2_var, epistringutils,
  valuelabelseditor_form2, LazFileUtils,
  managerprocs, LCLType, LCLIntf, project_settings,
  shortcuts, project_keyfields_form,
  align_form, RegExpr, project_studyunit_frame,
  design_properties_form, admin_form
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
      'If you continue all fields referecing the valuelabelset will be lost!' + LineEnding +
      LineEnding +
      'Continue loading the project?',
      mtWarning,
      mbYesNo,
      0,
      mbNo
    );
  Continue := Res = mrYes;
end;

procedure TProjectFrame.DocumentProgress(const Sender: TEpiCustomBase;
  ProgressType: TEpiProgressType; CurrentPos, MaxPos: Cardinal;
  var Canceled: Boolean);
Const
  LastUpdate: Cardinal = 0;
  ProgressUpdate: Cardinal = 0;
begin
  case ProgressType of
    eptInit:
      begin
        ProgressUpdate := MaxPos div 50;
        ProgressBar1.Position := CurrentPos;
        ProgressBar1.Visible := true;
        ProgressBar1.Max := MaxPos;
        Application.ProcessMessages;
      end;
    eptFinish:
      begin
        ProgressBar1.Visible := false;
        Application.ProcessMessages;
        LastUpdate := 0;
      end;
    eptRecords:
      begin
        if CurrentPos > (LastUpdate + ProgressUpdate) then
        begin
          ProgressBar1.Position := CurrentPos;
          {$IFNDEF MSWINDOWS}
          Application.ProcessMessages;
          {$ENDIF}
          LastUpdate := CurrentPos;
        end;
      end;
  end;
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

procedure TProjectFrame.AdminActionExecute(Sender: TObject);
var
  F: TAdminForm;
begin
  F := TAdminForm.Create(self, EpiDocument.Admin);
  F.ShowModal;
  F.Free;
end;

procedure TProjectFrame.DeleteDataFormActionUpdate(Sender: TObject);
begin
  DeleteDataFormAction.Enabled :=
    FProjectTreeView.SelectedObjectType = otRelation;
end;

procedure TProjectFrame.OpenProjectActionExecute(Sender: TObject);
begin
  PostMessage(MainForm.Handle, LM_MAIN_OPENPROJECT, 0, 0);
end;

procedure TProjectFrame.OpenRecentMenuItemClick(Sender: TObject);
begin
  PostMessage(MainForm.Handle, LM_MAIN_OPENRECENT, WParam(Sender), 0);
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
  case Dlg.FilterIndex of
    1: Dlg.DefaultExt := 'epx';
    2: Dlg.DefaultExt := 'epz';
  end;

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

procedure TProjectFrame.ValueLabelEditorActionExecute(Sender: TObject);
begin
  ShowValueLabelEditor2(EpiDocument.ValueLabelSets);
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
  UpdateCaption;
  UpdateShortCuts;
  InitBackupTimer;

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
    EpiDocument.IncCycleNo;
    Result := DocumentFile.SaveFile(AFileName);
    AddToRecent(AFileName);
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
end;

function TProjectFrame.DoOpenProject(const AFileName: string): boolean;
var
  i: Integer;
  T1: TDateTime;
  T2: TDateTime;
begin
  Result := false;
  try
    FDocumentFile := TDocumentFile.Create;
    FDocumentFile.OnProgress := @DocumentProgress;
    FDocumentFile.OnLoadError := @LoadError;
    FDocumentFile.DataDirectory := ManagerSettings.WorkingDirUTF8;
    T1 := Now;
    if not FDocumentFile.OpenFile(AFileName) then
    begin
      FreeAndNil(FDocumentFile);
      Exit;
    end;
    T2 := Now;
//    if IsConsole then
//      WriteLn('LoadProject: ', FormatDateTime('NN:SS:ZZZ', T2-T1));
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


    FProjectTreeView.SelectedObject := EpiDocument.Relations[0];
    EpiDocument.Modified := false;
    Result := true;

    AddToRecent(DocumentFile.FileName);
    UpdateCaption;
  finally
  end;
end;

function TProjectFrame.DoNewDataForm(ParentRelation: TEpiMasterRelation
  ): TEpiMasterRelation;
var
  MR: TEpiMasterRelation;
  Df: TEpiDataFile;
begin
  Result := nil;
  Df := nil;

  if Assigned(ParentRelation) then
  begin
    DF := ParentRelation.Datafile;
    if DF.KeyFields.Count = 0 then
    begin
      ShowMessage(
        'You must define a key with at least 1 field' + LineEnding +
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
          'You must define a key with at least 1 field more than' + LineEnding +
          'in the parent dataform!'
        );
        Exit;
      end;
    end;
  end;

  Result := FProjectTreeView.CreateRelation(ParentRelation);
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
  AlignForm.Hide;

  PropertiesForm.Free;

  FActiveFrame := nil;
  FreeAndNil(FBackupTimer);
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
  Allowed := true;
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
  Relation.Datafile.Caption.Text := 'Dataform ' + IntToStr(FrameCount);

  if Relation.InheritsFrom(TEpiDetailRelation)
  then
    begin
      Relation.Datafile.AfterRecordState := arsNewRecord;
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

  FActiveFrame.Activate;
  FActiveFrame.AssignActionLinks;

  if ObjectType = otRelation then
    AlignForm.DesignFrame := TRuntimeDesignFrame(AObject.FindCustomData(PROJECT_RUNTIMEFRAME_KEY));

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
        efceEntryMode,
        efceConfirmEntry:
          Exit;   // ignore

        efceShowValueLabel:
          for DetailField in DetailFields do
            DetailField.ShowValueLabel := MasterField.ShowValueLabel;

        efceShowValueLabelNotes:
          for DetailField in DetailFields do
            DetailField.ShowValueLabelNotes := MasterField.ShowValueLabelNotes;

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

    FActiveFrame.Activate;
  end;
end;

class procedure TProjectFrame.RestoreDefaultPos(F: TProjectFrame);
begin
  RestoreDefaultPosValueLabelEditor2;
  TProjectSettingsForm.RestoreDefaultPos;
  TKeyFieldsForm.RestoreDefaultPos;
  TAlignmentForm.RestoreDefaultPos;
  TPropertiesForm.RestoreDefaultPos(PropertiesForm);
  TRuntimeDesignFrame.RestoreDefaultPos(nil);
end;

procedure TProjectFrame.UpdateFrame;
begin
  UpdateShortCuts;

  TStudyUnitFrame(EpiDocument.FindCustomData(PROJECT_RUNTIMEFRAME_KEY)).UpdateFrame;
  EpiDocument.Relations.OrderedWalk(@RuntimeFrameUpdateFrameOrderedWalkCallBack);
end;

procedure TProjectFrame.UpdateStatusBar;
begin
  if Assigned(FActiveFrame) then
    FActiveFrame.UpdateStatusbar;
end;

function TProjectFrame.OpenProject(const AFileName: string): boolean;
var
  T1: TDateTime;
  T2: TDateTime;
begin
  T1 := Now;
  Result := DoOpenProject(AFileName);
  T2 := now;
//  if IsConsole then
//    WriteLn('ProjectFrame.OpenProject: ', FormatDateTime('NN:SS:ZZZ', T2-T1));
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

    if DocumentFile.IsSaved then
    begin
      Dlg.InitialDir := ExtractFilePath(DocumentFile.FileName);
      Dlg.FileName := DocumentFile.FileName
    end else begin
      Dlg.InitialDir := ManagerSettings.WorkingDirUTF8;
      SaveDlgTypeChange(Dlg);
    end;

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
    AdminMenuItem.Action             := AdminAction;
    ProjectPasswordMenuItem.Action   := ProjectPasswordAction;
    StudyInfoMenuItem.Action         := StudyInformationAction;

    // --project details popup-menu
    ProjectPropertiesPopupMenuItem.Action := ProjectSettingsAction;
    ValueLabelEditorPopupMenuItem.Action  := ValueLabelEditorAction;
    SetPasswordPopupMenuItem.Action       := ProjectPasswordAction;
    StudyInfoPopupMenuItem.Action         := StudyInformationAction;
  end;

  NewProjectToolBtn.Action := MainForm.NewProjectAction;
  FActiveFrame.AssignActionLinks;
end;

end.


