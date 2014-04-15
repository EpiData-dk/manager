unit project_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, ExtCtrls, ComCtrls, ActnList,
  Controls, Dialogs, epidocument, epidatafiles, epicustombase, epirelations,
  manager_messages, LMessages, epiv_documentfile, types, design_runtimedesigner,
  project_types;

type

  { TProjectFrame }

  TProjectFrame = class(TFrame)
    ProgressBar1: TProgressBar;
    Splitter1: TSplitter;
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
    procedure DataFilesTreeViewChanging(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
    procedure DataFilesTreeViewDeletion(Sender: TObject; Node: TTreeNode);
    procedure DataFilesTreeViewEdited(Sender: TObject; Node: TTreeNode;
      var S: string);
    procedure DataFilesTreeViewEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure DataFilesTreeViewEditingEnd(Sender: TObject; Node: TTreeNode;
      Cancel: Boolean);
    procedure DataFilesTreeViewSelectionChanged(Sender: TObject);
    procedure DeleteDataFormActionExecute(Sender: TObject);
    procedure DeleteDataFormActionUpdate(Sender: TObject);
    procedure DocumentProgress(const Sender: TEpiCustomBase;
      ProgressType: TEpiProgressType; CurrentPos, MaxPos: Cardinal;
      var Canceled: Boolean);
    procedure KeyFieldsActionExecute(Sender: TObject);
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
    FRootNode: TTreeNode;
    FDocumentFile: TDocumentFile;
    FActiveFrame: IProjectFrame;
    FModified: Boolean;
    FOnModified: TNotifyEvent;
    FrameCount: integer;
    FDataFileTreeViewCaptionUpdating: boolean;
    procedure OnDataFileCaptionChange(Const Sender, Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
    procedure OnTitleChange(Const Sender, Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
    procedure AddToRecent(Const AFileName: string);
    // Common for open/create
    procedure CommonProjectInit;
    function  DoNewDataForm(ParentNode: TTreeNode): TEpiDataFile;
    function  DoNewRuntimeFrame(Df: TEpiDataFile): TRuntimeDesignFrame;
    // open existing
    procedure DoCreateRelationalStructure;
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
    procedure UpdateRootNode;
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
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   CloseQuery(var CanClose: boolean);
    procedure   UpdateFrame;
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
  managerprocs, Menus, LCLType, LCLIntf, project_settings,
  shortcuts, project_keyfields_form,
  align_form, RegExpr, project_studyunit_frame, epidatafilestypes,
  design_properties_form;

type
  TNodeData = class
  public
    DataFile: TEpiDataFile;
    Frame: IProjectFrame;
    Relation: TEpiMasterRelation;
  end;

{ TProjectFrame }

procedure TProjectFrame.NewDataFormActionExecute(Sender: TObject);
var
  Selected: TTreeNode;
begin
  Selected := DataFilesTreeView.Selected;
  if not Assigned(Selected) then
    Selected := FRootNode;

  DoNewDataForm(Selected);
end;

procedure TProjectFrame.KeyFieldsActionExecute(Sender: TObject);
var
  F: TKeyFieldsForm;
  NodeData: TNodeData;
begin
  NodeData := TNodeData(DataFilesTreeView.Selected.Data);
  if NodeData.DataFile = nil then exit;

  F := TKeyFieldsForm.Create(Self, NodeData.DataFile, EpiDocument.ValueLabelSets);
  F.ShowModal;
  F.Free;

  PropertiesForm.ReloadControls;
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

procedure TProjectFrame.DataFilesTreeViewSelectionChanged(Sender: TObject);
var
  TN: TTreeNode;
  F: TForm;
  ND: TNodeData;
begin
  TN := DataFilesTreeView.Selected;
  if not Assigned(TN) then exit;

  ND := TNodeData(TN.Data);
  FActiveFrame := ND.Frame;
  FActiveFrame.Activate;
  FActiveFrame.AssignActionLinks;

  if not Assigned(ND.DataFile) then
    AlignForm.DesignFrame := nil
  else
    AlignForm.DesignFrame := TRuntimeDesignFrame(ND.DataFile.FindCustomData(PROJECT_RUNTIMEFRAME_KEY));
end;

procedure TProjectFrame.DeleteDataFormActionExecute(Sender: TObject);
var
  CurrentNode: TTreeNode;
  DF: TEpiDataFile;
  Res: TModalResult;
  NewNode: TTreeNode;
  ND: TNodeData;
  Relation: TEpiMasterRelation;
begin
  CurrentNode := DataFilesTreeView.Selected;
  if CurrentNode = FRootNode then exit;

  ND := TNodeData(CurrentNode.Data);
  Relation := ND.Relation;

  DF := ND.DataFile;
  Res :=
    MessageDlg('Warning!',
      'Are you sure you want to delete the dataform "' + Df.Caption.Text + '" ?',
      mtWarning,
      mbYesNo,
      0,
      mbNo
  );

  if Res = mrNo then exit;

  NewNode := CurrentNode.GetPrev;
  if not Assigned(NewNode) then
    NewNode := CurrentNode.GetNextSibling;

  CurrentNode.Free;
//  Relation.Free;
  FActiveFrame := nil;

  DataFilesTreeView.Selected := NewNode;
end;

procedure TProjectFrame.DeleteDataFormActionUpdate(Sender: TObject);
begin
  DeleteDataFormAction.Enabled :=
    Assigned(DataFilesTreeView.Selected) and
    (DataFilesTreeView.Selected <> FRootNode);
end;

procedure TProjectFrame.DataFilesTreeViewEdited(Sender: TObject;
  Node: TTreeNode; var S: string);
begin
  if Trim(S) = '' then
  begin
    ShowMessage('A dataform name cannot be empty!');
    S := TNodeData(Node.Data).DataFile.Caption.Text;
  end else
  begin
    FDataFileTreeViewCaptionUpdating := True;
    TNodeData(Node.Data).DataFile.Caption.Text := S;
    FDataFileTreeViewCaptionUpdating := False;
  end;
end;

procedure TProjectFrame.DataFilesTreeViewDeletion(Sender: TObject;
  Node: TTreeNode);
var
  ND: TNodeData;
  Frame: TObject;
begin
  ND := TNodeData(Node.Data);
  Node.Data := nil;

  // This is the case with the studyunit frame!
  if not Assigned(ND.DataFile) then
  begin
    ND.Free;
    exit;
  end;

//  ND.Frame.Free =>
  ND.DataFile.FindCustomData(PROJECT_RUNTIMEFRAME_KEY).Free;
  ND.DataFile.Caption.UnRegisterOnChangeHook(@OnDataFileCaptionChange);

  // Do not Free Relation structure here, the relation is automatically destroyed
  // when the Datafile is free'd.
  ND.DataFile.Free;
  ND.Free;
end;

procedure TProjectFrame.DataFilesTreeViewChanging(Sender: TObject;
  Node: TTreeNode; var AllowChange: Boolean);
var
  NodeData: TNodeData;
begin
  if csDestroying in ComponentState then exit;

  NodeData := TNodeData(Node.Data);
  if Assigned(NodeData) then
    AllowChange := NodeData.Frame.DeActivate(true);
end;

procedure TProjectFrame.DataFilesTreeViewEditing(Sender: TObject;
  Node: TTreeNode; var AllowEdit: Boolean);
begin
  if Node = FRootNode then AllowEdit := false;

  if AllowEdit then FActiveFrame.DeActivate(false);
end;

procedure TProjectFrame.DataFilesTreeViewEditingEnd(Sender: TObject;
  Node: TTreeNode; Cancel: Boolean);
begin
  FActiveFrame.Activate;
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
  TN: TTreeNode;
  Res: Integer;
begin
  ProjectSettings := TProjectSettingsForm.Create(self, EpiDocument);
  Res := ProjectSettings.ShowModal;
  ProjectSettings.Free;

  if Res <> mrOK then Exit;

  TN := DataFilesTreeView.TopItem;
  While Assigned(TN) do
  begin
    TNodeData(TN.Data).Frame.UpdateFrame;
    TN := TN.GetNext;
  end;
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
begin
  case Dlg.FilterIndex of
    1: Dlg.DefaultExt := 'epx';
    2: Dlg.DefaultExt := 'epz';
  end;

  // TODO : Must be changed when supporting multiple desinger frames (take only the topmost/first datafile).
{  if (Dlg.FileName = '') and
     (Assigned(DocumentFile)) and
     (TRuntimeDesignFrame(ActiveFrame).ImportedFileName <> '') then
    Dlg.FileName := ChangeFileExt(TRuntimeDesignFrame(ActiveFrame).ImportedFileName, Dlg.DefaultExt); }
end;

procedure TProjectFrame.SaveProjectAsActionExecute(Sender: TObject);
begin
  SaveProject(True);
end;

procedure TProjectFrame.StudyInformationActionExecute(Sender: TObject);
begin
  DataFilesTreeView.Selected := FRootNode;
end;

procedure TProjectFrame.ValueLabelEditorActionExecute(Sender: TObject);
begin
  ShowValueLabelEditor2(EpiDocument.ValueLabelSets);
end;

procedure TProjectFrame.OnDataFileCaptionChange(const Sender,
  Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
var
  TN: TTreeNode;
begin
  if not Initiator.InheritsFrom(TEpiTranslatedText) then exit;
  if not ((EventGroup = eegCustomBase) and (EventType = Word(ecceText))) then exit;

  // This will happen if we are upding from a treenode..., hence we do not need to
  // update the node again!
  if FDataFileTreeViewCaptionUpdating then exit;

  TN := TTreeNode(TEpiDataFile(Initiator.Owner).FindCustomData(PROJECT_TREE_NODE_KEY));
  TN.Text := TEpiDataFile(Initiator.Owner).Caption.Text;
end;

procedure TProjectFrame.OnTitleChange(const Sender, Initiator: TEpiCustomBase;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  UpdateCaption;
  UpdateRootNode;
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

  Result.OnModified := @EpiDocumentModified;
end;

procedure TProjectFrame.AddToRecent(const AFileName: string);
begin
  settings2.AddToRecent(AFileName);
  MainForm.UpdateRecentFiles;
end;

procedure TProjectFrame.CommonProjectInit;
var
  NodeData: TNodeData;
  Frame: TStudyUnitFrame;
begin
  UpdateCaption;
  UpdateShortCuts;
  InitBackupTimer;

  Frame := TStudyUnitFrame.Create(self, EpiDocument.Study, (not DocumentFile.IsSaved));
  Frame.Align := alClient;
  Frame.Parent := self;
  NodeData := TNodeData.Create;
  NodeData.Frame := Frame;
  NodeData.DataFile := nil;
  NodeData.Relation := nil;
  FRootNode.Data := NodeData;
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

function TProjectFrame.DoOpenProject(const AFileName: string): boolean;
var
  TmpDoc: TEpiDocument;
  i: Integer;
begin
  Result := false;
  try
    FDocumentFile := TDocumentFile.Create;
    FDocumentFile.OnProgress := @DocumentProgress;
    FDocumentFile.OnLoadError := @LoadError;
    FDocumentFile.DataDirectory := ManagerSettings.WorkingDirUTF8;
    FDocumentFile.BackupDirectory := '';
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
    try
      for i := 0 to EpiDocument.DataFiles.Count - 1 do
      begin
        Inc(FrameCount);
        DoNewRuntimeFrame(EpiDocument.DataFiles[i]);
      end;
    except
      if Assigned(FDocumentFile) then
        FreeAndNil(FDocumentFile);
      if Assigned(FActiveFrame) then FreeAndNil(FActiveFrame);
      raise
    end;

    EpiDocument.Study.Title.RegisterOnChangeHook(@OnTitleChange);

    DoCreateRelationalStructure;
    CommonProjectInit;

    DataFilesTreeView.FullExpand;
    DataFilesTreeView.Selected := FRootNode.GetFirstChild;

    EpiDocument.Modified := false;
    Result := true;

    AddToRecent(DocumentFile.FileName);
    UpdateCaption;
    UpdateRootNode;
  finally
  end;
end;

function TProjectFrame.DoNewDataForm(ParentNode: TTreeNode): TEpiDataFile;
var
  Frame: TRuntimeDesignFrame;
  ASelected: TTreeNode;
  MR: TEpiMasterRelation;
  DR: TEpiDetailRelation;
  Df: TEpiDataFile;
  TN: TTreeNode;
  NodeData: TNodeData;
  F: TEpiField;
  i: Integer;
  Ft: TEpiFieldType;
begin
  Result := nil;
  Df := nil;
  if (ParentNode <> FRootNode) then
  begin
    NodeData := TNodeData(ParentNode.Data);
    Df := NodeData.DataFile;
    if Df.KeyFields.Count = 0 then
    begin
      ShowMessage(
        'You must define at least 1 keyfield' + LineEnding +
        'before you can create a related dataform'
      );
      Exit;
    end;

    if (NodeData.Relation is TEpiDetailRelation) then
    begin
      MR := TEpiDetailRelation(NodeData.Relation).MasterRelation;
      if MR.Datafile.KeyFields.Count = Df.KeyFields.Count then
      begin
        ShowMessage(
          'This dataform MUST have at least 1 keyfield more than' + LineEnding +
          'its parent!'
        );
        Exit;
      end;
    end;
  end;

  Result := EpiDocument.DataFiles.NewDataFile;
  Result.Caption.Text := 'Dataform ' + IntToStr(FrameCount);
  Result.Caption.RegisterOnChangeHook(@OnDataFileCaptionChange);

  NodeData := TNodeData.Create;
  Frame := DoNewRuntimeFrame(Result);
  NodeData.Frame := Frame;
  if ParentNode = FRootNode then
    NodeData.Relation := EpiDocument.Relations.NewMasterRelation
  else begin
    NodeData.Relation := TNodeData(ParentNode.Data).Relation.NewDetailRelation;

    Frame.Activate;
    for i := 0 to Df.KeyFields.Count - 1 do
    begin
      // In a related form, the "primary" keys cannot be autoinc - it would
      // screw up the numbering.
      Ft := Df.KeyFields[i].FieldType;
      if Ft = ftAutoInc then Ft := ftInteger;

      F := Result.NewField(Ft);
      F.Assign(Df.KeyFields[i]);
      F.EntryMode := emNoEnter;
      Result.KeyFields.AddItem(F);

      SendMessage(Frame.Handle, LM_DESIGNER_ADD, WPARAM(F), 0);
    end;
    Frame.DeActivate(true);
  end;
  NodeData.Relation.Datafile := Result;
  NodeData.DataFile := Result;

  if NodeData.Relation.InheritsFrom(TEpiDetailRelation)
  then
    BindKeyFields(TEpiDetailRelation(NodeData.Relation));

  TN := DataFilesTreeView.Items.AddChildObject(ParentNode, Result.Caption.Text, NodeData);
  Result.AddCustomData(PROJECT_TREE_NODE_KEY, TN);
  Result.AddCustomData(PROJECT_RELATION_KEY, NodeData.Relation);
  DataFilesTreeView.Selected := TN;
  DataFilesTreeViewSelectionChanged(DataFilesTreeView);
end;

function TProjectFrame.DoNewRuntimeFrame(Df: TEpiDataFile): TRuntimeDesignFrame;
var
  Bogus: HWND;
begin
  Inc(FrameCount);

  Result := TRuntimeDesignFrame.Create(Self);
  Result.Name := GetRandomComponentName;
  Result.Align := alClient;
  Result.Parent := Self;
  Result.DataFile := Df;
{  Result.OpenProjectToolBtn.Action := OpenProjectAction;
  Result.SaveProjectToolBtn.Action := SaveProjectAction;
  Result.SaveProjectAsToolBtn.Action := SaveProjectAsAction;
  Result.ProjectToolBar.Images := ProjectImageList;   }
  Result.DeActivate(true);
  Df.AddCustomData(PROJECT_RUNTIMEFRAME_KEY, Result);
end;

procedure TProjectFrame.DoCreateRelationalStructure;
var
  Relations: TEpiRelationList;
  i: Integer;
  MR: TEpiMasterRelation;

  function AddRelation(ParentNode: TTreeNode; Relation: TEpiMasterRelation): TTreeNode;
  var
    NodeData: TNodeData;
    S: String;
  begin
    NodeData := TNodeData.Create;
    NodeData.Relation := Relation;
    NodeData.DataFile := Relation.Datafile;
    NodeData.Frame := TRuntimeDesignFrame(NodeData.DataFile.FindCustomData(PROJECT_RUNTIMEFRAME_KEY));
    NodeData.DataFile.AddCustomData(PROJECT_RELATION_KEY, Relation);
    NodeData.DataFile.Caption.RegisterOnChangeHook(@OnDataFileCaptionChange, true);

    if Relation.InheritsFrom(TEpiDetailRelation) then
      BindKeyFields(TEpiDetailRelation(Relation));

    S := NodeData.DataFile.Caption.Text;
    if Trim(S) = '' then
      S := '(Untitled Dataset)';

    Result :=
      DataFilesTreeView.Items.AddChildObject(
        ParentNode,
        S,
        NodeData
      );
    Relation.Datafile.AddCustomData(PROJECT_TREE_NODE_KEY, Result);
  end;

  procedure AddRelationRecursive(ParentNode: TTreeNode; MasterRelation: TEpiMasterRelation);
  var
    i: Integer;
    Df: TEpiDataFile;
  begin
    ParentNode := AddRelation(ParentNode, MasterRelation);

    for i := 0 to MasterRelation.DetailRelations.Count - 1 do
      AddRelationRecursive(ParentNode, MasterRelation.DetailRelation[i]);
  end;

begin
  Relations := EpiDocument.Relations;
  if (Relations.Count = 0) and
     (EpiDocument.DataFiles.Count = 1)
  then
    begin
      // This should only happen when loading .epx version 2 files!
      MR := Relations.NewMasterRelation;
      MR.Datafile := EpiDocument.DataFiles[0];

      AddRelation(FRootNode, Mr);
      Exit;
    end;

  for i := 0 to Relations.Count - 1 do
    AddRelationRecursive(FRootNode, Relations.MasterRelation[i]);
end;

procedure TProjectFrame.DoCreateNewProject;
begin
  MainForm.BeginUpdatingForm;
  DoCreateNewDocument;
  DoNewDataForm(FRootNode);
  CommonProjectInit;
  DataFilesTreeView.Selected := FRootNode;
  EpiDocument.Modified := false;
  MainForm.EndUpdatingForm;
end;

procedure TProjectFrame.DoCloseProject;
var
  TN: TTreeNode;
begin
  // Close ValueLabel Editor - else the reference to
  // ValueLabelSets is incomplete!
  CloseValueLabelEditor2;

  // Close Alignment Form.
  AlignForm.DesignFrame := nil;
  AlignForm.Hide;

  PropertiesForm.Free;

  EpiDocument.Study.Title.UnRegisterOnChangeHook(@OnTitleChange);
  FRootNode.Free;
  DataFilesTreeView.Items.Clear;

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

procedure TProjectFrame.UpdateRootNode;
begin
  FRootNode.Text := EpiDocument.Study.Title.Text;
end;

procedure TProjectFrame.KeyFieldEvent(const Sender: TEpiCustomBase;
  const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
var
  MasterField: TEpiField absolute Sender;
  DetailFields: TEpiFields;
  SelfInitiated: Boolean;
  i: Integer;
  DetailField: TEpiField;
  ParentField: TEpiField;

begin
  DetailFields := TEPiFields(MasterField.FindCustomData(PROJECT_RELATION_KEYFIELD_CHILD_KEY));
  SelfInitiated := (Sender = Initiator);

  case EventGroup of
    eegCustomBase:
      case TEpiCustomChangeEventType(EventType) of
        ecceDestroy,
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

  LoadSplitterPosition(Splitter1, 'ProjectSplitter');

  FRootNode := DataFilesTreeView.Items.AddObject(nil, 'Root', nil);
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
  TKeyFieldsForm.RestoreDefaultPos;
  TAlignmentForm.RestoreDefaultPos;

  // TODO:
  {
  if Assigned(F) then
    TRuntimeDesignFrame.RestoreDefaultPos(TRuntimeDesignFrame(F.ActiveFrame))
  else
    TRuntimeDesignFrame.RestoreDefaultPos(nil);        }
end;

procedure TProjectFrame.UpdateFrame;
var
  TN: TTreeNode;
begin
  UpdateShortCuts;

  TN := DataFilesTreeView.TopItem;
  while Assigned(TN) do
  begin
    TNodeData(TN.Data).Frame.UpdateFrame;
    TN := TN.GetNext;
  end;
end;

function TProjectFrame.OpenProject(const AFileName: string): boolean;
var
  T1: TDateTime;
  T2: TDateTime;
begin
  T1 := Now;
  Result := DoOpenProject(AFileName);
  T2 := now;
//  ShowMessage('Open Time: ' + FormatDateTime('NN:SS:ZZZ', T2-T1));
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
  TN: TTreeNode;
begin
  TN := FRootNode.GetFirstChild;
  DataFilesTreeView.Selected := TN;

  Frame := TRuntimeDesignFrame(TNodeData(TN.Data).DataFile.FindCustomData(PROJECT_RUNTIMEFRAME_KEY));
  if FromCB then
    Frame.ImportCBAction.Execute
  else
    Frame.ImportAction.Execute;
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
    ProjectPasswordMenuItem.Action   := ProjectPasswordAction;
    KeyFieldsMenuItem.Action         := KeyFieldsAction;
    StudyInfoMenuItem.Action         := StudyInformationAction;

    // --project details popup-menu
    ProjectPropertiesPopupMenuItem.Action := ProjectSettingsAction;
    ValueLabelEditorPopupMenuItem.Action  := ValueLabelEditorAction;
    SetPasswordPopupMenuItem.Action       := ProjectPasswordAction;
    KeyFieldsPopupMenuItem.Action         := KeyFieldsAction;
    StudyInfoPopupMenuItem.Action         := StudyInformationAction;
  end;
  FActiveFrame.AssignActionLinks;
end;

end.

