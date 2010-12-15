unit project_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, ExtCtrls, ComCtrls, ActnList,
  Controls, Dialogs, epidocument, epidatafiles, epicustombase, epiadmin,
  epivaluelabels;

type

  { TProjectFrame }

  TProjectFrame = class(TFrame)
    ValueLabelEditorAction: TAction;
    ShowStructureAction: TAction;
    ExportStataAction: TAction;
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
    procedure ExportStataActionExecute(Sender: TObject);
    procedure NewDataFormActionExecute(Sender: TObject);
    procedure ProjectSettingsActionExecute(Sender: TObject);
    procedure SaveProjectActionExecute(Sender: TObject);
    procedure SaveProjectAsActionExecute(Sender: TObject);
    procedure ShowStructureActionExecute(Sender: TObject);
    procedure ValueLabelEditorActionExecute(Sender: TObject);
  private
    { private declarations }
    FFileName: string;
    FActiveFrame: TFrame;
    FModified: Boolean;
    FOnModified: TNotifyEvent;
    FrameCount: integer;
    FEpiDocument: TEpiDocument;
    procedure OnDataFileChange(Sender: TObject; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
    function  DoCreateNewDocument: TEpiDocument;
    function  NewDataFileItem(Sender: TEpiCustomList; DefaultItemClass: TEpiCustomItemClass): TEpiCustomItemClass;
    procedure DoSaveProject(AFileName: string);
    procedure DoOpenProject(Const AFileName: string);
    procedure DoNewDataForm(Df: TEpiDataFile);
    procedure DoCloseProject;
    {$IFDEF EPI_DEBUG}
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
  design_frame, Clipbrd, project_settings, epimiscutils,
  epiexport, main, settings2, settings2_var, epistringutils,
  structure_form, valuelabelseditor_form, epidatafilestypes,
  strutils;

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
  Frame: TDesignFrame;
begin
  inc(FrameCount);

  Df := EpiDocument.DataFiles.NewDataFile;
  Df.Name.Text := 'Dataform ' + IntToStr(FrameCount);
  EpiDocument.Modified := false;

  DoNewDataForm(Df);
  UpdateCaption;
end;

procedure TProjectFrame.ExportStataActionExecute(Sender: TObject);
var
  SaveDlg: TSaveDialog;
  Exporter: TEpiExport;
begin
  SaveDlg := TSaveDialog.Create(Self);
  try
    SaveDlg.Title := 'Export current form to Stata file...';
    SaveDlg.InitialDir := ManagerSettings.WorkingDirUTF8;
    SaveDlg.Filter := GetEpiDialogFilter(false, false, false, false, false, false, true, false, false, false, false);
    SaveDlg.Options := SaveDlg.Options + [ofOverwritePrompt, ofExtensionDifferent];
    SaveDlg.DefaultExt := 'dta';
    SaveDlg.FileName   := ChangeFileExt(FFileName, '.dta');
    if not SaveDlg.Execute then exit;

    // TODO : Support different datafiles export.
    Exporter := TEpiExport.Create;
    Exporter.ExportStata(SaveDlg.FileName, EpiDocument.DataFiles[0]);
  finally
    SaveDlg.Free;
    Exporter.Free;
  end;
end;

procedure TProjectFrame.ProjectSettingsActionExecute(Sender: TObject);
var
  ProjectSettings: TProjectSettingsForm;
begin
  ProjectSettings := TProjectSettingsForm.Create(self, EpiDocument);
  ProjectSettings.ShowModal;
  ProjectSettings.Free;

  TDesignFrame(ActiveFrame).UpdateFrame;
  UpdateTimer;
end;

procedure TProjectFrame.SaveProjectActionExecute(Sender: TObject);
begin
  if ProjectFileName = '' then
    SaveProjectAsAction.Execute
  else
    DoSaveProject(ProjectFileName);
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
     (TDesignFrame(ActiveFrame).ImportedFileName <> '') then
    Dlg.FileName := ChangeFileExt(TDesignFrame(ActiveFrame).ImportedFileName, Dlg.DefaultExt);
end;

procedure TProjectFrame.SaveProjectAsActionExecute(Sender: TObject);
var
  Dlg: TSaveDialog;
begin
  Dlg := TSaveDialog.Create(Self);
  Dlg.Filter := GetEpiDialogFilter(true, true, false, false, false, false,
    false, false, false, false, false);
  Dlg.InitialDir := ManagerSettings.WorkingDirUTF8;
  Dlg.FilterIndex := ManagerSettings.SaveType + 1;
  SaveDlgTypeChange(Dlg);
  Dlg.OnTypeChange := @SaveDlgTypeChange;
  Dlg.Options := Dlg.Options + [ofOverwritePrompt, ofExtensionDifferent];
  if not Dlg.Execute then exit;

  if Dlg.FileName <> ProjectFileName then
    FFileName := Dlg.FileName;

  DoSaveProject(ProjectFileName);
  Dlg.Free;

  EpiDocument.Modified := false;
  UpdateCaption;
end;

procedure TProjectFrame.ShowStructureActionExecute(Sender: TObject);
var
  StructureForm: TProject_Structure_Form;
begin
  StructureForm := TProject_Structure_Form.Create(Self, EpiDocument);
  StructureForm.ShowModal;
  StructureForm.Free;
end;

procedure TProjectFrame.ValueLabelEditorActionExecute(Sender: TObject);
begin
  GetValueLabelsEditor(EpiDocument).Show;
end;

procedure TProjectFrame.OnDataFileChange(Sender: TObject;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
var
  Df: TEpiDataFileEx;
begin
  if (EventGroup = eegCustomBase) and (EventType = Word(ecceText)) then
  begin
    Df := TEpiDataFileEx(TEpiCustomItem(Sender).Owner);
    Df.TreeNode.Text := Df.Name.Text;
    UpdateCaption;
  end;
end;

function TProjectFrame.DoCreateNewDocument: TEpiDocument;
begin
  Result := TEpiDocument.Create('en');
  Result.DataFiles.OnNewItemClass := @NewDataFileItem;
  Result.OnModified := @EpiDocumentModified;
end;

function TProjectFrame.NewDataFileItem(Sender: TEpiCustomList;
  DefaultItemClass: TEpiCustomItemClass): TEpiCustomItemClass;
begin
  result := TEpiDataFileEx;
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
  try
    Ms := TMemoryStream.Create;
    EpiDocument.SaveToStream(Ms);
    Ms.Position := 0;

    if ExtractFileExt(UTF8ToSys(AFileName)) = '.epz' then
      StreamToZipFile(Ms, AFileName)
    else begin
      Fs := TFileStream.Create(AFileName, fmCreate);
      Fs.CopyFrom(Ms, Ms.Size);
      Fs.Free;
    end;
  finally
    ActiveFrame.Cursor := crDefault;
    Application.ProcessMessages;
    Ms.Free;
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

  Cursor := crHourGlass;
  Application.ProcessMessages;

  St := TMemoryStream.Create;
  if ExtractFileExt(UTF8ToSys(Fn)) = '.epz' then
    ZipFileToStream(St, Fn)
  else
    St.LoadFromFile(Fn);

  St.Position := 0;
  FEpiDocument := DoCreateNewDocument;
  FEpiDocument.LoadFromStream(St);
  FFileName := AFileName;
  DoNewDataForm(FEpiDocument.DataFiles[0]);
  St.Free;

  // Create backup process.
  InitBackupTimer;

  Cursor := crDefault;
  Application.ProcessMessages;
  if Res = mrYes then
    EpiDocument.Modified := true;

  UpdateCaption;
end;

procedure TProjectFrame.DoNewDataForm(Df: TEpiDataFile);
var
  Frame: TDesignFrame;
begin
  Frame := TDesignFrame.Create(Self);
  Frame.Align := alClient;
  Frame.Parent := Self;
  Frame.DataFile := Df;
  FActiveFrame := Frame;

  {$IFDEF EPI_DEBUG}
  AddDebugingContent;
  {$ENDIF}

  DataFilesTreeView.Selected := DataFilesTreeView.Items.AddObject(nil, Df.Name.Text, Frame);
  TEpiDataFileEx(Df).TreeNode := DataFilesTreeView.Selected;
  Df.Name.RegisterOnChangeHook(@OnDataFileChange);
end;

procedure TProjectFrame.DoCloseProject;
begin
  if not Assigned(FEpiDocument) then exit;
  FreeAndNil(FEpiDocument);
  CloseValueLabelsEditor;

  // TODO : Delete ALL dataforms!
  FreeAndNil(FActiveFrame);
  FreeAndNil(FBackupTimer);
  if FileExistsUTF8(ProjectFileName + '.bak') then
    DeleteFileUTF8(ProjectFileName + '.bak');
  DataFilesTreeView.Items.Clear;
end;

{$IFDEF EPI_DEBUG}
procedure TProjectFrame.AddDebugingContent;
var
  i: Integer;
  LocalAdm: TEpiAdmin;
  Grp: TEpiGroup;
  LocalVLSets: TEpiValueLabelSets;
  VLSet: TEpiValueLabelSet;
begin
{  // DEBUGGING!!!!
  LocalAdm := FEpiDocument.Admin;
  Grp := LocalAdm.NewGroup;
  Grp.Name.Text := 'Group 1';
  Grp.Rights := [earCreate, earRead, earUpdate, earDelete, earVerify,
    earStructure, earTranslate, earUsers, earPassword];
  Grp := LocalAdm.NewGroup;
  Grp.Name.Text := 'Group 2';
  Grp.Rights := [earCreate, earRead, earUpdate, earDelete, earVerify,
    earStructure, earTranslate, earUsers, earPassword];
  Grp := LocalAdm.NewGroup;
  Grp.Name.Text := 'Group 3';
  Grp.Rights := [earCreate, earRead, earUpdate, earDelete, earVerify,
    earStructure, earTranslate, earUsers, earPassword];
  FEpiDocument.DataFiles[0].MainSection.Groups.AddItem(Grp);

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
  S := 'EpiData Manager (v' + GetManagerVersion + ') test version';

  if Assigned(EpiDocument) then
  begin
    if FFileName <> '' then
      S := S + ' - ' + ExtractFileName(FFileName);
    if EpiDocument.Modified then
      S := S + '*';

    if Assigned(ActiveFrame) then
    begin
      T := TDesignFrame(ActiveFrame).DataFile.Name.Text;
      if (T <> '') then
        S := S + ' [' + EpiCutString(T, 20) + ']';
    end;
  end;
  MainForm.Caption := S;
end;

function TProjectFrame.InitBackupTimer: boolean;
begin
  if Assigned(FBackupTimer) then
    exit(true);

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
begin
  try
    FBackupTimer.Enabled := false;
    DoSaveProject(ProjectFileName + '.bak');
    UpdateTimer;
    FBackupTimer.Enabled := true;
  except
    //
  end;
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
  CanClose := true;
  {$IFDEF EPI_RELEASE}
  if Modified or EpiDocument.Modified then
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
  GetValueLabelsEditor(EpiDocument).RestoreDefaultPos;
  if Assigned(FActiveFrame) then
    TDesignFrame(FActiveFrame).RestoreDefaultPos;
  TProject_Structure_Form.RestoreDefaultPos;
end;

procedure TProjectFrame.UpdateFrame;
begin
  // TODO : Update all frames.
  if Assigned(FActiveFrame) then
    TDesignFrame(FActiveFrame).UpdateFrame;
end;

procedure TProjectFrame.OpenProject(const AFileName: string);
begin
  DoOpenProject(AFileName);
end;

end.

