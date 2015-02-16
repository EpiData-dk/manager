unit main;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Menus, ComCtrls, ActnList, StdActns, ExtCtrls, Buttons,
  project_frame, LMessages, manager_messages, epidocument, report_base,
  episervice_ipc, episervice_ipctypes, epiexportsettings, simpleipc, epiopenfile;

type

  { TMainForm }

  TMainForm = class(TForm)
    AdminMenuItem: TMenuItem;
    RecentFilesActionList: TActionList;
    AppendAction: TAction;
    DataFormBtn: TBitBtn;
    DocumentBtn: TBitBtn;
    EnterDataBtn: TBitBtn;
    ExportBtn: TBitBtn;
    MenuItem33: TMenuItem;
    DataformMenu: TMenuItem;
    DataformPropertiesMenuItem: TMenuItem;
    MenuItem1: TMenuItem;
    DataformPropertiesPopupMenuItem: TMenuItem;
    ClearAllDataFormsMenuItem: TMenuItem;
    DataformPopupMenu: TPopupMenu;
    ProjectDetailsBtn: TBitBtn;
    SelectProjectBtn: TBitBtn;
    VLSetFromDataAction: TAction;
    ImportCBInNewProjectAction: TAction;
    MenuItem13: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    AddStructFromBLMenuItem: TMenuItem;
    MenuItem31: TMenuItem;
    RenameControlsPopupMenuItem: TMenuItem;
    MenuItem32: TMenuItem;
    RenameControlsMenuItem: TMenuItem;
    MenuItem30: TMenuItem;
    ValidationReportAction: TAction;
    ImportInNewProjectAction: TAction;
    CodeBookReportAction: TAction;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    EpiDataTutorialsMenuItem: TMenuItem;
    WebTutorialsMenuItem: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    BrowseDatasetMenuItem: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    BrowseDataMenuItem: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    CodeBookReportMenuItem: TMenuItem;
    MenuItem27: TMenuItem;
    PasteAsDateMenuItem: TMenuItem;
    RecentFilesSubPopupMenu: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    CountsReportAction: TAction;
    AddStructureMenuItem: TMenuItem;
    KeyFieldsMenuItem: TMenuItem;
    EditMenuDivider0: TMenuItem;
    AlignMenu: TMenuItem;
    AlignBottomMenuItem: TMenuItem;
    AlignLeftMenuItem: TMenuItem;
    AlignRightMenuItem: TMenuItem;
    AlignTopMenuItem: TMenuItem;
    MenuItem6: TMenuItem;
    AlignMenuItem: TMenuItem;
    DefineProjectPopupMenu: TPopupMenu;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    KeyFieldsPopupMenuItem: TMenuItem;
    DocumentPopupMenu: TPopupMenu;
    MenuItem9: TMenuItem;
    ValueLabelEditorPopupMenuItem: TMenuItem;
    ProjectPropertiesPopupMenuItem: TMenuItem;
    SetPasswordPopupMenuItem: TMenuItem;
    StudyInfoPopupMenuItem: TMenuItem;
    ProjectPopupMenu: TPopupMenu;
    SelectAllBoolMenuItem: TMenuItem;
    SelectAllStringMenuItem: TMenuItem;
    SelectAllFloatMenuItem: TMenuItem;
    PrintDataFormMenuItem: TMenuItem;
    FileMenuDivider3: TMenuItem;
    SelectMenu: TMenuItem;
    SelectAllIntsMenuItem: TMenuItem;
    StudyInfoMenuItem: TMenuItem;
    PasteMenuItem: TMenuItem;
    CopyMenuItem: TMenuItem;
    CutMenuItem: TMenuItem;
    EditMenuDivider4: TMenuItem;
    RedoMenuItem: TMenuItem;
    UndoMenuItem: TMenuItem;
    VerifyDoubleEntryAction: TAction;
    PrepareDoubleEntryAction: TAction;
    ExportAction: TAction;
    ExportMenuItem: TMenuItem;
    ProjectPasswordMenuItem: TMenuItem;
    ProjectOverviewReportMenuItem: TMenuItem;
    ProjectReportAction: TAction;
    ExtendedListReportAction: TAction;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    ValueLabelListReportAction: TAction;
    ValueLabaleListReportMenuItem: TMenuItem;
    ExtendedReportMenuItem: TMenuItem;
    QuestionListReportMenuItem: TMenuItem;
    QuestionListReportAction: TAction;
    ReportGeneratorAction: TAction;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    PackAction: TAction;
    ToolMenuDivider1: TMenuItem;
    StartEntryClientAction: TAction;
    EntryClientMenuItem: TMenuItem;
    PackMenuItem: TMenuItem;
    RecentFilesSubMenu: TMenuItem;
    HelpMenuDivider1: TMenuItem;
    ProcessToolPanel: TPanel;
    TutorialSubMenu: TMenuItem;
    OpenProjectAction: TAction;
    CloseProjectAction: TAction;
    DefaultWindowPosAction: TAction;
    CheckVersionAction: TAction;
    CopyProjectInfoAction: TAction;
    HelpMenuDivider2: TMenuItem;
    CopyVersionInfoMenuItem: TMenuItem;
    HelpMenuDivider3: TMenuItem;
    AboutMenuItem: TMenuItem;
    CheckVersionMenuItem: TMenuItem;
    FileMenuDivider1: TMenuItem;
    CloseProjectMenuItem: TMenuItem;
    NewProjectMenuItem: TMenuItem;
    ResetWindowPosMenuItem: TMenuItem;
    ValueLabelsMenuItem: TMenuItem;
    ToolsMenu: TMenuItem;
    ShowAboutAction: TAction;
    FileExitAction: TFileExit;
    FileExitMenuItem: TMenuItem;
    HelpMenu: TMenuItem;
    FileMenuDivider2: TMenuItem;
    FileMenuDivider4: TMenuItem;
    EditMenuDivider7: TMenuItem;
    ProjectPropertiesMenuItem: TMenuItem;
    ProjectMenu: TMenuItem;
    PasteAsFloatMenuItem: TMenuItem;
    PasteAsStringMenuItem: TMenuItem;
    PasteAsHeadingMenuItem: TMenuItem;
    PasteAsIntMenuItem: TMenuItem;
    OpenProjectMenuItem: TMenuItem;
    SaveProjectAsMenuItem: TMenuItem;
    SaveProjectMenuItem: TMenuItem;
    ShortCutKeysMenuItem: TMenuItem;
    ShortIntroMenuItem: TMenuItem;
    SettingsAction: TAction;
    EditMenuItem: TMenuItem;
    SettingsMenuItem: TMenuItem;
    NewProjectAction: TAction;
    ActionList1: TActionList;
    MainMenu1: TMainMenu;
    FileMenuItem: TMenuItem;
    PageControl1: TPageControl;
    procedure ActionList1Update(AAction: TBasicAction; var Handled: Boolean);
    procedure AppendActionExecute(Sender: TObject);
    procedure CheckVersionActionExecute(Sender: TObject);
    procedure ClearAllDataFormsMenuItemClick(Sender: TObject);
    procedure CloseProjectActionExecute(Sender: TObject);
    procedure CloseProjectActionUpdate(Sender: TObject);
    procedure CodeBookReportActionExecute(Sender: TObject);
    procedure CopyProjectInfoActionExecute(Sender: TObject);
    procedure CountsReportActionExecute(Sender: TObject);
    procedure DataFormBtnClick(Sender: TObject);
    procedure DefaultWindowPosActionExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormChanged(Sender: TObject; Form: TCustomForm);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure RecentFilesActionListUpdate(AAction: TBasicAction;
      var Handled: Boolean);
    procedure SelectProjectBtnClick(Sender: TObject);
    procedure DocumentBtnClick(Sender: TObject);
    procedure EpiDataTutorialsMenuItemClick(Sender: TObject);
    procedure ExportActionExecute(Sender: TObject);
    procedure ExtendedListReportActionExecute(Sender: TObject);
    procedure FileMenuItemClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ImportCBInNewProjectActionExecute(Sender: TObject);
    procedure ImportInNewProjectActionExecute(Sender: TObject);
    procedure NewProjectActionExecute(Sender: TObject);
    procedure OpenProjectActionExecute(Sender: TObject);
    procedure PackActionExecute(Sender: TObject);
    procedure PrepareDoubleEntryActionExecute(Sender: TObject);
    procedure ProjectDetailsBtnClick(Sender: TObject);
    procedure ProjectReportActionExecute(Sender: TObject);
    procedure QuestionListReportActionExecute(Sender: TObject);
    procedure ReportGeneratorActionExecute(Sender: TObject);
    procedure SettingsActionExecute(Sender: TObject);
    procedure ShortCutKeysMenuItemClick(Sender: TObject);
    procedure ShortIntroMenuItemClick(Sender: TObject);
    procedure ShowAboutActionExecute(Sender: TObject);
    procedure StartEntryClientActionExecute(Sender: TObject);
    procedure StartEntryClientActionUpdate(Sender: TObject);
    procedure ValidationReportActionExecute(Sender: TObject);
    procedure ValueLabelListReportActionExecute(Sender: TObject);
    procedure VerifyDoubleEntryActionExecute(Sender: TObject);
    procedure VLSetFromDataActionExecute(Sender: TObject);
    procedure WebTutorialsMenuItem12Click(Sender: TObject);
  private
    { private declarations }
    FModified: boolean;
    FActiveFrame: TProjectFrame;
    TabNameCount: integer;
    procedure SetCaption;
    procedure SetModified(const AValue: boolean);
    procedure ProjectModified(Sender: TObject);
    procedure OpenTutorialMenuItemClick(Sender: TObject);
    procedure LoadTutorials;
    function  DoCloseProject: boolean;
    procedure NewProjectFrame;
    procedure DoNewProject;
    procedure DoOpenProject(Const AFileName: string);
    procedure UpdateMainMenu;
    procedure UpdateProcessToolbar;
    procedure UpdateShortCuts;
    procedure UpdateSettings;
    procedure LoadGlyphs;
    procedure CheckForUpdates(Data: PtrInt);
    procedure OpenRecentMenuItemClick(Sender: TObject);
    function  ToolsCheckOpenFile(Const ReadOnly: boolean;
      out LocalDoc: boolean): TEpiDocumentFile;
    function  RunReport(ReportClass: TReportBaseClass; const FreeAfterRun: boolean = true): TReportBase;
    function  RunReportEx(ReportClass: TReportBaseClass; const FreeAfterRun: boolean = true): TReportBase;
  private
    { Messages }
    procedure LMOpenProject(var Msg: TLMessage);  message LM_MAIN_OPENPROJECT;
    procedure LMOpenRecent(var Msg: TLMessage);   message LM_MAIN_OPENRECENT;
    procedure LMNewProject(var Msg: TLMessage);   message LM_MAIN_NEWPROJECT;
    procedure LMCloseProject(var Msg: TLMessage); message LM_MAIN_CLOSEPROJECT;
    procedure LMImportToProject(var Msg: TLMessage); message LM_MAIN_IMPORTTONEW;
    // Message relaying...
    procedure LMDesignerAdd(var Msg: TLMessage); message LM_DESIGNER_ADD;
  private
    { Process communication }
    {$IFDEF EPI_IPC_TEST}
    FEpiIPC:  TEpiIPC;
    procedure  SetupIPC;
    function   CheckEntryClientOpenFile(Const FileName: string): boolean;
    procedure  CheckHasOpenFile(Const MsgType: TMessageType; Const Msg: string; out Ack: TMessageType);
    {$ENDIF}
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    property  Modified: boolean read FModified write SetModified;
    procedure RestoreDefaultPos;
    procedure UpdateRecentFiles;
    procedure AssignActionLinks;
    procedure BeginUpdatingForm;
    procedure EndUpdatingForm;
  end;

var
  MainForm: TMainForm; 

implementation

{$R *.lfm}

uses
  epiv_datamodule,
  LCLProc, LCLIntf, LazUTF8Classes,
  settings2, settings2_var, about, Clipbrd, epiversionutils,
  epimiscutils,
  epicustombase, LCLType, UTF8Process,
  toolsform, epidatafiles, epistringutils, epiexport, reportgenerator,
  report_fieldlist, report_valuelabellist,
  viewer_form, staticreports_form,
  report_fieldlist_extended, report_project_overview,
  report_counts, report_double_entry_validation,
  report_codebook, report_project_validation,
  shortcuts, export_form, prepare_double_entry_form,
  managerprocs, process, epiv_documentfile,
  report_export, epireport_generator_txt,
  valuelabel_import_data,
  append_form, epitools_append,
  manager_globals,
  report_project_validation_frame2, reports_form,
  epiv_checkversionform;

type
  TAccessActionList = class(TActionList);

{ TMainForm }

procedure TMainForm.FormShow(Sender: TObject);
begin
  SetCaption;

  if ManagerSettings.SaveWindowPositions then
    LoadFormPosition(Self, 'MainForm');

  UpdateSettings;
  UpdateRecentFiles;

  Application.QueueAsyncCall(@CheckForUpdates, 0);
//  CheckForUpdates;
end;

procedure TMainForm.ImportCBInNewProjectActionExecute(Sender: TObject);
begin
  PostMessage(Self.Handle, LM_MAIN_IMPORTTONEW, 1, 0);
end;

procedure TMainForm.ImportInNewProjectActionExecute(Sender: TObject);
begin
  PostMessage(Self.Handle, LM_MAIN_IMPORTTONEW, 0, 0);
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := true;

  {$IFNDEF EPI_DEBUG}
  if Assigned(FActiveFrame) then
    FActiveFrame.CloseQuery(CanClose);
  {$ENDIF}

  if CanClose and ManagerSettings.SaveWindowPositions then
    SaveFormPosition(Self, 'MainForm');
  SaveSettingToIni(GetIniFileName);
end;

procedure TMainForm.CopyProjectInfoActionExecute(Sender: TObject);
var
  S: String;
begin
  S := GetProgramInfo;
  if Assigned(FActiveFrame) then
  with TProjectFrame(FActiveFrame).EpiDocument do
  begin
    S := S + LineEnding +
      'Filename: ' + TProjectFrame(FActiveFrame).DocumentFile.FileName + LineEnding +
      'XML Version: ' + IntToStr(Version) + LineEnding +
      'Field count: ' + IntToStr(DataFiles[0].Fields.Count) + LineEnding +
      'Record count: ' + IntToStr(DataFiles[0].Size);
  end;
  Clipboard.AsText := S;
  ShowMessage('Version info copied to clipboard!');
end;

procedure TMainForm.CountsReportActionExecute(Sender: TObject);
begin
  RunReportEx(TReportCounts);
end;

procedure TMainForm.DataFormBtnClick(Sender: TObject);
begin
  if not FActiveFrame.SelectDataformIfNotSelected then
    Exit;

  DataformPopupMenu.PopUp;
end;

procedure TMainForm.DefaultWindowPosActionExecute(Sender: TObject);
begin
  RestoreDefaultPos;
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  if Assigned(FActiveFrame) then
    FActiveFrame.UpdateStatusBar;
end;

procedure TMainForm.FormChanged(Sender: TObject; Form: TCustomForm);
begin
  if (Form <> Self) then
  begin
    ActionList1.State := asSuspended;
    RecentFilesActionList.State := asSuspended
  end
  else
  begin
    ActionList1.State := asNormal;
    RecentFilesActionList.State := asNormal;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Screen.RemoveAllHandlersOfObject(Self);
end;

procedure TMainForm.FormDropFiles(Sender: TObject;
  const FileNames: array of String);
var
  S: TString;
begin
  if Length(FileNames) = 0 then Exit;

  S := TString.Create(FileNames[0]);
  PostMessage(Self.Handle, LM_MAIN_OPENPROJECT, WPARAM(S), 0);
end;

procedure TMainForm.RecentFilesActionListUpdate(AAction: TBasicAction;
  var Handled: Boolean);
begin
{  if Screen.ActiveCustomForm <> MainForm then
    RecentFilesActionList.State := asSuspended
  else
    RecentFilesActionList.State := asNormal;      }
end;

procedure TMainForm.SelectProjectBtnClick(Sender: TObject);
begin
  UpdateRecentFiles;
  DefineProjectPopupMenu.PopUp;
end;

procedure TMainForm.DocumentBtnClick(Sender: TObject);
begin
  DocumentPopupMenu.PopUp;
end;

procedure TMainForm.EpiDataTutorialsMenuItemClick(Sender: TObject);
begin
  OpenURL('http://epidata.info/dokuwiki/doku.php?id=training:start');
end;

procedure TMainForm.ExportActionExecute(Sender: TObject);
var
  IsLocalDoc: boolean;
  Doc: TEpiDocumentFile;
  ExportForm: TExportForm;
  Settings: TEpiExportSetting;
  Exporter: TEpiExport;
  S: String;
  ASettings: TEpiExportSetting;
  FileList: TEpiDocumentFileList;
  R: TReportExport;
  FS: TFileStreamUTF8;
  ReportText: String;
  ReportTitle: String;
begin
  Settings := nil;
  Exporter := nil;
  ExportForm := nil;

  Doc := ToolsCheckOpenFile(True, IsLocalDoc);
  if not Assigned(Doc) then exit;

  try
    ExportForm := TExportForm.Create(Self, Doc.Document, Doc.FileName);
    if ExportForm.ShowModal <> mrOK then exit;

    Settings := ExportForm.ExportSetting;

    Exporter := TEpiExport.Create;
    if not Exporter.Export(Settings) then
      ShowMessage('Export Failed.')
    else
    with ExportForm do
    begin
      FS := nil;
      if ExportReportChkBox.Checked then
      begin
        FileList := TEpiDocumentFileList.Create;
        FileList.Add(Doc);

        R := TReportExport.Create(TEpiReportTXTGenerator);
        R.DocumentFiles := FileList;
        R.ExportSettings := ExportSetting;

        ReportTitle := R.ReportTitle;
        ReportText := R.RunReport;

        FS := TFileStreamUTF8.Create(ChangeFileExt(ExportSetting.ExportFileName, '.log'), fmCreate);
        FS.Write(ReportText[1], Length(ReportText));

        R.Free;
        FileList.Free;
      end;

      S := 'Export Succeeded' + LineEnding + LineEnding;
      S += 'Project: ' + Doc.FileName + LineEnding;

      ASettings := ExportSetting;
      while Assigned(ASettings) do
      begin
        S += 'Export: ' + ASettings.ExportFileName + LineEnding;
        ASettings := ASettings.AdditionalExportSettings;
      end;

      if Assigned(FS) then
        S += 'Report: ' + FS.FileName;

      ShowMessage(TrimRight(S));

      if Assigned(FS) then
        ShowReportForm(Self, ReportTitle, ReportText);

      if (ExportForm.ExportSetting is TEpiEPXExportSetting) then
        AddToRecent(ExportForm.ExportSetting.ExportFileName);

      FS.Free;
    end;
  finally
    ExportForm.Free;
    Exporter.Free;
    Settings.Free;
  end;
  if IsLocalDoc then
    Doc.Free;
end;

procedure TMainForm.ExtendedListReportActionExecute(Sender: TObject);
begin
  RunReport(TReportFieldListExtended).Free;
end;

procedure TMainForm.FileMenuItemClick(Sender: TObject);
begin
  //UpdateRecentFiles;
end;

procedure TMainForm.CheckVersionActionExecute(Sender: TObject);
var
  F: TCheckVersionForm;
begin
  F := TCheckVersionForm.Create(Self);
  F.Caption := 'EpiData Manager';
  F.CheckBoxValue := ManagerSettings.CheckForUpdates;
  F.ShowModal;
  ManagerSettings.CheckForUpdates := F.CheckBoxValue;
  F.Free;
end;

procedure TMainForm.ClearAllDataFormsMenuItemClick(Sender: TObject);
var
  Doc: TEpiDocument;
  DF: TEpiDataFile;
begin
  if not Assigned(FActiveFrame) then exit;
  if not Assigned(FActiveFrame.DocumentFile) then exit;
  Doc := FActiveFrame.DocumentFile.Document;

  for DF in Doc.DataFiles do
    DF.Size := 0;
end;

procedure TMainForm.ActionList1Update(AAction: TBasicAction;
  var Handled: Boolean);
begin
  // Hack to force short-cut handling of menuitems not be catched when form is not selected.
  MainMenu1.ShortcutHandled :=
    (ActionList1.State = asNormal);
end;

procedure TMainForm.AppendActionExecute(Sender: TObject);
var
  LocalDoc: boolean;
  DocFile: TEpiDocumentFile;
  AppendTool: TEpiToolAppend;
  ResultList: TStrings;
  Handler: TAppendHandler;
  S: String;

begin
  DocFile := ToolsCheckOpenFile(false, LocalDoc);
  if not Assigned(DocFile) then exit;

  AppendForm := nil;
  AppendTool := nil;
  ResultList := nil;

  try
    AppendForm := TAppendForm.Create(self);
    AppendForm.MainProject := DocFile;
    if AppendForm.ShowModal <> mrOK then exit;

    Handler := TAppendHandler.Create(Self);

    AppendForm.CreateSelectedList(ResultList);
    AppendTool := TEpiToolAppend.Create;
    AppendTool.FieldNames.Assign(ResultList);
    AppendTool.OnError := @Handler.AppendError;
    AppendTool.OnWarning := @Handler.AppendWarning;

    case AppendTool.Append(AppendForm.MainProject.Document, AppendForm.AppendProject.Document) of
      eapFailed:
        S := 'Append failed!';
      eapPartialSuccess:
        S := 'Append skipped'; // TODO: multiple dataforms
      eapSuccess:
        S := 'Sucessfully appended ' + IntToStr(AppendForm.AppendProject.Document.DataFiles[0].Size) +
             ' records!';
    end;

    ShowMessage(S);
    if Assigned(FActiveFrame) then
      FActiveFrame.UpdateFrame;

    AddToRecent(AppendForm.AppendProject.FileName);
    AddToRecent(AppendForm.MainProject.FileName);

    if LocalDoc then
      DocFile.SaveFile(DocFile.FileName);
  finally
    AppendForm.Free;
    AppendTool.Free;
    ResultList.Free;
    Handler.Free;
    if LocalDoc then
      DocFile.Free;
  end;
end;

procedure TMainForm.CloseProjectActionExecute(Sender: TObject);
begin
  PostMessage(Self.Handle, LM_MAIN_CLOSEPROJECT, 0, 0);
end;

procedure TMainForm.CloseProjectActionUpdate(Sender: TObject);
begin
  CloseProjectAction.Enabled := Assigned(FActiveFrame);
end;

procedure TMainForm.CodeBookReportActionExecute(Sender: TObject);
begin
  RunReport(TReportCodeBook).Free;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  Fn: String;
  i: Integer;
begin
  Modified := false;
  Screen.AddHandlerActiveFormChanged(@FormChanged);

  if Assigned(StartupFiles) then
  begin
    for i := 0 to StartupFiles.Count - 1 do
    begin
      Fn := StartupFiles[i];
      if FileExistsUTF8(Fn) then
        PostMessage(Self.Handle, LM_MAIN_OPENPROJECT, WPARAM(TString.Create(Fn)), 0);
    end;
  end;

  DM.MainOpenDialog.InitialDir := ManagerSettings.WorkingDirUTF8;
end;

procedure TMainForm.NewProjectActionExecute(Sender: TObject);
begin
  PostMessage(Self.Handle, LM_MAIN_NEWPROJECT, 0, 0);
end;

procedure TMainForm.OpenProjectActionExecute(Sender: TObject);
begin
  PostMessage(Self.Handle, LM_MAIN_OPENPROJECT, 0, 0);
end;

procedure TMainForm.PackActionExecute(Sender: TObject);
var
  F: TToolsForm;
  Doc: TEpiDocumentFile;
  EpiDoc: TEpiDocument;
  LocalDoc: Boolean;
  S: LongInt;
  T: Integer;
  Str: String;
  i: Integer;
begin
  try
    F := nil;
    Doc := ToolsCheckOpenFile(False, LocalDoc);
    if not Assigned(Doc) then exit;

    EpiDoc := Doc.Document;

    F := TToolsForm.Create(Self);
    F.Caption := 'Pack: ' + EpiDoc.Study.Title.Text;
    F.EpiDocument := EpiDoc;
    if F.ShowModal = mrCancel then exit;

    if F.SelectedDatafiles.Count = 0 then
    begin
      ShowMessage('No datasets selected.');
      Exit;
    end;

    if MessageDlg('Warning!',
      'Packing the dataset will permanently remove ALL records marked for deletion!' + LineEnding +
      'Do you wish to continue?',
      mtWarning,
      mbYesNo,
      0,
      mbNo
    ) = mrNo then
      Exit;

    T := 0;
    Str := '';
    for i := 0 to F.SelectedDatafiles.Count - 1 do
    with TEpiDataFile(F.SelectedDatafiles[i]) do
    begin
      S := Size;
      Pack;
      S := S - Size;

      Str := Str + LineEnding +
        Caption.Text + ': ' + IntToStr(S);
      T := T + S;
    end;

    ShowMessage(
      'Removed records:' +
      Str + LineEnding + LineEnding +
      'Total: ' + IntToStr(T));

    if LocalDoc then
      Doc.SaveFile(Doc.FileName);
  finally
    if LocalDoc and Assigned(Doc) then
      Doc.Free;
    if Assigned(F) then F.free;
  end;
end;

procedure TMainForm.PrepareDoubleEntryActionExecute(Sender: TObject);
var
  Local: boolean;
  Doc: TEpiDocumentFile;
begin
  Doc := ToolsCheckOpenFile(True, Local);
  if Assigned(Doc) then
  begin
    PrepareDoubleEntry(Doc);
    if Local then
      Doc.Free;
  end;
end;

procedure TMainForm.ProjectDetailsBtnClick(Sender: TObject);
begin
  ProjectPopupMenu.PopUp;
end;

procedure TMainForm.ProjectReportActionExecute(Sender: TObject);
begin
  RunReport(TReportProjectOverview).Free;
end;

procedure TMainForm.QuestionListReportActionExecute(Sender: TObject);
begin
  RunReport(TReportFieldLists).Free;
end;

procedure TMainForm.ReportGeneratorActionExecute(Sender: TObject);
var
  RGF: TReportGeneratorForm;
begin
  RGF := TReportGeneratorForm.Create(Self);
  RGF.ShowModal;
  RGF.Free;
end;

procedure TMainForm.SettingsActionExecute(Sender: TObject);
var
  SettingForm: TSettingsForm;
begin
  SettingForm := TSettingsForm.Create(Self);
  if SettingForm.ShowModal = mrCancel then exit;
  SettingForm.Free;

  UpdateSettings;
end;

procedure TMainForm.ShortCutKeysMenuItemClick(Sender: TObject);
var
  Fn: String;
begin
  Fn := ManagerSettings.TutorialDirUTF8 + '/epidatamanagershortcuts.pdf';
  if FileExistsUTF8(Fn) then
    OpenURL(Fn)
  else
  begin
    ShowMessage(
      'Introduction document was not found in tutorial folder:' + LineEnding +
      ManagerSettings.TutorialDirUTF8
    );
    OpenURL('http://epidata.info/dokuwiki/doku.php?id=documentation:keyboard_shortcuts');
  end;
end;

procedure TMainForm.ShortIntroMenuItemClick(Sender: TObject);
var
  Fn: String;
begin
  Fn := ManagerSettings.TutorialDirUTF8 + '/epidatamanagerintro.pdf';
  if FileExistsUTF8(Fn) then
    OpenURL(Fn)
  else
  begin
    ShowMessage(
      'Introduction document was not found in tutorial folder:' + LineEnding +
      ManagerSettings.TutorialDirUTF8
    );
    OpenURL('http://epidata.dk/php/downloadc.php?file=epidatamanagerintro.pdf');
  end;
end;

procedure TMainForm.ShowAboutActionExecute(Sender: TObject);
var
  Frm: TAboutForm;
begin
  Frm := TAboutForm.Create(Self);
  Frm.ShowModal;
  Frm.Free;
end;

procedure TMainForm.StartEntryClientActionExecute(Sender: TObject);
var
  Path: String;
  Ext: String;
  Entry: TProcessUTF8;
  Param: String;
  Res: TModalResult;
  CanClose: boolean;
begin
  if Assigned(FActiveFrame) and
     Assigned(FActiveFrame.EpiDocument) {and
     (FActiveFrame.ProjectFileName <> '')}
  then
  begin
    Res := MessageDlg(
      'Warning',
      'You are about to open EntryClient. Do you wish to close current project and open it in EntryClient?',
      mtWarning,
      mbYesNoCancel,
      0,
      mbCancel
    );

    if Res = mrCancel then exit;
    if Res = mrYes then
    begin
      CanClose := true;
      FActiveFrame.CloseQuery(CanClose);
      Param := FActiveFrame.DocumentFile.FileName;
      if not DoCloseProject then exit;
    end
    else
      Param := '';
  end;

  Path := ManagerSettings.EntryClientDirUTF8 + PathDelim;
  {$IFDEF DARWIN}
  // debugging path
  if DirectoryExistsUTF8(Path + 'epidataentryclient.app') then
    Path += 'epidataentryclient.app'
  // Installation path (hopefully)
  else if DirectoryExistsUTF8(Path + 'EpiData EntryClient.app') then
    Path += 'EpiData EntryClient.app';
  Path += '/Contents/MacOS/';
  {$ENDIF}

  Ext := ExtractFileExt(Application.ExeName);
  Entry := TProcessUTF8.Create(nil);
  Entry.Executable := Path + 'epidataentryclient' + ext;
  Entry.Parameters.Add(Param);
  Entry.Execute;
  Entry.Free;
end;

procedure TMainForm.StartEntryClientActionUpdate(Sender: TObject);
var
  Path: String;
  Ext: String;
begin
  Path := ManagerSettings.EntryClientDirUTF8 + PathDelim;
  {$IFDEF DARWIN}
  // debugging path
  if DirectoryExistsUTF8(Path + 'epidataentryclient.app') then
    Path += 'epidataentryclient.app'
  // Installation path (hopefully)
  else if DirectoryExistsUTF8(Path + 'EpiData EntryClient.app') then
    Path += 'EpiData EntryClient.app';
  Path += '/Contents/MacOS/';
  {$ENDIF}
  Ext := ExtractFileExt(Application.ExeName);
  TAction(Sender).Enabled := FileExistsUTF8(Path + 'epidataentryclient' + Ext);
end;

procedure TMainForm.ValidationReportActionExecute(Sender: TObject);
var
  R: TReportBase;
begin
  R := RunReportEx(TReportProjectValidation, false);

  if Assigned(R) and
     (R.DocumentFiles[0].IsSaved)
  then
    AddToRecent(R.DocumentFiles[0].FileName);

  R.Free;
end;

procedure TMainForm.ValueLabelListReportActionExecute(Sender: TObject);
begin
  RunReport(TReportValueLabelList).Free;
end;

procedure TMainForm.VerifyDoubleEntryActionExecute(Sender: TObject);
var
  R: TReportBase;
  Fn: String;
  i: Integer;
begin
  R := RunReportEx(TReportDoubleEntryValidation, false);
  if Assigned(R) and
     (
      (not Assigned(FActiveFrame)) or
      (not Assigned(FActiveFrame.EpiDocument))
     ) and
     R.DocumentFiles[0].Document.Modified
  then
    begin
      Fn := R.DocumentFiles[0].FileName + '.doubleentry-verification.epx';
      i := 0;
      while FileExistsUTF8(Fn) do
      begin
        Inc(i);
        Fn := R.DocumentFiles[0].FileName + '.doubleentry-verification.' + IntToStr(i) + '.epx';
      end;

      R.DocumentFiles[0].Document.SaveToFile(fn);
      ShowMessage('Validation saved to file:' + LineEnding +
                  Fn);
    end;

  if Assigned(R) then
  begin
    if R.DocumentFiles[1].IsSaved then
      AddToRecent(R.DocumentFiles[1].FileName);
    if R.DocumentFiles[0].IsSaved then
      AddToRecent(R.DocumentFiles[0].FileName);
    UpdateRecentFiles;
  end;

  R.Free;
end;

procedure TMainForm.VLSetFromDataActionExecute(Sender: TObject);
var
  Docfile: TEpiDocumentFile;
  LocalDoc: boolean;
  F: TValueLabelDataImport;
  Dlg: TSaveDialog;
  Fn: String;
begin
  Docfile := ToolsCheckOpenFile(false, LocalDoc);
  if not Assigned(DocFile) then exit;

  try
    F := TValueLabelDataImport.Create(Self);
    F.DocFile := DocFile;
    F.ShowModal;
  finally
    if (Docfile.Document.Modified) and
       (LocalDoc)
    then
    begin
      if (not Docfile.IsSaved)  then
      begin
        Dlg := TSaveDialog.Create(nil);
        Dlg.Filter := GetEpiDialogFilter([dfEPX, dfEPZ]);
        Dlg.InitialDir := ManagerSettings.WorkingDirUTF8;
        if Dlg.Execute then
          Fn := Dlg.FileName;
      end else
        Fn := Docfile.FileName;

      Docfile.SaveFile(Fn);
    end;

    F.Free;
    if LocalDoc and Assigned(Docfile) then
      Docfile.Free;
  end;
end;

procedure TMainForm.WebTutorialsMenuItem12Click(Sender: TObject);
begin
  OpenURL(ManagerSettings.TutorialURLUTF8);
end;

procedure TMainForm.SetCaption;
begin
  Caption := 'EpiData Manager (v' + GetManagerVersion + ')';
end;

procedure TMainForm.SetModified(const AValue: boolean);
begin
  if FModified = AValue then exit;
  FModified := AValue;
end;

procedure TMainForm.ProjectModified(Sender: TObject);
begin
  Modified := TProjectFrame(Sender).Modified;
end;

procedure TMainForm.OpenTutorialMenuItemClick(Sender: TObject);
begin
  OpenDocument(ManagerSettings.TutorialDirUTF8 + DirectorySeparator + TMenuItem(Sender).Caption + '.pdf');
end;

procedure TMainForm.LoadTutorials;
var
  FileList: TStringList;
  MenuItem: TMenuItem;
  i: Integer;
begin
  // First delete all previous tutorials.. (could be a change in tutorial dir).
  for i := TutorialSubMenu.Count - 1 downto 0 do
  begin
    MenuItem := TutorialSubMenu[i];
    TutorialSubMenu.Delete(i);
    MenuItem.Free;
  end;

  // Find all .pdf files in the directory set by TutorialsDirUTF8
  FileList := FindAllFiles(ManagerSettings.TutorialDirUTF8, '*.pdf', false);
  FileList.CustomSort(@EpiStringListSortStr);

  if FileList.Count = 0 then
  begin
    TutorialSubMenu.Enabled := false;
    FileList.Free;
    Exit;
  end;

  for i := 0 to FileList.Count - 1 do
  begin
    MenuItem := TMenuItem.Create(TutorialSubMenu);
    MenuItem.Name := 'TutorialMenuItem' + IntToStr(i);
    MenuItem.Caption := ExtractFileNameOnly(FileList[i]);
    MenuItem.OnClick := @OpenTutorialMenuItemClick;

    TutorialSubMenu.Add(MenuItem);
  end;
  FileList.Free;
end;

function TMainForm.DoCloseProject: boolean;
begin
  result := true;
  if Assigned(FActiveFrame) then
  begin
    FActiveFrame.CloseQuery(result);
    if not Result then exit;

    PageControl1.ActivePage.Free;
    FActiveFrame := nil;
  end;
  UpdateMainMenu;
  UpdateProcessToolbar;
  AssignActionLinks;
  SetCaption;
end;

procedure TMainForm.NewProjectFrame;
var
  TabSheet: TTabSheet;
begin
  TabSheet := TTabSheet.Create(PageControl1);
  TabSheet.PageControl := PageControl1;
  TabSheet.Name := 'TabSheet' + IntToStr(TabNameCount);
  TabSheet.Caption := 'Untitled';

//  if PageControl1.PageCount >= 1 then
//    PageControl1.ShowTabs := true;

  FActiveFrame := TProjectFrame.Create(TabSheet);
  FActiveFrame.Name := 'ProjectFrame' + IntToStr(TabNameCount);
  FActiveFrame.Align := alClient;
  FActiveFrame.Parent := TabSheet;
  FActiveFrame.OnModified := @ProjectModified;
  PageControl1.ActivePage := TabSheet;
end;

procedure TMainForm.DoNewProject;
begin
  // Close Old project
  if not DoCloseProject then exit;

  NewProjectFrame;
  FActiveFrame.CreateNewProject;
  AssignActionLinks;
  UpdateProcessToolbar;

  Inc(TabNameCount);
end;

procedure TMainForm.DoOpenProject(const AFileName: string);
begin
  if not DoCloseProject then exit;

  try
    Screen.Cursor := crHourGlass;
    Application.ProcessMessages;

    NewProjectFrame;
    if not FActiveFrame.OpenProject(AFileName) then
      DoCloseProject
    else
      AssignActionLinks;
  finally
    Screen.Cursor := crDefault;
    Application.ProcessMessages;
  end;

  UpdateProcessToolbar;
end;

procedure TMainForm.UpdateMainMenu;
begin
  // FILE:
  // New, -, Open, Open Recent,
  SaveProjectMenuItem.Visible := Assigned(FActiveFrame);
  SaveProjectAsMenuItem.Visible := Assigned(FActiveFrame);
//  CloseProjectAction.Enabled := Assigned(FActiveFrame);
  // -, AddStructur/Import, -
  PrintDataFormMenuItem.Visible := Assigned(FActiveFrame);
  // -
  FileMenuDivider4.Visible := PrintDataFormMenuItem.Visible;
  // -, Exit

  // EDIT:
  UndoMenuItem.Visible := Assigned(FActiveFrame);
  RedoMenuItem.Visible := Assigned(FActiveFrame);
  EditMenuDivider0.Visible := Assigned(FActiveFrame);
  // -
  CutMenuItem.Visible :=  Assigned(FActiveFrame);
  CopyMenuItem.Visible := Assigned(FActiveFrame);
  PasteMenuItem.Visible := Assigned(FActiveFrame);
  EditMenuDivider4.Visible := Assigned(FActiveFrame);
  // -
  PasteAsFloatMenuItem.Visible := Assigned(FActiveFrame);
  PasteAsHeadingMenuItem.Visible := Assigned(FActiveFrame);
  PasteAsIntMenuItem.Visible := Assigned(FActiveFrame);
  PasteAsStringMenuItem.Visible := Assigned(FActiveFrame);
  PasteAsDateMenuItem.Visible := Assigned(FActiveFrame);
  EditMenuDivider7.Visible := Assigned(FActiveFrame);
  // -
  AlignMenu.Visible         := Assigned(FActiveFrame);
  // -
  SelectMenu.Visible        := Assigned(FActiveFrame);
  MenuItem25.Visible        := Assigned(FActiveFrame);

  // PROJECT:
  ProjectMenu.Visible       := Assigned(FActiveFrame);

  // Dataform:
  DataformMenu.Visible      := Assigned(FActiveFrame);


  // TOOLS:
  ClearAllDataFormsMenuItem.Visible := {$IFDEF EPI_DEVELOPMENT}true;{$ELSE}false;{$ENDIF}

  // Document:
  BrowseDataMenuItem.Visible := Assigned(FActiveFrame);
  MenuItem4.Visible          := Assigned(FActiveFrame);
  // document popupmenu
  BrowseDatasetMenuItem.Visible := Assigned(FActiveFrame);
  MenuItem16.Visible            := Assigned(FActiveFrame);
end;

procedure TMainForm.UpdateProcessToolbar;
begin
  ProcessToolPanel.Visible :=
    ManagerSettings.ShowWorkToolBar;

  ProjectDetailsBtn.Enabled := Assigned(FActiveFrame);
  DataFormBtn.Enabled := ProjectDetailsBtn.Enabled;
end;

procedure TMainForm.UpdateShortCuts;
begin
  UpdateRecentFiles;

  NewProjectAction.ShortCut           := M_NewProject;
  SettingsAction.ShortCut             := M_Settings;
  FileExitAction.ShortCut             := M_Exit;
  ShowAboutAction.ShortCut            := M_ShowAbout;
  CopyProjectInfoAction.ShortCut      := M_CopyProjectInfo;
  CheckVersionAction.ShortCut         := M_CheckVersion;
  DefaultWindowPosAction.ShortCut     := M_DefaultPos;
  CloseProjectAction.ShortCut         := M_CloseProject;
  OpenProjectAction.ShortCut          := M_OpenProject;
  StartEntryClientAction.ShortCut     := M_StartEntryClient;
  PackAction.ShortCut                 := M_Pack;
  ExportAction.ShortCut               := M_Export;
  QuestionListReportAction.ShortCut   := M_QuestionListReport;
  ValueLabelListReportAction.ShortCut := M_ValueLabelListReport;
  ExtendedListReportAction.ShortCut   := M_ExtendedListReport;
  ProjectReportAction.ShortCut        := M_ProjectOverviewReport;
  ImportInNewProjectAction.ShortCut   := D_ImportData;
  ImportCBInNewProjectAction.ShortCut := D_ImportDataCB;
end;

procedure TMainForm.UpdateSettings;
begin
  BeginUpdatingForm;

  LoadTutorials;
  UpdateProcessToolbar;
  UpdateShortCuts;

  if Assigned(FActiveFrame) then
    TProjectFrame(FActiveFrame).UpdateFrame;

  EndUpdatingForm;
end;

procedure TMainForm.LoadGlyphs;
begin
  DM.Icons16.GetBitmap(17, ExportBtn.Glyph);
  DM.Icons16.GetBitmap(39, EnterDataBtn.Glyph);
  DM.Icons16.GetBitmap(41, SelectProjectBtn.Glyph);
  DM.Icons16.GetBitmap(43, DocumentBtn.Glyph);
  DM.Icons16.GetBitmap(44, DataFormBtn.Glyph);
  DM.Icons16.GetBitmap(45, ProjectDetailsBtn.Glyph);
end;

procedure TMainForm.CheckForUpdates(Data: PtrInt);
var
  F: TCheckVersionForm;
  D: Extended;
  N: TDateTime;
begin
  // User does not want to show updates.
  if not ManagerSettings.CheckForUpdates then exit;

  // Check if it is time to search for updates.
  D := (ManagerSettings.LastUpdateCheck + ManagerSettings.DaysBetweenChecks);
  N := Now;

  if (ManagerSettings.LastUpdateCheck + ManagerSettings.DaysBetweenChecks) >= Now
  then
    Exit;

  CheckVersionAction.Execute;

  ManagerSettings.LastUpdateCheck := Now;
end;

procedure TMainForm.OpenRecentMenuItemClick(Sender: TObject);
begin
  PostMessage(Self.Handle, LM_MAIN_OPENRECENT, WParam(Sender), 0);
end;

function TMainForm.ToolsCheckOpenFile(const ReadOnly: boolean; out
  LocalDoc: boolean): TEpiDocumentFile;
var
  Dlg: TOpenDialog;
begin
  Result := nil;
  if Assigned(FActiveFrame) then
  begin
    LocalDoc := false;
    Result := FActiveFrame.DocumentFile;
  end else begin
    Dlg := TOpenDialog.Create(Self);
    Dlg.Filter := GetEpiDialogFilter([dfEPX, dfEPZ, dfCollection]);
    Dlg.InitialDir := ManagerSettings.WorkingDirUTF8;
    if not Dlg.Execute then exit;

    Result := TDocumentFile.Create;
    if not Result.OpenFile(Dlg.FileName, ReadOnly) then
    begin
      FreeAndNil(Result);
      Exit;
    end;

    AddToRecent(Result.FileName);
    LocalDoc := true;
    Dlg.Free;
  end;
end;

function TMainForm.RunReport(ReportClass: TReportBaseClass;
  const FreeAfterRun: boolean): TReportBase;
var
  F: TStaticReportsForm;
  S: String;
begin
  Result := nil;

  F := TStaticReportsForm.Create(Self, ReportClass);

  if Assigned(FActiveFrame) and
     Assigned(FActiveFrame.EpiDocument)
  then
    F.AddInitialDocument(FActiveFrame.DocumentFile);

  if F.ShowModal = mrOK then
    Result := F.Report;

  if not Assigned(Result) then exit;

  Screen.Cursor := crHourGlass;
  Application.ProcessMessages;
  S := Result.RunReport;

  ShowReportForm(Self,
    'Report of: ' + Result.ReportTitle,
    S,
    F.RadioGroup1.ItemIndex = 1);

  Screen.Cursor := crDefault;
  Application.ProcessMessages;

  if FreeAfterRun then
    FreeAndNil(Result);
  F.Free;
end;

function TMainForm.RunReportEx(ReportClass: TReportBaseClass;
  const FreeAfterRun: boolean): TReportBase;
var
  F: TReportForm;
  S: String;
begin
  Result := nil;

  F := TReportForm.Create(Self, ReportClass);

  if Assigned(FActiveFrame) and
     Assigned(FActiveFrame.DocumentFile)
  then
    F.AddInitialDocumentFile(FActiveFrame.DocumentFile);

  try
    if F.ShowModal = mrOK then
      Result := F.Report;

    if not Assigned(Result) then exit;

    Screen.Cursor := crHourGlass;
    Application.ProcessMessages;
    S := Result.RunReport;

    ShowReportForm(Self,
      'Report of: ' + Result.ReportTitle,
      S,
      F.RadioGroup1.ItemIndex = 1);

    Screen.Cursor := crDefault;
    Application.ProcessMessages;

    if FreeAfterRun then
      FreeAndNil(Result);

  finally
    F.Free;
  end;
end;

{$IFDEF EPI_IPC_TEST}
function TMainForm.CheckEntryClientOpenFile(const FileName: string): boolean;
begin
  {$IFDEF EPI_USEIPC}
    result := FEpiIPC.IsFileOpenMsg(FileName);
  {$ELSE}
    result := false;
  {$ENDIF}
end;

procedure TMainForm.CheckHasOpenFile(const MsgType: TMessageType;
  const Msg: string; out Ack: TMessageType);
begin
{  Ack := epiIPC_Ack_FileNotOpen;
  if Assigned(FActiveFrame) and (FActiveFrame.ProjectFileName = Msg) then
    Ack := epiIPC_Ack_FileIsOpen;      }
end;
{$ENDIF}

procedure TMainForm.LMOpenProject(var Msg: TLMessage);
var
  Dlg: TOpenDialog;
  Fn: String;
begin
  if Msg.WParam = 0 then
  begin
    Dlg := TOpenDialog.Create(self);
    Dlg.InitialDir := ManagerSettings.WorkingDirUTF8;
    Dlg.Filter := GetEpiDialogFilter([dfEPX, dfEPZ, dfCollection]);

    if not Dlg.Execute then exit;

    Fn := Dlg.FileName;
    Dlg.Free;
  end else begin
    Fn := TString(Msg.WParam).Str;
    TString(Msg.WParam).Free;
  end;

  DoOpenProject(Fn);
end;

procedure TMainForm.LMOpenRecent(var Msg: TLMessage);
var
  A: TAction;
begin
  A := TAction(Msg.WParam);
  DoOpenProject(ExpandFileNameUTF8(A.Caption));
  UpdateProcessToolbar;
end;

procedure TMainForm.LMNewProject(var Msg: TLMessage);
begin
  DoNewProject;
end;

procedure TMainForm.LMCloseProject(var Msg: TLMessage);
begin
  DoCloseProject;
end;

procedure TMainForm.LMImportToProject(var Msg: TLMessage);
begin
  DoNewProject;
  FActiveFrame.Import(Boolean(Msg.WParam));
end;

procedure TMainForm.LMDesignerAdd(var Msg: TLMessage);
begin
  if Assigned(FActiveFrame) then
  with Msg do
    Result := SendMessage(FActiveFrame.Handle, Msg, WParam, LParam);
end;

{$IFDEF EPI_IPC_TEST}
procedure TMainForm.SetupIPC;
begin
  FEpiIPC := TEpiIPC.Create(ApplicationName, Self);
  FEpiIPC.OnRequest := @CheckHasOpenFile;
end;
{$ENDIF}

procedure TMainForm.UpdateRecentFiles;
var
  Mi: TMenuItem;
  i: Integer;
  A: TAction;
begin
  LoadRecentFilesIni(GetRecentIniFileName);

  RecentFilesSubMenu.Visible := RecentFiles.Count > 0;
  RecentFilesSubMenu.Clear;
  RecentFilesSubPopupMenu.Visible := RecentFilesSubMenu.Visible;
  RecentFilesSubPopupMenu.Clear;

  for i := 0 to MaxRecentFiles - 1 do
  begin
    // Main menu
    A := TAction(RecentFilesActionList.Actions[i]);

    // Disable actions if the list of recentfiles is not long enough.
    if i >= RecentFiles.Count then
    begin
      A.Enabled := false;
      Continue;
    end;

    A.Enabled := true;
    A.Caption := RecentFiles[i];

    Mi := TMenuItem.Create(RecentFilesSubMenu);
    Mi.Name := 'recent' + inttostr(i);
    Mi.Action := A;
    RecentFilesSubMenu.Add(Mi);

    // Popup menu
    Mi := TMenuItem.Create(RecentFilesSubPopupMenu);
    Mi.Action := A;
    RecentFilesSubPopupMenu.Add(Mi);
  end;
end;

procedure TMainForm.AssignActionLinks;
begin
  // Only as long as one project is created!
  if Not Assigned(FActiveFrame) then
  begin
    // The "importnewprojectaction" is an action that is only assigned if no
    // active project is assigned. This will create a new project and start
    // the import dialog.
    AddStructureMenuItem.Action    := ImportInNewProjectAction;
    AddStructFromBLMenuItem.Action := ImportCBInNewProjectAction;
    exit;
  end;

  UpdateMainMenu;
  FActiveFrame.AssignActionLinks;
end;

procedure TMainForm.BeginUpdatingForm;
begin
  BeginFormUpdate;
//  LockRealizeBounds;
end;

procedure TMainForm.EndUpdatingForm;
begin
//  UnlockRealizeBounds;
  EndFormUpdate;
end;

constructor TMainForm.Create(TheOwner: TComponent);
var
  i: Integer;
  A: TAction;
  K: Word;
  Shift: TShiftState;
begin
  inherited Create(TheOwner);

  FActiveFrame := nil;

  ShortCutToKey(M_OpenRecent, K, Shift);
  for i := 1 to MaxRecentFiles do
  begin
    A := TAction.Create(RecentFilesActionList);
    A.ShortCut := KeyToShortCut(VK_1 + (i - 1), Shift);
    A.Enabled  := false;
    A.OnExecute := @OpenRecentMenuItemClick;

    TAccessActionList(RecentFilesActionList).AddAction(A);
  end;

  {$IFDEF EPI_IPC_TEST}
  SetupIPC;
  {$ENDIF}

  LoadGlyphs;
  UpdateMainMenu;
  AssignActionLinks;
end;

procedure TMainForm.RestoreDefaultPos;
begin
  TProjectFrame.RestoreDefaultPos(FActiveFrame);

  TAboutForm.RestoreDefaultPos;
  TAppendForm.RestoreDefaultPos;
  TExportForm.RestoreDefaultPos;
  TPrepareDoubleEntryForm.RestoreDefaultPos;
  TSettingsForm.RestoreDefaultPos;
  TStaticReportsForm.RestoreDefaultPos;
  TToolsForm.RestoreDefaultPos;
  TValueLabelDataImport.RestoreDefaultPos;

  ReportFormRestoreDefaultPos;

  BeginFormUpdate;
  Width := 700;
  Height := 600;
  Top := 5;
  Left := 5;
  EndFormUpdate;

  SaveFormPosition(Self, 'MainForm');
end;

end.
