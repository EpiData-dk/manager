unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Menus, ComCtrls, ActnList, StdActns, ExtCtrls, StdCtrls, Buttons,
  project_frame, LMessages, manager_messages, epidocument, report_base,
  episervice_ipc, episervice_ipctypes, epiexportsettings, simpleipc;

type

  { TMainForm }

  TMainForm = class(TForm)
    AddStructureMenuItem: TMenuItem;
    EditMenuDivider2: TMenuItem;
    KeyFieldsMenuItem: TMenuItem;
    VerifyDoubleEntryAction: TAction;
    PrepareDoubleEntryAction: TAction;
    ExportAction: TAction;
    ExportMenuItem: TMenuItem;
    DataSetMenu: TMenuItem;
    DoubleEntryMenu: TMenuItem;
    PrepareDoubleEntryMenuItem: TMenuItem;
    ValidatDoubleEntryMenuItem: TMenuItem;
    ProjectPasswordMenuItem: TMenuItem;
    ViewDataSetMenuItem: TMenuItem;
    ValueLabelEditor2MenuItem: TMenuItem;
    ProjectOverviewReportMenuItem: TMenuItem;
    ProjectReportAction: TAction;
    ExtendedListReportAction: TAction;
    CombinedListReportAction: TAction;
    ExtendedReportMenuItem: TMenuItem;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    ValueLabelListReportAction: TAction;
    ValueLabaleListReportMenuItem: TMenuItem;
    CombinedReportMenuItem: TMenuItem;
    QuestionListReportMenuItem: TMenuItem;
    MenuItem5: TMenuItem;
    QuestionListReportAction: TAction;
    ReportGeneratorAction: TAction;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    CodeBookMenuItem: TMenuItem;
    PackAction: TAction;
    ToolMenuDivider1: TMenuItem;
    StartEntryClientAction: TAction;
    EpiDataTutorialsMenuItem: TMenuItem;
    MenuItem1: TMenuItem;
    EntryClientMenuItem: TMenuItem;
    PackMenuItem: TMenuItem;
    RecentFilesSubMenu: TMenuItem;
    UserAccessBtn: TButton;
    GCPBtn: TButton;
    OpenProjectBtn: TButton;
    NewProjectBtn: TButton;
    HelpMenuDivider1: TMenuItem;
    ProcessToolPanel: TPanel;
    TutorialsMenuDivider1: TMenuItem;
    WebTutorialsMenuItem: TMenuItem;
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
    EditMenuDivider1: TMenuItem;
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
    procedure CheckVersionActionExecute(Sender: TObject);
    procedure CloseProjectActionExecute(Sender: TObject);
    procedure CloseProjectActionUpdate(Sender: TObject);
    procedure CombinedListReportActionExecute(Sender: TObject);
    procedure CopyProjectInfoActionExecute(Sender: TObject);
    procedure DefaultWindowPosActionExecute(Sender: TObject);
    procedure EpiDataTutorialsMenuItemClick(Sender: TObject);
    procedure ExportActionExecute(Sender: TObject);
    procedure ExtendedListReportActionExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure NewProjectActionExecute(Sender: TObject);
    procedure OpenProjectActionExecute(Sender: TObject);
    procedure PackActionExecute(Sender: TObject);
    procedure PrepareDoubleEntryActionExecute(Sender: TObject);
    procedure ProjectReportActionExecute(Sender: TObject);
    procedure QuestionListReportActionExecute(Sender: TObject);
    procedure ReportGeneratorActionExecute(Sender: TObject);
    procedure SettingsActionExecute(Sender: TObject);
    procedure ShortCutKeysMenuItemClick(Sender: TObject);
    procedure ShortIntroMenuItemClick(Sender: TObject);
    procedure ShowAboutActionExecute(Sender: TObject);
    procedure StartEntryClientActionExecute(Sender: TObject);
    procedure StartEntryClientActionUpdate(Sender: TObject);
    procedure UserAccessBtnClick(Sender: TObject);
    procedure ValueLabelListReportActionExecute(Sender: TObject);
    procedure VerifyDoubleEntryActionExecute(Sender: TObject);
    procedure WebTutorialsMenuItemClick(Sender: TObject);
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
    procedure DoNewProject;
    procedure DoOpenProject(Const AFileName: string);
    procedure UpdateMainMenu;
    procedure UpdateProcessToolbar;
    procedure UpdateShortCuts;
    procedure UpdateSettings;
    procedure OpenRecentMenuItemClick(Sender: TObject);
    function  ToolsCheckOpenFile(out FileName: string; out LocalDoc: boolean): TEpiDocument;
    function  RunReport(ReportClass: TReportBaseClass): boolean;
  private
    { Messages }
    procedure LMOpenProject(var Msg: TLMessage);  message LM_MAIN_OPENPROJECT;
    procedure LMOpenRecent(var Msg: TLMessage);   message LM_MAIN_OPENRECENT;
    procedure LMNewProject(var Msg: TLMessage);   message LM_MAIN_NEWPROJECT;
    procedure LMCloseProject(var Msg: TLMessage); message LM_MAIN_CLOSEPROJECT;
    // Message relaying...
    procedure LMDesignerAdd(var Msg: TLMessage); message LM_DESIGNER_ADD;
  private
    { Process communication }
    FEpiIPC:  TEpiIPC;
    procedure  SetupIPC;
    function   CheckEntryClientOpenFile(Const FileName: string): boolean;
    procedure  CheckHasOpenFile(Const MsgType: TMessageType; Const Msg: string; out Ack: TMessageType);
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
  workflow_frame, LCLProc, LCLIntf,
  settings2, settings2_var, about, Clipbrd, epiversionutils,
  valuelabelseditor_form, epimiscutils,
  epicustombase, project_settings, LCLType, UTF8Process,
  toolsform, epidatafiles, epistringutils, epiexport, reportgenerator,
  strutils, report_fieldlist, report_valuelabellist,
  report_combinedlist, viewer_form, staticreports_form,
  report_fieldlist_extended, report_project_overview,
  shortcuts, valuelabelseditor_form2, export_form, epiadmin,
  epitools_integritycheck, datasetviewer_frame, prepare_double_entry_form,
  validate_double_entry_form, design_runtimedesigner,
  report_double_entry_validation;

{ TMainForm }

procedure TMainForm.FormShow(Sender: TObject);
begin
  SetCaption;

  if ManagerSettings.SaveWindowPositions then
    LoadFormPosition(Self, 'MainForm');

  UpdateSettings;
  UpdateRecentFiles;

  // Show welcome message
  {$IFNDEF EPI_DEBUG}
  if ManagerSettings.ShowWelcome then
    ShowMessagePos('EpiData Manager:' + LineEnding +
                   'See help menu above for an introduction.' + LineEnding +
                   'Get latest version from http://www.epidata.dk', 15, 15);
  {$ENDIF}

  {$IFDEF EPI_DEBUG}
  UserAccessBtn.Visible := true;
  {$ENDIF}
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  res: LongInt;
begin
  CanClose := true;

  {$IFNDEF EPI_DEBUG}
  if Assigned(FActiveFrame) then
    FActiveFrame.CloseQuery(CanClose);
  {$ENDIF}

  if CanClose and ManagerSettings.SaveWindowPositions then
    SaveFormPosition(Self, 'MainForm');
  SaveSettingToIni(ManagerSettings.IniFileName);
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
      'Filename: ' + TProjectFrame(FActiveFrame).ProjectFileName + LineEnding +
      'XML Version: ' + IntToStr(Version) + LineEnding +
      'Field count: ' + IntToStr(DataFiles[0].Fields.Count) + LineEnding +
      'Record count: ' + IntToStr(DataFiles[0].Size);
  end;
  Clipboard.AsText := S;
end;

procedure TMainForm.DefaultWindowPosActionExecute(Sender: TObject);
begin
  RestoreDefaultPos;
end;

procedure TMainForm.EpiDataTutorialsMenuItemClick(Sender: TObject);
begin
  OpenURL('http://www.epidata.org/dokuwiki/doku.php/documentation:tutorials');
end;

procedure TMainForm.ExportActionExecute(Sender: TObject);
var
  IsLocalDoc: boolean;
  Fn: string;
  Doc: TEpiDocument;
  ExportForm: TExportForm;
  Settings: TEpiExportSetting;
  Exporter: TEpiExport;
begin
  Settings := nil;
  Exporter := nil;
  ExportForm := nil;

  Doc := ToolsCheckOpenFile(Fn, IsLocalDoc);
  if not Assigned(Doc) then exit;

  try
    ExportForm := TExportForm.Create(Self, Doc, Fn);
    if ExportForm.ShowModal <> mrOK then exit;

    Exporter := TEpiExport.Create;
    if not Exporter.Export(ExportForm.ExportSetting) then
      ShowMessage('Export Failed.');
  finally
    ExportForm.Free;
    Exporter.Free;
    Settings.Free;
  end;
end;

procedure TMainForm.ExtendedListReportActionExecute(Sender: TObject);
begin
  RunReport(TReportFieldListExtended);
end;

procedure TMainForm.CheckVersionActionExecute(Sender: TObject);
var
  Stable: TEpiVersionInfo;
  Test: TEpiVersionInfo;
  Response: string;
  NewStable: Boolean;
  NewTest: Boolean;
  EntryScore: Integer;
  StableScore: Integer;
  TestScore: Integer;
  S: String;
begin
  if not CheckVersionOnline('epidatamanager', Stable, Test, Response) then
  begin
    ShowMessage(
      'ERROR: Could not find version information.' + LineEnding +
      'Response: ' + Response);
    exit;
  end;

  with ManagerVersion do
    EntryScore  := (VersionNo * 10000) + (MajorRev * 100) + (MinorRev);
  With Stable do
    StableScore := (VersionNo * 10000) + (MajorRev * 100) + (MinorRev);
  With Test do
    TestScore   := (VersionNo * 10000) + (MajorRev * 100) + (MinorRev);

  NewStable     := (StableScore - EntryScore) > 0;
  NewTest       := (TestScore   - EntryScore) > 0;

  with ManagerVersion do
    S := Format('Current Version: %d.%d.%d.%d', [VersionNo, MajorRev, MinorRev, BuildNo]) + LineEnding;
  with Stable do
    if NewStable then
      S := S + Format('New public release available: %d.%d.%d.%d', [VersionNo, MajorRev, MinorRev, BuildNo]) + LineEnding
    else
      S := S + Format('Latest public release: %d.%d.%d.%d', [VersionNo, MajorRev, MinorRev, BuildNo]) + LineEnding;
   with Test do
     if NewTest then
      S := S + Format('New test version available: %d.%d.%d.%d', [VersionNo, MajorRev, MinorRev, BuildNo])
    else
      S := S + Format('Latest test version: %d.%d.%d.%d', [VersionNo, MajorRev, MinorRev, BuildNo]);
  ShowMessage(S);
end;

procedure TMainForm.CloseProjectActionExecute(Sender: TObject);
begin
  PostMessage(Self.Handle, LM_MAIN_CLOSEPROJECT, 0, 0);
end;

procedure TMainForm.CloseProjectActionUpdate(Sender: TObject);
begin
  CloseProjectAction.Enabled := Assigned(FActiveFrame);
end;

procedure TMainForm.CombinedListReportActionExecute(Sender: TObject);
begin
  RunReport(TReportCombinedList);
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  Fn: String;
begin
  Modified := false;
  if Paramcount < 1 then exit;
  Fn := ParamStrUTF8(1);
  if FileExistsUTF8(Fn) then
    PostMessage(Self.Handle, LM_MAIN_OPENPROJECT, WPARAM(TString.Create(Fn)), 0);
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
  Doc: TEpiDocument;
  LocalDoc: Boolean;
  S: LongInt;
  T: Integer;
  Str: String;
  i: Integer;
  Fn: string;
begin
  try
    F := nil;
    Doc := ToolsCheckOpenFile(Fn, LocalDoc);
    if not Assigned(Doc) then exit;

    F := TToolsForm.Create(Self);
    F.Caption := 'Pack: ' + Doc.Study.Title.Text;
    F.EpiDocument := Doc;
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
      Doc.SaveToFile(Fn);
  finally
    if LocalDoc and Assigned(Doc) then
      Doc.Free;
    if Assigned(F) then F.free;
  end;
end;

procedure TMainForm.PrepareDoubleEntryActionExecute(Sender: TObject);
var
  Fn: string;
  Local: boolean;
  Doc: TEpiDocument;
begin
  Doc := ToolsCheckOpenFile(Fn, Local);
  PrepareDoubleEntry(Doc, Fn);
  if Local then
    Doc.Free;
end;

procedure TMainForm.ProjectReportActionExecute(Sender: TObject);
begin
  RunReport(TReportProjectOverview);
end;

procedure TMainForm.QuestionListReportActionExecute(Sender: TObject);
begin
  RunReport(TReportFieldLists);
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
begin
  OpenURL('http://www.epidata.org/dokuwiki/doku.php/documentation:program_keys');
end;

procedure TMainForm.ShortIntroMenuItemClick(Sender: TObject);
var
  Fn: String;
begin
  Fn := UTF8Encode(ExtractFilePath(Application.ExeName) + '/epidatamanagerintro.pdf');
  if FileExistsUTF8(Fn) then
    OpenURL(Fn)
  else
    OpenURL('http://epidata.dk/php/downloadc.php?file=epidatamanagerintro.pdf');
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
begin
  Path := ManagerSettings.EntryClientDirUTF8 + PathDelim;
  Ext := ExtractFileExt(Application.ExeName);
  Entry := TProcessUTF8.Create(nil);
  Entry.CommandLine := Path + 'epidataentryclient' + ext;
  Entry.Execute;
  Entry.Free;
end;

procedure TMainForm.StartEntryClientActionUpdate(Sender: TObject);
var
  Path: String;
  Ext: String;
begin
  Path := ManagerSettings.EntryClientDirUTF8 + PathDelim;
  Ext := ExtractFileExt(Application.ExeName);
{  showmessage(
    'Path: ' + Path + LineEnding +
    'Ext: ' + Ext + LineEnding +
    'File: ' + Path + 'epidataentryclient' + Ext
    );}
  TAction(Sender).Enabled := FileExistsUTF8(Path + 'epidataentryclient' + Ext);
end;

procedure TMainForm.UserAccessBtnClick(Sender: TObject);
begin
  ShowValueLabelEditor2(FActiveFrame.EpiDocument.ValueLabelSets);
end;

procedure TMainForm.ValueLabelListReportActionExecute(Sender: TObject);
begin
  RunReport(TReportValueLabelList);
end;

procedure TMainForm.VerifyDoubleEntryActionExecute(Sender: TObject);
var
  Fn: string;
  Local: boolean;
  Doc: TEpiDocument;
begin
  Doc := ToolsCheckOpenFile(Fn, Local);
  ValidateDoubleEntry(Doc, Fn);
  if Local then
    Doc.Free;
end;

procedure TMainForm.WebTutorialsMenuItemClick(Sender: TObject);
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
  OpenURL(ManagerSettings.TutorialDirUTF8 + DirectorySeparator + TMenuItem(Sender).Caption + '.pdf');
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
    if (TutorialSubMenu[i] = TutorialsMenuDivider1) or
       (TutorialSubMenu[i] = WebTutorialsMenuItem) or
       (TutorialSubMenu[i] = EpiDataTutorialsMenuItem)
       then continue;

    MenuItem := TutorialSubMenu[i];
    TutorialSubMenu.Delete(i);
    MenuItem.Free;
  end;

  // Find all .pdf files in the directory set by TutorialsDirUTF8
  FileList := FindAllFiles(ManagerSettings.TutorialDirUTF8, '*.pdf', false);
  TutorialsMenuDivider1.Visible := FileList.Count > 0;

  if FileList.Count = 0 then Exit;

  for i := 0 to FileList.Count - 1 do
  begin
    MenuItem := TMenuItem.Create(TutorialSubMenu);
    MenuItem.Name := 'TutorialMenuItem' + IntToStr(i);
    MenuItem.Caption := ExtractFileNameOnly(FileList[i]);
    MenuItem.OnClick := @OpenTutorialMenuItemClick;

    With TutorialSubMenu do
      Insert(IndexOf(TutorialsMenuDivider1), MenuItem);
  end;
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
  SetCaption;
end;

procedure TMainForm.DoNewProject;
var
  TabSheet: TTabSheet;
  CanClose: boolean;
begin
  // Close Old project
  if not DoCloseProject then exit;

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
  FActiveFrame.NewDataFormAction.Execute;
  PageControl1.ActivePage := TabSheet;

  AssignActionLinks;

  Inc(TabNameCount);
end;

procedure TMainForm.DoOpenProject(const AFileName: string);
begin
  DoNewProject;
  try
    FActiveFrame.OpenProject(AFileName);
  except
    on E: TEpiCoreException do
      begin
        ShowMessage('Unable to open the file: ' + AFileName + LineEnding +
                    E.Message);
        DoCloseProject;
      end;
    on E: EFOpenError do
      begin
        ShowMessage('Unable to open the file: ' + AFileName + LineEnding +
                    'File is corrupt or does not exist.');
        DoCloseProject;
      end;
    on EEpiBadPassword do
      begin
        MessageDlg('Error',
                   'Unable to open the file: ' + AFileName + LineEnding + LineEnding +
                   'Invalid Password!',
                   mtError,
                   [mbOK], 0);
        DoCloseProject;
      end;
  else
    begin
      ShowMessage('Unable to open the file: ' + AFileName + LineEnding +
                  'An unknown error occured.');
      DoCloseProject;
    end;
  end;
end;

procedure TMainForm.UpdateMainMenu;
begin
  // FILE:
  CloseProjectAction.Enabled := Assigned(FActiveFrame);
  SaveProjectMenuItem.Visible := Assigned(FActiveFrame);
  SaveProjectAsMenuItem.Visible := Assigned(FActiveFrame);

  // EDIT:
  PasteAsFloatMenuItem.Visible := Assigned(FActiveFrame);
  PasteAsHeadingMenuItem.Visible := Assigned(FActiveFrame);
  PasteAsIntMenuItem.Visible := Assigned(FActiveFrame);
  PasteAsStringMenuItem.Visible := Assigned(FActiveFrame);
  EditMenuDivider1.Visible := Assigned(FActiveFrame);
  AddStructureMenuItem.Visible := Assigned(FActiveFrame);
  EditMenuDivider2.Visible := Assigned(FActiveFrame);

  // PROJECT:
  ProjectMenu.Visible       := Assigned(FActiveFrame);
  KeyFieldsMenuItem.Visible := Assigned(FActiveFrame);

  // TOOLS:
  DataSetMenu.Visible := Assigned(FActiveFrame);
end;

procedure TMainForm.UpdateProcessToolbar;
begin
  ProcessToolPanel.Visible :=
    (not Assigned(FActiveFrame)) and
    ManagerSettings.ShowWorkToolBar;
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
  CombinedListReportAction.ShortCut   := M_CombinedListReport;
  ExtendedListReportAction.ShortCut   := M_ExtendedListReport;
  ProjectReportAction.ShortCut        := M_ProjectOverviewReport;
end;

procedure TMainForm.UpdateSettings;
begin
  LoadTutorials;
  UpdateProcessToolbar;
  UpdateShortCuts;

  if Assigned(FActiveFrame) then
    TProjectFrame(FActiveFrame).UpdateFrame;
end;

procedure TMainForm.OpenRecentMenuItemClick(Sender: TObject);
begin
  PostMessage(Self.Handle, LM_MAIN_OPENRECENT, WParam(Sender), 0);
end;

function TMainForm.ToolsCheckOpenFile(out FileName: string;
  out LocalDoc: boolean): TEpiDocument;
var
  Dlg: TOpenDialog;
  St: TMemoryStream;
begin
  Result := nil;
  if Assigned(FActiveFrame) then
  begin
    LocalDoc := false;
    Result := FActiveFrame.EpiDocument;
    FileName := FActiveFrame.ProjectFileName;
  end else begin
    Dlg := TOpenDialog.Create(Self);
    Dlg.Filter := GetEpiDialogFilter([dfEPX, dfEPZ, dfCollection]);
    Dlg.InitialDir := ManagerSettings.WorkingDirUTF8;
    if not Dlg.Execute then exit;

    St := TMemoryStream.Create;
    if ExtractFileExt(UTF8ToSys(Dlg.FileName)) = '.epz' then
      ZipFileToStream(St, Dlg.FileName)
    else
      St.LoadFromFile(Dlg.FileName);

    St.Position := 0;
    Result := TEpiDocument.Create(ManagerSettings.StudyLang);
    Result.LoadFromStream(St);
    LocalDoc := true;
    FileName := Dlg.FileName;
  end;
end;

function TMainForm.RunReport(ReportClass: TReportBaseClass): boolean;
var
  F: TStaticReportsForm;
  R: TReportBase;
  H: TReportViewerForm;
  S: String;
begin
  R := nil;

  F := TStaticReportsForm.Create(Self, ReportClass);
  if Assigned(FActiveFrame) and
     Assigned(FActiveFrame.EpiDocument)
  then
  begin
    S := '(Not Saved)';
    if FActiveFrame.ProjectFileName <> '' then
      S := FActiveFrame.ProjectFileName;
    F.AddInitialDocument(S, FActiveFrame.EpiDocument);
  end;
  if F.ShowModal = mrOK then
    R := F.Report;
  if not Assigned(R) then exit;

  ShowReportForm(Self,
    'Report of: ' + R.ReportTitle,
    R.RunReport,
    F.RadioGroup1.ItemIndex = 0);
  F.Free;
end;

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
  Ack := epiIPC_Ack_FileNotOpen;
  if Assigned(FActiveFrame) and (FActiveFrame.ProjectFileName = Msg) then
    Ack := epiIPC_Ack_FileIsOpen;
end;

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

  if CheckEntryClientOpenFile(Fn) then
  begin
    if MessageDlg('Warning',
      'The file: ' + LineEnding +
      Fn + LineEnding +
      'is already opened by EpiData EntryClient.' + LineEnding +
      'Having the same file open in both programs may cause loss of data, due to overwriting of the file.' + LineEnding +
      'Do you wish to open file anyway?',
      mtWarning,
      mbYesNo,
      0,
      mbNo) = mrNo then exit;
  end;
  if not DoCloseProject then exit;

  DoOpenProject(Fn);
  UpdateProcessToolbar;
end;

procedure TMainForm.LMOpenRecent(var Msg: TLMessage);
var
  Item: TMenuItem;
begin
  Item := TMenuItem(Msg.WParam);
  DoOpenProject(ExpandFileNameUTF8(Item.Caption));
  UpdateProcessToolbar;
end;

procedure TMainForm.LMNewProject(var Msg: TLMessage);
begin
  DoNewProject;
  UpdateProcessToolbar;
end;

procedure TMainForm.LMCloseProject(var Msg: TLMessage);
begin
  DoCloseProject;
  UpdateProcessToolbar;
end;

procedure TMainForm.LMDesignerAdd(var Msg: TLMessage);
begin
  if Assigned(FActiveFrame) then
  with Msg do
    Result := SendMessage(FActiveFrame.Handle, Msg, WParam, LParam);
end;

procedure TMainForm.SetupIPC;
begin
  {$IFDEF EPI_USEIPC}
  FEpiIPC := TEpiIPC.Create(ApplicationName, Self);
  FEpiIPC.OnRequest := @CheckHasOpenFile;
  {$ENDIF}
end;

procedure TMainForm.UpdateRecentFiles;
var
  Mi: TMenuItem;
  i: Integer;
  K: Word;
  Shift: TShiftState;
begin
  ShortCutToKey(M_OpenRecent, K, Shift);

  RecentFilesSubMenu.Visible := RecentFiles.Count > 0;
  RecentFilesSubMenu.Clear;
  for i := 0 to RecentFiles.Count - 1 do
  begin
    Mi := TMenuItem.Create(RecentFilesSubMenu);
    Mi.Name := 'recent' + inttostr(i);
    Mi.Caption := RecentFiles[i];
    Mi.OnClick := @OpenRecentMenuItemClick;
    if i < 9 then
      Mi.ShortCut := KeyToShortCut(VK_1 + i, Shift);
    RecentFilesSubMenu.Add(Mi);
  end;
end;

procedure TMainForm.AssignActionLinks;
begin
  // Only as long as one project is created!
  if Not Assigned(FActiveFrame) then exit;

  UpdateMainMenu;
  SaveProjectMenuItem.Action   := FActiveFrame.SaveProjectAction;
  SaveProjectAsMenuItem.Action := FActiveFrame.SaveProjectAsAction;

  PasteAsHeadingMenuItem.Action := TRuntimeDesignFrame(FActiveFrame.ActiveFrame).PasteAsHeadingAction;
  PasteAsIntMenuItem.Action     := TRuntimeDesignFrame(FActiveFrame.ActiveFrame).PasteAsIntAction;
  PasteAsFloatMenuItem.Action   := TRuntimeDesignFrame(FActiveFrame.ActiveFrame).PasteAsFloatAction;
  PasteAsStringMenuItem.Action  := TRuntimeDesignFrame(FActiveFrame.ActiveFrame).PasteAsStringAction;
  ViewDataSetMenuItem.Action    := TRuntimeDesignFrame(FActiveFrame.ActiveFrame).ViewDatasetAction;
  AddStructureMenuItem.Action   := TRuntimeDesignFrame(FActiveFrame.ActiveFrame).ImportAction;

  ProjectPropertiesMenuItem.Action := FActiveFrame.ProjectSettingsAction;
  ValueLabelsMenuItem.Action       := FActiveFrame.ValueLabelEditorAction;
  ProjectPasswordMenuItem.Action   := FActiveFrame.ProjectPasswordAction;
  KeyFieldsMenuItem.Action         := FActiveFrame.KeyFieldsAction;
end;

procedure TMainForm.BeginUpdatingForm;
begin
  BeginFormUpdate;
end;

procedure TMainForm.EndUpdatingForm;
begin
  EndFormUpdate;
end;

constructor TMainForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FActiveFrame := nil;
  SetupIPC;
  UpdateMainMenu;
end;

procedure TMainForm.RestoreDefaultPos;
begin
  if Assigned(FActiveFrame) then
    FActiveFrame.RestoreDefaultPos;

  TSettingsForm.RestoreDefaultPos;
  TProjectSettingsForm.RestoreDefaultPos;

  BeginFormUpdate;
  Width := 700;
  Height := 600;
  Top := 5;
  Left := 5;
  EndFormUpdate;

  SaveFormPosition(Self, 'MainForm');
end;

end.
