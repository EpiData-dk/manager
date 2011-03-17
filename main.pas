unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Menus, ComCtrls, ActnList, StdActns, ExtCtrls, StdCtrls, Buttons,
  project_frame, LMessages, manager_messages;

type

  { TMainForm }

  TMainForm = class(TForm)
    EpiDataTutorialsMenuItem: TMenuItem;
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
    ExportStataMenuItem: TMenuItem;
    FileMenuDivider1: TMenuItem;
    CloseProjectMenuItem: TMenuItem;
    NewProjectMenuItem: TMenuItem;
    ResetWindowPosMenuItem: TMenuItem;
    ValueLabelsMenuItem: TMenuItem;
    ProjectMenuDivider1: TMenuItem;
    ProjectStructureMenuItem: TMenuItem;
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
    procedure CopyProjectInfoActionExecute(Sender: TObject);
    procedure DefaultWindowPosActionExecute(Sender: TObject);
    procedure EpiDataTutorialsMenuItemClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure NewProjectActionExecute(Sender: TObject);
    procedure OpenProjectActionExecute(Sender: TObject);
    procedure SettingsActionExecute(Sender: TObject);
    procedure ShortCutKeysMenuItemClick(Sender: TObject);
    procedure ShortIntroMenuItemClick(Sender: TObject);
    procedure ShowAboutActionExecute(Sender: TObject);
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
    procedure UpdateSettings;
    procedure OpenRecentMenuItemClick(Sender: TObject);
    procedure LMOpenProject(var Msg: TLMessage);  message LM_MAIN_OPENPROJECT;
    procedure LMOpenRecent(var Msg: TLMessage);   message LM_MAIN_OPENRECENT;
    procedure LMNewProject(var Msg: TLMessage);   message LM_MAIN_NEWPROJECT;
    procedure LMCloseProject(var Msg: TLMessage); message LM_MAIN_CLOSEPROJECT;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    property  Modified: boolean read FModified write SetModified;
    procedure RestoreDefaultPos;
    procedure UpdateRecentFiles;
    procedure BeginUpdatingForm;
    procedure EndUpdatingForm;
  end;

var
  MainForm: TMainForm; 

implementation

{$R *.lfm}

uses
  workflow_frame, LCLProc, LCLIntf, design_frame,
  settings2, settings2_var, about, Clipbrd, epiversionutils,
  design_controls, structure_form, valuelabelseditor_form, epimiscutils,
  epicustombase, project_settings, LCLType;

{ TMainForm }

procedure TMainForm.FormShow(Sender: TObject);
begin
  SetCaption;

  if ManagerSettings.SaveWindowPositions then
    LoadFormPosition(Self, 'MainForm');

  UpdateSettings;
  UpdateRecentFiles;

  // Show welcome message
  {$IFDEF EPI_RELEASE}
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

  {$IFDEF EPI_RELEASE}
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
      'XML Version: ' + IntToStr(XMLSettings.Version) + LineEnding +
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

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Modified := false;
  {$IFDEF DARWIN}
  NewProjectAction.ShortCut := ShortCut(VK_N, [ssMeta]);
  SettingsAction.ShortCut   := ShortCut(VK_OEM_COMMA, [ssMeta]);
  CloseProjectAction.ShortCut := ShortCut(VK_W, [ssShift, ssMeta]);
  OpenProjectAction.ShortCut  := ShortCut(VK_O, [ssMeta]);
  {$ENDIF}
end;

procedure TMainForm.NewProjectActionExecute(Sender: TObject);
begin
  PostMessage(Self.Handle, LM_MAIN_NEWPROJECT, 0, 0);
end;

procedure TMainForm.OpenProjectActionExecute(Sender: TObject);
begin
  PostMessage(Self.Handle, LM_MAIN_OPENPROJECT, 0, 0);
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

  // Only as long as one project is created!
  UpdateMainMenu;
  SaveProjectMenuItem.Action := FActiveFrame.SaveProjectAction;
  SaveProjectAsMenuItem.Action := FActiveFrame.SaveProjectAsAction;

  PasteAsHeadingMenuItem.Action := TDesignFrame(FActiveFrame.ActiveFrame).PasteAsHeadingAction;
  PasteAsIntMenuItem.Action     := TDesignFrame(FActiveFrame.ActiveFrame).PasteAsIntAction;
  PasteAsFloatMenuItem.Action   := TDesignFrame(FActiveFrame.ActiveFrame).PasteAsFloatAction;
  PasteAsStringMenuItem.Action  := TDesignFrame(FActiveFrame.ActiveFrame).PasteAsStringAction;

  ProjectPropertiesMenuItem.Action := FActiveFrame.ProjectSettingsAction;
  ExportStataMenuItem.Action       := FActiveFrame.ExportStataAction;
  ProjectStructureMenuItem.Action  := FActiveFrame.ShowStructureAction;
  ValueLabelsMenuItem.Action       := FActiveFrame.ValueLabelEditorAction;

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

  // PROJECT:
  ProjectMenu.Visible := Assigned(FActiveFrame);

  // TOOLS:
  ToolsMenu.Visible := Assigned(FActiveFrame);
end;

procedure TMainForm.UpdateProcessToolbar;
begin
  ProcessToolPanel.Visible :=
    (not Assigned(FActiveFrame)) and
    ManagerSettings.ShowWorkToolBar;
end;

procedure TMainForm.UpdateSettings;
begin
  LoadTutorials;
  UpdateProcessToolbar;

  if Assigned(FActiveFrame) then
    TProjectFrame(FActiveFrame).UpdateFrame;
end;

procedure TMainForm.OpenRecentMenuItemClick(Sender: TObject);
begin
  PostMessage(Self.Handle, LM_MAIN_OPENRECENT, WParam(Sender), 0);
end;

procedure TMainForm.LMOpenProject(var Msg: TLMessage);
var
  Dlg: TOpenDialog;
begin
  Dlg := TOpenDialog.Create(self);
  Dlg.InitialDir := ManagerSettings.WorkingDirUTF8;
  Dlg.Filter := GetEpiDialogFilter(true, true, false, false, false, false,
    false, false, false, true, false);

  if not Dlg.Execute then exit;
  if not DoCloseProject then exit;

  DoOpenProject(Dlg.FileName);
  UpdateProcessToolbar;
  Dlg.Free;
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

procedure TMainForm.UpdateRecentFiles;
var
  Mi: TMenuItem;
  i: Integer;
begin
  RecentFilesSubMenu.Visible := RecentFiles.Count > 0;

  RecentFilesSubMenu.Clear;
  for i := 0 to RecentFiles.Count - 1 do
  begin
    Mi := TMenuItem.Create(RecentFilesSubMenu);
    Mi.Name := 'recent' + inttostr(i);
    Mi.Caption := RecentFiles[i];
    Mi.OnClick := @OpenRecentMenuItemClick;
    if i < 9 then
      {$IFDEF DARWIN}
      Mi.ShortCut := KeyToShortCut(VK_1 + i, [ssMeta, ssShift]);
      {$ELSE}
      Mi.ShortCut := KeyToShortCut(VK_1 + i, [ssCtrl, ssShift]);
      {$ENDIF}
    RecentFilesSubMenu.Add(Mi);
  end;
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
  UpdateMainMenu;
end;

procedure TMainForm.RestoreDefaultPos;
begin
  if Assigned(FActiveFrame) then
    FActiveFrame.RestoreDefaultPos;

  TSettingsForm.RestoreDefaultPos;
  TProject_Structure_Form.RestoreDefaultPos;
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
