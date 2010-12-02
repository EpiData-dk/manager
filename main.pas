unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Menus, ComCtrls, ActnList, StdActns, project_frame;

type

  { TMainForm }

  TMainForm = class(TForm)
    OpenProjectAction: TAction;
    CloseProjectAction: TAction;
    DefaultWindowPosAction: TAction;
    CheckVersionAction: TAction;
    CopyProjectInfoAction: TAction;
    HelpMenuDivider1: TMenuItem;
    CopyVersionInfoMenuItem: TMenuItem;
    HelpMenuDivider2: TMenuItem;
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
    PasteAsQESMenuItem: TMenuItem;
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
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure NewProjectActionExecute(Sender: TObject);
    procedure OpenProjectActionExecute(Sender: TObject);
    procedure SettingsActionExecute(Sender: TObject);
    procedure ShortCutKeysMenuItemClick(Sender: TObject);
    procedure ShortIntroMenuItemClick(Sender: TObject);
    procedure ShowAboutActionExecute(Sender: TObject);
  private
    { private declarations }
    FModified: boolean;
    FActiveFrame: TProjectFrame;
    TabNameCount: integer;
    procedure SetCaption;
    procedure SetModified(const AValue: boolean);
    procedure ProjectModified(Sender: TObject);
    procedure LoadIniFile;
    function  DoCloseProject: boolean;
    procedure DoNewProject;
    procedure DoOpenProject(Const AFileName: string);
    procedure UpdateMainMenu;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    property Modified: boolean read FModified write SetModified;
    procedure RestoreDefaultPos;
  end;

var
  MainForm: TMainForm; 

implementation

{$R *.lfm}

uses
  workflow_frame, LCLProc, LCLIntf, design_frame,
  settings2, settings2_var, about, Clipbrd, epiversionutils,
  design_controls, structure_form, valuelabelseditor_form, epimiscutils;

{ TMainForm }

procedure TMainForm.FormShow(Sender: TObject);
begin
  SetCaption;

  LoadIniFile;
  if ManagerSettings.SaveWindowPositions then
    LoadFormPosition(Self, 'MainForm');
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  res: LongInt;
begin
  CanClose := true;

  {$IFDEF EPI_RELEASE}
  if Assigned(FActiveFrame) then
  begin
    TProjectFrame(FActiveFrame).CloseQuery(CanClose);
  end;
  {$ENDIF}

  if CanClose and ManagerSettings.SaveWindowPositions then
    SaveFormPosition(Self, 'MainForm');
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
  DoCloseProject;
end;

procedure TMainForm.CloseProjectActionUpdate(Sender: TObject);
begin
  CloseProjectAction.Enabled := Assigned(FActiveFrame);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Modified := false;
end;

procedure TMainForm.NewProjectActionExecute(Sender: TObject);
begin
  DoNewProject;
end;

procedure TMainForm.OpenProjectActionExecute(Sender: TObject);
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
  Dlg.Free;
end;

procedure TMainForm.SettingsActionExecute(Sender: TObject);
var
  SettingForm: TSettingsForm;
begin
  SettingForm := TSettingsForm.Create(Self);
  if SettingForm.ShowModal = mrCancel then exit;
  SettingForm.Free;

  if Assigned(FActiveFrame) then
    TProjectFrame(FActiveFrame).UpdateFrame;
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

procedure TMainForm.LoadIniFile;
const
  IniName = 'epidatamanager.ini';
var
  S: String;
begin
  // TODO : Settings can be loaded from commandline?

  if LoadSettingsFromIni(GetAppConfigFileUTF8(false)) then exit;

  // Todo - this is not optimal on Non-windows OS's. Do some checks for writeability first.
  if LoadSettingsFromIni(ExtractFilePath(Application.ExeName) + IniName) then exit;

  if not DirectoryExistsUTF8(GetAppConfigDirUTF8(false)) then
    CreateDirUTF8(GetAppConfigDirUTF8(false));
  ManagerSettings.IniFileName := GetAppConfigFileUTF8(false);
end;

function TMainForm.DoCloseProject: boolean;
begin
  result := true;
  if Assigned(FActiveFrame) then
  begin
    TProjectFrame(FActiveFrame).CloseQuery(result);
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

  PasteAsQESMenuItem.Action     := TDesignFrame(FActiveFrame.ActiveFrame).PasteAsQESAction;
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
  FActiveFrame.OpenProject(AFileName);
end;

procedure TMainForm.UpdateMainMenu;
begin
  // FILE:
  CloseProjectAction.Enabled := Assigned(FActiveFrame);
  SaveProjectMenuItem.Visible := Assigned(FActiveFrame);
  SaveProjectAsMenuItem.Visible := Assigned(FActiveFrame);

  // EDIT:
  PasteAsQESMenuItem.Visible := Assigned(FActiveFrame);
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

constructor TMainForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FActiveFrame := nil;
  UpdateMainMenu;
end;

procedure TMainForm.RestoreDefaultPos;
begin
  if Assigned(FActiveFrame) then;
    TProjectFrame(FActiveFrame).RestoreDefaultPos;

  TSettingsForm.RestoreDefaultPos;

  BeginFormUpdate;
  Width := 700;
  Height := 600;
  Top := 5;
  Left := 5;
  EndFormUpdate;

  SaveFormPosition(Self, 'MainForm');
end;

end.

