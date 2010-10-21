unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Menus, ComCtrls, ActnList, StdActns;

type

  { TMainForm }

  TMainForm = class(TForm)
    CheckVersionAction: TAction;
    CopyProjectInfoAction: TAction;
    HelpMenuDivider1: TMenuItem;
    CopyVersionInfoMenuItem: TMenuItem;
    HelpMenuDivider2: TMenuItem;
    AboutMenuItem: TMenuItem;
    CheckVersionMenuItem: TMenuItem;
    ExportStataMenuItem: TMenuItem;
    ProjectMenuDivider1: TMenuItem;
    ShowAboutAction: TAction;
    FileExitAction: TFileExit;
    FileExitMenuItem: TMenuItem;
    HelpMenu: TMenuItem;
    FileMenuDivider1: TMenuItem;
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
    ShowWorkFlowAction: TAction;
    ActionList1: TActionList;
    MainMenu1: TMainMenu;
    FileMenuItem: TMenuItem;
    PageControl1: TPageControl;
    procedure CheckVersionActionExecute(Sender: TObject);
    procedure CopyProjectInfoActionExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure NewProjectActionExecute(Sender: TObject);
    procedure SettingsActionExecute(Sender: TObject);
    procedure ShortCutKeysMenuItemClick(Sender: TObject);
    procedure ShortIntroMenuItemClick(Sender: TObject);
    procedure ShowAboutActionExecute(Sender: TObject);
    procedure ShowWorkFlowActionExecute(Sender: TObject);
  private
    FModified: boolean;
    { private declarations }
    WorkFlowSheet: TTabSheet;
    FActiveFrame: TFrame;
    TabNameCount: integer;
    procedure SetCaption;
    procedure SetModified(const AValue: boolean);
    procedure ProjectModified(Sender: TObject);
    procedure LoadIniFile;
  public
    { public declarations }
    property Modified: boolean read FModified write SetModified;
  end;

var
  MainForm: TMainForm; 

implementation

{$R *.lfm}

uses
  workflow_frame, project_frame, LCLProc, LCLIntf, design_frame,
  settings2, settings2_var, about, Clipbrd, epiversionutils;

{ TMainForm }

procedure TMainForm.FormShow(Sender: TObject);
begin
  SetCaption;
//  ShowWorkFlowAction.Execute;

  LoadIniFile;

  NewProjectAction.Execute;
  TDesignFrame(TProjectFrame(PageControl1.ActivePage.Controls[0]).ActiveFrame).UpdateFrame;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  res: LongInt;
begin
  CanClose := true;

  {$IFDEF EPI_RELEASE}
  if Modified then
  begin
    res := MessageDlg('Warning', 'Content has been modified since last save.' + LineEnding +
             'Save before close?', mtWarning, mbYesNoCancel, 0, mbCancel);
    case res of
      mrYes:    SaveProjectMenuItem.Action.Execute;
      mrNo:     exit;
      mrCancel: CanClose := false;
    end;
  end;
  {$ENDIF}
end;

procedure TMainForm.CopyProjectInfoActionExecute(Sender: TObject);
var
  S: String;
begin
  S := GetProgramInfo;
  if Assigned(TProjectFrame(FActiveFrame).EpiDocument) then
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

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Modified := false;
  {$IFDEF EPI_RELEASE}
  Width := 800;
  Height := 600;
  {$ENDIF}
end;

procedure TMainForm.NewProjectActionExecute(Sender: TObject);
var
  TabSheet: TTabSheet;
  Frame: TProjectFrame;
begin
  TabSheet := TTabSheet.Create(PageControl1);
  TabSheet.PageControl := PageControl1;
  TabSheet.Name := 'TabSheet' + IntToStr(TabNameCount);
  TabSheet.Caption := 'Untitled';

//  if PageControl1.PageCount >= 1 then
//    PageControl1.ShowTabs := true;

  Frame := TProjectFrame.Create(TabSheet);
  Frame.Name := 'ProjectFrame' + IntToStr(TabNameCount);
  Frame.Align := alClient;
  Frame.Parent := TabSheet;
  Frame.OnModified := @ProjectModified;
  Frame.NewDataFormAction.Execute;
  FActiveFrame := Frame;
  PageControl1.ActivePage := TabSheet;

  // Only as long as one project is created!
  SaveProjectMenuItem.Action := Frame.SaveProjectAction;
  SaveProjectAsMenuItem.Action := Frame.SaveProjectAsAction;
  OpenProjectMenuItem.Action := Frame.OpenProjectAction;

  PasteAsQESMenuItem.Action     := TDesignFrame(Frame.ActiveFrame).PasteAsQESAction;
  PasteAsHeadingMenuItem.Action := TDesignFrame(Frame.ActiveFrame).PasteAsHeadingAction;
  PasteAsIntMenuItem.Action     := TDesignFrame(Frame.ActiveFrame).PasteAsIntAction;
  PasteAsFloatMenuItem.Action   := TDesignFrame(Frame.ActiveFrame).PasteAsFloatAction;
  PasteAsStringMenuItem.Action  := TDesignFrame(Frame.ActiveFrame).PasteAsStringAction;

  ProjectPropertiesMenuItem.Action := Frame.ProjectSettingsAction;
  ExportStataMenuItem.Action       := Frame.ExportStataAction;

  Inc(TabNameCount);
end;

procedure TMainForm.SettingsActionExecute(Sender: TObject);
var
  SettingForm: TSettingsForm;
begin
  SettingForm := TSettingsForm.Create(Self);
  if SettingForm.ShowModal = mrCancel then exit;
  SettingForm.Free;

  TDesignFrame(TProjectFrame(PageControl1.ActivePage.Controls[0]).ActiveFrame).UpdateFrame;
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

procedure TMainForm.ShowWorkFlowActionExecute(Sender: TObject);
begin
  if not Assigned(WorkFlowSheet) then
  begin
    WorkFlowSheet := TTabSheet.Create(PageControl1);
    FActiveFrame := TWorkFlowFrame.Create(WorkFlowSheet);
    with FActiveFrame do
    begin
      Parent := WorkFlowSheet;
      Align := alClient;
    end;
    WorkFlowSheet.Parent := PageControl1;
    WorkFlowSheet.PageIndex := 0;
    WorkFlowSheet.Caption := 'WorkFlow';
  end;
  PageControl1.ActivePage := WorkFlowSheet;
end;

procedure TMainForm.SetCaption;
begin
  Caption := 'EpiData Manager (v' + GetManagerVersion + ')';
end;

procedure TMainForm.SetModified(const AValue: boolean);
begin
  if FModified = AValue then exit;
  FModified := AValue;
//  SetCaption;
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

end.

