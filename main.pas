unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Menus, ComCtrls, ActnList, StdActns;

type

  { TMainForm }

  TMainForm = class(TForm)
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
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure NewProjectActionExecute(Sender: TObject);
    procedure SettingsActionExecute(Sender: TObject);
    procedure ShortCutKeysMenuItemClick(Sender: TObject);
    procedure ShortIntroMenuItemClick(Sender: TObject);
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
  workflow_frame, project_frame, settings, LCLProc, LCLIntf, design_frame;

{ TMainForm }

procedure TMainForm.FormShow(Sender: TObject);
begin
  SetCaption;
//  ShowWorkFlowAction.Execute;
  {$IFDEF EPI_RELEASE}
  Width := 800;
  Height := 600;
  {$ENDIF}

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

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Modified := false;
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
  Caption := 'EpiData Project and Data Manager (v' + GetManagerVersion + ')';
  if Modified then
    Caption := Caption + ' (modified)';
end;

procedure TMainForm.SetModified(const AValue: boolean);
begin
  if FModified = AValue then exit;
  FModified := AValue;
  SetCaption;
end;

procedure TMainForm.ProjectModified(Sender: TObject);
begin
  Modified := TProjectFrame(Sender).Modified;
end;

procedure TMainForm.LoadIniFile;
const
  IniName = 'epidatamanager.ini';
begin
  // TODO : Settings can be loaded from commandline?

  if LoadSettingsFromIni(GetAppConfigDirUTF8(false) + IniName) then exit;

  // Todo - this is not optimal on Non-windows OS's. Do some checks for writeability first.
  if LoadSettingsFromIni(ExtractFilePath(Application.ExeName) + IniName) then exit;
end;

end.

