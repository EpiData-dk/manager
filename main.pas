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
    procedure FormCreate(Sender: TObject);
    procedure NewProjectActionExecute(Sender: TObject);
    procedure SettingsActionExecute(Sender: TObject);
    procedure ShortCutKeysMenuItemClick(Sender: TObject);
    procedure ShortIntroMenuItemClick(Sender: TObject);
    procedure ShowWorkFlowActionExecute(Sender: TObject);
  private
    { private declarations }
    WorkFlowSheet: TTabSheet;
    FActiveFrame: TFrame;
    TabNameCount: integer;
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm; 

implementation

{$R *.lfm}

uses
  workflow_frame, project_frame, settings, LCLProc, LCLIntf;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Caption := 'EpiData Project and Data Manager'; // + ' (v' + GetManagerVersion + ')';
//  ShowWorkFlowAction.Execute;
  {$IFDEF EPI_RELEASE}
  Width := 800;
  Height := 600;
  {$ENDIF}
  NewProjectAction.Execute;
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
  Frame.NewDataFormAction.Execute;
  FActiveFrame := Frame;
  PageControl1.ActivePage := TabSheet;


  // Only as long as one project is created!
  SaveProjectMenuItem.Action := Frame.SaveProjectAction;
  SaveProjectAsMenuItem.Action := Frame.SaveProjectAsAction;
  OpenProjectMenuItem.Action := Frame.OpenProjectAction;


  Inc(TabNameCount);
end;

procedure TMainForm.SettingsActionExecute(Sender: TObject);
var
  SettingForm: TSettingsForm;
begin
  SettingForm := TSettingsForm.Create(Self);
  if SettingForm.ShowModal = mrCancel then exit;
  SettingForm.Free;
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

end.

