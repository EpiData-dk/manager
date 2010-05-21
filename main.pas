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
  workflow_frame, project_frame, settings;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Caption := 'EpiData Project and Data Manager'; // + ' (v' + GetManagerVersion + ')';
//  ShowWorkFlowAction.Execute;
  Width := 1024;
  Height := 768;
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

