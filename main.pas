unit main; 

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Menus, ActnList, StdActns, ExtCtrls, Buttons, ComCtrls, StdCtrls, UDataFileTypes;

type

  { TMainForm }

  TMainForm = class(TForm)
    EditMenu: TMenuItem;
    FeatureInfoLabel: TLabel;
    MenuItem1: TMenuItem;
    shortIntroItem: TMenuItem;
    MaintenanceBtn: TBitBtn;
    ProgressBar1: TProgressBar;
    SettingsMenu: TMenuItem;
    SettingsAction: TAction;
    Button1: TButton;
    GCPbtn: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    MetaDataBtn: TBitBtn;
    DesignBtn: TBitBtn;
    MainFormActionList: TActionList;
    FileExit1: TFileExit;
    MainFormMenu: TMainMenu;
    FileMenu: TMenuItem;
    ExitMenuItem: TMenuItem;
    PageControl1: TPageControl;
    Panel1: TPanel;
    ProgressPanel: TPanel;
    StatusBar1: TStatusBar;
    StatusBar2: TStatusBar;
    procedure Button1Click(Sender: TObject);
    procedure DesignBtnClick(Sender: TObject);
    procedure GCPbtnClick(Sender: TObject);
    procedure shortIntroItemClick(Sender: TObject);
    procedure MetaDataBtnClick(Sender: TObject);
    procedure SettingsActionExecute(Sender: TObject);
  private
    { private declarations }
    TabNameCount: integer;
    procedure CloseTab(Sender: TObject);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    function ShowProgress(Sender: TObject; Percent: Cardinal; Msg: string): TProgressResult;
  end; 

var
  MainForm: TMainForm;

implementation

uses
  design_frame, settings;


{ TMainForm }

procedure TMainForm.DesignBtnClick(Sender: TObject);
var
  Frame: TFrame;
  TabSheet: TTabSheet;
begin
  FeatureInfoLabel.Visible:= False;
  TabSheet := TTabSheet.Create(PageControl1);
  TabSheet.PageControl := PageControl1;
  TabSheet.Name := 'TabSheet' + IntToStr(TabNameCount);
  TabSheet.Caption := 'Untitled';
  PageControl1.ActivePage := TabSheet;

  if PageControl1.PageCount >= 1 then
  begin
    PageControl1.ShowTabs := true;
    PageControl1.Options := PageControl1.Options + [nboShowCloseButtons];
    PageControl1.OnCloseTabClicked := @CloseTab;
  end;
  Frame := TDesignFrame.Create(TabSheet);
  Frame.Name := 'Frame' + IntToStr(TabNameCount);
  Frame.Align := alClient;
  Frame.Parent := TabSheet;

  Inc(TabNameCount);
end;

procedure TMainForm.GCPbtnClick(Sender: TObject);
begin
  FeatureInfoLabel.Caption := 'GCP not ready yet';
  FeatureInfoLabel.Visible:= True;
end;

procedure TMainForm.shortIntroItemClick(Sender: TObject);
begin
    FeatureInfoLabel.Caption := 'Help System Not Ready';
  FeatureInfoLabel.Visible:= True;
end;

procedure TMainForm.MetaDataBtnClick(Sender: TObject);
begin
  FeatureInfoLabel.Caption := ' Not implementet yet';
  FeatureInfoLabel.Visible:= True;
end;


procedure TMainForm.SettingsActionExecute(Sender: TObject);
var
  SettingsForm: TSettingsForm;
begin
  SettingsForm := TSettingsForm.Create(self);
  if SettingsForm.ShowModal = mrCancel then exit;

  // TODO : Update design form if showing properties have changed.
  TDesignFrame(PageControl1.ActivePage.Controls[0]).UpdateAllFields;
end;

procedure TMainForm.Button1Click(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to Panel1.ControlCount - 1 do
    if Panel1.Controls[i] is TLabel then
      TLabel(Panel1.Controls[i]).Caption := Panel1.Controls[i].Name;
end;

procedure TMainForm.CloseTab(Sender: TObject);
begin
  if not (Sender is TTabSheet) then exit;

  PageControl1.ActivePage := PageControl1.FindNextPage(TTabSheet(Sender), True, True);
  (Sender as TTabSheet).Free;

  if PageControl1.PageCount < 1 then
    PageControl1.ShowTabs := false;;
end;

constructor TMainForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  TabNameCount := 1;
end;

function TMainForm.ShowProgress(Sender: TObject; Percent: Cardinal; Msg: string
  ): TProgressResult;
begin
  if Percent <> ProgressBar1.Position then
  begin
    ProgressBar1.Position := Percent;
    Application.ProcessMessages;
  end;
  result := prNormal;
end;

initialization
  {$I main.lrs}

end.

