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
    NewDesignFormAction: TAction;
    ClosePageAction: TAction;
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
    procedure ClosePageActionExecute(Sender: TObject);
    procedure DesignBtnClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure GCPbtnClick(Sender: TObject);
    procedure NewDesignFormActionExecute(Sender: TObject);
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
    procedure ReadClipBoard(ClipBoardLine: TStrings);
  end; 

var
  MainForm: TMainForm;

implementation

uses
  design_frame, settings, Clipbrd;


{ TMainForm }

procedure TMainForm.DesignBtnClick(Sender: TObject);
var
  Frame: TFrame;
  TabSheet: TTabSheet;
begin
  FeatureInfoLabel.Caption := 'Add fields: Click toolbar';
  TabSheet := TTabSheet.Create(PageControl1);
  TabSheet.PageControl := PageControl1;
  TabSheet.Name := 'TabSheet' + IntToStr(TabNameCount);
  TabSheet.Caption := 'Untitled';
  if TabNameCount > 1 then
    TabSheet.Caption := TabSheet.Caption + ' (' + IntToStr(TabNameCount) + ')';
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

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
{$IFNDEF EPI_DEBUG}
var
  TmpMod: boolean;
  i, idx: integer;
{$ENDIF}
begin
  {$IFNDEF EPI_DEBUG}
  CanClose := false;
  TmpMod := false;
  for i := 0 to PageControl1.PageCount -1 do
  begin
    if TDesignFrame(PageControl1.Pages[i].Controls[0]).Modified then
    begin
      TmpMod := true;
      Idx := i;
    end;
  end;

  if TmpMod and (MessageDlg('Dataform modified since last save.' +
     LineEnding + 'Close Form ?', mtWarning, mbYesNo, 0) = mrNo) then
  begin
    PageControl1.ActivePage := PageControl1.Pages[Idx];
    Exit;
  end;
  {$ENDIF}
  CanClose := true;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Caption := 'EpiData Project and Data Manager' +
    ' (v' + GetManagerVersion + ')';

  {$IFDEF EPI_DEBUG}
  Panel1.Visible := true;
  {$ELSE EPI_DEBUG}
  Panel1.Visible := false;
  {$ENDIF}
end;

procedure TMainForm.GCPbtnClick(Sender: TObject);
begin
  FeatureInfoLabel.Caption := 'GCP not ready yet';
end;

procedure TMainForm.NewDesignFormActionExecute(Sender: TObject);
var
  TabSheet: TTabSheet;
  Frame: TDesignFrame;
begin
  FeatureInfoLabel.Caption := 'Add fields: Click toolbar';
  TabSheet := TTabSheet.Create(PageControl1);
  TabSheet.PageControl := PageControl1;
  TabSheet.Name := 'TabSheet' + IntToStr(TabNameCount);
  TabSheet.Caption := 'Untitled';
  if TabNameCount > 1 then
    TabSheet.Caption := TabSheet.Caption + ' (' + IntToStr(TabNameCount) + ')';
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

procedure TMainForm.shortIntroItemClick(Sender: TObject);
begin
  FeatureInfoLabel.Caption := 'Help System Not Ready';
end;

procedure TMainForm.MetaDataBtnClick(Sender: TObject);
begin
  FeatureInfoLabel.Caption := 'Not implementet yet';
end;


procedure TMainForm.SettingsActionExecute(Sender: TObject);
var
  SettingsForm: TSettingsForm;
  i: Integer;
begin
  SettingsForm := TSettingsForm.Create(self);
  if SettingsForm.ShowModal = mrCancel then exit;

  // TODO : Update design form if showing properties have changed.
  for i := 0 to PageControl1.PageCount -1 do
    TDesignFrame(PageControl1.Pages[i].Controls[0]).UpdateAllFields;
end;

procedure TMainForm.Button1Click(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to Panel1.ControlCount - 1 do
    if Panel1.Controls[i] is TLabel then
      TLabel(Panel1.Controls[i]).Caption := Panel1.Controls[i].Name;
end;

procedure TMainForm.ClosePageActionExecute(Sender: TObject);
begin
  CloseTab(PageControl1.ActivePage);
end;

procedure TMainForm.CloseTab(Sender: TObject);
begin
  if not (Sender is TTabSheet) then exit;

  {$IFNDEF EPI_DEBUG}
  if (TDesignFrame(PageControl1.ActivePage.Controls[0]).Modified) and
     (MessageDlg('Dataform is modified since last save.' +
                 LineEnding + 'Continue?', mtWarning, mbYesNo, 0) = mrNo) then
    Exit;
  {$ENDIf}

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

procedure TMainForm.ReadClipBoard(ClipBoardLine: TStrings);
var
  TmpStr: String;
begin
  if Clipboard.HasFormat(CF_Text) then
  begin
    TmpStr := Clipboard.AsText;
    TmpStr := StringReplace(TmpStr, LineEnding, #1, [rfReplaceAll]);
    ClipBoardLine.Delimiter := #1;
    ClipBoardLine.StrictDelimiter := true;
    ClipBoardLine.DelimitedText := TmpStr;
  end;
end;

initialization
  {$I main.lrs}

end.

