unit main; 

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Menus, ActnList, StdActns, ExtCtrls, Buttons, ComCtrls, StdCtrls,
  UDataFileTypes;

type

  { TMainForm }

  TMainForm = class(TForm)
    Alignmenu: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    NewDataFormBtn: TToolButton;
    OpenToolBtn: TToolButton;
    StartEditorAction: TAction;
    Image6: TImage;
    Image7: TImage;
    MenuItem1: TMenuItem;
    ToolsMenu: TMenuItem;
    NewDesignFormAction: TAction;
    ClosePageAction: TAction;
    EditMenu: TMenuItem;
    HelpMenu: TMenuItem;
    shortIntroItem: TMenuItem;
    MaintenanceBtn: TBitBtn;
    SettingsMenu: TMenuItem;
    SettingsAction: TAction;
    EditorBtn: TBitBtn;
    MetaDataBtn: TBitBtn;
    DesignBtn: TBitBtn;
    MainFormActionList: TActionList;
    ExitAction: TFileExit;
    MainFormMenu: TMainMenu;
    FileMenu: TMenuItem;
    ExitMenuItem: TMenuItem;
    PageControl1: TPageControl;
    ProgressPanel: TPanel;
    StatusBar1: TStatusBar;
    procedure ClosePageActionExecute(Sender: TObject);
    procedure DesignBtnClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure EditorBtnClick(Sender: TObject);
    procedure NewDesignFormActionExecute(Sender: TObject);
    procedure PageControl1Changing(Sender: TObject; var AllowChange: Boolean);
    procedure PageControl1PageChanged(Sender: TObject);
    procedure shortIntroItemClick(Sender: TObject);
    procedure MetaDataBtnClick(Sender: TObject);
    procedure SettingsActionExecute(Sender: TObject);
    procedure StartEditorActionExecute(Sender: TObject);
    procedure StatusBar1DblClick(Sender: TObject);
    procedure StatusBar1DrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
  private
    { private declarations }
    TabNameCount: integer;
    ProgressBarMain: TProgressBar;
    procedure CloseTab(Sender: TObject);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    function ShowProgress(Sender: TObject; Percent: Cardinal; Msg: string): TProgressResult;
    procedure ReadClipBoard(ClipBoardLine: TStrings);
    procedure ShowOnStatusBar(Msg: string; Idx: integer);
  end; 

var
  MainForm: TMainForm;

implementation

uses
  design_frame, UEpiLog, settings, Clipbrd,
  InterfaceBase, LCLType, editormain;


{ TMainForm }

procedure TMainForm.DesignBtnClick(Sender: TObject);
var
  Frame: TFrame;
  TabSheet: TTabSheet;
begin
  ShowOnStatusBar('Click toolbar to add fields or read files', 0);
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

  if TmpMod and (MessageDlg('Dataform was modified since last save.' +
     LineEnding + 'Close Form ?', mtWarning, mbYesNo, 0) = mrNo) then
  begin
    PageControl1.ActivePage := PageControl1.Pages[Idx];
    Exit;
  end;
  {$ENDIF}
  CanClose := true;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  PbStyle: LongInt;
begin
  Caption := 'EpiData Project and Data Manager' +
    ' (v' + GetManagerVersion + ')';
  StatusBar1.Hint := ManagerSettings.WorkingDirUTF8;
  StatusBar1.ShowHint := true;

  ProgressBarMain := TProgressBar.Create(Self);
  {$IFDEF MSWINDOWS}
    StatusBar1.Panels[3].Style := psOwnerDraw;
    ProgressBarMain.Parent := StatusBar1;
    ProgressBarMain.Align := alCustom;
    PbStyle := WidgetSet.GetWindowLong(ProgressBarMain.Handle, GWL_EXSTYLE);
    PbStyle := PbStyle - WS_EX_STATICEDGE;
    WidgetSet.SetWindowLong(ProgressBarMain.Handle, GWL_EXSTYLE, PbStyle);
  {$ENDIF}
  {$IFDEF UNIX}
    ProgressBarMain.Parent := ProgressPanel;
    ProgressBarMain.Align := alNone;
    ProgressBarMain.Left := EditorBtn.Left + EditorBtn.Width + 10;
    ProgressBarMain.Width := (ProgressPanel.Width - 10) - ProgressBarMain.Left;
    ProgressBarMain.Top := (ProgressPanel.Height - ProgressBarMain.Height) div 2;
    ProgressBarMain.Anchors := [akLeft, akRight];
  {$ENDIF}
  ProgressBarMain.Smooth := true;
  ProgressBarMain.Visible := false;
end;


procedure TMainForm.NewDesignFormActionExecute(Sender: TObject);
var
  TabSheet: TTabSheet;
  Frame: TDesignFrame;
begin
  ShowOnStatusBar('Add fields: Click toolbar or read files', 0);
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

procedure TMainForm.PageControl1Changing(Sender: TObject;
  var AllowChange: Boolean);
begin
  // Event happens before activepage is changed to new sheet.
  // - hence we disable shortcut events here.
  AllowChange := true;
  if not Assigned(PageControl1.ActivePage.Components[0]) then
    Exit;
  TDesignFrame(PageControl1.ActivePage.Components[0]).DesignFrameActionList.State := asSuspended;
end;

procedure TMainForm.PageControl1PageChanged(Sender: TObject);
begin
  // Event happens after activepage is changed to new sheet.
  // - hence we enable shortcut events here.
  // On creating a new page this event is called before components are created,
  // - hence component[0] does not yet exists. This does not matter since the
  //   actionlist is created with state asNormal by default.
  if not Assigned(PageControl1.ActivePage.Components[0]) then
    Exit;
  TDesignFrame(PageControl1.ActivePage.Components[0]).DesignFrameActionList.State := asNormal;
  TDesignFrame(PageControl1.ActivePage.Components[0]).UpdateNonInteractiveVisuals;
end;

procedure TMainForm.EditorBtnClick(Sender: TObject);
begin
  ShowOnStatusBar('GCP not ready yet', 0);
end;

procedure TMainForm.shortIntroItemClick(Sender: TObject);
var
  mr : integer;
begin
  ShowOnStatusBar('Help System Not Ready', 0);
end;

procedure TMainForm.MetaDataBtnClick(Sender: TObject);
begin
  ShowOnStatusBar('Not implementet yet', 0);
end;


procedure TMainForm.SettingsActionExecute(Sender: TObject);
var
  SettingsForm: TSettingsForm;
  i: Integer;
begin
  SettingsForm := TSettingsForm.Create(self);
  if SettingsForm.ShowModal = mrCancel then exit;

  for i := 0 to PageControl1.PageCount -1 do
    TDesignFrame(PageControl1.Pages[i].Controls[0]).UpdateAllFields;
end;

procedure TMainForm.StartEditorActionExecute(Sender: TObject);
begin
  if not Assigned(EditorForm) then
    EditorForm := TEditorForm.Create(Self);
  EditorForm.Show;
end;

procedure TMainForm.StatusBar1DblClick(Sender: TObject);
var
  Dlg: TSelectDirectoryDialog;
begin
  Dlg := TSelectDirectoryDialog.Create(Self);
  try
    Dlg.InitialDir := ManagerSettings.WorkingDirUTF8;
    if not Dlg.Execute then exit;
    ManagerSettings.WorkingDirUTF8 := Dlg.FileName;
    StatusBar1.Hint := ManagerSettings.WorkingDirUTF8;
  finally
    Dlg.Free;
  end;
end;

procedure TMainForm.StatusBar1DrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
begin
  if Panel = StatusBar.Panels[3] then
  with ProgressBarMain do begin
    ProgressBarMain.Top := Rect.Top;
    ProgressBarMain.Left := Rect.Left;
    ProgressBarMain.Width := Rect.Right - Rect.Left;
    ProgressBarMain.Height := Rect.Bottom - Rect.Top;
  end;
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
     (MessageDlg('Dataform was modified since last save.' +
                 LineEnding + 'Close Form?', mtWarning, mbYesNo, 0) = mrNo) then
    Exit;
  {$ENDIf}

  PageControl1.ActivePage := PageControl1.FindNextPage(TTabSheet(Sender), True, True);
  (Sender as TTabSheet).Free;

  if PageControl1.PageCount < 1 then
  begin
    PageControl1.ShowTabs := false;;
    ShowOnStatusBar('', 1);
    ShowOnStatusBar('', 2);
  end;
end;

constructor TMainForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  TabNameCount := 1;
end;

function TMainForm.ShowProgress(Sender: TObject; Percent: Cardinal; Msg: string
  ): TProgressResult;
begin
  if Percent <> ProgressBarMain.Position then
  begin
    ProgressBarMain.Visible := true;
    ProgressBarMain.Position := Percent;
    ProgressBarMain.Repaint;
  end;
  if Percent = 0 then
    Application.ProcessMessages;
  if Percent = 100 then
    ProgressBarMain.Visible := false;;
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

procedure TMainForm.ShowOnStatusBar(Msg: string; Idx: integer);
begin
  if (Idx < 0) or (Idx > StatusBar1.Panels.Count -1) then exit;
  StatusBar1.Panels[Idx].Text := Msg;
end;


initialization
  {$I main.lrs}

end.

