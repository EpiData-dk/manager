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
    ChangeWorkDirAction: TAction;
    filedivider2: TMenuItem;
    filedivider3: TMenuItem;
    editdivider1: TMenuItem;
    ChangeWorkDirMenuItem: TMenuItem;
    filedivider1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem4: TMenuItem;
    CloseFormItem: TMenuItem;
    WindowsMenu: TMenuItem;
    ToolsDivider1: TMenuItem;
    ShowWorkFlowAction: TAction;
    MenuItem3: TMenuItem;
    NewDataFormBtn: TToolButton;
    OpenToolBtn: TToolButton;
    PageControl2: TPageControl;
    StartEditorAction: TAction;
    Image6: TImage;
    Image7: TImage;
    MenuItem1: TMenuItem;
    TabSheet2: TTabSheet;
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
    procedure ChangeWorkDirActionExecute(Sender: TObject);
    procedure ClosePageActionExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure MainFormMenuChange(Sender: TObject; Source: TMenuItem;
      Rebuild: Boolean);
    procedure NewDesignFormActionExecute(Sender: TObject);
    procedure PageControl1Changing(Sender: TObject; var AllowChange: Boolean);
    procedure PageControl1PageChanged(Sender: TObject);
    procedure shortIntroItemClick(Sender: TObject);
    procedure MetaDataBtnClick(Sender: TObject);
    procedure SettingsActionExecute(Sender: TObject);
    procedure ShowWorkFlowActionExecute(Sender: TObject);
    procedure StartEditorActionExecute(Sender: TObject);
    procedure StatusBar1DblClick(Sender: TObject);
    procedure StatusBar1DrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
  private
    { private declarations }
    FActiveFrame: TFrame;
    TabNameCount: integer;
    ProgressBarMain: TProgressBar;
    WorkFlowSheet: TTabSheet;
    procedure CloseTab(Sender: TObject);
    function GetWorkFlowShown: boolean;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    function ShowProgress(Sender: TObject; Percent: Cardinal; Msg: string): TProgressResult;
    procedure ReadClipBoard(ClipBoardLine: TStrings);
    procedure ShowOnStatusBar(Msg: string; Idx: integer);
    procedure AddToMenu(MenuItem: TMenuItem; Section: Cardinal); overload;
    procedure AddToMenu(aAction: TBasicAction; Section: Cardinal); overload;
    procedure RemoveFromMenu(Section: Cardinal);
    property ActiveFrame: TFrame read FActiveFrame;
    property WorkFlowShown: boolean read GetWorkFlowShown;
  end;

const
  // Menu sections.
  MMFile    = $000000;
   MMFileRW   = $0200;
  MMEdit    = $010000;
   MMEditTop =  $0000;
  MMTools   = $020000;
   MMToolsTop = $0000;
  MMHelp    = $030000;

var
  MainForm: TMainForm;

implementation

uses
  UEpiLog, settings, Clipbrd,
  InterfaceBase, LCLType, LCLIntf, editormain,
  workflow_frame, design_frame, managertypes;


{ TMainForm }

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
{$IFNDEF EPI_DEBUG}
var
  i: integer;
  Res: word;
{$ENDIF}
begin
  {$IFNDEF EPI_DEBUG}
  CanClose := false;
  for i := PageControl1.PageCount -1 downto 0 do
  begin
    if PageControl1.Pages[i].Controls[0] is TDesignFrame then
    begin
      if TDesignFrame(PageControl1.Pages[i].Controls[0]).Modified then
      begin
        Res := MessageDlg(
          'Warning',
          Format(
            'Dataform (%s) was modified since last save.' + LineEnding +
            'Close Form and loose changes?',
            [PageControl1.Pages[i].Caption]),
          mtWarning,
          mbYesNo + [mbYesToAll],
          0,
          mbNo
        );

        case Res of
          mrYes: begin
            TDesignFrame(PageControl1.Pages[i].Controls[0]).Modified := false;
            CloseTab(PageControl1.Pages[i]);
            continue;
          end;
          mrYesToAll: break;
        end;
        PageControl1.ActivePage := PageControl1.Pages[i];
        Exit;
      end;
    end;
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
  ShowOnStatusBar(ManagerSettings.WorkingDirUTF8, 0);

  // Progress bar.
  ProgressBarMain := TProgressBar.Create(Self);
  {$IFDEF MSWINDOWS}
//    StatusBar1.Panels[3].Style := psOwnerDraw;
    ProgressBarMain.Parent := StatusBar1;
    ProgressBarMain.Align := alCustom;
    PbStyle := WidgetSet.GetWindowLong(ProgressBarMain.Handle, GWL_EXSTYLE);
    PbStyle := PbStyle - WS_EX_STATICEDGE;
    WidgetSet.SetWindowLong(ProgressBarMain.Handle, GWL_EXSTYLE, PbStyle);
  {$ELSE}
    ProgressBarMain.Parent := ProgressPanel;
    ProgressBarMain.Align := alNone;
    ProgressBarMain.Left := EditorBtn.Left + EditorBtn.Width + 10;
    ProgressBarMain.Width := (ProgressPanel.Width - 10) - ProgressBarMain.Left;
    ProgressBarMain.Top := (ProgressPanel.Height - ProgressBarMain.Height) div 2;
    ProgressBarMain.Anchors := [akLeft, akRight];
  {$ENDIF}
  ProgressBarMain.Smooth := true;
  ProgressBarMain.Visible := false;

  {$IFNDEF EPI_DEBUG}
  Self.Width := 800;
  Self.Height := 600;
  {$ENDIF}

  // WorkFlowSheet sheet.
  PageControl1.Options := PageControl1.Options + [nboShowCloseButtons];
  PageControl1.OnCloseTabClicked := @CloseTab;
  ShowWorkFlowAction.Execute;
  MainFormMenuChange(nil, nil, true);
end;

procedure TMainForm.MainFormMenuChange(Sender: TObject; Source: TMenuItem;
  Rebuild: Boolean);
var
  i: Integer;
  j: Integer;
begin
  for i := 0 to MainFormMenu.Items.Count - 1 do
  with MainFormMenu.Items[i] do
  begin
    for j := Count -1 downto 0 do
    begin
      Items[j].Visible := true;
      if (j = 0) and (Items[j].Caption = '-') then
        Items[j].Visible := false;
      if (j > 0) and (Items[j].Caption = Items[j-1].Caption) then
        Items[j].Visible := false;
    end;
  end;
end;


procedure TMainForm.NewDesignFormActionExecute(Sender: TObject);
var
  TabSheet: TTabSheet;
  Frame: TDesignFrame;
begin
//  ShowOnStatusBar('Add fields: Click toolbar or read files', 0);
  TabSheet := TTabSheet.Create(PageControl1);
  TabSheet.PageControl := PageControl1;
  TabSheet.Name := 'TabSheet' + IntToStr(TabNameCount);
  TabSheet.Caption := 'Untitled';

  if TabNameCount > 1 then
    TabSheet.Caption := TabSheet.Caption + ' (' + IntToStr(TabNameCount-1) + ')';

  if PageControl1.PageCount >= 1 then
    PageControl1.ShowTabs := true;

  Frame := TDesignFrame.Create(TabSheet);
  Frame.Name := 'Frame' + IntToStr(TabNameCount);
  Frame.Align := alClient;
  Frame.Parent := TabSheet;
  FActiveFrame := Frame;

  PageControl1.ActivePage := TabSheet;

  Inc(TabNameCount);
end;

procedure TMainForm.PageControl1Changing(Sender: TObject;
  var AllowChange: Boolean);
begin
  // Event happens before activepage is changed to new sheet.
  // - hence we call deactive here.
  AllowChange := true;
  if not Assigned(PageControl1.ActivePage.Components[0]) then
    Exit;

  // On all tabsheets component [0] is a TFrame that implements IManagerFrame.
  (PageControl1.ActivePage.Components[0] as IManagerFrame).DeActivateFrame;
end;

procedure TMainForm.PageControl1PageChanged(Sender: TObject);
begin
  // Event happens after activepage is changed to new sheet.
  // - hence we call activate here
  // On creating a new page this event is called before components are created,
  // - hence component[0] does not yet exists. This does not matter since the
  //   actionlist is created with state asNormal by default.
  if not Assigned(PageControl1.ActivePage.Components[0]) then
    Exit;

  FActiveFrame := TFrame(PageControl1.ActivePage.Components[0]);

  // On all tabsheets component [0] is a TFrame that implements IManagerFrame.
  (PageControl1.ActivePage.Components[0] as IManagerFrame).ActivateFrame;
end;

procedure TMainForm.shortIntroItemClick(Sender: TObject);
begin
  OpenURL(ExtractFilePath(UTF8Decode(Application.ExeName)) + '/docs/epidatamanagerintro.pdf');
end;

procedure TMainForm.MetaDataBtnClick(Sender: TObject);
begin
//  ShowOnStatusBar('Not implementet yet', 0);
end;


procedure TMainForm.SettingsActionExecute(Sender: TObject);
var
  SettingsForm: TSettingsForm;
  i: Integer;
begin
  SettingsForm := TSettingsForm.Create(self);
  if SettingsForm.ShowModal = mrCancel then exit;

  ShowOnStatusBar(ManagerSettings.WorkingDirUTF8, 0);

  for i := 0 to PageControl1.PageCount -1 do
  if PageControl1.Pages[i].Controls[0] is TDesignFrame then
    TDesignFrame(PageControl1.Pages[i].Controls[0]).UpdateAllFields;
end;

procedure TMainForm.ShowWorkFlowActionExecute(Sender: TObject);
begin
  if not Assigned(WorkFlowSheet) then
  begin
    WorkFlowSheet := TTabSheet.Create(PageControl1);
    with TWorkFlowFrame.Create(WorkFlowSheet) do
    begin
      Parent := WorkFlowSheet;
      Align := alClient;
    end;
    FActiveFrame := TFrame(WorkFlowSheet.Components[0]);
    WorkFlowSheet.Parent := PageControl1;
    WorkFlowSheet.TabIndex := 0;
    WorkFlowSheet.Caption := 'WorkFlow';
  end;
  PageControl1.ActivePage := WorkFlowSheet;
end;

procedure TMainForm.StartEditorActionExecute(Sender: TObject);
begin
  if not Assigned(EditorForm) then
    EditorForm := TEditorForm.Create(Self);
  EditorForm.Show;
end;

procedure TMainForm.StatusBar1DblClick(Sender: TObject);
begin
  ChangeWorkDirAction.Execute;
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

procedure TMainForm.ChangeWorkDirActionExecute(Sender: TObject);
var
  Dlg: TSelectDirectoryDialog;
begin
  Dlg := TSelectDirectoryDialog.Create(Self);
  try
    Dlg.InitialDir := ManagerSettings.WorkingDirUTF8;
    if not Dlg.Execute then exit;
    ManagerSettings.WorkingDirUTF8 := Dlg.FileName;
    ShowOnStatusBar(ManagerSettings.WorkingDirUTF8, 0);
  finally
    Dlg.Free;
  end;
end;

procedure TMainForm.CloseTab(Sender: TObject);
begin
  if not (Sender is TTabSheet) then exit;


  {$IFNDEF EPI_DEBUG}
  if (PageControl1.ActivePage.Components[0] is TDesignFrame) and
     (TDesignFrame(PageControl1.ActivePage.Controls[0]).Modified) and
     (MessageDlg( 'Warning',
                  'Dataform was modified since last save.' + LineEnding +
                  'Close Form - and loose changes ?',
                  mtWarning,
                  mbYesNo,
                  0,
                  mbNo) = mrNo) then
    Exit;
  {$ENDIf}

  PageControl1.ActivePage := PageControl1.FindNextPage(TTabSheet(Sender), False, True);
  if Sender = WorkFlowSheet then
    WorkFlowSheet := nil;
  (Sender as TTabSheet).Free;

  if (PageControl1.PageCount < 1) or ((PageControl1.PageCount = 1) and (PageControl1.ActivePage = WorkFlowSheet)) then
  begin
    PageControl1.ShowTabs := false;;
    ShowOnStatusBar('', 1);
    ShowOnStatusBar('', 2);
  end;
end;

function TMainForm.GetWorkFlowShown: boolean;
begin
  result := Assigned(WorkFlowSheet);
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
    ProgressBarMain.Position := Percent;
    ProgressBarMain.Repaint;
  end;
  if Percent = 0 then
  begin
    {$IFDEF MSWINDOWS}
    StatusBar1.Panels[3].Style := psOwnerDraw;
    {$ENDIF}
    ProgressBarMain.Visible := true;
    Application.ProcessMessages;
  end;
  if Percent = 100 then
  begin
    ProgressBarMain.Visible := false;
    {$IFDEF MSWINDOWS}
    StatusBar1.Panels[3].Style := psText;
    {$ENDIF}
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

procedure TMainForm.ShowOnStatusBar(Msg: string; Idx: integer);
begin
  if (Idx < 0) or (Idx > StatusBar1.Panels.Count -1) then exit;
  StatusBar1.Panels[Idx].Text := Msg;
  if Idx = 0 then
    StatusBar1.Hint := Msg;
end;

procedure TMainForm.AddToMenu(MenuItem: TMenuItem; Section: Cardinal);
var
  M, S, Idx: Byte;
  MenuSection: TMenuItem;
  i: Integer;
  j: Integer;
begin
  // Section is read this way (byte wise - high to low):
  // Byte   |   Content
  // --------------------------------------------------------
  //   1    |    (not used)
  //   2    |    Main section, ie. File -> Help
  //   3    |    Sub section in main section (devided by line).
  //   4    |    Index in subsection.
  M := Section shr 16;
  S := Section shr 8;
  Idx := Section;

  MenuSection := MainFormMenu.Items[M];
  i := 0;
  j := 0;
  while (i < MenuSection.Count) and (j < S) do
  begin
    if MenuSection.Items[i].Caption = '-' then
      inc(j);
    inc(i);
  end;

  MenuSection.Insert(I + Idx, MenuItem);
end;

procedure TMainForm.AddToMenu(aAction: TBasicAction; Section: Cardinal);
var
  MenuItem: TMenuItem;
begin
  MenuItem := TMenuItem.Create(aAction.Owner);
  MenuItem.Action := aAction;
  AddToMenu(MenuItem, Section);
end;

procedure TMainForm.RemoveFromMenu(Section: Cardinal);
var
  M, S, Idx: Byte;
  MenuSection: TMenuItem;
  i: Integer;
  j: Integer;
begin
  // See AddToMenu...
  M := Section shr 16;
  S := Section shr 8;
  Idx := Section;

  MenuSection := MainFormMenu.Items[M];
  i := 0;
  j := 0;
  while (i < MenuSection.Count) and (j < S) do
  begin
    if MenuSection.Items[i].Caption = '-' then
      inc(j);
    inc(i);
  end;
  MenuSection.Delete(I + Idx);
end;


initialization
  {$I main.lrs}

end.

