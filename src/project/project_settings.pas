unit project_settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, Buttons, epicustombase, epidocument;

type

  { TProjectSettingsForm }

  TProjectSettingsForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Panel1: TPanel;
    Splitter1: TSplitter;
    ProjectSettingsView: TTreeView;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
    procedure ProjectSettingsViewChange(Sender: TObject; Node: TTreeNode);
    procedure ProjectSettingsViewChanging(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
  private
    { private declarations }
    FEpiDocument: TEpiDocument;
    FActiveFrame: TFrame;
    function ApplySettingForCurrentFrame: boolean;
  protected
    constructor Create(TheOwner: TComponent); override;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; AEpiDocument: TEpiDocument);
    class procedure RestoreDefaultPos;
  end; 

implementation

{$R *.lfm}

uses
  project_settings_field_frame, project_settings_interface,
  project_settings_general_frame, settings2, settings2_var,
  project_settings_autoincrement_frame, project_settings_extended_access,
  main;

{ TProjectSettingsForm }

procedure TProjectSettingsForm.FormShow(Sender: TObject);
begin
  ProjectSettingsView.Selected := ProjectSettingsView.Items[0];
  if ManagerSettings.SaveWindowPositions then
    LoadFormPosition(Self, 'ProjectSettings');
end;

procedure TProjectSettingsForm.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
var
  Frame: TFrame;
begin
  if ModalResult = mrCancel then exit;

  CanClose := ApplySettingForCurrentFrame;
  if CanClose and ManagerSettings.SaveWindowPositions then
    SaveFormPosition(Self, 'ProjectSettings');
end;

procedure TProjectSettingsForm.ProjectSettingsViewChange(Sender: TObject;
  Node: TTreeNode);
begin
  // Happens after the change...
  if csDestroying in ComponentState then exit;

  FActiveFrame := TFrame(Node.Data);
  (FActiveFrame as IProjectSettingsFrame).SetProjectSettings(FEpiDocument);
  FActiveFrame.Parent := Self;
  FActiveFrame.Align := alClient;
  FActiveFrame.Show;
end;

procedure TProjectSettingsForm.ProjectSettingsViewChanging(Sender: TObject;
  Node: TTreeNode; var AllowChange: Boolean);
begin
  // Happens before the change...
  if csDestroying in ComponentState then exit;

  AllowChange := ApplySettingForCurrentFrame;
  if not AllowChange then exit;

  if (Assigned(FActiveFrame)) then
    FActiveFrame.Hide;
end;

function TProjectSettingsForm.ApplySettingForCurrentFrame: boolean;
begin
  Result := true;

  Mainform.Cursor := crHourGlass;
  Application.ProcessMessages;
  MainForm.BeginUpdatingForm;
  if (Assigned(FActiveFrame)) then
    Result := (FActiveFrame as IProjectSettingsFrame).ApplySettings;
  MainForm.EndUpdatingForm;
  MainForm.Cursor := crDefault;
  Application.ProcessMessages;
end;

constructor TProjectSettingsForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

constructor TProjectSettingsForm.Create(TheOwner: TComponent;
  AEpiDocument: TEpiDocument);
begin
  Create(TheOwner);
  FEpiDocument := AEpiDocument;

  with ProjectSettingsView.Items do
  begin
    FindNodeWithText('Backup').Data             := Pointer(TProjectSettings_BackupFrame.Create(Self));
    FindNodeWithText('Auto Increment').Data     := Pointer(TProjectSettings_AutoIncFrame.Create(Self));
    FindNodeWithText('Display of Variables').Data  := Pointer(TProjectSettings_FieldFrame.Create(Self));
  end;

  // Extended Access
  if FEpiDocument.Admin.Initialized then
    begin
      ProjectSettingsView.Items.AddObject(
        nil, 'Extended Access',
        TProjectSettings_ExternalAccessFrame.Create(Self)
      );
    end;
end;

class procedure TProjectSettingsForm.RestoreDefaultPos;
var
  Aform: TForm;
begin
  Aform := TForm.Create(nil);
  Aform.Width := 580;
  Aform.Height := 320;
  Aform.top := (Screen.Monitors[0].Height - Aform.Height) div 2;
  Aform.Left := (Screen.Monitors[0].Width - Aform.Width) div 2;
  SaveFormPosition(Aform, 'ProjectSettings');
  AForm.free;
end;

end.

