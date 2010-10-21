unit project_settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, Buttons, episettings, epicustombase;

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
    FEpiDocument: TEpiCustomBase;
    FActiveFrame: TFrame;
  protected
    constructor Create(TheOwner: TComponent); override;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; AEpiDocument: TEpiCustomBase);
  end; 

implementation

{$R *.lfm}

uses
  project_settings_field_frame, project_settings_interface, project_settings_general_frame;

{ TProjectSettingsForm }

procedure TProjectSettingsForm.FormShow(Sender: TObject);
begin
{  FActiveFrame := TFrame(ProjectSettingsView.Items[0].Data);
  (FActiveFrame as IProjectSettingsFrame).SetProjectSettings(FEpiDocument);
  FActiveFrame.Parent := Self;
  FActiveFrame.Align := alClient;}
  ProjectSettingsView.Selected := ProjectSettingsView.Items[0];
//  ProjectSettingsView.SetFocus;
end;

procedure TProjectSettingsForm.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
var
  Frame: TFrame;
begin
  if ModalResult = mrCancel then exit;

  CanClose := (FActiveFrame as IProjectSettingsFrame).ApplySettings;
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

  FActiveFrame := TFrame(Node.Data);
  AllowChange := (FActiveFrame as IProjectSettingsFrame).ApplySettings;
  if not AllowChange then exit;

  FActiveFrame.Hide;
end;

constructor TProjectSettingsForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

constructor TProjectSettingsForm.Create(TheOwner: TComponent;
  AEpiDocument: TEpiCustomBase);
begin
  Create(TheOwner);
  FEpiDocument := AEpiDocument;

  ProjectSettingsView.Items.FindNodeWithText('General').Data := Pointer(TProjectSettings_GeneralFrame.Create(Self));
  ProjectSettingsView.Items.FindNodeWithText('Fields').Data := Pointer(TProjectSettings_FieldFrame.Create(Self));
end;

end.

