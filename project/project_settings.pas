unit project_settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, Buttons, episettings;

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
    FProjectSettings: TEpiProjectSettings;
  protected
    constructor Create(TheOwner: TComponent); override;
    property    ProjectSettings: TEpiProjectSettings read FProjectSettings;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; AProjectSettings: TEpiProjectSettings);
  end; 

implementation

{$R *.lfm}

uses
  project_settings_field_frame, project_settings_interface;

{ TProjectSettingsForm }

procedure TProjectSettingsForm.FormShow(Sender: TObject);
var
  Frame: TFrame;
begin
  Frame := TFrame(ProjectSettingsView.Items[0].Data);
  (Frame as IProjectSettingsFrame).SetProjectSettings(ProjectSettings);
  Frame.Parent := Self;
  Frame.Align := alClient;
end;

procedure TProjectSettingsForm.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
var
  Frame: TFrame;
begin
  if ModalResult = mrCancel then exit;

  // TODO : Implement ActiveFrame - to handling shutdown of current frame.
  Frame := TFrame(ProjectSettingsView.Items[0].Data);
  CanClose := (Frame as IProjectSettingsFrame).ApplySettings;
end;

procedure TProjectSettingsForm.ProjectSettingsViewChange(Sender: TObject;
  Node: TTreeNode);
begin
  // Happens after the change...  (not used at the moment)
end;

procedure TProjectSettingsForm.ProjectSettingsViewChanging(Sender: TObject;
  Node: TTreeNode; var AllowChange: Boolean);
begin
  // Happens before the change...  (not used at the moment)
end;

constructor TProjectSettingsForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

constructor TProjectSettingsForm.Create(TheOwner: TComponent;
  AProjectSettings: TEpiProjectSettings);
begin
  Create(TheOwner);
  FProjectSettings := AProjectSettings;

  ProjectSettingsView.Items.FindNodeWithText('Fields').Data := Pointer(TFieldSettingsFrame.Create(Self));
end;

end.

