unit settings2;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ComCtrls;

type

  { TSettingsForm2 }

  TSettingsForm2 = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Panel1: TPanel;
    SettingsView: TTreeView;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
    procedure SettingsViewChange(Sender: TObject; Node: TTreeNode);
    procedure SettingsViewChanging(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
  private
    { private declarations }
    FActiveFrame: TFrame;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
  end;

procedure RegisterSettingFrame(const Order: byte;
  const AValue: TCustomFrameClass; const AName: string);

implementation

{$R *.lfm}

uses
  settings2_interface, settings2_var,
  settings;

var
  Frames: TStringList = nil;

{ TSettingsForm2 }

procedure TSettingsForm2.SettingsViewChanging(Sender: TObject; Node: TTreeNode;
  var AllowChange: Boolean);
begin
  // Happens before the change...
  if csDestroying in ComponentState then exit;

  FActiveFrame := TFrame(Node.Data);
  AllowChange := (FActiveFrame as ISettingsFrame).ApplySettings;
  if not AllowChange then exit;

  FActiveFrame.Hide;
end;

procedure TSettingsForm2.SettingsViewChange(Sender: TObject; Node: TTreeNode);
begin
  // Happens after the change...
  if csDestroying in ComponentState then exit;

  FActiveFrame := TFrame(Node.Data);
  FActiveFrame.Show;
end;

procedure TSettingsForm2.FormShow(Sender: TObject);
begin
  FActiveFrame := TFrame(SettingsView.Items[0].Data);
  SettingsView.Selected := SettingsView.Items[0];

  if SettingsView.Items.Count > 0 then
    TFrame(SettingsView.Items[0].Data).Show;
  SettingsView.SetFocus;
end;

procedure TSettingsForm2.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := false;
  if not (FActiveFrame as ISettingsFrame).ApplySettings then exit;
  SaveSettingToIni(ManagerSettings2.IniFileName);
  CanClose := true;
end;

constructor TSettingsForm2.Create(TheOwner: TComponent);
var
  i: Integer;
  Frame: TCustomFrame;
  FrameClass: TCustomFrameClass;
begin
  inherited Create(TheOwner);

  for i := 0 to Frames.Count - 1 do
  begin
    FrameClass := TCustomFrameClass(Frames.Objects[i]);
    Frame := FrameClass.Create(Self);
    (Frame as ISettingsFrame).SetSettings(@ManagerSettings2);
    Frame.Hide;
    Frame.Align := alClient;
    Frame.Parent := Self;
    SettingsView.Items.AddObject(nil, Frames[i], Pointer(Frame));
  end;
end;

procedure RegisterSettingFrame(const Order: byte;
  const AValue: TCustomFrameClass; const AName: string);
begin
  if not Assigned(Frames) then
    Frames := TStringList.Create;

  if not Supports(AValue, ISettingsFrame) then
    Raise Exception.CreateFmt('Class %s does not support required interface', [AValue.ClassName]);

  if Order >= Frames.Count then
    Frames.AddObject(AName, TObject(AValue))
  else
    Frames.InsertObject(Order, AName, TObject(AValue));
end;

procedure FinalizeFrames;
var
  i: integer;
begin
  for i := 0 to Frames.Count - 1 do
    Frames.Strings[i] := '';
  Frames.Free;
end;

finalization

begin
  FinalizeFrames;
end;

end.

