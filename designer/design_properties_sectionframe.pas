unit design_properties_sectionframe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, StdCtrls, Buttons,
  ExtCtrls, epicustombase, design_types, design_properties_baseframe, design_properties_groupassign_frame;

type


  { TSectionPropertiesFrame }

  TSectionPropertiesFrame = class(TDesignPropertiesFrame, IDesignPropertiesFrame)
    Image1: TImage;
    Label4: TLabel;
    Label9: TLabel;
    NameEdit: TEdit;
    CaptionEdit: TEdit;
    Label2: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    BasicSheet: TTabSheet;
    RightsSheet: TTabSheet;
  private
    { private declarations }
    FSections: TEpiCustomControlItemArray;
    procedure UpdateVisibility;
    procedure UpdateContent;
    procedure DoUpdateCaption;
  private
    { Group Rights }
    FGroupAssignFrame: TGroupsAssignFrame;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    procedure FocusOnNewControl;
    procedure SetEpiControls(EpiControls: TEpiCustomControlItemArray);
    procedure ResetControls;
    function ApplyChanges: boolean;
  end;

implementation

{$R *.lfm}

uses
  epidatafiles, LazUTF8, epistringutils, epiv_datamodule, epiadmin,
  admin_authenticator;

{ TSectionPropertiesFrame }

procedure TSectionPropertiesFrame.UpdateVisibility;
begin
  NameEdit.Enabled :=
    (Length(FSections) = 1) and
    (FSections[0].Name <> 'MAIN') and
    (IsAuthorized(earDefineProject));

  CaptionEdit.Enabled := NameEdit.Enabled;

  RightsSheet.Visible := Authenticator.Admin.Users.Count > 0;
  PageControl1.ShowTabs := RightsSheet.Visible;
  FGroupAssignFrame.Enabled := IsAuthorized(earGroups);
end;

procedure TSectionPropertiesFrame.UpdateContent;
var
  i: Integer;
  S: TEpiSection;
begin
  NameEdit.Text := FSections[0].Name;
  CaptionEdit.Text := TEpiSection(FSections[0]).Caption.Text;
  DM.Icons16.GetBitmap(DM.GetImageIndex(TEpiSection(FSections[0])), Image1.Picture.Bitmap);


  for i := Low(FSections)+1 to High(FSections) do
    if TEpiSection(FSections[i]).Caption.Text <> CaptionEdit.Text then
      begin
        CaptionEdit.Text := '';
        break;
      end;


  if Length(FSections) = 0 then exit;

  S := TEpiSection(FSections[0]);

  FGroupAssignFrame.Admin := Authenticator.Admin;
  FGroupAssignFrame.GroupRights := S.GroupRights;

//  for i := Low(FSections)+1 to High(FSections) do
//    if TEpiSection(FSections[i]).GroupRights.;

end;

procedure TSectionPropertiesFrame.DoUpdateCaption;
var
  S: String;
  i: Integer;
begin
  S := FSections[0].Name;
  for i := 1 to High(FSections) do
    S := S + ', ' + FSections[i].Name;

  S := EpiCutString(S, 20);
  UpdateCaption('Sections Properties: ' + S);
end;

constructor TSectionPropertiesFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  DisableAutoSizing;

  FGroupAssignFrame := TGroupsAssignFrame.Create(Self);
  FGroupAssignFrame.Parent := RightsSheet;
  FGroupAssignFrame.Align := alClient;
  FGroupAssignFrame.BorderSpacing.Around := 10;

  EnableAutoSizing;
end;


procedure TSectionPropertiesFrame.FocusOnNewControl;
begin
  PageControl1.ActivePage := BasicSheet;
  if CaptionEdit.CanFocus then
    CaptionEdit.SetFocus;
end;

procedure TSectionPropertiesFrame.SetEpiControls(
  EpiControls: TEpiCustomControlItemArray);
begin
  FSections := EpiControls;

  if not Assigned(FSections[0]) then exit;

  UpdateVisibility;
  UpdateContent;
  DoUpdateCaption;
end;

procedure TSectionPropertiesFrame.ResetControls;
begin
  UpdateContent;
end;

function TSectionPropertiesFrame.ApplyChanges: boolean;
var
  i: Integer;
begin
  result := false;

  if NameEdit.Modified then
    if not FSections[0].ValidateRename(NameEdit.Text, false)
    then
      begin
        ShowHintMsg('Name already exists or invalid identifier', NameEdit);
        Exit;
      end;

  if CaptionEdit.Modified then
    for i := Low(FSections) to High(FSections) do
      TEpiSection(FSections[i]).Caption.Text := CaptionEdit.Text;

  ShowHintMsg('', nil);
  Result := true;

  DoUpdateCaption;
end;

end.

