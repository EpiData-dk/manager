unit admin_group_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, CheckBoxThemed, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, StdCtrls, EditBtn, Buttons, ComCtrls, CheckLst, epiadmin;

type

  { TAdminGroupForm }

  TAdminGroupForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    ManageRightsChkGrp: TCheckGroup;
    CaptionEdit: TEdit;
    Label3: TLabel;
    PageControl1: TPageControl;
    Panel2: TPanel;
    BasicSheet: TTabSheet;
    procedure BitBtn1Click(Sender: TObject);
  private
    FGroup: TEpiGroup;
    procedure FillRights;
    procedure FormShow(Sender: TObject);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    property Group: TEpiGroup read FGroup write FGroup;
  end;

var
  AdminGroupForm: TAdminGroupForm;

implementation

{$R *.lfm}

uses
  admin_authenticator;

{ TAdminGroupForm }

procedure TAdminGroupForm.BitBtn1Click(Sender: TObject);
var
  i: Integer;
  Item: TEpiManagerRight;
begin
  Group.Caption.Text := CaptionEdit.Text;

  Group.ManageRights := [];
  for i := 0 to ManageRightsChkGrp.Items.Count - 1 do
  begin
    Item := TEpiManagerRight(PtrInt(ManageRightsChkGrp.Items.Objects[I]));
    if ManageRightsChkGrp.Checked[i] then
      Group.ManageRights := Group.ManageRights + [Item];
  end;
end;

procedure TAdminGroupForm.FillRights;
var
  Item: TEpiManagerRight;
  I: Integer;
  R: TEpiGroupRelation;
  P: TEpiGroupRelation;
  ParentGroup: TEpiGroup;
begin
  R := Authenticator.RelationFromGroup(Group);
  P := R.ParentRelation;

  ParentGroup := nil;
  if Assigned(P) then
    ParentGroup := P.Group;

  for I := 0 to ManageRightsChkGrp.Items.Count - 1 do
  begin
    Item := TEpiManagerRight(PtrInt(ManageRightsChkGrp.Items.Objects[I]));

    ManageRightsChkGrp.Checked[I]      := (Item in Group.ManageRights);

                                          // AuthedUserInGroup will return false if ParentGroup = nil,
                                          // hence evaluation after "and" is NOT done.
    ManageRightsChkGrp.CheckEnabled[I] := Authenticator.AuthedUserInGroup(ParentGroup, true) and
                                          (Item in ParentGroup.ManageRights);
  end;
end;

procedure TAdminGroupForm.FormShow(Sender: TObject);
begin
  PageControl1.ActivePage := BasicSheet;

  CaptionEdit.Text := Group.Caption.Text;

  FillRights;

  if CaptionEdit.CanFocus then
    CaptionEdit.SetFocus;
end;

constructor TAdminGroupForm.Create(TheOwner: TComponent);
var
  Item: TEpiManagerRight;
begin
  inherited Create(TheOwner);

  ManageRightsChkGrp.Items.BeginUpdate;
  ManageRightsChkGrp.Items.Clear;

  for Item in TEpiManagerRight do
    ManageRightsChkGrp.Items.AddObject(EpiManagerRightCaptions[Item], TObject(PtrInt(Item)));

  ManageRightsChkGrp.Items.EndUpdate;

  OnShow := @FormShow;
end;

end.

