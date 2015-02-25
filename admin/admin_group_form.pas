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
    UserChkLstBox: TCheckListBox;
    ManageRightsChkGrp: TCheckGroup;
    CaptionEdit: TEdit;
    Label3: TLabel;
    PageControl1: TPageControl;
    Panel2: TPanel;
    BasicSheet: TTabSheet;
    UsersSheet: TTabSheet;
    procedure BitBtn1Click(Sender: TObject);
  private
    FAdminUsers: TEpiUsers;
    FGroup: TEpiGroup;
    procedure FillRights;
    procedure FillUserList;
    procedure FormShow(Sender: TObject);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    property Group: TEpiGroup read FGroup write FGroup;
    property AdminUsers: TEpiUsers read FAdminUsers write FAdminUsers;
  end;

var
  AdminGroupForm: TAdminGroupForm;

implementation

{$R *.lfm}

{ TAdminGroupForm }

procedure TAdminGroupForm.BitBtn1Click(Sender: TObject);
var
  User: TEpiUser;
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

  for i := 0 to UserChkLstBox.Count - 1 do
  begin
    User := TEpiUser(UserChkLstBox.Items.Objects[i]);

    if (UserChkLstBox.Checked[i]) and
       (User.Groups.IndexOf(Group) < 0)
    then
      User.Groups.AddItem(Group);

    if (not UserChkLstBox.Checked[i]) and
       (User.Groups.IndexOf(Group) >= 0)
    then
      User.Groups.RemoveItem(Group);
  end;
end;

procedure TAdminGroupForm.FillRights;
var
  Item: TEpiManagerRight;
  I: Integer;
begin
  for I := 0 to ManageRightsChkGrp.Items.Count - 1 do
  begin
    Item := TEpiManagerRight(PtrInt(ManageRightsChkGrp.Items.Objects[I]));
    ManageRightsChkGrp.Checked[I] := (Item in Group.ManageRights);
  end;
end;

procedure TAdminGroupForm.FillUserList;
var
  User: TEpiUser;
begin
  UserChkLstBox.Items.BeginUpdate;
  UserChkLstBox.Items.Clear;

  for User in AdminUsers do
  begin
    UserChkLstBox.AddItem(User.FullName + ' (' + User.Login + ')', User);
    if User.Groups.IndexOf(Group) >= 0 then
      UserChkLstBox.Checked[UserChkLstBox.Count - 1] := true;
  end;

  UserChkLstBox.Items.BeginUpdate;
end;

procedure TAdminGroupForm.FormShow(Sender: TObject);
begin
  PageControl1.ActivePage := BasicSheet;

  CaptionEdit.Text := Group.Caption.Text;

  FillRights;
  FillUserList;
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

