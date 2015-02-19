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
begin
  Group.Caption.Text := CaptionEdit.Text;

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
begin
  // TODO
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

  FillUserList;
end;

constructor TAdminGroupForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  OnShow := @FormShow;
end;

end.

