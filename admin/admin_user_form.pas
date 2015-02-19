unit admin_user_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, CheckBoxThemed, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, StdCtrls, ComboEx, EditBtn, Buttons, ComCtrls, CheckLst,
  epiadmin;

type

  { TAdminUserForm }

  TAdminUserForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    NeverExpireChkBox: TCheckBoxThemed;
    GroupChkLstBox: TCheckListBox;
    PasswordEdit: TEditButton;
    ExpiresDateEdit: TDateEdit;
    Label1: TLabel;
    LoginEdit: TEdit;
    FullnameEdit: TEdit;
    LastLoginEdit: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    PageControl1: TPageControl;
    Panel2: TPanel;
    BasicSheet: TTabSheet;
    GroupsSheet: TTabSheet;
    procedure BitBtn1Click(Sender: TObject);
    procedure CheckBoxThemed1Change(Sender: TObject);
    procedure PasswordEditButtonClick(Sender: TObject);
  private
    FPasswordModified: Boolean;
    FAdminGroups: TEpiGroups;
    FUser: TEpiUser;
    procedure FormShow(Sender: TObject);
    procedure FillGroupList;
  public
    constructor Create(TheOwner: TComponent); override;
    property User: TEpiUser read FUser write FUser;
    property AdminGroups: TEpiGroups read FAdminGroups write FAdminGroups;
  end;

implementation

{$R *.lfm}

{ TAdminUserForm }

procedure TAdminUserForm.CheckBoxThemed1Change(Sender: TObject);
begin
  ExpiresDateEdit.Enabled := not NeverExpireChkBox.Checked;

  if NeverExpireChkBox.Checked then
    ExpiresDateEdit.Date := NullDate
  else
    ExpiresDateEdit.Date := User.ExpireDate;
end;

procedure TAdminUserForm.PasswordEditButtonClick(Sender: TObject);
var
  PW1: String;
  PW2: String;
  Header: String;
begin
  Header := 'Set User Password';
  if not InputQuery(
           Header,
           'Enter New User Password:',
           True,
           PW1)
  then
    Exit;

  if PW1 <> '' then
    PW2 := PasswordBox(Header, 'Re-enter Password:');

  if PW1 <> PW2 then
    MessageDlg(Header, 'The two passwords are not identical!' + LineEnding + 'Password NOT set.', mtError, [mbOK], 0)
  else
    begin
      PasswordEdit.Text := PW1;
      FPasswordModified := true;
    end;
end;

procedure TAdminUserForm.BitBtn1Click(Sender: TObject);
var
  Group: TEpiGroup;
  i: Integer;
begin
  // TODO: Check for login etc...
  User.Login      := LoginEdit.Text;
  User.FullName   := FullnameEdit.Text;
  User.ExpireDate := ExpiresDateEdit.Date;

  if FPasswordModified then
    User.Password   := PasswordEdit.Text;

  for i := 0 to GroupChkLstBox.Count - 1 do
  begin
    Group := TEpiGroup(GroupChkLstBox.Items.Objects[i]);

    if (GroupChkLstBox.Checked[i]) and
       (User.Groups.IndexOf(Group) < 0)
    then
      User.Groups.AddItem(Group);

    if (not GroupChkLstBox.Checked[i]) and
       (User.Groups.IndexOf(Group) >= 0)
    then
      User.Groups.RemoveItem(Group);
  end;
end;

procedure TAdminUserForm.FormShow(Sender: TObject);
begin
  PageControl1.ActivePage := BasicSheet;

  // Fill content!
  LoginEdit.Text       := User.Login;
  FullnameEdit.Text    := User.FullName;

  NeverExpireChkBox.Checked := (User.ExpireDate = 0);
  ExpiresDateEdit.Date := User.ExpireDate;

  PasswordEdit.Text    := User.Password;

  if User.LastLogin > 0 then
    LastLoginEdit.Text := DateTimeToStr(User.LastLogin);

  FillGroupList;
end;

procedure TAdminUserForm.FillGroupList;
var
  Group: TEpiGroup;
begin
  GroupChkLstBox.Items.BeginUpdate;
  GroupChkLstBox.Items.Clear;

  for Group in AdminGroups do
  begin
    GroupChkLstBox.AddItem(GRoup.Caption.Text, GRoup);
    if User.Groups.IndexOf(Group) >= 0 then
      GroupChkLstBox.Checked[GroupChkLstBox.Count - 1] := true;
  end;

  GroupChkLstBox.Items.BeginUpdate;
end;

constructor TAdminUserForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FPasswordModified := false;

  OnShow := @FormShow;
end;

end.

