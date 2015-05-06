unit admin_user_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, FileUtil, CheckBoxThemed, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, StdCtrls, ComboEx, EditBtn, Buttons, ComCtrls, CheckLst,
  epiadmin;

type

  { TAdminUserForm }

  TAdminUserForm = class(TForm)
    OkBtn: TBitBtn;
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
    procedure LoginEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure OkBtnClick(Sender: TObject);
    procedure CheckBoxThemed1Change(Sender: TObject);
    procedure PasswordEditButtonClick(Sender: TObject);
    procedure PasswordEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FHintWindow: THintWindow;
    procedure ShowHint(Const Ctrl: TControl; Const Msg: String);
  private
    FPasswordModified: Boolean;
    FAdminGroups: TEpiGroups;
    FUser: TEpiUser;
    procedure PasswordEditOpen(Data: PtrInt);
    procedure FormShow(Sender: TObject);
    procedure FillGroupList;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property User: TEpiUser read FUser write FUser;
    property AdminGroups: TEpiGroups read FAdminGroups write FAdminGroups;
  end;

implementation

{$R *.lfm}

uses
  LCLType;

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
      OkBtn.Enabled     := true;
    end;
end;

procedure TAdminUserForm.PasswordEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) then
    Application.QueueAsyncCall(@PasswordEditOpen, 0);

  if (Key = VK_TAB) and (Shift = [ssShift]) then
    Key := VK_TAB;
end;

procedure TAdminUserForm.ShowHint(const Ctrl: TControl; const Msg: String);
var
  R: types.TRect;
  P: TPoint;
begin
  if (Msg = '') and (Ctrl = nil) then
  begin
    FHintWindow.Hide;
    Exit;
  end;

  R := FHintWindow.CalcHintRect(0, Msg, nil);
  P := Ctrl.ClientToScreen(Point(0, Ctrl.Height + 2));
  OffsetRect(R, P.X, P.Y);
  FHintWindow.ActivateHint(R, Msg);
end;

procedure TAdminUserForm.PasswordEditOpen(Data: PtrInt);
begin
  PasswordEditButtonClick(nil);
end;

procedure TAdminUserForm.OkBtnClick(Sender: TObject);
var
  Group: TEpiGroup;
  i: Integer;
begin
  if (not User.ValidateRename(LoginEdit.Text, false))
  then
    begin
      ShowHint(
        LoginEdit,
        'Login invalid or already exists!' + LineEnding +
        'The login must consist of the characters: A-Z, a-z and/or 0-9'
      );
      ModalResult := mrNone;
      Exit;
    end;

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

procedure TAdminUserForm.LoginEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) then
    LoginEdit.Parent.SelectNext(LoginEdit, true, true);
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
  if (User.Password = '') then
    OkBtn.Enabled := false;

  if User.LastLogin > 0 then
    LastLoginEdit.Text := DateTimeToStr(User.LastLogin)
  else
    LastLoginEdit.Text := '(N/A)';

  FillGroupList;
  LoginEdit.SetFocus;
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

  FHintWindow       := THintWindow.Create(Self);
  FHintWindow.AutoHide     := true;
  FHintWindow.HideInterval := 2500;  //2.5 secs.

  OnShow := @FormShow;
end;

destructor TAdminUserForm.Destroy;
begin
  FHintWindow.Free;
  inherited Destroy;
end;

end.

