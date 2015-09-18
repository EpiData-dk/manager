unit admin_user_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, FileUtil, CheckBoxThemed, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, StdCtrls, ComboEx, EditBtn, Buttons, ComCtrls, CheckLst,
  epiadmin, LCLType, VirtualTrees, epistringutils;

type

  { TAdminUserForm }

  TAdminUserForm = class(TForm)
    PasswordEdit: TEdit;
    ExpiresDateEdit: TDateEdit;
    FullnameEdit: TEdit;
    Label1: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    LastLoginEdit: TEdit;
    LoginEdit: TEdit;
    NeverExpireChkBox: TCheckBoxThemed;
    OkBtn: TBitBtn;
    BitBtn2: TBitBtn;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure LoginEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure OkBtnClick(Sender: TObject);
    procedure CheckBoxThemed1Change(Sender: TObject);
    procedure PasswordEditButtonClick(Sender: TObject);
    procedure PasswordEditExit(Sender: TObject);
    procedure PasswordEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FHintWindow: THintWindow;
    procedure ShowHint(Const Ctrl: TControl; Const Msg: String);
    procedure ShowPasswordBoxes(Const KeyData: TString);
  private
    FAdmin: TEpiAdmin;
    FUser: TEpiUser;
    FPasswordModified: Boolean;
    FPasswordReset: boolean;
    procedure FocusUserForm(Data: PtrInt);
    procedure PasswordEditOpen(Data: PtrInt);
    procedure FormShow(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property User: TEpiUser read FUser write FUser;
    property Admin: TEpiAdmin read FAdmin write FAdmin;
    property PasswordReset: boolean read FPasswordReset write FPasswordReset;
  end;

implementation

{$R *.lfm}

uses
  ButtonPanel, LazUTF8, math, admin_authenticator;

var
  InputFormEdit: TEdit;

const
  ADMIN_USERFORM_NODE_KEY = 'ADMIN_USERFORM_NODE_KEY';


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
begin
  ShowPasswordBoxes(nil);
end;

procedure TAdminUserForm.PasswordEditExit(Sender: TObject);
begin
  if PasswordEdit.Modified then
    Application.QueueAsyncCall(@PasswordEditOpen, 0);
end;

procedure TAdminUserForm.PasswordEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (Shift = [])
  then
    begin
      if PasswordEdit.Modified then
        Application.QueueAsyncCall(@PasswordEditOpen, 0)
      else
        PasswordEdit.PerformTab(true);
    end;
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

procedure TAdminUserForm.ShowPasswordBoxes(const KeyData: TString);
var
  PW1: String;
  PW2: String;
  Header: String;

begin
  Header := 'Set User Password';

  PW1 := PasswordEdit.Text;

  if (PW1 <> '') then
    PW2 := PasswordBox(Header, 'Re-enter Password:')
  else
    begin
      FPasswordModified := true;
      PasswordEdit.Modified := false;
      OkBtn.Enabled := false;
      Exit;
    end;

  if PW1 <> PW2 then
    MessageDlg(Header, 'The two passwords are not identical!' + LineEnding + 'Password NOT set.', mtError, [mbOK], 0)
  else
    begin
      PasswordEdit.Text     := PW1;
      PasswordEdit.Modified := false;
      FPasswordModified     := true;
      OkBtn.Enabled         := true;
      Application.QueueAsyncCall(@FocusUserForm, 0);
    end;
end;

procedure TAdminUserForm.FocusUserForm(Data: PtrInt);
begin
  Self.SetFocus;
end;

procedure TAdminUserForm.PasswordEditOpen(Data: PtrInt);
var
  S: TString;
begin
  S := TString(Data);
  ShowPasswordBoxes(S);
  S.Free;
end;

procedure TAdminUserForm.OkBtnClick(Sender: TObject);
var
  Node: PVirtualNode;
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
end;

procedure TAdminUserForm.LoginEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) then
    LoginEdit.Parent.SelectNext(LoginEdit, true, true);
end;

procedure TAdminUserForm.FormShow(Sender: TObject);
begin
  // Fill content!
  LoginEdit.Text       := User.Login;
  LoginEdit.Enabled    := Authenticator.CheckAuthedUserHierachy(User, true) and
                          (not PasswordReset);

  FullnameEdit.Text    := User.FullName;
  FullnameEdit.Enabled := Authenticator.CheckAuthedUserHierachy(User, true) and
                          (not PasswordReset);

  if PasswordReset then
    begin
      PasswordEdit.Enabled := Authenticator.IsAuthorized([earPassword]) and
                              (not Authenticator.UserInGroup(User, Admin.Admins, false));
    end
  else
    PasswordEdit.Enabled := Authenticator.CheckAuthedUserHierachy(User, true);

  NeverExpireChkBox.Checked := (User.ExpireDate = 0);
  NeverExpireChkBox.Enabled := Authenticator.CheckAuthedUserHierachy(User, true) and
                               (not PasswordReset);

  ExpiresDateEdit.Date    := User.ExpireDate;
  ExpiresDateEdit.Enabled := Authenticator.CheckAuthedUserHierachy(User, true) and
                             (not PasswordReset);

  PasswordEdit.Text    := User.Password;
  if (User.Password = '') then
    OkBtn.Enabled := false;

  if User.LastLogin > 0 then
    LastLoginEdit.Text := DateTimeToStr(User.LastLogin)
  else
    LastLoginEdit.Text := '(N/A)';

  if LoginEdit.CanFocus then
    LoginEdit.SetFocus;
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

