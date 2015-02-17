unit admin_user_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, CheckBoxThemed, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, StdCtrls, ComboEx, EditBtn, Buttons, ComCtrls, epiadmin;

type

  { TAdminUserForm }

  TAdminUserForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    CheckBoxThemed2: TCheckBoxThemed;
    ExpiresDateEdit: TDateEdit;
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
  private
    FUser: TEpiUser;
    procedure FormShow(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    property User: TEpiUser read FUser write FUser;
  end;

implementation

{$R *.lfm}

{ TAdminUserForm }

procedure TAdminUserForm.CheckBoxThemed1Change(Sender: TObject);
begin
  ExpiresDateEdit.Enabled := not CheckBoxThemed2.Checked;
end;

procedure TAdminUserForm.BitBtn1Click(Sender: TObject);
begin
  // TODO: Check for login etc...
  User.Login      := LoginEdit.Text;
  User.FullName   := FullnameEdit.Text;
  User.ExpireDate := ExpiresDateEdit.Date;
end;

procedure TAdminUserForm.FormShow(Sender: TObject);
begin
  // Fill content!
  LoginEdit.Text := User.Login;
  FullnameEdit.Text := User.FullName;
  ExpiresDateEdit.Date := User.ExpireDate;

  if User.LastLogin > 0 then
    LastLoginEdit.Text := DateToStr(User.LastLogin);
end;

constructor TAdminUserForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  OnShow := @FormShow;
end;

end.

