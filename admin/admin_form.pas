unit admin_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, Grids, StdCtrls, ComCtrls, ActnList, epiadmin, epicustombase;

type

  { TAdminForm }

  TAdminForm = class(TForm)
    RemoveUserFromGroupAction: TAction;
    AddUserToGroupAction: TAction;
    EditGroupAction: TAction;
    DeleteGroupAction: TAction;
    NewGroupAction: TAction;
    EditUserAction: TAction;
    DeleteUserAction: TAction;
    NewUserAction: TAction;
    ActionList1: TActionList;
    BitBtn1: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    UserGrid: TStringGrid;
    GroupGrid: TStringGrid;
    UserGroupGrid: TStringGrid;
    ToolBar1: TToolBar;
    ToolBar2: TToolBar;
    ToolBar3: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    procedure AddUserToGroupActionExecute(Sender: TObject);
    procedure DeleteGroupActionExecute(Sender: TObject);
    procedure DeleteUserActionExecute(Sender: TObject);
    procedure EditGroupActionExecute(Sender: TObject);
    procedure EditUserActionExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure NewGroupActionExecute(Sender: TObject);
    procedure NewUserActionExecute(Sender: TObject);
    procedure RemoveUserFromGroupActionExecute(Sender: TObject);
  private
    { private declarations }
    FAdmin: TEpiAdmin;
    procedure FillGrids;
    procedure FillUserGrid;
    function ShowUserForm(Const User: TEpiUser): TModalResult;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; AAdmin: TEpiAdmin);
    property Admin: TEpiAdmin read FAdmin;
  end;

implementation

{$R *.lfm}

uses
  admin_user_form, admin_group_form;

{ TAdminForm }

procedure TAdminForm.NewUserActionExecute(Sender: TObject);
var
  User: TEpiUser;
begin
  User := Admin.Users.NewUser;
  if ShowUserForm(User) = mrCancel then
    User.Free
  else
    FillUserGrid;
end;

procedure TAdminForm.RemoveUserFromGroupActionExecute(Sender: TObject);
begin
  //
end;

procedure TAdminForm.FillGrids;
var
  Idx: Integer;
  Group: TEpiGroup;
begin
  // Fill Users:
  FillUserGrid;

  // Fill Groups:
  Idx := 1;

  GroupGrid.RowCount := Admin.Groups.Count + 1;
  for Group in Admin.Groups do
  begin
    GroupGrid.Cells[0, Idx] := Group.Caption.Text;
    GroupGrid.Cells[1, Idx] := '';
  end;
end;

procedure TAdminForm.FillUserGrid;
var
  Idx: Integer;
  User: TEpiUser;
begin
  Idx := 1;

  UserGrid.RowCount := Admin.Users.Count + 1;
  for User in Admin.Users do
  begin
    UserGrid.Cells[0, Idx] := User.Login;
    UserGrid.Cells[1, Idx] := User.FullName;

    if User.ExpireDate > 0 then
      UserGrid.Cells[2, Idx] := DateToStr(User.ExpireDate);

    if User.LastLogin > 0 then
      UserGrid.Cells[3, Idx] := DateToStr(User.LastLogin);

    Inc(Idx);
  end;
end;

function TAdminForm.ShowUserForm(const User: TEpiUser): TModalResult;
var
  F: TAdminUserForm;
begin
  F := TAdminUserForm.Create(Self);
  F.User := User;
  Result := F.ShowModal;
  F.Free;
end;

constructor TAdminForm.Create(TheOwner: TComponent; AAdmin: TEpiAdmin);
begin
  inherited Create(TheOwner);

  FAdmin := AAdmin;
end;

procedure TAdminForm.DeleteUserActionExecute(Sender: TObject);
begin
  //
end;

procedure TAdminForm.EditGroupActionExecute(Sender: TObject);
begin
  //
end;

procedure TAdminForm.DeleteGroupActionExecute(Sender: TObject);
begin
  //
end;

procedure TAdminForm.AddUserToGroupActionExecute(Sender: TObject);
begin
  //
end;

procedure TAdminForm.EditUserActionExecute(Sender: TObject);
var
  User: TEpiUser;
begin
  User := Admin.Users[UserGrid.Row - 1];
  ShowUserForm(User);
  FillGrids;
end;

procedure TAdminForm.FormShow(Sender: TObject);
begin
  FillGrids;
end;

procedure TAdminForm.NewGroupActionExecute(Sender: TObject);
begin
  //
end;

end.

