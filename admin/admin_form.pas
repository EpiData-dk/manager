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
    procedure UserGridStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure UserGroupGridDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure UserGroupGridDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
  private
    { private declarations }
    FAdmin: TEpiAdmin;
    procedure FillGrids;
    procedure FillUserGrid;
    procedure FillGroupGrid;

  { User methods }
  private
    function UserFromGrid: TEpiUser;
    function ShowUserForm(Const User: TEpiUser): TModalResult;

  { Group methods }
  private
    function GroupFromGrid: TEpiGroup;
    function ShowGroupForm(Const Group: TEpiGroup): TModalResult;

  public
    { public declarations }
    constructor Create(TheOwner: TComponent; AAdmin: TEpiAdmin);
    property Admin: TEpiAdmin read FAdmin;
  end;

implementation

{$R *.lfm}

uses
  admin_user_form, admin_group_form, types;

type

  { TImageDragObject }

  TImageDragObject = class(TDragControlObject)
  private
    FDragImages: TDragImageList;
  protected
    function GetDragImages: TDragImageList; override;
  public
    constructor Create(AControl: TControl); override;
    destructor Destroy; override;
  end;

{ TImageDragObject }

function TImageDragObject.GetDragImages: TDragImageList;
begin
  Result := FDragImages;
end;

constructor TImageDragObject.Create(AControl: TControl);
var
  Bitmap: TBitmap;
begin
  inherited Create(AControl);
  FDragImages := TDragImageList.Create(AControl);
  AlwaysShowDragImages := True;
end;

destructor TImageDragObject.Destroy;
begin
  FDragImages.Free;
  inherited Destroy;
end;

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

procedure TAdminForm.UserGridStartDrag(Sender: TObject;
  var DragObject: TDragObject);
var
  CR: TRect;
  W: Integer;
  H: Integer;
  Bitmap: TBitmap;
  R: TRect;
  T: LongInt;
  B: LongInt;
begin
  DragObject := TImageDragObject.Create(Sender as TControl);
  with TImageDragObject(DragObject) do
  begin
    T := UserGrid.Selection.Top;
    B := UserGrid.Selection.Bottom;

    CR := UserGrid.CellRect(0, T);
    W := CR.Right - CR.Left;

    CR.Bottom := UserGrid.CellRect(0, B).Bottom;
    H := CR.Bottom - CR.Top;

    FDragImages.Width := W;
    FDragImages.Height := H;
    FDragImages.DragHotspot := Point(W, H);

    R := Rect(0, 0, W, H);

    Bitmap := TBitmap.Create;
    Bitmap.Width := W;
    Bitmap.Height := H;
    Bitmap.Canvas.CopyRect(R, UserGrid.Canvas, CR);
    FDragImages.Add(Bitmap, nil);
    Bitmap.Free;

    CR := UserGrid.CellRect(0,2);
    Bitmap := TBitmap.Create;
    Bitmap.Width := W;
    Bitmap.Height := H;
    Bitmap.Canvas.CopyRect(R, UserGrid.Canvas, CR);
    FDragImages.Add(Bitmap, nil);
    Bitmap.Free;
  end;
end;

procedure TAdminForm.UserGroupGridDragDrop(Sender, Source: TObject; X,
  Y: Integer);
begin
  //
end;

procedure TAdminForm.UserGroupGridDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (TImageDragObject(Source).Control = UserGrid);
end;

procedure TAdminForm.FillGrids;
begin
  // Fill Users:
  FillUserGrid;

  // Fill Groups:
  FillGroupGrid;
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

procedure TAdminForm.FillGroupGrid;
var
  Idx: Integer;
  Group: TEpiGroup;
begin
  Idx := 1;

  GroupGrid.RowCount := Admin.Groups.Count + 1;
  for Group in Admin.Groups do
  begin
    GroupGrid.Cells[0, Idx] := Group.Caption.Text;
    GroupGrid.Cells[1, Idx] := '';

    Inc(Idx);
  end;
end;

function TAdminForm.UserFromGrid: TEpiUser;
begin
  Result := nil;
  if UserGrid.RowCount <= 1 then Exit;
  Result := Admin.Users[UserGrid.Row - 1];
end;

function TAdminForm.ShowUserForm(const User: TEpiUser): TModalResult;
var
  F: TAdminUserForm;
begin
  if not Assigned(User) then exit;

  F := TAdminUserForm.Create(Self);
  F.User := User;
  Result := F.ShowModal;
  F.Free;
end;

function TAdminForm.GroupFromGrid: TEpiGroup;
begin
  Result := nil;
  if GroupGrid.RowCount <= 1 then Exit;
  Result := Admin.Groups[GroupGrid.Row - 1];
end;

function TAdminForm.ShowGroupForm(const Group: TEpiGroup): TModalResult;
var
  F: TAdminGroupForm;
begin
  if not Assigned(Group) then exit;

  F := TAdminGroupForm.Create(Self);
  F.Group := Group;
  Result := F.ShowModal;
  F.Free;
end;

constructor TAdminForm.Create(TheOwner: TComponent; AAdmin: TEpiAdmin);
begin
  inherited Create(TheOwner);

  FAdmin := AAdmin;
end;

procedure TAdminForm.DeleteUserActionExecute(Sender: TObject);
var
  User: TEpiUser;
begin
  User := UserFromGrid;

  if not Assigned(User) then exit;

  if MessageDlg('Warning',
       'Are you sure you want to delete the user:' + LineEnding +
         User.FullName + ' (' + User.Login + ')',
       mtWarning,
       mbYesNo,
       0,
       mbNo
     ) = mrYes
  then
    begin
      User.Free;
      FillUserGrid;
    end;
end;

procedure TAdminForm.EditGroupActionExecute(Sender: TObject);
begin
  if ShowGroupForm(GroupFromGrid) = mrCancel then
    Exit;

  FillGroupGrid;
end;

procedure TAdminForm.DeleteGroupActionExecute(Sender: TObject);
var
  Group: TEpiGroup;
begin
  Group := GroupFromGrid;
  if not Assigned(Group) then exit;

  if MessageDlg('Warning',
       'Are you sure you want to delete the group:' + LineEnding +
         Group.Caption.Text,
       mtWarning,
       mbYesNo,
       0,
       mbNo
     ) = mrYes
  then
    begin
      Group.Free;
      FillGroupGrid;
    end;
end;

procedure TAdminForm.AddUserToGroupActionExecute(Sender: TObject);
begin
  //
end;

procedure TAdminForm.EditUserActionExecute(Sender: TObject);
begin
  if ShowUserForm(UserFromGrid) = mrCancel then
    Exit;

  FillGrids;
end;

procedure TAdminForm.FormShow(Sender: TObject);
begin
  FillGrids;
end;

procedure TAdminForm.NewGroupActionExecute(Sender: TObject);
var
  Group: TEpiGroup;
begin
  Group := Admin.Groups.NewGroup;
  if ShowGroupForm(Group) = mrCancel then
    Group.Free
  else
    FillGroupGrid;
end;

end.

