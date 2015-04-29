unit admin_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, Grids, StdCtrls, ComCtrls, ActnList, epiadmin, epicustombase,
  VirtualTrees;

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
    procedure EditUserActionExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GroupGridSelection(Sender: TObject; aCol, aRow: Integer);
    procedure NewGroupActionExecute(Sender: TObject);
    procedure NewUserActionExecute(Sender: TObject);
    procedure RemoveUserFromGroupActionExecute(Sender: TObject);
    procedure UserGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure UserGridMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure UserGridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure UserGridStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure UserGroupGridDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure UserGroupGridDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
  private
    FAdmin: TEpiAdmin;
    procedure FillGrids;
    procedure FillUserGrid;
    procedure FillGroupGrid;
    procedure FillUsersToGroupGrid;
    procedure UpdateShortcuts;

  { User methods }
  private
    FMouseDown: Boolean;
    FDragging: Boolean;
    FStartPos: TPoint;
    FSelection: TGridRect;
    function UserFromGrid: TEpiUser;
    function ShowUserForm(Const User: TEpiUser): TModalResult;

  { Group vars/methods }
  private
    FGroupVST: TVirtualStringTree;
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
  admin_user_form, admin_group_form, types, shortcuts, epiv_datamodule;

type

  { TUsersDragObject }

  TUsersDragObject = class(TDragControlObject)
  private
    FDragImages: TDragImageList;
    FUsers: TEpiUsers;
  protected
    function GetDragImages: TDragImageList; override;
  public
    constructor Create(AControl: TControl); override;
    destructor Destroy; override;
  end;

{ TImageDragObject }

function TUsersDragObject.GetDragImages: TDragImageList;
begin
  Result := FDragImages;
end;

constructor TUsersDragObject.Create(AControl: TControl);
var
  Bitmap: TBitmap;
begin
  inherited Create(AControl);
  FDragImages := TDragImageList.Create(AControl);
  FUsers      := TEpiUsers.Create(nil);
  FUsers.ItemOwner := false;

  AlwaysShowDragImages := True;
end;

destructor TUsersDragObject.Destroy;
begin
  FUsers.Free;
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
var
  T: LongInt;
  B: LongInt;
  Group: TEpiGroup;
  User: TEpiUser;
  I: LongInt;
begin
  T := UserGroupGrid.Selection.Top;
  B := UserGroupGrid.Selection.Bottom;

  if (T = 0) and (B = 0) then exit;

  Group := GroupFromGrid;
  if not Assigned(Group) then exit;

  for I := T to B do
  begin
    User := TEpiUser(UserGroupGrid.Objects[0, I]);
    User.Groups.RemoveItem(Group);
  end;
  FillUsersToGroupGrid;
end;

procedure TAdminForm.UserGridMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ACol: Longint;
  ARow: Longint;
begin
  FMouseDown := true;
  FDragging := false;
  FStartPos := Point(X, Y);

  UserGrid.MouseToCell(X, Y, ACol, ARow);
  FSelection := UserGrid.Selection;
  if (ARow < FSelection.Top) or
     (ARow > FSelection.Bottom)
  then
  begin
    // The click was outside the selection - hence just the one cell need to be dragged.
    FSelection.Top    := ARow;
    FSelection.Bottom := ARow;
  end;
end;

procedure TAdminForm.UserGridMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if FDragging then exit;
  if not FMouseDown then exit;
  if (Shift <> [ssLeft]) then exit;

  if (Abs(FStartPos.X - X) > 5) or
     (Abs(FStartPos.Y - Y) > 5)
  then
    begin
      UserGrid.BeginDrag(true, -1);
      FDragging := true;
    end;
end;

procedure TAdminForm.UserGridMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FMouseDown := false;
  FDragging := false;
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
  i: LongInt;
begin
  T := FSelection.Top;
  B := FSelection.Bottom;
  if (T = 0) and (B = 0) then Exit;

  DragObject := TUsersDragObject.Create(Sender as TControl);
  with TUsersDragObject(DragObject) do
  begin
    for i := T to B do
      FUsers.AddItem(Admin.Users[i - 1]);

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
  end;
end;

procedure TAdminForm.UserGroupGridDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  Udo: TUsersDragObject;
  Group: TEpiGroup;
  User: TEpiUser;
begin
  Group := GroupFromGrid;
  if not Assigned(Group) then exit;

  Udo := TUsersDragObject(Source);
  for User in Udo.FUsers do
    if User.Groups.IndexOf(Group) < 0 then
      User.Groups.AddItem(Group);

  FillUsersToGroupGrid;
end;

procedure TAdminForm.UserGroupGridDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept :=
    (Source is TUsersDragObject) and
    (TUsersDragObject(Source).Control = UserGrid) and
    (Assigned(GroupFromGrid))
end;

procedure TAdminForm.FillGrids;
begin
  FillUserGrid;
  FillGroupGrid;
  FillUsersToGroupGrid;
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
      UserGrid.Cells[2, Idx] := DateToStr(User.ExpireDate)
    else
      UserGrid.Cells[2, Idx] := '(never)';

    if User.LastLogin > 0 then
      UserGrid.Cells[3, Idx] := DateTimeToStr(User.LastLogin)
    else
      UserGrid.Cells[3, Idx] := '(N/A)';

    Inc(Idx);
  end;
end;

procedure TAdminForm.FillGroupGrid;
var
  Idx: Integer;
  Group: TEpiGroup;

begin
  FGroupVST.BeginUpdate;
  FGroupVST.Clear;

  FGroupVST.AddChild(nil, Admin.AdminRelation.Group);


  FGroupVST.EndUpdate;

{  Idx := 1;

  GroupGrid.RowCount := Admin.Groups.Count + 1;
  for Group in Admin.Groups do
  begin
    GroupGrid.Cells[0, Idx] := Group.Caption.Text;
    GroupGrid.Cells[1, Idx] := '';

    Inc(Idx);
  end; }
end;

procedure TAdminForm.FillUsersToGroupGrid;
var
  Group: TEpiGroup;
  User: TEpiUser;
  LocalUsers: TEpiUsers;
  Idx: Integer;
begin
  Group := GroupFromGrid;
  if not Assigned(Group) then Exit;

  LocalUsers := TEpiUsers.Create(nil);
  LocalUsers.ItemOwner := false;

  for User in Admin.Users do
    if User.Groups.IndexOf(Group) >= 0 then
      LocalUsers.AddItem(User);

  Idx := 1;
  UserGroupGrid.RowCount := LocalUsers.Count + 1;
  for User In LocalUsers do
  begin
    UserGroupGrid.Cells[0, Idx] := User.Login;
    UserGroupGrid.Cells[1, Idx] := User.FullName;
    UserGroupGrid.Objects[0, Idx] := User;
    Inc(Idx);
  end;
  LocalUsers.Free;
end;

procedure TAdminForm.UpdateShortcuts;
begin
  NewGroupAction.ShortCut            := A_NewGroup;
  NewUserAction.ShortCut             := A_NewUser;
  DeleteGroupAction.ShortCut         := A_DeleteGroup;
  DeleteUserAction.ShortCut          := A_DeleteUser;
  AddUserToGroupAction.ShortCut      := A_AddUserToGroup;
  RemoveUserFromGroupAction.ShortCut := A_RemoveUserFromGroup;
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
  F.AdminGroups := Admin.Groups;
  Result := F.ShowModal;
  F.Free;
end;

function TAdminForm.GroupFromGrid: TEpiGroup;
begin
{  Result := nil;
  if GroupGrid.RowCount <= 1 then Exit;
  Result := Admin.Groups[GroupGrid.Row - 1];         }
end;

function TAdminForm.ShowGroupForm(const Group: TEpiGroup): TModalResult;
var
  F: TAdminGroupForm;
begin
  if not Assigned(Group) then exit;

  F := TAdminGroupForm.Create(Self);
  F.Group := Group;
  F.AdminUsers := Admin.Users;
  Result := F.ShowModal;
  F.Free;
end;

constructor TAdminForm.Create(TheOwner: TComponent; AAdmin: TEpiAdmin);
begin
  inherited Create(TheOwner);

  FAdmin := AAdmin;

  FGroupVST := TVirtualStringTree.Create(Self);
  with FGroupVST do
  begin
    BeginUpdate;

    with TreeOptions do
    begin
      AnimationOptions := [];
      AutoOptions      := [];
      MiscOptions      := [toFullRepaintOnResize, toGridExtensions,
                           toToggleOnDblClick, toWheelPanning];
      PaintOptions     := [toShowButtons, toShowDropmark, toShowRoot,
                           toShowTreeLines, toThemeAware, toUseBlendedImages];
      SelectionOptions := [toExtendedFocus, toFullRowSelect];
      StringOptions    := [];
    end;

    with Header do
    begin
      Options := [hoAutoResize, hoColumnResize, hoDblClickResize, hoVisible,
                  hoFullRepaintOnResize];

      with Columns.Add do
      begin
        Text := 'Caption';
        CheckBox   := false;
        CheckState := csUncheckedNormal;
        CheckType  := ctCheckBox;
        Options    := [coAllowClick, coEnabled, coParentBidiMode,
                       coParentColor, coResizable, coShowDropMark, coVisible,
                       coSmartResize, coAllowFocus];
        Width      := 150;
      end;

      with Columns.Add do
      begin
        Text := 'Rights';
        CheckBox   := false;
        CheckState := csUncheckedNormal;
        CheckType  := ctCheckBox;
        Options    := [coAllowClick, coEnabled, coParentBidiMode,
                       coParentColor, coResizable, coShowDropMark, coVisible,
                       coSmartResize, coAllowFocus];
      end;

      MainColumn := 0;
      AutoSizeIndex := 1;
    end;

    Align := alClient;
    Parent := Panel2;

    EndUpdate;
  end;
  FGroupVST.Images := DM.Icons16;
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
var
  T: LongInt;
  B: LongInt;
  User: TEpiUser;
  Group: TEpiGroup;
  i: LongInt;
begin
  T := UserGrid.Selection.Top;
  B := UserGrid.Selection.Bottom;

  if (T = 0) or (B = 0) then exit;
  Group := GroupFromGrid;
  if not Assigned(Group) then exit;

  for i := T to B do
  begin
    User := Admin.Users[i-1];
    if User.Groups.IndexOf(Group) < 0 then
      User.Groups.AddItem(Group);
  end;
  FillUsersToGroupGrid;
end;

procedure TAdminForm.EditUserActionExecute(Sender: TObject);
begin
  if ShowUserForm(UserFromGrid) = mrCancel then
    Exit;

  FillGrids;
end;

procedure TAdminForm.FormShow(Sender: TObject);
begin
  UpdateShortcuts;
  FillGrids;
end;

procedure TAdminForm.GroupGridSelection(Sender: TObject; aCol, aRow: Integer);
begin
  FillUsersToGroupGrid;
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

