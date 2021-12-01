unit admin_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, Grids, StdCtrls, ComCtrls, ActnList, CheckLst, epiadmin,
  epicustombase, VirtualTrees;

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
    procedure DeleteGroupActionUpdate(Sender: TObject);
    procedure DeleteUserActionExecute(Sender: TObject);
    procedure DeleteUserActionUpdate(Sender: TObject);
    procedure EditGroupActionExecute(Sender: TObject);
    procedure EditUserActionExecute(Sender: TObject);
    procedure EditUserActionUpdate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure NewGroupActionExecute(Sender: TObject);
    procedure NewGroupActionUpdate(Sender: TObject);
    procedure NewUserActionExecute(Sender: TObject);
    procedure RemoveUserFromGroupActionExecute(Sender: TObject);
    procedure UserGridPrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
  private
    FAdmin: TEpiAdmin;
    procedure FillGrids;
    procedure FillUserGrid;
    procedure FillGroupGrid;
    procedure FillUsersInGroupGrid;
    procedure UpdateShortcuts;

  { User methods }
  private
    FSelection: TGridRect;
    function UserFromGrid: TEpiUser;
    function ShowUserForm(Const User: TEpiUser): TModalResult;

  { Group vars/methods }
  private
    FUpdatingGroupVST: boolean;
    FGroupVST: TVirtualStringTree;
    function GroupFromNode(Const Node: PVirtualNode): TEpiGroup;
    function GroupFromSelectedNode: TEpiGroup;
    function RelationFromNode(Const Node: PVirtualNode): TEpiGroupRelation;
    function RelationFromSelectedNode: TEpiGroupRelation;
    function NodeFromRelation(Const Relation: TEpiGroupRelation): PVirtualNode;
    procedure FocusAndSelectNode(Const Node: PVirtualNode);

    function ShowGroupForm(Const Group: TEpiGroup): TModalResult;
    procedure GetGroupGridText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure GroupBeforeItemErase(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect;
      var ItemColor: TColor; var EraseAction: TItemEraseAction);
    procedure GroupDblClick(Sender: TObject);
    procedure GroupFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure GroupFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);

  { Users in Group vars/methods }
  private
    FUpdatingUserInGroupVST: boolean;
    FUserInGroupVST: TVirtualStringTree;
    function UsersInGroup_UserFromNode(Const Node: PVirtualNode): TEpiUser;
    procedure UsersInGroupRemoveUserFromChildGroups(
        Const Relation: TEpiGroupRelation;
        Const Depth: Cardinal;
        Const Index: Cardinal;
        var aContinue: boolean;
        Data: Pointer = nil
      );
    procedure UsersInGroupBeforeItemErase(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect;
      var ItemColor: TColor; var EraseAction: TItemEraseAction);
    procedure UsersInGroupChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure UsersInGroupChecking(Sender: TBaseVirtualTree;
      Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
    procedure UsersInGroupGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure UsersInGroupInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);

  public
    { public declarations }
    constructor Create(TheOwner: TComponent; AAdmin: TEpiAdmin);
    property Admin: TEpiAdmin read FAdmin;
  end;

implementation

{$R *.lfm}

uses
  admin_user_form, admin_group_form, types, shortcuts, epiv_datamodule,
  admin_authenticator, epigrouprelation_helper;

const
  ADMIN_FORM_NODE_KEY = 'ADMIN_FORM_NODE_KEY';

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
  UserList: TEpiUsers;
begin
{  T := UserGroupGrid.Selection.Top;
  B := UserGroupGrid.Selection.Bottom;

  if (T = 0) and (B = 0) then exit;

  Group := GroupFromSelectedNode;
  if not Assigned(Group) then exit;


  // Build the list of selected users:
  UserList := TEpiUsers.Create(nil);
  for I := T to B do
  begin
    User := TEpiUser(UserGroupGrid.Objects[0, I]);
    if Authenticator.UserInGroup(User, Group, false)
    then
      UserList.AddItem(User);
  end;

  // You cannot delete the last member of Admin group, unless this is the last
  // person in the whole project.
  if (Group = Admin.Admins) and
     (UserList.Count = Group.Users.Count)
  then
    begin
      ShowMessage(
        'You cannot remove all members of the Admin group!' + LineEnding +
        'At least 1 person must exist in this group!'
      );
      Exit;
    end;

  // You cannot remove yourself from the group, if this is highest group
  // in the hieracy you have access to.
  if (Group <> Admin.Admins) and
     (UserList.IndexOf(Authenticator.AuthedUser) >= 0) and
     (Authenticator.AuthedUserInGroup(Group, false)) and
     (not
       Authenticator.AuthedUserInGroup(
         Authenticator.RelationFromGroup(Group).ParentRelation.Group,
         true
       )
     )
  then
    begin
      ShowMessage(
        'You cannot remove youself from this group!'
      );
      Exit;
    end;

  for User in UserList do
    User.Groups.RemoveItem(Group);

  UserList.Free;
  FillUsersInGroupGrid;   }
end;

procedure TAdminForm.UserGridPrepareCanvas(sender: TObject; aCol,
  aRow: Integer; aState: TGridDrawState);
var
  User: TEpiUser;
begin
  if aRow = 0 then exit;
  User := TEpiUser(UserGrid.Objects[0, aRow]);

  if (not Authenticator.CheckAuthedUserHierachy(User, false))
  then
    UserGrid.Canvas.Font.Color := clInactiveCaption;

  if (User = Authenticator.AuthedUser)
  then
    UserGrid.Canvas.Brush.Color := clGradientActiveCaption;
end;

{procedure TAdminForm.UserGroupGridPrepareCanvas(sender: TObject; aCol,
  aRow: Integer; aState: TGridDrawState);
begin
  User := TEpiUser(UserGroupGrid.Objects[0, aRow]);
  Group := GroupFromSelectedNode;

  if (not Authenticator.UserInGroup(User, Group, false)) then
    UserGroupGrid.Canvas.Font.Color := clInactiveCaption;
end;

procedure TAdminForm.UserGroupGridSelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
var
  User: TEpiUser;
  Group: TEpiGroup;
begin
  User := TEpiUser(UserGroupGrid.Objects[0, aRow]);
  Group := GroupFromSelectedNode;

  CanSelect := Authenticator.UserInGroup(User, Group, false);
end;                                                              }

procedure TAdminForm.FillGrids;
begin
  FillUserGrid;
  FillGroupGrid;
  FillUsersInGroupGrid;
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
    UserGrid.Objects[0, Idx] := User;
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

  procedure AddRecusive(Root: PVirtualNode; Const Relation: TEpiGroupRelation);
  var
    R: TEpiGroupRelation;
  begin
    Root := FGroupVST.AddChild(Root, Relation);
    Relation.AddCustomData(ADMIN_FORM_NODE_KEY, TObject(Root));

    for R in Relation.GroupRelations do
      AddRecusive(Root, R);
  end;

begin
  FUpdatingGroupVST := true;

  FGroupVST.BeginUpdate;
  FGroupVST.Clear;

  AddRecusive(nil, Admin.AdminRelation);

  FGroupVST.FullExpand();
  FGroupVST.EndUpdate;

  FUpdatingGroupVST := false;
end;

procedure TAdminForm.FillUsersInGroupGrid;
const
  CaptionText = 'Users in group: ';
var
  G: TEpiGroup;
begin
  FUpdatingUserInGroupVST := true;

  FUserInGroupVST.RootNodeCount := Admin.Users.Count;
  FUserInGroupVST.ReinitChildren(nil, true);

  G := GroupFromSelectedNode;
  if Assigned(G) then
    Label2.Caption := CaptionText + G.Caption.Text;

  FUpdatingUserInGroupVST := false;
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

  Result := TEpiUser(UserGrid.Objects[0, UserGrid.Row]);
end;

function TAdminForm.ShowUserForm(const User: TEpiUser): TModalResult;
var
  F: TAdminUserForm;
begin
  if not Assigned(User) then exit;

  F := TAdminUserForm.Create(Self);
  F.User  := User;
  F.Admin := Admin;
  Result := F.ShowModal;
  F.Free;

  if Result = mrOK then
    FillUsersInGroupGrid;
end;

function TAdminForm.GroupFromNode(const Node: PVirtualNode): TEpiGroup;
var
  R: TEpiGroupRelation;
begin
  Result := nil;

  R := RelationFromNode(Node);
  if Assigned(R) then
    Result := R.Group;
end;

function TAdminForm.GroupFromSelectedNode: TEpiGroup;
var
  R: TEpiGroupRelation;
begin
  Result := nil;

  R := RelationFromSelectedNode;
  if Assigned(R) then
    Result := R.Group;
end;

function TAdminForm.RelationFromNode(const Node: PVirtualNode
  ): TEpiGroupRelation;
begin
  result := nil;

  if Assigned(Node) then
    Result := TEpiGroupRelation(FGroupVST.GetNodeData(Node)^);
end;

function TAdminForm.RelationFromSelectedNode: TEpiGroupRelation;
begin
  Result := nil;

  if Assigned(FGroupVST.FocusedNode) then
    result := RelationFromNode(FGroupVST.FocusedNode);
end;

function TAdminForm.NodeFromRelation(const Relation: TEpiGroupRelation
  ): PVirtualNode;
begin
  result := PVirtualNode(Relation.FindCustomData(ADMIN_FORM_NODE_KEY));
end;

procedure TAdminForm.FocusAndSelectNode(const Node: PVirtualNode);
begin
  FGroupVST.FocusedNode := Node;
  FGroupVST.Selected[Node] := true;
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

procedure TAdminForm.GetGroupGridText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
begin
  case Column of
    0: CellText := GroupFromNode(Node).Caption.Text;
    1: CellText := Authenticator.PrintGroupRights(GroupFromNode(Node), true);
  end;
end;

procedure TAdminForm.GroupBeforeItemErase(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect;
  var ItemColor: TColor; var EraseAction: TItemEraseAction);
var
  G: TEpiGroup;
begin
  G := GroupFromNode(Node);

  if (G = Admin.Admins) or
     (not Authenticator.AuthedUserInGroup(G, true))
  then
    begin
      ItemColor := clLtGray;
      EraseAction := eaColor;
    end;
end;

procedure TAdminForm.GroupDblClick(Sender: TObject);
begin
  ShowGroupForm(GroupFromSelectedNode);
end;

procedure TAdminForm.GroupFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  FillUsersInGroupGrid;
  FUserInGroupVST.InvalidateChildren(nil, true);
end;

procedure TAdminForm.GroupFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  G: TEpiGroup;
begin
  if (FUpdatingGroupVST) or
     (csDestroying in ComponentState)
  then
    Exit;

  if not Assigned(Node) then exit;

  G := GroupFromNode(Node);
  G.Free;
end;

function TAdminForm.UsersInGroup_UserFromNode(const Node: PVirtualNode
  ): TEpiUser;
begin
  result := TEpiUser(FUserInGroupVST.GetNodeData(Node)^);
end;

procedure TAdminForm.UsersInGroupRemoveUserFromChildGroups(
  const Relation: TEpiGroupRelation; const Depth: Cardinal;
  const Index: Cardinal; var aContinue: boolean; Data: Pointer);
var
  User: TEpiUser;
begin
  User := TEpiUser(Data);

  if (User.Groups.IndexOf(Relation.Group) >= 0) then
    User.Groups.RemoveItem(Relation.Group);
end;

procedure TAdminForm.UsersInGroupBeforeItemErase(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect;
  var ItemColor: TColor; var EraseAction: TItemEraseAction);
var
  User: TEpiUser;
  Group: TEpiGroup;
  Allowed: Boolean;
begin
  User := UsersInGroup_UserFromNode(Node);
  Group := GroupFromSelectedNode;

  Allowed :=
     (User <> Authenticator.AuthedUser) and
     (Authenticator.AuthedUserInGroup(Group, true));

  if (Sender.CheckState[Node] = csMixedNormal) or
     (not Allowed)
  then
    begin
      ItemColor := clLtGray;
      EraseAction := eaColor;
      Exit;
    end;
end;

procedure TAdminForm.UsersInGroupChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  User: TEpiUser;
  Group: TEpiGroup;
begin
  if FUpdatingUserInGroupVST then exit;

  User := UsersInGroup_UserFromNode(Node);
  Group := GroupFromSelectedNode;

  if Sender.CheckState[Node] = csCheckedNormal then
  begin
    if Authenticator.UserInGroup(User, Group, true, false) then
      Authenticator.RelationFromGroup(Group).GroupRelations.OrderedWalk(
        @UsersInGroupRemoveUserFromChildGroups,
        User
      );

    User.Groups.AddItem(Group)
  end
  else
    User.Groups.RemoveItem(Group);
end;

procedure TAdminForm.UsersInGroupChecking(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
var
  User: TEpiUser;
  Group: TEpiGroup;
begin
  if FUpdatingUserInGroupVST then
    begin
      Allowed := true;
      Exit;
    end;

  // Cannot remove inherited users
  if (Sender.CheckState[Node] = csMixedNormal) then
    begin
      Allowed := false;
      Exit;
    end;

  User := UsersInGroup_UserFromNode(Node);
  Group := GroupFromSelectedNode;

  Allowed :=
     (User <> Authenticator.AuthedUser) and
     (Authenticator.AuthedUserInGroup(Group, true));
end;

procedure TAdminForm.UsersInGroupGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
begin
  case Column of
    0: CellText := UsersInGroup_UserFromNode(Node).Login;
    1: CellText := UsersInGroup_UserFromNode(Node).FullName;
  end;
end;

procedure TAdminForm.UsersInGroupInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  User: TEpiUser;
  Group: TEpiGroup;
begin
  InitialStates := [];

  User := Admin.Users[Node^.Index];
  Group := GroupFromSelectedNode;

  Pointer(Sender.GetNodeData(Node)^) := User;
  Sender.CheckType[Node] := ctCheckBox;

  if Authenticator.UserInGroup(User, Group, false) then
    Sender.CheckState[Node] := csCheckedNormal
  else
  if Authenticator.UserInGroup(User, Group, True) then
    Sender.CheckState[Node] := csMixedNormal
  else
    Sender.CheckState[Node] := csUncheckedNormal;
end;

constructor TAdminForm.Create(TheOwner: TComponent; AAdmin: TEpiAdmin);
begin
  inherited Create(TheOwner);

  FAdmin := AAdmin;

  FGroupVST := TVirtualStringTree.Create(Self);
  with FGroupVST do
  begin
    BeginUpdate;

    NodeDataSize := SizeOf(Pointer);

    with TreeOptions do
    begin
      AnimationOptions := [];
      AutoOptions      := [];
      MiscOptions      := [toFullRepaintOnResize, toGridExtensions,
                           toWheelPanning];
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

    OnBeforeItemErase := @GroupBeforeItemErase;
    OnDblClick     := @GroupDblClick;
    OnFocusChanged := @GroupFocusChanged;
    OnFreeNode     := @GroupFreeNode;
    OnGetText      := @GetGroupGridText;

    EndUpdate;
  end;

  FUserInGroupVST := TVirtualStringTree.Create(Self);
  with FUserInGroupVST do
  begin
    BeginUpdate;

    NodeDataSize := SizeOf(Pointer);

    with TreeOptions do
    begin
      AnimationOptions := [];
      AutoOptions      := [];
      MiscOptions      := [toFullRepaintOnResize, toGridExtensions, toCheckSupport,
                           toWheelPanning];
      PaintOptions     := [toShowButtons, toShowVertGridLines, toFullVertGridLines,
                           toShowHorzGridLines,
                           toThemeAware, toUseBlendedImages];
      SelectionOptions := [toExtendedFocus, toFullRowSelect];
      StringOptions    := [];
    end;

    with Header do
    begin
      Options := [hoAutoResize, hoColumnResize, hoDblClickResize, hoVisible,
                  hoFullRepaintOnResize];

      with Columns.Add do
      begin
        Text := 'Login';
        CheckBox   := true;
        CheckState := csUncheckedNormal;
        CheckType  := ctCheckBox;
        Options    := [coAllowClick, coEnabled, coParentBidiMode,
                       coParentColor, coResizable, coShowDropMark, coVisible,
                       coSmartResize, coAllowFocus];
        Width      := 150;
      end;

      with Columns.Add do
      begin
        Text := 'Name';
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
    Parent := Panel3;

    OnBeforeItemErase := @UsersInGroupBeforeItemErase;
    OnChecked         := @UsersInGroupChecked;
    OnChecking        := @UsersInGroupChecking;
    OnGetText         := @UsersInGroupGetText;
    OnInitNode        := @UsersInGroupInitNode;

    EndUpdate;
  end;
end;

procedure TAdminForm.DeleteUserActionExecute(Sender: TObject);
var
  User: TEpiUser;
  S: String;
begin
  User := UserFromGrid;

  if not Assigned(User) then exit;

  if (not Authenticator.CheckAuthedUserHierachy(User, true)) then
    Exit;

  // You cannot delete yourself, unless this is the last account. Otherwise
  // the whole security model will fail.
  if (Admin.Users.Count > 1) and
     (Authenticator.AuthedUser = User)
  then
    begin
      ShowMessage(
        'You cannot delete yourself as a user!' + LineEnding +
        'To delete this account do the following:' + LineEnding +
        LineEnding +
        ' 1) Create a new account' + LineEnding +
        ' 2) Login with new account' + LineEnding +
        ' 3) Delete this account'
      );
      Exit;
    end;

  if (Admin.Users.Count = 1) then
    S := 'Deleting the last user will restore the project to an' + LineEnding +
         'un-encrypted state!' + LineEnding +
         LineEnding +
         'Continue?'
  else
    S := 'Are you sure you want to delete the user:' + LineEnding +
         User.FullName + ' (' + User.Login + ')';

  if MessageDlg(
       'Warning',
       S,
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

procedure TAdminForm.DeleteUserActionUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Authenticator.CheckAuthedUserHierachy(UserFromGrid, true);
end;

procedure TAdminForm.EditGroupActionExecute(Sender: TObject);
begin
  ShowGroupForm(GroupFromSelectedNode);
end;

procedure TAdminForm.DeleteGroupActionExecute(Sender: TObject);
var
  Group: TEpiGroup;
begin
  Group := GroupFromSelectedNode;
  if not Assigned(Group) then exit;

  if Group = Admin.Admins then
    begin
      ShowMessage('The Admin group cannot be deleted.');
      Exit;
    end;

  if MessageDlg('Warning',
       'Are you sure you want to delete the group:' + LineEnding +
         Group.Caption.Text + LineEnding +
         LineEnding +
         'This also deletes the groups under this group!',
       mtWarning,
       mbYesNo,
       0,
       mbNo
     ) = mrYes
  then
    FGroupVST.DeleteNode(FGroupVST.FocusedNode);
end;

procedure TAdminForm.DeleteGroupActionUpdate(Sender: TObject);
var
  Group: TEpiGroup;
begin
  Group := GroupFromSelectedNode;

  TAction(Sender).Enabled :=
    Authenticator.IsAuthorized([earGroups]) and
    Authenticator.AuthedUserInGroup(Group, true) and
    (not Authenticator.AuthedUserInGroup(Group, false));
end;

procedure TAdminForm.AddUserToGroupActionExecute(Sender: TObject);
var
  T: LongInt;
  B: LongInt;
  User: TEpiUser;
  Group: TEpiGroup;
  i: LongInt;
  FUsers: TEpiUsers;
begin
  T := UserGrid.Selection.Top;
  B := UserGrid.Selection.Bottom;

  if (T = 0) or (B = 0) then exit;
  Group := GroupFromSelectedNode;
  if not Assigned(Group) then exit;

  FUsers := TEpiUsers.Create(nil);
  FUsers.ItemOwner := false;
  for i := T to B do
    FUsers.AddItem(TEpiUser(UserGrid.Objects[0, i]));

  for User in FUsers do
    if (not Authenticator.UserInGroup(User, Group, true)) and
       (Authenticator.CheckAuthedUserHierachy(User, true)) and
       (Authenticator.AuthedUserInGroup(Group, true))
    then
      User.Groups.AddItem(Group);

  FillUsersInGroupGrid;
end;

procedure TAdminForm.EditUserActionExecute(Sender: TObject);
begin
  if (not Authenticator.CheckAuthedUserHierachy(UserFromGrid, false))
  then
    Exit;

  if ShowUserForm(UserFromGrid) = mrCancel then
    Exit;

  FillGrids;
end;

procedure TAdminForm.EditUserActionUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Authenticator.CheckAuthedUserHierachy(UserFromGrid, false);
end;

procedure TAdminForm.FormShow(Sender: TObject);
begin
  UpdateShortcuts;
  FillGrids;
  FocusAndSelectNode(FGroupVST.GetFirst());
end;

procedure TAdminForm.NewGroupActionExecute(Sender: TObject);
var
  Group: TEpiGroup;
  R: TEpiGroupRelation;
begin
  Group := Admin.Groups.NewGroup;
  R := RelationFromSelectedNode;
  R := R.GroupRelations.NewGroupRelation;
  R.Group := Group;

  if ShowGroupForm(Group) = mrCancel then
  begin
    Group.Free
  end else begin
    FillGroupGrid;
    FocusAndSelectNode(NodeFromRelation(R));
  end;
end;

procedure TAdminForm.NewGroupActionUpdate(Sender: TObject);
var
  Group: TEpiGroup;
begin
  Group := GroupFromSelectedNode;

  TAction(Sender).Enabled :=
    Authenticator.IsAuthorized([earGroups]) and
    Authenticator.AuthedUserInGroup(Group, true);
end;

end.

