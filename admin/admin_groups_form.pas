unit admin_groups_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, Buttons, ActnList, VirtualTrees, epiadmin, epicustombase;

type

  { TDefineGroupsForm }

  TDefineGroupsForm = class(TForm)
    Panel2: TPanel;
    Panel6: TPanel;
    Label4: TLabel;
    ToolBar2: TToolBar;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton8: TToolButton;
    Panel4: TPanel;
    BitBtn1: TBitBtn;
    Panel3: TPanel;
    Panel7: TPanel;
    Label2: TLabel;
    ActionList1: TActionList;
    Splitter1: TSplitter;
    AddGroupAction: TAction;
    DeleteGroupAction: TAction;
    EditGroupAction: TAction;
    CloseFormAction: TAction;
    procedure EditGroupActionExecute(Sender: TObject);
    procedure AddGroupActionExecute(Sender: TObject);
    procedure AddGroupActionUpdate(Sender: TObject);
    procedure DeleteGroupActionUpdate(Sender: TObject);
    procedure DeleteGroupActionExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure CloseFormActionExecute(Sender: TObject);
    procedure EditGroupActionUpdate(Sender: TObject);
  private
    FAdmin: TEpiAdmin;
    procedure UpdateShortCuts;
    procedure FormChanged(Sender: TObject; Form: TCustomForm);
    procedure SetAdmin(AValue: TEpiAdmin);
    procedure AssignAdminHooks;
    procedure AdminResetHook(const Sender: TEpiCustomBase;
      const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
    procedure UsersHook(const Sender: TEpiCustomBase;
      const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);

   { Group }
  private
    FGroupVST: TVirtualStringTree;
    FGroupVSTUpdating: Boolean;

    { VST Events }
    procedure GroupBeforeItemErase(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect;
      var ItemColor: TColor; var EraseAction: TItemEraseAction);
    procedure GroupNodeDblClick(Sender: TBaseVirtualTree;
      const HitInfo: THitInfo);
    procedure GroupFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure GroupFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure GroupGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: String);
    procedure GroupGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure GroupInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure GroupInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var ChildCount: Cardinal);

    { Other }
    procedure InitGroupVST;
    function  GroupRelationFromNode(Node: PVirtualNode): TEpiGroupRelation;
    function  CurrentGroup: TEpiGroup;
    function  ShowGroupForm(Const Group: TEpiGroup): TModalResult;
    procedure AsyncOpenGroupForm(Data: PtrInt);


  { Users }
  private
    FUsersVST: TVirtualStringTree;
    FUpdatingUserChecks: boolean;

    { VST Events }
    procedure UserBeforeItemErase(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect;
      var ItemColor: TColor; var EraseAction: TItemEraseAction);
    procedure UsersChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure UsersChecking(Sender: TBaseVirtualTree;
      Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
    procedure UsersGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure UsersInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);


    { Other }
    procedure UpdateUsersCaption(Const Group: TEpiGroup);
    procedure InitUserVST;
    procedure UsersResetChecks(Const Group: TEpiGroup);
    function  UserFromNode(Node: PVirtualNode): TEpiUser;
    procedure UsersRemoveUserFromChildGroups(const Relation: TEpiGroupRelation;
      const Depth: Cardinal; const Index: Cardinal; var aContinue: boolean;
      Data: Pointer = nil);

  public
    constructor Create(TheOwner: TComponent); override;
    destructor  Destroy; override;
    property    Admin: TEpiAdmin read FAdmin write SetAdmin;
  end;

procedure ShowDefineGroupsForm(Owner: TComponent; Admin: TEpiAdmin);
procedure RestoreDefaultPosDefineGroupsForm;

implementation

{$R *.lfm}

uses
  admin_group_form, admin_authenticator, epigrouprelation_helper,
  settings2, settings2_var, LCLType, shortcuts;

var
  DefineGroupsForm: TDefineGroupsForm = nil;

procedure ShowDefineGroupsForm(Owner: TComponent; Admin: TEpiAdmin);
begin
  if (not Assigned(DefineGroupsForm)) then
    DefineGroupsForm := TDefineGroupsForm.Create(Owner);

  DefineGroupsForm.Admin := Admin;
  DefineGroupsForm.Show;
end;

procedure RestoreDefaultPosDefineGroupsForm;
var
  F: TForm;
begin
  if Assigned(DefineGroupsForm) then
    F := DefineGroupsForm
  else
    F := TForm.Create(nil);

  with F do
  begin
    LockRealizeBounds;
    Width := 700;
    Height := 700;
    Left := 100;
    Top := 100;
    UnlockRealizeBounds;
  end;
  SaveFormPosition(F, F.ClassName);

  if F <> DefineGroupsForm then F.Free;
end;


{ TDefineGroupsForm }

procedure TDefineGroupsForm.EditGroupActionExecute(Sender: TObject);
begin
  ShowGroupForm(CurrentGroup);
end;

procedure TDefineGroupsForm.AddGroupActionExecute(Sender: TObject);
var
  Group: TEpiGroup;
  R: TEpiGroupRelation;
begin
  Group := Admin.Groups.NewGroup;
  R := GroupRelationFromNode(FGroupVST.FocusedNode);
  R := R.GroupRelations.NewGroupRelation;
  R.Group := Group;

  if ShowGroupForm(Group) = mrCancel then
  begin
    Group.Free
  end else begin
    InitGroupVST;
  end;
end;

procedure TDefineGroupsForm.AddGroupActionUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled :=
    Authenticator.AuthedUserInGroup(CurrentGroup, true) and
    Authenticator.IsAuthorized([earGroups]);
end;

procedure TDefineGroupsForm.DeleteGroupActionUpdate(Sender: TObject);
var
  Group: TEpiGroup;
begin
  Group := CurrentGroup;

  TAction(Sender).Enabled :=
    (Group <> Admin.Admins) and
    Authenticator.AuthedUserInGroup(Group, true) and
    (not Authenticator.AuthedUserInGroup(Group, false)) and
    Authenticator.IsAuthorized([earGroups]);
end;

procedure TDefineGroupsForm.DeleteGroupActionExecute(Sender: TObject);
var
  Group: TEpiGroup;
  NewNode: PVirtualNode;
begin
  Group := CurrentGroup;

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
    begin
      NewNode := FGroupVST.NodeParent[FGroupVST.FocusedNode];
      FGroupVST.DeleteNode(FGroupVST.FocusedNode);
      FGroupVST.FocusedNode := NewNode;
      FGroupVST.Selected[NewNode] := true;
    end;
end;

procedure TDefineGroupsForm.FormShow(Sender: TObject);
begin
  if ManagerSettings.SaveWindowPositions then
  begin
    LoadFormPosition(Self, Self.ClassName);
    LoadSplitterPosition(Splitter1, Self.ClassName);
  end;
  UpdateShortCuts;
end;

procedure TDefineGroupsForm.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  CanClose := true;
  if ManagerSettings.SaveWindowPositions
  then
    begin
      SaveFormPosition(Self, Self.ClassName);
      SaveSplitterPosition(Splitter1, Self.ClassName);
    end;
end;

procedure TDefineGroupsForm.CloseFormActionExecute(Sender: TObject);
begin
  Close;
end;

procedure TDefineGroupsForm.EditGroupActionUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Authenticator.IsAuthorized([earGroups]);;
end;

procedure TDefineGroupsForm.UpdateShortCuts;
begin
  AddGroupAction.ShortCut    := AG_NewGroup;
  DeleteGroupAction.ShortCut := AG_DeleteGroup;
  EditGroupAction.ShortCut   := AG_EditGroup;
end;

procedure TDefineGroupsForm.FormChanged(Sender: TObject; Form: TCustomForm);
begin
  if (Form <> Self) then
    ActionList1.State := asSuspended
  else
    ActionList1.State := asNormal;
end;

procedure TDefineGroupsForm.SetAdmin(AValue: TEpiAdmin);
begin
  if FAdmin = AValue then Exit;
  FAdmin := AValue;

  InitGroupVST;
  InitUserVST;

  AssignAdminHooks;

  // To allways have a focused node
  FGroupVST.FocusedNode := FGroupVST.GetFirst();
end;

procedure TDefineGroupsForm.AssignAdminHooks;
begin
  Admin.RegisterOnChangeHook(@AdminResetHook, true);
  Admin.Users.RegisterOnChangeHook(@UsersHook, true);
end;

procedure TDefineGroupsForm.AdminResetHook(const Sender: TEpiCustomBase;
  const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
begin
  if (EventGroup <> eegAdmin) then exit;
  if (TEpiAdminChangeEventType(EventType) <> eaceAdminResetting) then exit;

  Admin.UnRegisterOnChangeHook(@AdminResetHook);
  Admin.Users.UnRegisterOnChangeHook(@UsersHook);
  Self.Free;
end;

procedure TDefineGroupsForm.UsersHook(const Sender: TEpiCustomBase;
  const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
begin
  if not (EventGroup in [eegCustomBase, eegAdmin]) then exit;

  // Event if a user is added/deleted
  if (Initiator is TEpiUsers) then
    case TEpiCustomChangeEventType(EventType) of
      ecceAddItem,
      ecceDelItem,
      ecceListMove:
        begin
          InitUserVST;
          UsersResetChecks(CurrentGroup);
        end;
    end;

  // Event if properties of a user is changed
  if (Initiator is TEpiUser) then
    case EventGroup of
      eegCustomBase:
        case TEpiCustomChangeEventType(EventType) of
          ecceName:
            FUsersVST.Invalidate;
        end;

      eegAdmin:
        case TEpiAdminChangeEventType(EventType) of
          eaceUserSetFullName:
            FUsersVST.Invalidate;
        end;
    end;

  // Event if user<->group relationship has changed
  if (Initiator is TEpiGroups) and
     (EventGroup = eegCustomBase)
  then
    case TEpiCustomChangeEventType(EventType) of
      ecceAddItem,
      ecceDelItem:
          UsersResetChecks(CurrentGroup);
    end;
end;

procedure TDefineGroupsForm.GroupBeforeItemErase(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect;
  var ItemColor: TColor; var EraseAction: TItemEraseAction);
var
  G: TEpiGroup;
begin
  G := GroupRelationFromNode(Node).Group;

  if //(G.ProtectedItem) or
     (G = Admin.Admins) or
     (not Authenticator.AuthedUserInGroup(G, true))
  then
    begin
      ItemColor := clLtGray;
      EraseAction := eaColor;
    end;
end;

procedure TDefineGroupsForm.GroupNodeDblClick(Sender: TBaseVirtualTree;
  const HitInfo: THitInfo);
begin
  EditGroupAction.Execute;
end;

procedure TDefineGroupsForm.GroupFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  G: TEpiGroup;
begin
  if not Assigned(Node) then exit;

  G := GroupRelationFromNode(Node).Group;
  UsersResetChecks(G);
  UpdateUsersCaption(G);
end;

procedure TDefineGroupsForm.GroupFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  G: TEpiGroup;
begin
  if (FGroupVSTUpdating) or
     (csDestroying in ComponentState)
  then
    Exit;

  if not Assigned(Node) then exit;

  G := GroupRelationFromNode(Node).Group;
  G.Free;
end;

procedure TDefineGroupsForm.GroupGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: String);
var
  Group: TEpiGroup;
  Item: TEpiManagerRight;
begin
  Group := GroupRelationFromNode(Node).Group;

  case Column of
    0: HintText := Group.Caption.Text;
    1: begin
         HintText := '';
         for Item in Group.ManageRights do
           HintText += EpiManagerRightCaptionsShort[Item] + ': ' +
                       StringReplace(EpiManagerRightCaptions[Item], '&', '', [rfReplaceAll]) + LineEnding;
       end;
  end;
end;

procedure TDefineGroupsForm.GroupGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  G: TEpiGroup;
begin
  G := GroupRelationFromNode(Node).Group;

  case Column of
    0: CellText := G.Caption.Text;
    1: CellText := Authenticator.PrintGroupRights(G, true);
  end;
end;

procedure TDefineGroupsForm.GroupInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  Relation: TEpiGroupRelation;
begin
  if (not Assigned(ParentNode)) then
    Relation := Admin.AdminRelation
  else
    Relation := GroupRelationFromNode(ParentNode).GroupRelation[Node^.Index];

  TEpiGroupRelation(Sender.GetNodeData(Node)^) := Relation;
  if Relation.GroupRelations.Count > 0 then
    Include(InitialStates, ivsHasChildren);
end;

procedure TDefineGroupsForm.GroupInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);
begin
  ChildCount := GroupRelationFromNode(Node).GroupRelations.Count;
end;

procedure TDefineGroupsForm.InitGroupVST;
begin
  if not Assigned(Admin) then exit;

  FGroupVSTUpdating := true;

  FGroupVST.BeginUpdate;
  FGroupVST.RootNodeCount := 1;
  FGroupVST.ReinitNode(nil, true);
  FGroupVST.FullExpand();
  FGroupVST.EndUpdate;

  FGroupVSTUpdating := false;
end;

function TDefineGroupsForm.GroupRelationFromNode(Node: PVirtualNode
  ): TEpiGroupRelation;
begin
  result := TEpiGroupRelation(FGroupVST.GetNodeData(Node)^);
end;

function TDefineGroupsForm.CurrentGroup: TEpiGroup;
begin
  result := GroupRelationFromNode(FGroupVST.FocusedNode).Group;
end;

function TDefineGroupsForm.ShowGroupForm(const Group: TEpiGroup): TModalResult;
var
  F: TAdminGroupForm;
begin
  if not Assigned(Group) then exit;

  F := TAdminGroupForm.Create(Self);
  F.Group := Group;
  Result := F.ShowModal;
  F.Free;
end;

procedure TDefineGroupsForm.AsyncOpenGroupForm(Data: PtrInt);
begin
  ShowGroupForm(TEpiGroup(Data));
end;

procedure TDefineGroupsForm.UserBeforeItemErase(
  Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  const ItemRect: TRect; var ItemColor: TColor;
  var EraseAction: TItemEraseAction);
var
  User: TEpiUser;
  Group: TEpiGroup;
  Allowed: Boolean;
begin
  User := UserFromNode(Node);
  Group := CurrentGroup;

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

procedure TDefineGroupsForm.UsersChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  User: TEpiUser;
  Group: TEpiGroup;
begin
  if FUpdatingUserChecks then exit;

  User :=  UserFromNode(Node);
  Group := CurrentGroup;

  if Sender.CheckState[Node] = csCheckedNormal then
  begin
    if Authenticator.UserInGroup(User, Group, true, false) then
      Authenticator.RelationFromGroup(Group).GroupRelations.OrderedWalk(
        @UsersRemoveUserFromChildGroups,
        User
      );

    User.Groups.AddItem(Group)
  end
  else
    User.Groups.RemoveItem(Group);
end;

procedure TDefineGroupsForm.UsersChecking(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
var
  User: TEpiUser;
  Group: TEpiGroup;
begin
  if FUpdatingUserChecks then
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

  User := UserFromNode(Node);
  Group := CurrentGroup;

  Allowed :=
     (User <> Authenticator.AuthedUser) and
     (Authenticator.AuthedUserInGroup(Group, true));
end;
procedure TDefineGroupsForm.UsersGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
begin
  case Column of
    0: CellText := UserFromNode(Node).Login;
    1: CellText := UserFromNode(Node).FullName;
  end;
end;

procedure TDefineGroupsForm.UsersInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  User: TEpiUser;
begin
  InitialStates := [];

  User := Admin.Users[Node^.Index];
  TEpiUser(Sender.GetNodeData(Node)^) := User;

  Sender.CheckType[Node] := ctCheckBox;
end;

procedure TDefineGroupsForm.UpdateUsersCaption(const Group: TEpiGroup);
const
  CaptionText = 'Users in group: ';
begin
  if Assigned(Group) then
    Label2.Caption := CaptionText + Group.Caption.Text;
end;

procedure TDefineGroupsForm.InitUserVST;
begin
  if not Assigned(Admin) then exit;

  FUsersVST.BeginUpdate;
  FUsersVST.RootNodeCount := Admin.Users.Count;
  FUsersVST.ReinitNode(nil, true);
  FUsersVST.EndUpdate;
end;

procedure TDefineGroupsForm.UsersResetChecks(const Group: TEpiGroup);
var
  Node: PVirtualNode;
  User: TEpiUser;
begin
  FUpdatingUserChecks := true;

  for Node in FUsersVST.Nodes() do
  begin
    User := UserFromNode(Node);

    if Authenticator.UserInGroup(User, Group, false) then
      FUsersVST.CheckState[Node] := csCheckedNormal
    else
    if Authenticator.UserInGroup(User, Group, True) then
      FUsersVST.CheckState[Node] := csMixedNormal
    else
      FUsersVST.CheckState[Node] := csUncheckedNormal;
  end;

  FUpdatingUserChecks := false;
end;

function TDefineGroupsForm.UserFromNode(Node: PVirtualNode): TEpiUser;
begin
  result := TEpiUser(FUsersVST.GetNodeData(Node)^);
end;

procedure TDefineGroupsForm.UsersRemoveUserFromChildGroups(
  const Relation: TEpiGroupRelation; const Depth: Cardinal;
  const Index: Cardinal; var aContinue: boolean; Data: Pointer);
var
  User: TEpiUser;
begin
  User := TEpiUser(Data);

  if (User.Groups.IndexOf(Relation.Group) >= 0) then
    User.Groups.RemoveItem(Relation.Group);
end;

constructor TDefineGroupsForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Screen.AddHandlerActiveFormChanged(@FormChanged);

  FGroupVST := TVirtualStringTree.Create(Self);
  with FGroupVST do
  begin
    BeginUpdate;

    NodeDataSize := SizeOf(TEpiGroupRelation);

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
                  hoShowHint, hoFullRepaintOnResize];

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
    ShowHint := true;
    HintMode := hmHint;

    OnBeforeItemErase := @GroupBeforeItemErase;
    OnNodeDblClick    := @GroupNodeDblClick;
    OnFocusChanged    := @GroupFocusChanged;
    OnInitNode        := @GroupInitNode;
    OnInitChildren    := @GroupInitChildren;
    OnFreeNode        := @GroupFreeNode;
    OnGetText         := @GroupGetText;
    OnGetHint         := @GroupGetHint;

    EndUpdate;
  end;

  FUsersVST := TVirtualStringTree.Create(Self);
  with FUsersVST do
  begin
    BeginUpdate;

    NodeDataSize := SizeOf(TEpiUser);

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

    OnBeforeItemErase := @UserBeforeItemErase;
    OnChecked := @UsersChecked;
    OnChecking := @UsersChecking;
    OnGetText := @UsersGetText;
    OnInitNode := @UsersInitNode;

    EndUpdate;
  end;
end;

destructor TDefineGroupsForm.Destroy;
begin
  DefineGroupsForm := nil;
  Screen.RemoveHandlerActiveFormChanged(@FormChanged);
  inherited Destroy;
end;

end.

