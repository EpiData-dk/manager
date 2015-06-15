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
    Splitter1: TSplitter;
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
    FShowGroups: Boolean;
    FAdmin: TEpiAdmin;
    FUser: TEpiUser;
    FPasswordModified: Boolean;
    procedure FocusUserForm(Data: PtrInt);
    procedure PasswordEditOpen(Data: PtrInt);
    procedure FormShow(Sender: TObject);

  { Groups }
  private
    FGroupVST: TVirtualStringTree;
    FUpdatingGroupVST: Boolean;
    procedure FillGroupList;
    // VST Aux methods
    function RelationFromNode(Node: PVirtualNode): TEpiGroupRelation;
    function GroupFromNode(Node: PVirtualNode): TEpiGroup;
    function NodeFromRealtion(Const Relation: TEpiGroupRelation): PVirtualNode;

    // VST Methods
    procedure GetGroupText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure GroupBeforeItemErase(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect;
      var ItemColor: TColor; var EraseAction: TItemEraseAction);
    procedure GroupChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure GroupChecking(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var NewState: TCheckState; var Allowed: Boolean);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property User: TEpiUser read FUser write FUser;
    property Admin: TEpiAdmin read FAdmin write FAdmin;
    property ShowGroups: Boolean read FShowGroups write FShowGroups;
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

  User.Groups.Clear;
  for Node in FGroupVST.CheckedNodes() do
    User.Groups.AddItem(GroupFromNode(Node));
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
  LoginEdit.Enabled    := Authenticator.CheckAuthedUserHierachy(User, true);

  FullnameEdit.Text    := User.FullName;
  FullnameEdit.Enabled := Authenticator.CheckAuthedUserHierachy(User, true);

  PasswordEdit.Enabled := Authenticator.CheckAuthedUserHierachy(User, true) or
                          Authenticator.IsAuthorized([earPassword]);

  NeverExpireChkBox.Checked := (User.ExpireDate = 0);
  NeverExpireChkBox.Enabled := Authenticator.CheckAuthedUserHierachy(User, true);

  ExpiresDateEdit.Date    := User.ExpireDate;
  ExpiresDateEdit.Enabled := Authenticator.CheckAuthedUserHierachy(User, true);

  PasswordEdit.Text    := User.Password;
  if (User.Password = '') then
    OkBtn.Enabled := false;

  if User.LastLogin > 0 then
    LastLoginEdit.Text := DateTimeToStr(User.LastLogin)
  else
    LastLoginEdit.Text := '(N/A)';

  if (not ShowGroups) then
    begin
      FGroupVST.Visible := false;
      Splitter1.Visible := false;
      Panel1.Align := alClient;
    end
  else
    FillGroupList;

  if LoginEdit.CanFocus then
    LoginEdit.SetFocus;
end;

procedure TAdminUserForm.FillGroupList;

  procedure AddRecusive(Root: PVirtualNode; Const Relation: TEpiGroupRelation);
  var
    R: TEpiGroupRelation;
  begin
    Root := FGroupVST.AddChild(Root, Relation);
    Relation.AddCustomData(ADMIN_USERFORM_NODE_KEY, TObject(Root));

    FGroupVST.CheckType[Root] := ctCheckBox;

    if Authenticator.UserInGroup(User, Relation.Group, true) then
      FGroupVST.CheckState[Root] := csMixedNormal;

    if Authenticator.UserInGroup(User, Relation.Group, false) then
      FGroupVST.CheckState[Root] := csCheckedNormal;

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

function TAdminUserForm.RelationFromNode(Node: PVirtualNode): TEpiGroupRelation;
begin
  result := TEpiGroupRelation(FGroupVST.GetNodeData(Node)^);
end;

function TAdminUserForm.GroupFromNode(Node: PVirtualNode): TEpiGroup;
begin
  result := RelationFromNode(Node).Group;
end;

function TAdminUserForm.NodeFromRealtion(const Relation: TEpiGroupRelation
  ): PVirtualNode;
begin
  result := PVirtualNode(Relation.FindCustomData(ADMIN_USERFORM_NODE_KEY));
end;

procedure TAdminUserForm.GetGroupText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
begin
  case Column of
    0: CellText := GroupFromNode(Node).Caption.Text;
    1: CellText := Authenticator.PrintGroupRights(GroupFromNode(Node), true);
  end;
end;

procedure TAdminUserForm.GroupBeforeItemErase(Sender: TBaseVirtualTree;
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

procedure TAdminUserForm.GroupChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  R: TEpiGroupRelation;
  Child: TEpiGroupRelation;
begin
  if FUpdatingGroupVST then exit;

  R := RelationFromNode(Node);

  for Child in R.GroupRelations do
    begin
      if Sender.CheckState[Node] in [csCheckedNormal, csMixedNormal] then
        Sender.CheckState[NodeFromRealtion(Child)] := csMixedNormal
      else
        Sender.CheckState[NodeFromRealtion(Child)] := csUncheckedNormal;
    end;
end;

procedure TAdminUserForm.GroupChecking(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
var
  G: TEpiGroup;
begin
  if FUpdatingGroupVST then exit;

  G := GroupFromNode(Node);

  // You must be "owner" of group AND "owner" of user,
  // otherwise you cannot assign user to group.
  if not
      (
       Authenticator.AuthedUserInGroup(G, true, true) and
       Authenticator.CheckAuthedUserHierachy(User, false)
      )
  then
    begin
      Allowed := false;
      Exit;
    end;

  // Special case for admins in admin group!
  if ((G = Admin.Admins) and (User = Authenticator.AuthedUser))
     or
     (not Authenticator.AuthedUserInGroup(G, true))
  then
    begin
      Allowed := false;
      Exit;
    end;

  // You cannot remove yourself from the group, if this is highest group
  // in the hieracy you have access to.
  if (User = Authenticator.AuthedUser) and
     (Authenticator.AuthedUserInGroup(G, false)) and
     (not
       Authenticator.AuthedUserInGroup(
         Authenticator.RelationFromGroup(G).ParentRelation.Group,
         true
       )
     )
  then
    begin
      Allowed := false;
      Exit;
    end;

  if (Sender.CheckState[Node^.Parent] in [csCheckedNormal, csMixedNormal]) and
     (Sender.CheckState[Node] = csMixedNormal) then
    begin
      Allowed := false;
      Exit;
    end;
end;

constructor TAdminUserForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FPasswordModified := false;
  FShowGroups       := false;

  FHintWindow       := THintWindow.Create(Self);
  FHintWindow.AutoHide     := true;
  FHintWindow.HideInterval := 2500;  //2.5 secs.

  FGroupVST := TVirtualStringTree.Create(Self);
  with FGroupVST do
  begin
    BeginUpdate;

    NodeDataSize := SizeOf(Pointer);

    with TreeOptions do
    begin
      AnimationOptions := [];
      AutoOptions      := [];
      MiscOptions      := [toCheckSupport, toFullRepaintOnResize, toGridExtensions,
                           toWheelPanning];
      PaintOptions     := [toShowButtons, toShowDropmark, toShowRoot,
                           toShowTreeLines, toThemeAware, toUseBlendedImages];
      SelectionOptions := [toExtendedFocus, toFullRowSelect, toAlwaysSelectNode];
      StringOptions    := [];
    end;

    with Header do
    begin
      Options := [hoAutoResize, hoColumnResize, hoDblClickResize, hoVisible,
                  hoFullRepaintOnResize];

      with Columns.Add do
      begin
        Text := 'Group';
        CheckBox   := True;
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
        CheckType  := ctNone;
        Options    := [coAllowClick, coEnabled, coParentBidiMode,
                       coParentColor, coResizable, coShowDropMark, coVisible,
                       coSmartResize, coAllowFocus];
      end;

      MainColumn := 0;
      AutoSizeIndex := 1;
    end;

    Align := alClient;
    Parent := Self;

    OnBeforeItemErase := @GroupBeforeItemErase;
    OnChecked         := @GroupChecked;
    OnChecking        := @GroupChecking;
    OnGetText         := @GetGroupText;

    EndUpdate;
  end;

  OnShow := @FormShow;
end;

destructor TAdminUserForm.Destroy;
begin
  FHintWindow.Free;
  inherited Destroy;
end;

end.

