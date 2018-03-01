unit admin_users_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ComCtrls, ActnList, StdCtrls, VirtualTrees, epiadmin, epicustombase;

type

  { TDefineUsersForm }

  TDefineUsersForm = class(TForm)
    CloseAction: TAction;
    Panel4: TPanel;
    BitBtn1: TBitBtn;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton7: TToolButton;
    ActionList1: TActionList;
    AddUserAction: TAction;
    DeleteUserAction: TAction;
    EditUserAction: TAction;
    Panel6: TPanel;
    Label4: TLabel;
    procedure CloseActionExecute(Sender: TObject);
    procedure EditUserActionExecute(Sender: TObject);
    procedure AddUserActionExecute(Sender: TObject);
    procedure DeleteUserActionUpdate(Sender: TObject);
    procedure DeleteUserActionExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
  private
    FAdmin: TEpiAdmin;
    procedure UpdateShortCuts;
    procedure SetAdmin(AValue: TEpiAdmin);
    procedure FormChanged(Sender: TObject; Form: TCustomForm);
    procedure AdminResettingHook(const Sender: TEpiCustomBase;
      const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);

  { Users }
  private
    FUsersVST: TVirtualStringTree;
    FPasswordReset: boolean;

    { VST Events }
    procedure UsersBeforeItemErase(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect;
      var ItemColor: TColor; var EraseAction: TItemEraseAction);
    procedure UsersGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure UsersInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure UsersNodeDblClick(Sender: TBaseVirtualTree;
      const HitInfo: THitInfo);

    { Other }
    procedure AsyncOpenUserForm(Data: PtrInt);
    procedure InitUserVST;
    function UserFromNode(Node: PVirtualNode): TEpiUser;
    function ShowUserForm(User: TEpiUser; NewUser: boolean = false): TModalResult;
    procedure SetPasswordReset(AValue: boolean);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor  Destroy; override;
    property    Admin: TEpiAdmin read FAdmin write SetAdmin;
    property    PasswordReset: boolean read FPasswordReset write SetPasswordReset;
  end;

procedure ShowDefineUsersForm(Owner: TComponent; Admin: TEpiAdmin);
procedure RestoreDefaultPosDefineUsersForm;

implementation

{$R *.lfm}

uses
  admin_authenticator, admin_user_form, settings2, settings2_var,
  LCLType, shortcuts;

var
  DefineUsersForm: TDefineUsersForm = nil;

procedure ShowDefineUsersForm(Owner: TComponent; Admin: TEpiAdmin);
begin
  if (not Assigned(DefineUsersForm)) then
    DefineUsersForm := TDefineUsersForm.Create(Owner);

  DefineUsersForm.Admin := Admin;
  DefineUsersForm.Show;
end;

procedure RestoreDefaultPosDefineUsersForm;
var
  F: TForm;
begin
  if Assigned(DefineUsersForm) then
    F := DefineUsersForm
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

  if F <> DefineUsersForm then F.Free;
end;

{ TDefineUsersForm }

procedure TDefineUsersForm.EditUserActionExecute(Sender: TObject);
begin
  ShowUserForm(UserFromNode(FUsersVST.FocusedNode));
end;

procedure TDefineUsersForm.CloseActionExecute(Sender: TObject);
begin
  Close;
end;

procedure TDefineUsersForm.AddUserActionExecute(Sender: TObject);
var
  User: TEpiUser;
begin
  User := Admin.Users.NewUser;
  if ShowUserForm(User, True) = mrCancel then
    User.Free
  else
    InitUserVST;
end;

procedure TDefineUsersForm.DeleteUserActionUpdate(Sender: TObject);
var
  User: TEpiUser;
begin
//  User := UserFromNode(FUsersVST.FocusedNode);
//  TAction(Sender).Enabled := Authenticator.CheckAuthedUserHierachy(User, true);
end;

procedure TDefineUsersForm.DeleteUserActionExecute(Sender: TObject);
var
  User: TEpiUser;
  S: String;
begin
  User := UserFromNode(FUsersVST.FocusedNode);

  if not Assigned(User) then exit;

  if (not Authenticator.CheckAuthedUserHierachy(User, true)) then
    Exit;

  // You cannot delete yourself, unless this is the last account. Otherwise
  // the whole security model will fail.
  if (Authenticator.AuthedUser = User)
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

  S := 'Are you sure you want to delete the user:' + LineEnding +
       User.FullName + ' (' + User.Login + ')';

  if MessageDlg('Warning', S, mtWarning, mbYesNo, 0, mbNo) = mrYes
  then
    begin
      User.Free;
      InitUserVST;
    end;
end;

procedure TDefineUsersForm.FormShow(Sender: TObject);
begin
  if ManagerSettings.SaveWindowPositions then
    LoadFormPosition(Self, Self.ClassName);

  UpdateShortCuts;
  ToolBar1.Enabled := (not PasswordReset);
  ToolBar1.Visible := (not PasswordReset);
  Panel4.Visible   := (PasswordReset);
end;

procedure TDefineUsersForm.FormCloseQuery(Sender: TObject; var CanClose: boolean
  );
begin
  CanClose := true;
  if ManagerSettings.SaveWindowPositions
  then
    SaveFormPosition(Self, Self.ClassName);
end;

procedure TDefineUsersForm.UpdateShortCuts;
begin
  AddUserAction.ShortCut    := AU_NewUser;
  DeleteUserAction.ShortCut := AU_DeleteUser;
  EditUserAction.ShortCut   := AU_EditUser;
end;

procedure TDefineUsersForm.SetAdmin(AValue: TEpiAdmin);
begin
  if FAdmin = AValue then Exit;
  FAdmin := AValue;

  Admin.RegisterOnChangeHook(@AdminResettingHook, true);

  InitUserVST;
end;

procedure TDefineUsersForm.FormChanged(Sender: TObject; Form: TCustomForm);
begin
  if (Form <> Self) then
    ActionList1.State := asSuspended
  else
    ActionList1.State := asNormal;
end;

procedure TDefineUsersForm.AdminResettingHook(const Sender: TEpiCustomBase;
  const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
begin
  if (EventGroup <> eegAdmin) then exit;
  if (TEpiAdminChangeEventType(EventType) <> eaceAdminResetting) then exit;

  Admin.UnRegisterOnChangeHook(@AdminResettingHook);
  Self.Free;
end;

procedure TDefineUsersForm.UsersBeforeItemErase(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect;
  var ItemColor: TColor; var EraseAction: TItemEraseAction);
var
  User: TEpiUser;
  OrgItemColor: TColor;
begin
  User := UserFromNode(Node);

  OrgItemColor := ItemColor;

  if PasswordReset then
    begin
      if Authenticator.UserInGroup(User, Authenticator.Admin.Admins, false)
      then
        ItemColor := clInactiveCaption
    end
  else
    begin
      if (not Authenticator.CheckAuthedUserHierachy(User, false))
      then
        ItemColor := clInactiveCaption;
    end;

  if (User = Authenticator.AuthedUser)
  then
    ItemColor := clGradientActiveCaption;

  if ItemColor <> OrgItemColor then
    EraseAction := eaColor;
end;

procedure TDefineUsersForm.UsersGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
var
  User: TEpiUser;
begin
  User := UserFromNode(Node);

  case Column of
    0: CellText := User.Login;
    1: CellText := User.FullName;
    2: if User.ExpireDate > 0 then
         CellText := DateToStr(User.ExpireDate)
       else
         CellText := '(never)';
    3: if User.LastLogin > 0 then
         CellText := DateTimeToStr(User.LastLogin)
       else
         CellText := '(N/A)';
    4: if User.LastPWChange > 0 then
         CellText := DateTimeToStr(User.LastPWChange)
       else
         CellText := '(N/A)';
  end;
end;

procedure TDefineUsersForm.UsersInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  TEpiUser(Sender.GetNodeData(Node)^) := Admin.Users[Node^.Index];
end;

procedure TDefineUsersForm.UsersNodeDblClick(Sender: TBaseVirtualTree;
  const HitInfo: THitInfo);
begin
  Application.QueueAsyncCall(@AsyncOpenUserForm, PtrInt(HitInfo.HitNode));
end;

procedure TDefineUsersForm.AsyncOpenUserForm(Data: PtrInt);
var
  User: TEpiUser;
begin
  User := UserFromNode(PVirtualNode(Data));
  ShowUserForm(User);
end;

procedure TDefineUsersForm.InitUserVST;
begin
  if not Assigned(Admin) then exit;

  FUsersVST.BeginUpdate;
  FUsersVST.RootNodeCount := Admin.Users.Count;
  FUsersVST.ReinitNode(nil, true);
  FUsersVST.Header.AutoFitColumns(false, smaUseColumnOption, 0, 1);
  FUsersVST.EndUpdate;

  FUsersVST.FocusedNode := FUsersVST.GetFirst();
end;

function TDefineUsersForm.UserFromNode(Node: PVirtualNode): TEpiUser;
begin
  result := TEpiUser(FUsersVST.GetNodeData(Node)^);
end;

function TDefineUsersForm.ShowUserForm(User: TEpiUser; NewUser: boolean
  ): TModalResult;
var
  F: TAdminUserForm;
begin
  if not Assigned(User) then exit;

  F := TAdminUserForm.Create(Self);
  F.User  := User;
  F.Admin := Admin;
  F.PasswordReset := PasswordReset;
  F.ShowGroups := (not PasswordReset);
  F.NewUser := NewUser;
  Result := F.ShowModal;
  F.Free;

  FUsersVST.Invalidate;
end;

procedure TDefineUsersForm.SetPasswordReset(AValue: boolean);
begin
  if FPasswordReset = AValue then Exit;
  FPasswordReset := AValue;
end;

constructor TDefineUsersForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Screen.AddHandlerActiveFormChanged(@FormChanged);
  PasswordReset := false;

  FUsersVST := TVirtualStringTree.Create(Self);
  with FUsersVST do
  begin
    BeginUpdate;

    NodeDataSize := SizeOf(TEpiUser);

    with TreeOptions do
    begin
      AnimationOptions := [];
      AutoOptions      := [];
      MiscOptions      := [toFullRepaintOnResize, toGridExtensions, toWheelPanning];
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
        CheckType  := ctNone;
        Options    := [coAllowClick, coEnabled, coParentBidiMode,
                       coParentColor, coResizable, coVisible,
                       coSmartResize, coAllowFocus];
        Width      := 150;
      end;

      with Columns.Add do
      begin
        Text := 'Name';
        CheckBox   := false;
        CheckType  := ctNone;
        Options    := [coAllowClick, coEnabled, coParentBidiMode,
                       coParentColor, coResizable, coVisible,
                       coSmartResize, coAllowFocus];
      end;

      with Columns.Add do
      begin
        Text := 'Expires';
        CheckBox   := false;
        CheckType  := ctNone;
        Options    := [coAllowClick, coEnabled, coParentBidiMode,
                       coParentColor, coResizable, coVisible,
                       coSmartResize, coAllowFocus];
        Width      := 150;
      end;

      with Columns.Add do
      begin
        Text := 'Last Login';
        CheckBox   := false;
        CheckType  := ctNone;
        Options    := [coAllowClick, coEnabled, coParentBidiMode,
                       coParentColor, coResizable, coVisible,
                       coSmartResize, coAllowFocus];
        Width      := 150;
      end;

      with Columns.Add do
      begin
        Text := 'Last Password Change';
        CheckBox   := false;
        CheckType  := ctNone;
        Options    := [coAllowClick, coEnabled, coParentBidiMode,
                       coParentColor, coResizable, coVisible,
                       coSmartResize, coAllowFocus];
        Width      := 150;
      end;

      MainColumn := 0;
      AutoSizeIndex := 1;
    end;

    Align := alClient;
    Parent := Self;

    OnBeforeItemErase := @UsersBeforeItemErase;
    OnGetText         := @UsersGetText;
    OnInitNode        := @UsersInitNode;
    OnNodeDblClick    := @UsersNodeDblClick;

    EndUpdate;
  end;
end;

destructor TDefineUsersForm.Destroy;
begin
  DefineUsersForm := nil;

  Screen.RemoveHandlerActiveFormChanged(@FormChanged);
  inherited Destroy;
end;

end.

