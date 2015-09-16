unit admin_users_accum_rights_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, FileUtil, Forms, Controls, epidatafiles,
  VirtualTrees, epiadmin, epirights, Graphics, epicustombase;

type

  { TUsersAccumulatedRightsFrame }

  TUsersAccumulatedRightsFrame = class(TFrame)
  private
    FUsersRightsVST: TVirtualStringTree;

    { Other }
    procedure InitUserRightsVST;
    procedure UserToNode(Node: PVirtualNode; Const User: TEpiUser);
    function  UserFromNode(Node: PVirtualNode): TEpiUser;
    function  UserEntryRights(Const User: TEpiUser): TEpiEntryRights;

    { Events }
    procedure UserVSFBeforeItemErase(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect;
      var ItemColor: TColor; var EraseAction: TItemEraseAction);
    procedure UserVSTAfterCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      const CellRect: TRect);
    procedure UserVSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure UserVSTInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
  private
    FAdmin: TEpiAdmin;
    FDataFile: TEpiDataFile;
    procedure SetAdmin(AValue: TEpiAdmin);
    procedure SetDataFile(AValue: TEpiDataFile);

    procedure GroupRightsEventHook(const Sender: TEpiCustomBase;
      const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
    procedure UsersEventHook(const Sender: TEpiCustomBase;
      const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property DataFile: TEpiDataFile read FDataFile write SetDataFile;
    property Admin: TEpiAdmin read FAdmin write SetAdmin;
  end;

implementation

{$R *.lfm}

uses
  Themes, admin_authenticator;

{ TUsersAccumulatedRightsFrame }


procedure TUsersAccumulatedRightsFrame.InitUserRightsVST;
begin
  if not Assigned(DataFile) then exit;
  if not Assigned(Admin) then exit;

  FUsersRightsVST.BeginUpdate;
  FUsersRightsVST.RootNodeCount := Authenticator.Admin.Users.Count;
  FUsersRightsVST.ReinitNode(nil, true);
  FUsersRightsVST.EndUpdate;
end;

procedure TUsersAccumulatedRightsFrame.UserToNode(Node: PVirtualNode;
  const User: TEpiUser);
begin
  TEpiUser(FUsersRightsVST.GetNodeData(Node)^) := User;
end;

function TUsersAccumulatedRightsFrame.UserFromNode(Node: PVirtualNode
  ): TEpiUser;
begin
  Result := TEpiUser(FUsersRightsVST.GetNodeData(Node)^);
end;

function TUsersAccumulatedRightsFrame.UserEntryRights(const User: TEpiUser
  ): TEpiEntryRights;
var
  G: TEpiGroup;
  GR: TEpiGroupRight;
begin
  Result := [];

  for G in User.Groups do
  begin
    GR := DataFile.GroupRights.GroupRightFromGroup(G);
    Result += GR.EntryRights;
  end;
end;

procedure TUsersAccumulatedRightsFrame.UserVSFBeforeItemErase(
  Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  const ItemRect: TRect; var ItemColor: TColor;
  var EraseAction: TItemEraseAction);
begin

end;

procedure TUsersAccumulatedRightsFrame.UserVSTAfterCellPaint(
  Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; const CellRect: TRect);
var
  Details: TThemedElementDetails;
  Sz: TSize;
  FCheckBoxHeight: LongInt;
  FCheckBoxWidth: LongInt;
  X: Integer;
  Y: Integer;
  R: TRect;
  User: TEpiUser;
  Checked: Boolean;
begin
  if Column = 0 then exit;

  { Paint Check boxes by ourselves - since VT's only allow for one checkbox column }
  User := UserFromNode(Node);
  Checked :=  TEpiEntryRight(Column - 1) in UserEntryRights(User);

  if Checked then
    Details := ThemeServices.GetElementDetails(tbCheckBoxCheckedNormal)
  else
    Details := ThemeServices.GetElementDetails(tbCheckBoxUncheckedNormal);

  Sz := ThemeServices.GetDetailSize(Details);

  FCheckBoxHeight := Sz.cy;
  FCheckBoxWidth  := SZ.cx;

  X := CellRect.Left + (CellRect.Right - CellRect.Left - FCheckBoxWidth) div 2;
  Y := CellRect.Top + (CellRect.Bottom - CellRect.Top - FCheckBoxHeight) div 2;
  R := Rect(X, Y, X + FCheckBoxWidth, Y + FCheckBoxHeight);
  ThemeServices.DrawElement(TargetCanvas.Handle, Details, R);
end;

procedure TUsersAccumulatedRightsFrame.UserVSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
begin
  if Column <> 0 then exit;

  CellText := UserFromNode(Node).Login;
end;

procedure TUsersAccumulatedRightsFrame.UserVSTInitNode(
  Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  User: TEpiUser;
begin
  User := Authenticator.Admin.Users[Node^.Index];
  UserToNode(Node, User);
end;

procedure TUsersAccumulatedRightsFrame.SetDataFile(AValue: TEpiDataFile);
begin
  if FDataFile = AValue then Exit;

  if Assigned(FDataFile) then
    FDataFile.GroupRights.UnRegisterOnChangeHook(@GroupRightsEventHook);

  FDataFile := AValue;

  if Assigned(FDataFile) then
    FDataFile.GroupRights.RegisterOnChangeHook(@GroupRightsEventHook, true);

  InitUserRightsVST;
end;

procedure TUsersAccumulatedRightsFrame.GroupRightsEventHook(
  const Sender: TEpiCustomBase; const Initiator: TEpiCustomBase;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  if (EventGroup <> eegRights) then exit;
  FUsersRightsVST.Invalidate;
end;

procedure TUsersAccumulatedRightsFrame.UsersEventHook(
  const Sender: TEpiCustomBase; const Initiator: TEpiCustomBase;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  if (EventGroup <> eegCustomBase) then exit;
  case TEpiCustomChangeEventType(EventType) of
    ecceAddItem,
    ecceDelItem:
      begin
        // A new user is added
        if (Initiator = Admin.Users) then
          InitUserRightsVST;

        // A user was added to/deleted from a group
        if (Initiator is TEpiGroups) then
          FUsersRightsVST.Invalidate;
      end;

    ecceName:
      begin
        if not (Initiator is TEpiUser) then exit;
        // A user login was changes
        FUsersRightsVST.Invalidate;
      end;
  end;
end;

procedure TUsersAccumulatedRightsFrame.SetAdmin(AValue: TEpiAdmin);
begin
  if FAdmin = AValue then Exit;

  if Assigned(FAdmin) then
    FAdmin.Users.UnRegisterOnChangeHook(@UsersEventHook);

  FAdmin := AValue;

  if Assigned(FAdmin) then
    FAdmin.Users.RegisterOnChangeHook(@UsersEventHook, true);

  InitUserRightsVST;
end;

constructor TUsersAccumulatedRightsFrame.Create(TheOwner: TComponent);
var
  Item: TEpiEntryRight;
begin
  inherited Create(TheOwner);

  FUsersRightsVST := TVirtualStringTree.Create(Self);
  with FUsersRightsVST do
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
      SelectionOptions := [toExtendedFocus];
      StringOptions    := [];
    end;

    with Header do
    begin
      Options := [hoAutoResize, hoColumnResize, hoDblClickResize, hoVisible,
                  hoFullRepaintOnResize];

      with Columns.Add do
      begin
        Text := 'Login';
        CheckBox   := false;
        CheckType  := ctNone;
        Options    := [coEnabled, coParentBidiMode,
                       coParentColor, coResizable, coVisible,
                       coSmartResize];
        Width      := 150;
      end;

      for Item in TEpiEntryRights do
      begin
        with Columns.Add do
        begin
          Text       := EpiEntryRightCaption[Item];
          CheckBox   := false;
          CheckType  := ctNone;
          Options    := [coEnabled, coParentBidiMode,
                         coParentColor, coResizable, coVisible,
                         coSmartResize];
        end;
      end;

      MainColumn := 0;
      AutoSizeIndex := 0;
    end;

    Align := alClient;
    Parent := Self;

    OnAfterCellPaint := @UserVSTAfterCellPaint;
//    OnBeforeItemErase := @UserVSFBeforeItemErase;

    OnGetText := @UserVSTGetText;
    OnInitNode := @UserVSTInitNode;

    EndUpdate;
  end;
end;

destructor TUsersAccumulatedRightsFrame.Destroy;
begin
  if Assigned(Admin) then Admin.Users.UnRegisterOnChangeHook(@UsersEventHook);
  if Assigned(DataFile) then DataFile.GroupRights.UnRegisterOnChangeHook(@GroupRightsEventHook);

  inherited Destroy;
end;

end.

