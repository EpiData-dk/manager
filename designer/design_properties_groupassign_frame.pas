unit design_properties_groupassign_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, FileUtil, Forms, Controls, epiadmin, VirtualTrees,
  epirights, Graphics, StdCtrls, ExtCtrls, epidatafilerelations, epidatafiles;

type

  { TGroupsAssignFrame }

  TGroupsAssignFrame = class(TFrame)
    Button1: TButton;
    Panel1: TPanel;
    Splitter1: TSplitter;
    Panel2: TPanel;
  private
    FAdmin: TEpiAdmin;
    FGroupRights: TEpiGroupRights;
    procedure SetAdmin(AValue: TEpiAdmin);
    procedure SetGroupRights(AValue: TEpiGroupRights);
    procedure CopyParentRightsBtnClick(Sender: TObject);

  { GroupRightsVST }
  private
    FDataFileRelation: TEpiMasterRelation;
    FGroupRightsVst: TVirtualStringTree;

    procedure InitGroupVST;

    function  CanCheck(Node: PVirtualNode; Right: TEpiEntryRight): boolean;
    procedure NodeCheck(Node: PVirtualNode; Right: TEpiEntryRight; Value: Boolean);
    function  NodeChecked(Node: PVirtualNode; Right: TEpiEntryRight): boolean;
    function  RelationFromNode(Const Node: PVirtualNode): TEpiGroupRelation;
    procedure RelationToNode(Const Node: PVirtualNode; Const Relation: TEpiGroupRelation);
    procedure SetDataFileRelation(AValue: TEpiMasterRelation);

    procedure GroupVSTAfterCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      const CellRect: TRect);
    procedure GroupVSTBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure GroupVSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure GroupVSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure GroupVSTInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var ChildCount: Cardinal);
    procedure GroupVSTInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure GroupVSTKeyAction(Sender: TBaseVirtualTree; var CharCode: Word;
      var Shift: TShiftState; var DoDefault: Boolean);
    procedure GroupVSTNodeClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);

  { UserRightsVST }
  private
    FUserRightsVst: TVirtualStringTree;

    procedure InitUserVST;

    procedure UserVSTAfterCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      const CellRect: TRect);
    procedure UserVSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure UserVSTInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure ApplyChanges;
    function ValidateChanges: boolean;
    property Admin: TEpiAdmin read FAdmin write SetAdmin;
    property DataFileRelation: TEpiMasterRelation read FDataFileRelation write SetDataFileRelation;
    property GroupRights: TEpiGroupRights read FGroupRights write SetGroupRights;
  end;

implementation

{$R *.lfm}

uses
  Themes, LCLType, admin_authenticator;

type
  TCheckedRecord = record
    GroupRelation: TEpiGroupRelation;
    CreateChecked: boolean;
    ReadChecked:   boolean;
    UpdateChecked: boolean;
    DeleteChecked: boolean;
  end;
  PCheckedRecord = ^TCheckedRecord;

const
  CHECKED_NODE_KEY = 'CHECKED_NODE_KEY';


{ TGroupsAssignFrame }

procedure TGroupsAssignFrame.CopyParentRightsBtnClick(Sender: TObject);
var
  Detail: TEpiDetailRelation;
  MasterDF: TEpiDataFile;
  MasterGRs: TEpiGroupRights;
  GR: TEpiGroupRight;
  Item: TEpiEntryRight;
  Node: PVirtualNode;

begin
  Detail := TEpiDetailRelation(DataFileRelation);
  MasterDF := Detail.MasterRelation.Datafile;
  MasterGRs := MasterDF.GroupRights;

  FGroupRightsVst.BeginUpdate;

  // This unchecks all!
  NodeCheck(FGroupRightsVst.GetFirst(), eerRead, false);

  for Node in FGroupRightsVst.Nodes() do
    begin
      GR := MasterGRs.GroupRightFromGroup(RelationFromNode(Node).Group);

      if (Assigned(GR))
      then
        for Item in GR.EntryRights do
          NodeCheck(Node, Item, true);
    end;

  FGroupRightsVst.EndUpdate;
end;

procedure TGroupsAssignFrame.InitGroupVST;
begin
  if Assigned(GroupRights) and
     Assigned(Admin)
  then
    begin
      if FGroupRightsVst.RootNodeCount > 0 then
        FGroupRightsVst.ReinitNode(nil, true)
      else
        FGroupRightsVst.RootNodeCount := 1;
    end
  else
    FGroupRightsVst.RootNodeCount := 0;

  InitUserVST;
  FGroupRightsVst.Invalidate;
end;

procedure TGroupsAssignFrame.SetAdmin(AValue: TEpiAdmin);
begin
  FAdmin := AValue;
  InitGroupVST;

{  if Assigned(GroupRights) and
     (FGroupRightsVst.RootNodeCount = 0)
  then
    FGroupRightsVst.RootNodeCount := 1
  else
    FGroupRightsVst.ReinitNode(nil, true);

  FGroupRightsVst.Invalidate;  }
end;

procedure TGroupsAssignFrame.SetGroupRights(AValue: TEpiGroupRights);
begin
  FGroupRights := AValue;
  InitGroupVST;

{  if Assigned(Admin) and
     (FGroupRightsVst.RootNodeCount = 0)
  then
    FGroupRightsVst.RootNodeCount := 1
  else
    FGroupRightsVst.ReinitNode(nil, true);

  FGroupRightsVst.Invalidate;}
end;

function TGroupsAssignFrame.CanCheck(Node: PVirtualNode; Right: TEpiEntryRight
  ): boolean;
var
  Group: TEpiGroup;
begin
  Group := RelationFromNode(Node).Group;
  result := (earViewData in Group.ManageRights);
end;

procedure TGroupsAssignFrame.NodeCheck(Node: PVirtualNode;
  Right: TEpiEntryRight; Value: Boolean);
var
  CR: PCheckedRecord;
begin
  if (not Assigned(Node)) then exit;
  if (not CanCheck(Node, Right)) then exit;

  CR := PCheckedRecord(FGroupRightsVst.GetNodeData(Node)^);

  case Right of
    eerRead:   CR^.ReadChecked   := Value;
    eerUpdate: CR^.UpdateChecked := Value;
    eerCreate: CR^.CreateChecked := Value;
    eerDelete: CR^.DeleteChecked := Value;
  end;

//  if Value then
//    FGroupRightsVst.CheckState[Node] := csCheckedNormal;

  if (Right > eerRead) and
     (Value)
  then
    NodeCheck(Node, Pred(Right), Value);

  if (Right < eerDelete) and
     (not Value)
  then
    NodeCheck(Node, Succ(Right), Value);

  FGroupRightsVst.InvalidateNode(Node);

  if Value
  then
    NodeCheck(FGroupRightsVst.NodeParent[Node], Right, Value)
  else
    for Node in FGroupRightsVst.ChildNodes(Node) do
      NodeCheck(Node, Right, Value)
end;

function TGroupsAssignFrame.NodeChecked(Node: PVirtualNode;
  Right: TEpiEntryRight): boolean;
var
  CR: PCheckedRecord;
begin
  CR := PCheckedRecord(FGroupRightsVst.GetNodeData(Node)^);

  case Right of
    eerRead:   Result := CR^.ReadChecked;
    eerUpdate: Result := CR^.UpdateChecked;
    eerCreate: Result := CR^.CreateChecked;
    eerDelete: Result := CR^.DeleteChecked;
  end;
end;

function TGroupsAssignFrame.RelationFromNode(const Node: PVirtualNode
  ): TEpiGroupRelation;
begin
  Result := PCheckedRecord(FGroupRightsVst.GetNodeData(Node)^)^.GroupRelation;
end;

procedure TGroupsAssignFrame.RelationToNode(const Node: PVirtualNode;
  const Relation: TEpiGroupRelation);
var
  CheckedRecord: PCheckedRecord;
  GR: TEpiGroupRight;
begin
  CheckedRecord := New(PCheckedRecord);
  GR := GroupRights.GroupRightFromGroup(Relation.Group);

  with CheckedRecord^ do
  begin
    GroupRelation := Relation;
    ReadChecked   := Assigned(GR) and (eerRead   in GR.EntryRights);
    UpdateChecked := Assigned(GR) and (eerUpdate in GR.EntryRights);
    CreateChecked := Assigned(GR) and (eerCreate in GR.EntryRights);
    DeleteChecked := Assigned(GR) and (eerDelete in GR.EntryRights);
  end;

  PCheckedRecord(FGroupRightsVst.GetNodeData(Node)^) := CheckedRecord;
  Relation.AddCustomData(CHECKED_NODE_KEY, TObject(Node));
end;

procedure TGroupsAssignFrame.SetDataFileRelation(AValue: TEpiMasterRelation);
begin
  if FDataFileRelation = AValue then Exit;
  FDataFileRelation := AValue;

  Button1.Enabled := FDataFileRelation.InheritsFrom(TEpiDetailRelation);
end;

procedure TGroupsAssignFrame.GroupVSTAfterCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const CellRect: TRect);
var
  Details: TThemedElementDetails;
  X, Y: Integer;
  R: TRect;
  Checked: Boolean;
  Sz: TSize;
  FCheckBoxWidth: Integer;
  FCheckBoxHeight: Integer;
begin
  if Column = 0 then exit;

  { Paint Check boxes by ourselves - since VT's only allow for one checkbox column }
  Checked := NodeChecked(Node, TEpiEntryRight(Column - 1));

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

procedure TGroupsAssignFrame.GroupVSTBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  Detail: TEpiDetailRelation;
  MasterDF: TEpiDataFile;
  MasterGRs: TEpiGroupRights;
  Item: TEpiEntryRight;
  GR: TEpiGroupRight;
  Group: TEpiGroup;
begin
  // A group that has no VIEW rights on data, cannot be assigned a dataform
  // entry right. Paint the cells accordingly.
  Group := RelationFromNode(Node).Group;
  if not (earViewData in Group.ManageRights) then
    begin
      TargetCanvas.Brush.Color := clMedGray;
      TargetCanvas.FillRect(CellRect);
      Exit;
    end;

  if (not DataFileRelation.InheritsFrom(TEpiDetailRelation)) or
     (Column = 0)
  then
    Exit;

  if (CellPaintMode = cpmGetContentMargin) then Exit;

  Detail := TEpiDetailRelation(DataFileRelation);
  MasterDF := Detail.MasterRelation.Datafile;
  MasterGRs := MasterDF.GroupRights;

  GR := MasterGRs.GroupRightFromGroup(Group);
  Item := TEpiEntryRight(Column - 1);

  if (Assigned(GR)) and
     (Item in GR.EntryRights)
  then
    begin
      TargetCanvas.Brush.Color := clLtGray;
      TargetCanvas.FillRect(CellRect);
    end;
end;

procedure TGroupsAssignFrame.GroupVSTFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  CR: PCheckedRecord;
begin
  CR := PCheckedRecord(FGroupRightsVst.GetNodeData(Node)^);
  Dispose(CR);
end;

procedure TGroupsAssignFrame.GroupVSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
begin
  case Column of
    0: CellText := RelationFromNode(Node).Group.Caption.Text;
  else
    CellText := '';
  end;
end;

procedure TGroupsAssignFrame.GroupVSTInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);
begin
  ChildCount := RelationFromNode(Node).GroupRelations.Count;
end;

procedure TGroupsAssignFrame.GroupVSTInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  Relation: TEpiGroupRelation;
begin
  if (not Assigned(ParentNode)) then
    Relation := Admin.AdminRelation
  else
    Relation := RelationFromNode(ParentNode).GroupRelation[Node^.Index];

  RelationToNode(Node, Relation);

  Include(InitialStates, ivsExpanded);
  if RelationFromNode(Node).GroupRelations.Count > 0 then;
    Include(InitialStates, ivsHasChildren);
end;

procedure TGroupsAssignFrame.GroupVSTKeyAction(Sender: TBaseVirtualTree;
  var CharCode: Word; var Shift: TShiftState; var DoDefault: Boolean);
var
  Item: TEpiEntryRight;
begin
  if (FGroupRightsVst.FocusedColumn <= 0) or
     (FGroupRightsVst.FocusedColumn > (Integer(High(TEpiEntryRight))) + 1) or
     (not Assigned(FGroupRightsVst.FocusedNode))
  then
    Exit;

  if (CharCode = VK_SPACE) and (Shift = []) then
    begin
      DoDefault := false;
      Item := TEpiEntryRight(FGroupRightsVst.FocusedColumn - 1);

      if CanCheck(FGroupRightsVst.FocusedNode, Item) then
        NodeCheck(FGroupRightsVst.FocusedNode, Item, not NodeChecked(FGroupRightsVst.FocusedNode, Item));
    end;
end;

procedure TGroupsAssignFrame.GroupVSTNodeClick(Sender: TBaseVirtualTree;
  const HitInfo: THitInfo);
var
  Item: TEpiEntryRight;
  Node: PVirtualNode;
begin
  if (HitInfo.HitColumn = 0) then exit;
  if (hiNowhere in HitInfo.HitPositions) then exit;

  Node := HitInfo.HitNode;
  Item := TEpiEntryRight(HitInfo.HitColumn - 1);

  if CanCheck(Node, Item) then
    begin
      NodeCheck(Node, Item, not NodeChecked(Node, Item));
      FUserRightsVst.Invalidate;
    end;
end;

procedure TGroupsAssignFrame.InitUserVST;
begin
  if Assigned(Admin) and
     (FGroupRightsVst.RootNodeCount > 0)
  then
    FUserRightsVst.RootNodeCount := Admin.Users.Count
  else
    FUserRightsVst.RootNodeCount := 0;

  FUserRightsVst.Invalidate;
end;

procedure TGroupsAssignFrame.UserVSTAfterCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const CellRect: TRect);
var
  User: TEpiUser;
  Checked: Boolean;
  Details: TThemedElementDetails;
  Sz: TSize;
  FCheckBoxHeight: LongInt;
  FCheckBoxWidth: LongInt;
  X: Integer;
  Y: Integer;
  R: TRect;
begin
  if Column = 0 then exit;

  { Paint Check boxes by ourselves - since VT's only allow for one checkbox column }
  User := TEpiUser(Sender.GetNodeData(Node)^);
  //TODO: Denne function skal omhandle data i FGroupRightsVst / PCheckedRecord i stedet!
  Checked := (TEpiEntryRight(Column - 1) in  Authenticator.UserEntryRights(User, DataFileRelation.Datafile));

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

procedure TGroupsAssignFrame.UserVSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
begin
  if Column <> 0 then Exit;

  CellText := TEpiUser(Sender.GetNodeData(Node)^).Login;
end;

procedure TGroupsAssignFrame.UserVSTInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  TEpiUser(Sender.GetNodeData(Node)^) := Admin.Users[Node^.Index];
end;

constructor TGroupsAssignFrame.Create(TheOwner: TComponent);
var
  Item: TEpiEntryRight;
begin
  inherited Create(TheOwner);

  FGroupRightsVst := TVirtualStringTree.Create(self);
  with FGroupRightsVst do
  begin
    BeginUpdate;

    NodeDataSize := SizeOf(PCheckedRecord);

    with TreeOptions do
    begin
      AnimationOptions := [];
      AutoOptions      := [];
      MiscOptions      := [toFullRepaintOnResize, toGridExtensions, toWheelPanning];
      PaintOptions     := [toShowButtons, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages];
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
        CheckBox   := false;
        CheckState := csUncheckedNormal;
        CheckType  := ctNone;
        Options    := [coAllowClick, coEnabled, coParentBidiMode,
                       coParentColor, coResizable, coVisible,
                       coSmartResize, coAllowFocus];
      end;

      for Item in TEpiEntryRights do
      begin
        with Columns.Add do
        begin
          Text       := EpiEntryRightCaption[Item];
          CheckBox   := false;
          CheckType  := ctNone;
          Options    := [coAllowClick, coEnabled, coParentBidiMode,
                         coParentColor, coResizable, coVisible,
                         coSmartResize, coAllowFocus];
        end;
      end;

      MainColumn := 0;
      AutoSizeIndex := 0;
    end;

    Parent := Panel1;

    AnchorAsAlign(alTop,   0);
    AnchorToNeighbour(akBottom, 10, Button1);

    OnAfterCellPaint  := @GroupVSTAfterCellPaint;
    OnBeforeCellPaint := @GroupVSTBeforeCellPaint;

    OnFreeNode        := @GroupVSTFreeNode;
    OnGetText         := @GroupVSTGetText;
    OnInitChildren    := @GroupVSTInitChildren;
    OnInitNode        := @GroupVSTInitNode;

    OnKeyAction       := @GroupVSTKeyAction;

    OnNodeClick       := @GroupVSTNodeClick;

    EndUpdate;
  end;

  FUserRightsVst := TVirtualStringTree.Create(self);
  with FUserRightsVst do
  begin
    BeginUpdate;

    NodeDataSize := SizeOf(TEpiUser);

    with TreeOptions do
    begin
      AnimationOptions := [];
      AutoOptions      := [];
      MiscOptions      := [toFullRepaintOnResize, toGridExtensions, toWheelPanning];
      PaintOptions     := [toShowButtons, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages];
      SelectionOptions := [];
      StringOptions    := [];
    end;

    with Header do
    begin
      Options := [hoAutoResize, hoColumnResize, hoDblClickResize, hoVisible,
                  hoFullRepaintOnResize];

      with Columns.Add do
      begin
        Text := 'User';
        CheckBox   := false;
        CheckState := csUncheckedNormal;
        CheckType  := ctNone;
        Options    := [coEnabled, coParentBidiMode,
                       coParentColor, coResizable, coVisible,
                       coSmartResize, coAllowFocus];
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
                         coSmartResize, coAllowFocus];
        end;
      end;

      MainColumn := 0;
      AutoSizeIndex := 0;
    end;

    Parent := Panel2;

    AnchorAsAlign(alBottom, 0);
    AnchorParallel(akTop, 5, Panel2);

    OnAfterCellPaint := @UserVSTAfterCellPaint;
//    OnBeforeCellPaint := @GroupVSTBeforeCellPaint;

//    OnFreeNode        := @GroupVSTFreeNode;
    OnGetText := @UserVSTGetText;
//    OnInitChildren    := @GroupVSTInitChildren;
    OnInitNode := @UserVSTInitNode;

//    OnKeyAction       := @GroupVSTKeyAction;

//    OnNodeClick       := @GroupVSTNodeClick;

    EndUpdate;
  end;


  Button1.OnClick := @CopyParentRightsBtnClick;
end;

procedure TGroupsAssignFrame.ApplyChanges;
var
  Node: PVirtualNode;
  CR: PCheckedRecord;
  GR: TEpiGroupRight;
  Rights: TEpiEntryRights;
begin
  for Node in FGroupRightsVst.Nodes() do
    begin
      Rights := [];
      CR := PCheckedRecord(FGroupRightsVst.GetNodeData(Node)^);

      if CR^.ReadChecked   then Include(Rights, eerRead);
      if CR^.UpdateChecked then Include(Rights, eerUpdate);
      if CR^.CreateChecked then Include(Rights, eerCreate);
      if CR^.DeleteChecked then Include(Rights, eerDelete);

      GR := TEpiGroupRight(GroupRights.GroupRightFromGroup(CR^.GroupRelation.Group));

      if (Rights = []) and
         (Assigned(GR))
      then
        GR.Free;

      if (Rights <> [])
      then
        begin
          if (not Assigned(GR)) then
            begin
              GR := GroupRights.NewGroupRight;
              GR.Group := CR^.GroupRelation.Group;
            end;

          GR.EntryRights := Rights;
        end;
    end;
end;

function TGroupsAssignFrame.ValidateChanges: boolean;
begin
  result := true;
end;

end.

