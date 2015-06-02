unit design_properties_groupassign_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ValEdit, epiadmin, VirtualTrees,
  epirights, Graphics;

type

  { TGroupsAssignFrame }

  TGroupsAssignFrame = class(TFrame)
  private
    FAdmin: TEpiAdmin;
    FGroupRights: TEpiGroupRights;
    procedure SetAdmin(AValue: TEpiAdmin);
    procedure SetGroupRights(AValue: TEpiGroupRights);
  { VST }
  private
    FVst: TVirtualStringTree;

    function  RelationFromNode(Const Node: PVirtualNode): TEpiGroupRelation;
    procedure RelationToNode(Const Node: PVirtualNode; Const Relation: TEpiGroupRelation);

    procedure VSTAfterCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      const CellRect: TRect);
    procedure VSTBeforeItemErase(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect;
      var ItemColor: TColor; var EraseAction: TItemEraseAction);
    procedure VSTChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTChecking(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var NewState: TCheckState; var Allowed: Boolean);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure VSTInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var ChildCount: Cardinal);
    procedure VSTInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure VSTNodeClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
  public
    constructor Create(TheOwner: TComponent); override;
    property Admin: TEpiAdmin read FAdmin write SetAdmin;
    property GroupRights: TEpiGroupRights read FGroupRights write SetGroupRights;
  end;

implementation

{$R *.lfm}

uses
  CheckBoxThemed, Themes;

{ TGroupsAssignFrame }

procedure TGroupsAssignFrame.SetAdmin(AValue: TEpiAdmin);
begin
  if FAdmin = AValue then Exit;
  FAdmin := AValue;

  if Assigned(GroupRights) and
     (FVst.RootNodeCount = 0)
  then
    FVst.RootNodeCount := 1;
end;

procedure TGroupsAssignFrame.SetGroupRights(AValue: TEpiGroupRights);
begin
  if FGroupRights = AValue then Exit;
  FGroupRights := AValue;

  if Assigned(Admin) and
     (FVst.RootNodeCount = 0)
  then
    FVst.RootNodeCount := 1;
end;

function TGroupsAssignFrame.RelationFromNode(const Node: PVirtualNode
  ): TEpiGroupRelation;
begin
  Result := TEpiGroupRelation(FVst.GetNodeData(Node)^);
end;

procedure TGroupsAssignFrame.RelationToNode(const Node: PVirtualNode;
  const Relation: TEpiGroupRelation);
begin
  TEpiGroupRelation(FVst.GetNodeData(Node)^) := Relation;
end;

procedure TGroupsAssignFrame.VSTAfterCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const CellRect: TRect);
var
  Details: TThemedElementDetails;
  X, Y: Integer;
  R: TRect;
  G: TEpiGroup;
  GR: TEpiGroupRight;
  Checked: Boolean;

const
  CHECKBOX_SIZE = 13;

begin
  if Column = 0 then exit;

  { Paint Check boxes by ourselves - since VT's only allow for one checkbox column }
  G  := RelationFromNode(Node).Group;
  GR := GroupRights.GroupRightFromGroup(G);
  Checked := false;

  if (Assigned(GR)) then
    Checked := (TEpiEntryRight(Column - 1) in GR.EntryRights);

  if Checked then
    Details := ThemeServices.GetElementDetails(tbCheckBoxCheckedNormal)
  else
    Details := ThemeServices.GetElementDetails(tbCheckBoxUncheckedNormal);

  X := CellRect.Left + (CellRect.Right - CellRect.Left - CHECKBOX_SIZE) div 2;
  Y := CellRect.Top + (CellRect.Bottom - CellRect.Top - CHECKBOX_SIZE) div 2;
  R := Rect(X, Y, X + CHECKBOX_SIZE, Y + CHECKBOX_SIZE);
  ThemeServices.DrawElement(TargetCanvas.Handle, Details, R);
end;

procedure TGroupsAssignFrame.VSTBeforeItemErase(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect;
  var ItemColor: TColor; var EraseAction: TItemEraseAction);
begin
  //
end;

procedure TGroupsAssignFrame.VSTChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin

end;

procedure TGroupsAssignFrame.VSTChecking(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
begin

end;

procedure TGroupsAssignFrame.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
begin
  case Column of
    0: CellText := RelationFromNode(Node).Group.Caption.Text;
  else
    CellText := '';
  end;
end;

procedure TGroupsAssignFrame.VSTInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);
begin
  ChildCount := RelationFromNode(Node).GroupRelations.Count;
end;

procedure TGroupsAssignFrame.VSTInitNode(Sender: TBaseVirtualTree; ParentNode,
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

  Sender.CheckType[node] := ctCheckBox;

  if Assigned(GroupRights.GroupRightFromGroup(Relation.Group)) then
    Sender.CheckState[Node] := csCheckedNormal;
end;

procedure TGroupsAssignFrame.VSTNodeClick(Sender: TBaseVirtualTree;
  const HitInfo: THitInfo);
var
  Idx: Integer;
  G: TEpiGroup;
  GR: TEpiGroupRight;
  Item: TEpiEntryRight;
begin
  if (HitInfo.HitColumn = 0) then exit;
  if (hiNowhere in HitInfo.HitPositions) then exit;

  G := RelationFromNode(HitInfo.HitNode).Group;
  GR := GroupRights.GroupRightFromGroup(G);
  if not Assigned(GR) then
    begin
      GR := GroupRights.NewGroupRight;
      GR.Group := G;
    end;

  Item := TEpiEntryRight(HitInfo.HitColumn - 1);
  GR.EntryRights := GR.EntryRights + [Item];

  Sender.InvalidateNode(HitInfo.HitNode);
end;

constructor TGroupsAssignFrame.Create(TheOwner: TComponent);
var
  Item: TEpiEntryRight;
begin
  inherited Create(TheOwner);

  FVst := TVirtualStringTree.Create(self);
  with FVst do
  begin
    BeginUpdate;

    NodeDataSize := SizeOf(Pointer);

    with TreeOptions do
    begin
      AnimationOptions := [];
      AutoOptions      := [];
      MiscOptions      := [toCheckSupport, toFullRepaintOnResize, toGridExtensions,
                           toWheelPanning];
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
        CheckBox   := True;
        CheckState := csUncheckedNormal;
        CheckType  := ctCheckBox;
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
          Options    := [coAllowClick, coEnabled, coParentBidiMode,
                         coParentColor, coResizable, coVisible,
                         coSmartResize, coAllowFocus];
        end;
      end;

      MainColumn := 0;
      AutoSizeIndex := 0;
    end;

    Align := alClient;
    Parent := Self;

    OnAfterCellPaint  := @VSTAfterCellPaint;
    OnBeforeItemErase := @VSTBeforeItemErase;

    OnChecked         := @VSTChecked;
    OnChecking        := @VSTChecking;

    OnInitChildren    := @VSTInitChildren;
    OnInitNode        := @VSTInitNode;
    OnGetText         := @VSTGetText;

    OnNodeClick       := @VSTNodeClick;

    EndUpdate;
  end;
end;

end.

