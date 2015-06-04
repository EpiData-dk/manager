unit design_properties_groupassign_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, FileUtil, Forms, Controls, epiadmin,
  VirtualTrees, epirights, Graphics, epidatafilerelations, epidatafiles;

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
    FDataFileRelation: TEpiMasterRelation;
    FVst: TVirtualStringTree;
    FCheckBoxWidth: Integer;
    FCheckBoxHeight: Integer;

    function  CanCheck(Node: PVirtualNode; Right: TEpiEntryRight): boolean;
    procedure NodeCheck(Node: PVirtualNode; Right: TEpiEntryRight; Value: Boolean);
    function  NodeChecked(Node: PVirtualNode; Right: TEpiEntryRight): boolean;
    function  RelationFromNode(Const Node: PVirtualNode): TEpiGroupRelation;
    procedure RelationToNode(Const Node: PVirtualNode; Const Relation: TEpiGroupRelation);
    procedure SetDataFileRelation(AValue: TEpiMasterRelation);

    procedure VSTAfterCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      const CellRect: TRect);
    procedure VSTBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure VSTChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTChecking(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var NewState: TCheckState; var Allowed: Boolean);
    procedure VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure VSTInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var ChildCount: Cardinal);
    procedure VSTInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure VSTNodeClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
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
  Themes;

type
  TCheckedRecord = record
    GroupRelation: TEpiGroupRelation;
    CreateChecked: boolean;
    ReadChecked:   boolean;
    UpdateChecked: boolean;
    DeleteChecked: boolean;
  end;
  PCheckedRecord = ^TCheckedRecord;


{ TGroupsAssignFrame }

procedure TGroupsAssignFrame.SetAdmin(AValue: TEpiAdmin);
begin
  FAdmin := AValue;

  if Assigned(GroupRights) and
     (FVst.RootNodeCount = 0)
  then
    FVst.RootNodeCount := 1
  else
    FVst.ReinitNode(nil, true);

  FVst.Invalidate;
end;

procedure TGroupsAssignFrame.SetGroupRights(AValue: TEpiGroupRights);
begin
  FGroupRights := AValue;

  if Assigned(Admin) and
     (FVst.RootNodeCount = 0)
  then
    FVst.RootNodeCount := 1
  else
    FVst.ReinitNode(nil, true);

  FVst.Invalidate;
end;

function TGroupsAssignFrame.CanCheck(Node: PVirtualNode; Right: TEpiEntryRight
  ): boolean;
begin
  // TODO!
  result := true;
end;

procedure TGroupsAssignFrame.NodeCheck(Node: PVirtualNode;
  Right: TEpiEntryRight; Value: Boolean);
var
  CR: PCheckedRecord;
begin
  if (not Assigned(Node)) then exit;
  if (not CanCheck(Node, Right)) then exit;

  CR := PCheckedRecord(FVst.GetNodeData(Node)^);

  case Right of
    eerRead:   CR^.ReadChecked   := Value;
    eerUpdate: CR^.UpdateChecked := Value;
    eerCreate: CR^.CreateChecked := Value;
    eerDelete: CR^.DeleteChecked := Value;
  end;

  if Value then
    FVst.CheckState[Node] := csCheckedNormal;

  if (Right > eerRead) and
     (Value)
  then
    NodeCheck(Node, Pred(Right), Value);

  if (Right < eerDelete) and
     (not Value)
  then
    NodeCheck(Node, Succ(Right), Value);

  FVst.InvalidateNode(Node);

  if Value
  then
    NodeCheck(FVst.NodeParent[Node], Right, Value)
  else
    for Node in FVst.ChildNodes(Node) do
      NodeCheck(Node, Right, Value)
end;

function TGroupsAssignFrame.NodeChecked(Node: PVirtualNode;
  Right: TEpiEntryRight): boolean;
var
  CR: PCheckedRecord;
begin
  CR := PCheckedRecord(FVst.GetNodeData(Node)^);

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
  Result := PCheckedRecord(FVst.GetNodeData(Node)^)^.GroupRelation;
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

  PCheckedRecord(FVst.GetNodeData(Node)^) := CheckedRecord;
end;

procedure TGroupsAssignFrame.SetDataFileRelation(AValue: TEpiMasterRelation);
begin
  if FDataFileRelation = AValue then Exit;
  FDataFileRelation := AValue;
end;

procedure TGroupsAssignFrame.VSTAfterCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const CellRect: TRect);
var
  Details: TThemedElementDetails;
  X, Y: Integer;
  R: TRect;
  Checked: Boolean;
  Sz: TSize;

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

procedure TGroupsAssignFrame.VSTBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  Detail: TEpiDetailRelation;
  MasterDF: TEpiDataFile;
  MasterGRs: TEpiGroupRights;
  Item: TEpiEntryRight;
  GR: TEpiGroupRight;
begin
  //
  if (not DataFileRelation.InheritsFrom(TEpiDetailRelation)) then
    Exit;

  if (CellPaintMode = cpmGetContentMargin) then Exit;

  Detail := TEpiDetailRelation(DataFileRelation);
  MasterDF := Detail.MasterRelation.Datafile;
  MasterGRs := MasterDF.GroupRights;

  GR := MasterGRs.GroupRightFromGroup(RelationFromNode(Node).Group);
  Item := TEpiEntryRight(Column - 1);

  if (Assigned(GR)) and
     (Item in GR.EntryRights)
  then
    begin
      TargetCanvas.Brush.Color := clInactiveCaption;
      TargetCanvas.FillRect(CellRect);
    end;
end;

procedure TGroupsAssignFrame.VSTChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Item: TEpiEntryRight;
begin
//  if Sender.CheckState[Node] = csUncheckedNormal then
//    for Item in TEpiEntryRights do
//      NodeCheck(Node, Item, false);
end;

procedure TGroupsAssignFrame.VSTChecking(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
begin

end;

procedure TGroupsAssignFrame.VSTFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  CR: PCheckedRecord;
begin
  CR := PCheckedRecord(FVst.GetNodeData(Node)^);
  Dispose(CR);
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
end;

procedure TGroupsAssignFrame.VSTNodeClick(Sender: TBaseVirtualTree;
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
    NodeCheck(Node, Item, not NodeChecked(Node, Item));
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

    NodeDataSize := SizeOf(PCheckedRecord);

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
    OnBeforeCellPaint := @VSTBeforeCellPaint;

    OnChecked         := @VSTChecked;
    OnChecking        := @VSTChecking;

    OnFreeNode        := @VSTFreeNode;
    OnGetText         := @VSTGetText;
    OnInitChildren    := @VSTInitChildren;
    OnInitNode        := @VSTInitNode;

    OnNodeClick       := @VSTNodeClick;

    EndUpdate;
  end;
end;

procedure TGroupsAssignFrame.ApplyChanges;
var
  Node: PVirtualNode;
  CR: PCheckedRecord;
  GR: TEpiGroupRight;
  Rights: TEpiEntryRights;
begin
  for Node in FVst.Nodes() do
    begin
      Rights := [];
      CR := PCheckedRecord(FVst.GetNodeData(Node)^);

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

