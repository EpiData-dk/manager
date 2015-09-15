unit design_properties_groupassign_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, FileUtil, Forms, Controls, epiadmin, VirtualTrees,
  epirights, Graphics, StdCtrls, ExtCtrls, epidatafilerelations, epidatafiles,
  epicustombase;

type

  { TGroupsAssignFrame }

  TGroupsAssignFrame = class(TFrame)
    Button1: TButton;
    Panel1: TPanel;
    Panel2: TPanel;
  private
    FAdmin: TEpiAdmin;
    FGroupRights: TEpiGroupRights;
    procedure SetAdmin(AValue: TEpiAdmin);
    procedure SetGroupRights(AValue: TEpiGroupRights);
    procedure CopyParentRightsBtnClick(Sender: TObject);

    procedure AdminChange(const Sender: TEpiCustomBase;
      const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
    procedure GroupCaptionChange(const Sender: TEpiCustomBase;
      const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
    procedure GroupRightHook(const Sender: TEpiCustomBase;
      const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);


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
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property Admin: TEpiAdmin read FAdmin write SetAdmin;
    property DataFileRelation: TEpiMasterRelation read FDataFileRelation write SetDataFileRelation;
  end;

implementation

{$R *.lfm}

uses
  Themes, LCLType, admin_authenticator, epicustomrelations;

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

procedure TGroupsAssignFrame.AdminChange(const Sender: TEpiCustomBase;
  const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
begin
  if (Initiator is TEpiGroupRelation) and
     (EventGroup = eegRelations) and
     (TEpiCustomRelationEvent(EventType) = ecreAssignObject) and
     (Assigned(PEpiCustomRelationAssignObjectData(Data)^.ObjectData))
  then
    with PEpiCustomRelationAssignObjectData(Data)^ do
    begin
      TEpiGroup(ObjectData).Caption.RegisterOnChangeHook(@GroupCaptionChange, true);
      InitGroupVST;
    end;

  if (Initiator is TEpiGroups) and
     (EventGroup = eegCustomBase) and
     (TEpiCustomChangeEventType(EventType) = ecceDelItem)
  then
    InitGroupVST;


  if (Initiator is TEpiAdmin) and
     (EventGroup = eegCustomBase) and
     (TEpiCustomChangeEventType(EventType) = ecceDestroy)
  then
    begin
      Admin.UnRegisterOnChangeHook(@AdminChange);
      Admin := nil;
    end;
end;

procedure TGroupsAssignFrame.GroupCaptionChange(const Sender: TEpiCustomBase;
  const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
begin
  if not (Initiator is TEpiTranslatedText) then exit;
  if (EventGroup <> eegCustomBase) then exit;
  if (TEpiCustomChangeEventType(EventType) <> ecceText) then exit;

  FGroupRightsVst.Invalidate;
end;

procedure TGroupsAssignFrame.GroupRightHook(const Sender: TEpiCustomBase;
  const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
begin
  if not (Initiator is TEpiGroupRight) then exit;
  if (EventGroup <> eegRights) then exit;
  if (TEpiGroupRightEvent(EventType) <> egreSetEntryRights) then exit;

  FGroupRightsVst.Invalidate;
end;

procedure TGroupsAssignFrame.InitGroupVST;
begin
  if (csDestroying in ComponentState) then exit;

  if Assigned(FGroupRights) and
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

  FGroupRightsVst.Invalidate;
end;

procedure TGroupsAssignFrame.SetAdmin(AValue: TEpiAdmin);
var
  G: TEpiGroup;
begin
  FAdmin := AValue;

  if Assigned(Admin) then
  begin
    FAdmin.RegisterOnChangeHook(@AdminChange, true);

    for G in Admin.Groups do
      G.Caption.RegisterOnChangeHook(@GroupCaptionChange, true);
  end;

  InitGroupVST;
end;

procedure TGroupsAssignFrame.SetGroupRights(AValue: TEpiGroupRights);
begin
  if Assigned(FGroupRights) then
    FGroupRights.UnRegisterOnChangeHook(@GroupRightHook);

  FGroupRights := AValue;
  InitGroupVST;

  if Assigned(FGroupRights) then
    FGroupRights.RegisterOnChangeHook(@GroupRightHook, true);
end;

function TGroupsAssignFrame.CanCheck(Node: PVirtualNode; Right: TEpiEntryRight
  ): boolean;
var
  Group: TEpiGroup;
begin
  Group := RelationFromNode(Node).Group;
  result := (earViewData in Group.ManageRights) and
            (Group <> Admin.Admins);
end;

procedure TGroupsAssignFrame.NodeCheck(Node: PVirtualNode;
  Right: TEpiEntryRight; Value: Boolean);
var
//  CR: PCheckedRecord;
  G: TEpiGroup;
  GR: TEpiGroupRight;
begin
  if (not Assigned(Node)) then exit;
  if (not CanCheck(Node, Right)) then exit;

  G := RelationFromNode(Node).Group;
  GR := FGroupRights.GroupRightFromGroup(G);

  if Value then
    GR.EntryRights := GR.EntryRights + [Right]
  else
    GR.EntryRights := GR.EntryRights - [Right];

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
  GR: TEpiGroupRight;
begin
  GR := FGroupRights.GroupRightFromGroup(RelationFromNode(Node).Group);
  Result := Right in GR.EntryRights;
end;

function TGroupsAssignFrame.RelationFromNode(const Node: PVirtualNode
  ): TEpiGroupRelation;
begin
  Result := TEpiGroupRelation(FGroupRightsVst.GetNodeData(Node)^);
end;

procedure TGroupsAssignFrame.RelationToNode(const Node: PVirtualNode;
  const Relation: TEpiGroupRelation);
begin
  TEpiGroupRelation(FGroupRightsVst.GetNodeData(Node)^) := Relation;
  Relation.Group.AddCustomData(CHECKED_NODE_KEY, TObject(Node));
end;

procedure TGroupsAssignFrame.SetDataFileRelation(AValue: TEpiMasterRelation);
begin
  if FDataFileRelation = AValue then Exit;
  FDataFileRelation := AValue;

  SetGroupRights(FDataFileRelation.Datafile.GroupRights);

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
//var
//  CR: PCheckedRecord;
begin
//  CR := PCheckedRecord(FGroupRightsVst.GetNodeData(Node)^);
//  Dispose(CR);
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
  if RelationFromNode(Node).GroupRelations.Count > 0 then
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
//      FUserRightsVst.Invalidate;
    end;
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

    NodeDataSize := SizeOf(TEpiGroupRelation);

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

    Align := alClient;
    Parent := Panel1;

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

  Button1.OnClick := @CopyParentRightsBtnClick;
end;

destructor TGroupsAssignFrame.Destroy;
begin
  if Assigned(Admin) then
    Admin.UnRegisterOnChangeHook(@AdminChange);

  inherited Destroy;
end;

end.

