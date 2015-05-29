unit design_properties_groupassign_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ValEdit, epiadmin, VirtualTrees,
  epirights;

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

    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure VSTInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var ChildCount: Cardinal);
    procedure VSTInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
  public
    constructor Create(TheOwner: TComponent); override;
    property Admin: TEpiAdmin read FAdmin write SetAdmin;
    property GroupRights: TEpiGroupRights read FGroupRights write SetGroupRights;
  end;

implementation

{$R *.lfm}

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

procedure TGroupsAssignFrame.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  GR: TEpiGroupRight;
begin
  case Column of
    0: CellText := RelationFromNode(Node).Group.Caption.Text;
    1: begin
         GR := GroupRights.GroupRightFromGroup(RelationFromNode(Node).Group);
//         if Assigned(GR) then
//           CellText := Write(S, GR.EntryRights);
         CellText := 'test';
       end;
  end;
end;

procedure TGroupsAssignFrame.VSTInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);
begin
  ChildCount := RelationFromNode(Node).GroupRelations.Count;
end;

procedure TGroupsAssignFrame.VSTInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  if (not Assigned(ParentNode)) then
  begin
    RelationToNode(Node, Admin.AdminRelation);
  end else begin
    RelationToNode(Node, RelationFromNode(ParentNode).GroupRelation[Node^.Index]);
  end;

  Include(InitialStates, ivsExpanded);
  if RelationFromNode(Node).GroupRelations.Count > 0 then;
    Include(InitialStates, ivsHasChildren);
end;

constructor TGroupsAssignFrame.Create(TheOwner: TComponent);
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
        Width      := 150;
      end;

      with Columns.Add do
      begin
        Text := 'Rights';
        CheckBox   := false;
        CheckState := csUncheckedNormal;
        CheckType  := ctNone;
        Options    := [coAllowClick, coEnabled, coParentBidiMode,
                       coParentColor, coResizable, coVisible,
                       coSmartResize, coAllowFocus];
      end;

      MainColumn := 0;
      AutoSizeIndex := 1;
    end;

    Align := alClient;
    Parent := Self;

    OnInitChildren := @VSTInitChildren;
    OnInitNode := @VSTInitNode;
//    OnBeforeItemErase := @GroupBeforeItemErase;
//    OnChecked         := @GroupChecked;
//    OnChecking        := @GroupChecking;
    OnGetText := @VSTGetText;

    EndUpdate;
  end;
end;

end.

