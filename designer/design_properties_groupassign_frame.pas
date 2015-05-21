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
    procedure SetGroupRights(AValue: TEpiGroupRights);
  { VST }
  private
    FVst: TVirtualStringTree;
    procedure GetGroupText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure InitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
  public
    constructor Create(TheOwner: TComponent); override;
    property Admin: TEpiAdmin read FAdmin write FAdmin;
    property GroupRights: TEpiGroupRights read FGroupRights write SetGroupRights;
  end;

implementation

{$R *.lfm}

{ TGroupsAssignFrame }

procedure TGroupsAssignFrame.SetGroupRights(AValue: TEpiGroupRights);
begin
  if FGroupRights = AValue then Exit;
  FGroupRights := AValue;

  FVst.RootNodeCount := 1;
end;

procedure TGroupsAssignFrame.GetGroupText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
begin
  case Column of
    0: CellText := 'G';
    1: CellText := 'ALL';
  end;
end;

procedure TGroupsAssignFrame.InitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  if (not Assigned(ParentNode)) then
  begin

  end else begin

  end;
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

    OnInitNode := @InitNode;
//    OnBeforeItemErase := @GroupBeforeItemErase;
//    OnChecked         := @GroupChecked;
//    OnChecking        := @GroupChecking;
    OnGetText := @GetGroupText;

    EndUpdate;
  end;
end;

end.

