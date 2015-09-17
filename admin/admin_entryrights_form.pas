unit admin_entryrights_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, ComCtrls, epiv_projecttreeview_frame, epidocument,
  epicustombase, design_properties_groupassign_frame, epidatafilerelations,
  VirtualTrees;

type

  { TDefineEntryRightsForm }

  TDefineEntryRightsForm = class(TForm)
    Panel4: TPanel;
    BitBtn1: TBitBtn;
    Panel2: TPanel;
    Splitter1: TSplitter;
    Panel3: TPanel;
    Panel6: TPanel;
    Label4: TLabel;
    Panel7: TPanel;
    Label2: TLabel;
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure DataFileCaptionChange(const Sender: TEpiCustomBase;
      const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
    procedure RelationChange(const Sender: TEpiCustomBase;
      const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
    procedure AdminResetHook(const Sender: TEpiCustomBase;
      const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
  private
    FDocument: TEpiDocument;
    procedure SetDocument(AValue: TEpiDocument);
    procedure AddHooks;
    procedure RemoveHooks;


  { Project Tree View }
  private
    FDataFormsVST: TVirtualStringTree;
    procedure InitDataFormTree;
    function RelationFromNode(Node: PVirtualNode): TEpiMasterRelation;
    procedure RelationToNode(Node: PVirtualNode; Relation: TEpiMasterRelation);

    { Events }
    procedure DataformGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure DataformFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure DataformInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure DataformInitChildren(Sender: TBaseVirtualTree;
      Node: PVirtualNode; var ChildCount: Cardinal);

  { Group Rights }
  private
    FGroupRightsFrame: TGroupsAssignFrame;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property Document: TEpiDocument read FDocument write SetDocument;
  end;

procedure ShowDefineEntryRightsForm(Owner: TComponent; Document: TEpiDocument);

implementation

{$R *.lfm}

uses
  Themes, epiadmin, settings2, settings2_var, epicustomrelations;

var
  DefineEntryRightsForm: TDefineEntryRightsForm = nil;

procedure ShowDefineEntryRightsForm(Owner: TComponent; Document: TEpiDocument);
begin
  if (not Assigned(DefineEntryRightsForm)) then
    DefineEntryRightsForm := TDefineEntryRightsForm.Create(Owner);

  DefineEntryRightsForm.Document := Document;
  DefineEntryRightsForm.Show;
end;

{ TDefineEntryRightsForm }

procedure TDefineEntryRightsForm.FormShow(Sender: TObject);
begin
  if ManagerSettings.SaveWindowPositions then
  begin
    LoadFormPosition(Self, Self.ClassName);
    LoadSplitterPosition(Splitter1, Self.ClassName);
  end;
end;

procedure TDefineEntryRightsForm.FormCloseQuery(Sender: TObject;
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

procedure TDefineEntryRightsForm.DataFileCaptionChange(
  const Sender: TEpiCustomBase; const Initiator: TEpiCustomBase;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  if not (Initiator is TEpiTranslatedText) then exit;
  if (EventGroup <> eegCustomBase) then Exit;
  if (TEpiCustomChangeEventType(EventType) <> ecceText) then exit;

  FDataFormsVST.Invalidate;
end;

procedure TDefineEntryRightsForm.RelationChange(const Sender: TEpiCustomBase;
  const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
begin
  if (Initiator is TEpiDatafileRelationList) then
    begin
      if (EventGroup <> eegCustomBase) then exit;
      if (TEpiCustomChangeEventType(EventType) <> ecceDelItem) then exit;

      InitDataFormTree;
    end;

  if (Initiator is TEpiCustomRelationItem) then
    begin
      if (EventGroup <> eegRelations) then exit;
      if (TEpiCustomRelationEvent(EventType) <> ecreAssignObject) then exit;
      if (not Assigned(PEpiCustomRelationAssignObjectData(Data)^.ObjectData)) then exit;

      InitDataFormTree;
    end;
end;

procedure TDefineEntryRightsForm.AdminResetHook(const Sender: TEpiCustomBase;
  const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
begin
  if (EventGroup <> eegAdmin) and (TEpiAdminChangeEventType(EventType) <> eaceAdminResetting) then exit;

  RemoveHooks;
  Self.Free;
end;

procedure TDefineEntryRightsForm.SetDocument(AValue: TEpiDocument);
begin
  if FDocument = AValue then Exit;
  FDocument := AValue;

  AddHooks;

  InitDataFormTree;

//  FProjectTreeView.AddDocument(FDocument);
  FGroupRightsFrame.Admin := FDocument.Admin;
//  FProjectTreeView.SelectedObject := FDocument.Relations[0];
end;

procedure TDefineEntryRightsForm.AddHooks;
begin
  FDocument.Admin.RegisterOnChangeHook(@AdminResetHook, true);
  FDocument.DataFiles.RegisterOnChangeHook(@DataFileCaptionChange, true);
  FDocument.Relations.RegisterOnChangeHook(@RelationChange, true);
end;

procedure TDefineEntryRightsForm.RemoveHooks;
begin
  FDocument.Admin.UnRegisterOnChangeHook(@AdminResetHook);
  FDocument.DataFiles.UnRegisterOnChangeHook(@DataFileCaptionChange);
  FDocument.Relations.UnRegisterOnChangeHook(@RelationChange);
end;

procedure TDefineEntryRightsForm.InitDataFormTree;
begin
  if not Assigned(FDocument) then exit;
  if (csDestroying in ComponentState) then exit;

  FDataFormsVST.RootNodeCount := FDocument.Relations.Count;
  FDataFormsVST.ReinitNode(nil, true);
  FDataFormsVST.FullExpand();
  FDataFormsVST.FocusedNode := FDataFormsVST.GetFirst();
end;

function TDefineEntryRightsForm.RelationFromNode(Node: PVirtualNode
  ): TEpiMasterRelation;
begin
  result := TEpiMasterRelation(FDataFormsVST.GetNodeData(Node)^);
end;

procedure TDefineEntryRightsForm.RelationToNode(Node: PVirtualNode;
  Relation: TEpiMasterRelation);
begin
  TEpiMasterRelation(FDataFormsVST.GetNodeData(Node)^) := Relation;
end;

procedure TDefineEntryRightsForm.DataformGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  MR: TEpiMasterRelation;
begin
  MR := RelationFromNode(Node);
  if Assigned(MR.Datafile) then
    CellText := MR.Datafile.Caption.Text;
end;

procedure TDefineEntryRightsForm.DataformFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  FGroupRightsFrame.DataFileRelation := RelationFromNode(Node);
end;

procedure TDefineEntryRightsForm.DataformInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  Relation: TEpiMasterRelation;
begin
  if not Assigned(ParentNode) then
    Relation := FDocument.Relations[Node^.Index]
  else
    Relation := RelationFromNode(ParentNode)[Node^.Index];

  RelationToNode(Node, Relation);

  if (Relation.DetailRelations.Count > 0) then
    Include(InitialStates, ivsHasChildren);
end;

procedure TDefineEntryRightsForm.DataformInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);
begin
  ChildCount := RelationFromNode(Node).DetailRelations.Count;
end;

constructor TDefineEntryRightsForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

{  FProjectTreeView := TEpiVProjectTreeViewFrame.Create(Self);
  with FProjectTreeView do
  begin
    ShowCheckBoxes := false;
    ShowHint := false;
    ShowProject := false;
    ShowRecordCount := false;

    MinDocumentCount := 1;
    MaxDocumentCount := 1;

    AllowSelectProject := false;
    DisplayMode := pdmCommon;
    EditCaption := false;
    EditStructure := false;

    OnDelete := @ProjectTreeDelete;
    OnTreeNodeSelected := @ProjectTreeNodeSelected;
    OnTreeNodeSelecting := @ProjectTreeNodeSelecting;

    Align := alClient;
    Parent := Panel2;
  end;                    }


  FDataFormsVST := TVirtualStringTree.Create(Self);
  with FDataFormsVST do
  begin
    NodeDataSize    := SizeOf(TEpiMasterRelation);

    with TreeOptions do
    begin
      AnimationOptions := [];
      AutoOptions      := [toAutoDropExpand, toAutoScrollOnExpand];
      MiscOptions      := [toFullRepaintOnResize, toWheelPanning];
      PaintOptions     := [toShowButtons, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages];
      SelectionOptions := [toFullRowSelect, toRightClickSelect];
      StringOptions    := [];
    end;

    OnGetText      := @DataformGetText;
    OnFocusChanged := @DataformFocusChanged;
    OnInitNode     := @DataformInitNode;
    OnInitChildren := @DataformInitChildren;

    Align           := alClient;
    Parent          := Panel2;
  end;

  FGroupRightsFrame := TGroupsAssignFrame.Create(Self);
  with FGroupRightsFrame do
  begin
    Align := alClient;
    Parent := Panel3;
  end;
end;

destructor TDefineEntryRightsForm.Destroy;
begin
  DefineEntryRightsForm := nil;
  inherited Destroy;
end;

end.

