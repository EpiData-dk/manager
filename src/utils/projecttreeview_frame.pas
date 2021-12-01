unit projecttreeview_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, VirtualTrees,
  epidocument, epirelations, epidatafiles;

type

  TProjectDisplayMode = (
    pdmSeperate,          // Display each document with distinct rootnodes
    pdmCommon             // Display only common dataform (structure AND name)
  );


  TProjectTreeSelectDataFile = procedure(Const DataFile: TEpiDataFile) of object;
  TProjectTreeAllowSelectDataFile = procedure(Const OldDataFile, NewDataFile: TEpiDataFile;
    var Allowed: Boolean) of object;

  { TProjectTreeViewFrame }

  TProjectTreeViewFrame = class(TFrame)
    VST: TVirtualStringTree;
  private
    { VST Methods }
    procedure VSTChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTChecking(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var NewState: TCheckState; var Allowed: Boolean);
    procedure VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure VSTFocusChanging(Sender: TBaseVirtualTree; OldNode,
      NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
      var Allowed: Boolean);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
  private
    FAllowSelectProject: Boolean;
    FDisplayMode: TProjectDisplayMode;
    FDocumentList: TList;
    FOnDataFileAllowSelect: TProjectTreeAllowSelectDataFile;
    FOnDataFileSelected: TProjectTreeSelectDataFile;
    function  DataFileFromNode(Const Node: PVirtualNode): TEpiDataFile;
    procedure SetAllowSelectProject(AValue: Boolean);
    procedure SetDisplayMode(AValue: TProjectDisplayMode);
    function  AllRelationsAreEqual: boolean;
    procedure DoUpdateTree;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure AddDocument(Const Doc: TEpiDocument);
    property  DisplayMode: TProjectDisplayMode read FDisplayMode write SetDisplayMode;
    property  AllowSelectProject: Boolean read FAllowSelectProject write SetAllowSelectProject;
  public
    { Events }
    property  OnDataFileSelected: TProjectTreeSelectDataFile read FOnDataFileSelected write FOnDataFileSelected;
    property  OnDataFileAllowSelect: TProjectTreeAllowSelectDataFile read FOnDataFileAllowSelect write FOnDataFileAllowSelect;
  end;

implementation

{$R *.lfm}

{ TProjectTreeViewFrame }

procedure TProjectTreeViewFrame.VSTChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  //
end;

procedure TProjectTreeViewFrame.VSTChecking(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
begin
  //
end;

procedure TProjectTreeViewFrame.VSTFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  DF: TEpiDataFile;
begin
  if Node^.Parent = Sender.RootNode then
    begin

    end
  else
    begin
      DF := DataFileFromNode(Node);

      if Assigned(OnDataFileSelected) then
        OnDataFileSelected(DF);
    end;
end;

procedure TProjectTreeViewFrame.VSTFocusChanging(Sender: TBaseVirtualTree;
  OldNode, NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
  var Allowed: Boolean);
var
  OldDF: TEpiDataFile;
  NewDF: TEpiDataFile;
begin
  if not Assigned(NewNode) then
    begin
      Allowed := false;
      Exit;
    end;

  if (NewNode^.Parent = Sender.RootNode) and
     (not AllowSelectProject)
  then
    begin
      Allowed := false;
      Exit;
    end;

  if NewNode^.Parent = Sender.RootNode then
    begin

    end
  else
    begin
      OldDF := DataFileFromNode(OldNode);
      NewDF := DataFileFromNode(NewNode);

      if Assigned(OnDataFileAllowSelect) then
        OnDataFileAllowSelect(OldDF, NewDF, Allowed);
    end;
end;

procedure TProjectTreeViewFrame.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  Obj: TObject;
  Doc: TEpiDocument absolute Obj;
  MR:  TEpiMasterRelation absolute Obj;
begin
  if TextType <> ttNormal then exit;

  Obj := TObject(Sender.GetNodeData(Node)^);

  if Node^.Parent = Sender.RootNode then
    if Assigned(Obj) then
      CellText := Doc.Study.Title.Text
    else
      CellText := 'Incompatible relational structures!'
  else
    CellText := Mr.Datafile.Caption.Text;
end;

procedure TProjectTreeViewFrame.SetDisplayMode(AValue: TProjectDisplayMode);
begin
  if FDisplayMode = AValue then Exit;
  FDisplayMode := AValue;
  DoUpdateTree;
end;

procedure TProjectTreeViewFrame.DoUpdateTree;

  procedure BuildTreeRecursive(Const Parent: PVirtualNode; MR: TEpiMasterRelation);
  var
    i: Integer;
    Node: PVirtualNode;
  begin
    Node := VST.AddChild(Parent, MR);
    VST.CheckType[Node] := ctTriStateCheckBox;

    for i := 0 to MR.DetailRelations.Count - 1 do
      BuildTreeRecursive(Node, MR.DetailRelation[i]);
  end;

  procedure BuildDocumentTree(Const Parent: PVirtualNode; Doc: TEpiDocument);
  var
    i: Integer;
    Node: PVirtualNode;
  begin
    Node := VST.AddChild(Parent, Doc);
    VST.CheckType[Node] := ctTriStateCheckBox;

    for i := 0 to Doc.Relations.Count - 1 do
      BuildTreeRecursive(Node, Doc.Relations.MasterRelation[i]);
  end;

var
  i: Integer;
begin
  if FDocumentList.Count = 0 then exit;

  VST.BeginUpdate;
  VST.Clear;

  if DisplayMode = pdmSeperate then
    for i := 0 to FDocumentList.Count -1 do
      BuildDocumentTree(VST.RootNode, TEpiDocument(FDocumentList[i]))
  else if AllRelationsAreEqual then
    BuildDocumentTree(VST.RootNode, TEpiDocument(FDocumentList[0]))
  else
    VST.AddChild(VST.RootNode, nil);

  VST.EndUpdate;
end;

procedure TProjectTreeViewFrame.SetAllowSelectProject(AValue: Boolean);
begin
  if FAllowSelectProject = AValue then Exit;
  FAllowSelectProject := AValue;
end;

constructor TProjectTreeViewFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FDisplayMode := pdmCommon;
  FDocumentList := TList.Create;

  with VST do
  begin
    NodeDataSize := SizeOf(Pointer);

    OnGetText  := @VSTGetText;
    OnChecking := @VSTChecking;
    OnChecked  := @VSTChecked;

    OnFocusChanging := @VSTFocusChanging;
    OnFocusChanged := @VSTFocusChanged;
  end;
end;

function TProjectTreeViewFrame.DataFileFromNode(const Node: PVirtualNode
  ): TEpiDataFile;
begin
  Result := nil;

  if (Node = nil) then exit;
  if (Node^.Parent = VST.RootNode) then exit;

  Result := TEpiMasterRelation(VST.GetNodeData(Node)^).Datafile;
end;

procedure TProjectTreeViewFrame.AddDocument(const Doc: TEpiDocument);
begin
  FDocumentList.Add(Doc);
  DoUpdateTree;
end;

function TProjectTreeViewFrame.AllRelationsAreEqual: boolean;

  function CompareTreeStructure(Const RelationListA, RelationListB: TEpiRelationList): boolean;
  var
    i: Integer;
    MRA: TEpiMasterRelation;
    MRB: TEpiMasterRelation;
  begin
    result := (RelationListA.Count = RelationListB.Count);
    if not Result then exit;

    for i := 0 to RelationListA.Count - 1 do
    begin
      MRA := RelationListA.MasterRelation[i];
      MRB := RelationListB.MasterRelation[i];

      Result :=
        (MRA.Datafile.Name = MRB.Datafile.Name) and
        CompareTreeStructure(MRA.DetailRelations, MRB.DetailRelations);

      if not Result then exit;
    end;
  end;

var
  MainDoc: TEpiDocument;
  CompareDoc: TEpiDocument;
  i: Integer;
begin
  Result := true;

  if (FDocumentList.Count = 1)
  then
    Exit;

  MainDoc := TEpiDocument(FDocumentList[0]);

  for i := 1 to FDocumentList.Count - 1 do
  begin
    CompareDoc := TEpiDocument(FDocumentList[i]);

    Result := Result and
      CompareTreeStructure(MainDoc.Relations, CompareDoc.Relations);
  end;
end;

end.

