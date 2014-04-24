unit toolsform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, Buttons, VirtualTrees,
  epidocument, epidatafiles, epirelations, contnrs;

type

  { TToolsForm }

  TToolsForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    DataSetInfoPanel: TPanel;
    Panel5: TPanel;
    FieldCountPanel: TPanel;
    SectionCountPanel: TPanel;
    RecordCountPanel: TPanel;
    DeletedCountPanel: TPanel;
    VirtualStringTree1: TVirtualStringTree;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
    procedure VirtualStringTree1Checked(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure VirtualStringTree1Checking(Sender: TBaseVirtualTree;
      Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
    procedure VirtualStringTree1GetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
    procedure VirtualStringTree1InitChildren(Sender: TBaseVirtualTree;
      Node: PVirtualNode; var ChildCount: Cardinal);
    procedure VirtualStringTree1InitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
  private
    FEpiDocument: TEpiDocument;
    FSelectedDatafiles: TList;
    procedure SetEpiDocument(const AValue: TEpiDocument);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    property EpiDocument: TEpiDocument read FEpiDocument write SetEpiDocument;
    property SelectedDatafiles: TList read FSelectedDatafiles;
  public
    class procedure RestoreDefaultPos;
  end; 

implementation

{$R *.lfm}

uses
  settings2, settings2_var;

{ TToolsForm }

procedure TToolsForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  i: Integer;
begin
  CanClose := true;

{  for i := 0 to DataFilesListView.Items.Count - 1 do
    with DataFilesListView.Items[i] do
      if Checked then
        FSelectedDatafiles.Add(Data);}

  if ManagerSettings.SaveWindowPositions then
    SaveFormPosition(Self, Self.ClassName);
end;

procedure TToolsForm.FormShow(Sender: TObject);
begin
  if ManagerSettings.SaveWindowPositions then
    LoadFormPosition(Self, Self.ClassName);
end;

procedure TToolsForm.VirtualStringTree1Checked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  CNode: PVirtualNode;
begin
  CNode := Node^.FirstChild;
  while Assigned(CNode) do
  begin
    if Sender.CheckState[Node] in [csCheckedNormal, csMixedNormal] then
      Sender.CheckState[CNode] := csMixedNormal
    else
      Sender.CheckState[CNode] := csUncheckedNormal;
    CNode := CNode^.NextSibling;
  end;
end;

procedure TToolsForm.VirtualStringTree1Checking(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
begin
  if (Sender.CheckState[Node] = csMixedNormal) and
     (Sender.CheckState[Node^.Parent] in [csCheckedNormal, csMixedNormal])
  then
  begin
    Allowed := false;
    Exit;
  end;

  if (Node^.Parent <> nil) and (NewState = csUncheckedNormal) then
    Allowed := Sender.CheckState[Node^.Parent] = csUncheckedNormal;
end;

procedure TToolsForm.VirtualStringTree1GetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  MR: TEpiMasterRelation;
begin
  if TextType <> ttNormal then exit;

  MR := TEpiMasterRelation(Sender.GetNodeData(Node)^);
  CellText := MR.Datafile.Caption.Text;
end;

procedure TToolsForm.VirtualStringTree1InitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);
var
  MR: TEpiMasterRelation;
begin
  MR := TEpiMasterRelation(Sender.GetNodeData(Node)^);
  ChildCount := MR.DetailRelations.Count;
end;

procedure TToolsForm.VirtualStringTree1InitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  MR: TEpiMasterRelation;
  RelationList: TEpiRelationList;
begin
  if ParentNode = nil then
    RelationList := EpiDocument.Relations
  else
    RelationList := TEpiMasterRelation(Sender.GetNodeData(ParentNode)^).DetailRelations;

  MR := RelationList.MasterRelation[Node^.Index];
  Pointer(Sender.GetNodeData(Node)^) := MR;

  Include(InitialStates, ivsExpanded);

  if ParentNode = nil
    then
      Sender.CheckType[Node] := ctCheckBox
    else
      Sender.CheckType[Node] := ctTriStateCheckBox;

  if MR.DetailRelations.Count > 0 then
    Include(InitialStates, ivsHasChildren);
end;

procedure TToolsForm.SetEpiDocument(const AValue: TEpiDocument);
var
  i: Integer;
  Item: TListItem;
begin
  if FEpiDocument = AValue then exit;
  FEpiDocument := AValue;

  VirtualStringTree1.NodeDataSize := SizeOf(TEpiMasterRelation);
  VirtualStringTree1.RootNodeCount := EpiDocument.Relations.Count;
end;

constructor TToolsForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FSelectedDatafiles := TList.Create;
end;

class procedure TToolsForm.RestoreDefaultPos;
var
  Aform: TForm;
begin
  Aform := TForm.Create(nil);
  Aform.Width := 500;
  Aform.Height := 500;
  Aform.top := (Screen.Monitors[0].Height - Aform.Height) div 2;
  Aform.Left := (Screen.Monitors[0].Width - Aform.Width) div 2;
  SaveFormPosition(Aform, TToolsForm.ClassName);
  AForm.free;
end;

end.

