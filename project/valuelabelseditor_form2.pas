unit valuelabelseditor_form2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, ComCtrls, Menus, VirtualTrees, epidatafilestypes,
  valuelabelgrid_frame, epivaluelabels;

type

  { TValueLabelEditor2 }

  TValueLabelEditor2 = class(TForm)
    BitBtn1: TBitBtn;
    ImageList1: TImageList;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    PopupMenu1: TPopupMenu;
    Splitter1: TSplitter;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    VLSetsTree: TVirtualStringTree;
    procedure FormCreate(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure VLSetsTreeGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure VLSetsTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure VLSetsTreeInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
  private
    { private declarations }
    FGridFrame: TValueLabelGridFrame;
    FValueLabelSets: TEpiValueLabelSets;
    procedure DoAddNewValueLabelSet(FieldType: TEpiFieldType);
    procedure SetValueLabelSets(AValue: TEpiValueLabelSets);
    procedure UpdateTreeView;
    function  ValueLabelSetFromNode(Node: PVirtualNode): TEpiValueLabelSet;
  public
    { public declarations }
    property ValueLabelSets: TEpiValueLabelSets read FValueLabelSets write SetValueLabelSets;
  end;


procedure ShowValueLabelEditor2(ValueLabelSet: TEpiValueLabelSets);

implementation

{$R *.lfm}

uses
  Main;

var
  Editor: TValueLabelEditor2 = nil;

procedure ShowValueLabelEditor2(ValueLabelSet: TEpiValueLabelSets);
begin
  if not Assigned(Editor) then
    Editor := TValueLabelEditor2.Create(MainForm);
  Editor.ValueLabelSets := ValueLabelSet;
  Editor.Show;
end;

{ TValueLabelEditor2 }

procedure TValueLabelEditor2.ToolButton1Click(Sender: TObject);
begin
  DoAddNewValueLabelSet(ftInteger);
end;

procedure TValueLabelEditor2.VLSetsTreeGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
begin
  Case ValueLabelSetFromNode(Node).LabelType of
    ftInteger:     ImageIndex := 0;
    ftFloat:       ImageIndex := 1;
    ftString,
    ftUpperString: ImageIndex := 2;
  end;
end;

procedure TValueLabelEditor2.VLSetsTreeGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
begin
  CellText := FValueLabelSets[Node^.Index].Name;
end;

procedure TValueLabelEditor2.VLSetsTreeInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  //
end;

procedure TValueLabelEditor2.FormCreate(Sender: TObject);
begin
  FGridFrame := TValueLabelGridFrame.Create(Self);
  with FGridFrame do
  begin
    Align := alClient;
    Parent := Self;
    ValueLabelSet := nil;
  end;
end;

procedure TValueLabelEditor2.DoAddNewValueLabelSet(FieldType: TEpiFieldType);
begin

end;

procedure TValueLabelEditor2.SetValueLabelSets(AValue: TEpiValueLabelSets);
begin
  if FValueLabelSets = AValue then Exit;
  FValueLabelSets := AValue;

  VLSetsTree.RootNodeCount := FValueLabelSets.Count;
end;

procedure TValueLabelEditor2.UpdateTreeView;
begin
  //
end;

function TValueLabelEditor2.ValueLabelSetFromNode(Node: PVirtualNode
  ): TEpiValueLabelSet;
begin
  result := FValueLabelSets[Node^.Index];
end;

end.

