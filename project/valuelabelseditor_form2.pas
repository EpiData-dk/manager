unit valuelabelseditor_form2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Buttons, StdCtrls, ComCtrls, Menus, VirtualTrees, epidatafilestypes,
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
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure VLSetsTreeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VLSetsTreeGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure VLSetsTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure VLSetsTreeInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure VLSetsTreeNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; const NewText: String);
  private
    { private declarations }
    FGridFrame: TValueLabelGridFrame;
    FValueLabelSets: TEpiValueLabelSets;
    procedure DoAddNewValueLabelSet(FieldType: TEpiFieldType);
    procedure SetValueLabelSets(AValue: TEpiValueLabelSets);
    function  ValueLabelSetFromNode(Node: PVirtualNode): TEpiValueLabelSet;
  private
    { Hint }
    FHintWindow: THintWindow;
    procedure DoShowHintMsg(Sender: TObject; Ctrl: TControl; Const Msg: string);
  public
    { public declarations }
    property ValueLabelSets: TEpiValueLabelSets read FValueLabelSets write SetValueLabelSets;
  end;


procedure ShowValueLabelEditor2(ValueLabelSet: TEpiValueLabelSets);

implementation

{$R *.lfm}

uses
  Main, settings2_var;

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

procedure TValueLabelEditor2.VLSetsTreeChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  if Assigned(Node) then
    FGridFrame.ValueLabelSet := ValueLabelSetFromNode(Node);
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

procedure TValueLabelEditor2.VLSetsTreeNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; const NewText: String);
var
  S: String;
begin
  if Trim(NewText) = '' then
  begin
    DoShowHintMsg(nil, VLSetsTree, 'Value label name must not be empty!');
    Exit;
  end;

  if ValueLabelSetFromNode(Node).Name = NewText then exit;

  S := ValueLabelSetFromNode(Node).Name;
  ValueLabelSetFromNode(Node).Name := NewText;
  if S = ValueLabelSetFromNode(Node).Name then
  begin
    DoShowHintMsg(nil, VLSetsTree, 'Value label name already used!');
  end;
end;

procedure TValueLabelEditor2.FormCreate(Sender: TObject);
begin
  FGridFrame := TValueLabelGridFrame.Create(Self);
  with FGridFrame do
  begin
    Align := alClient;
    Parent := Self;
    ValueLabelSet := nil;
    OnShowHintMsg := @DoShowHintMsg;
  end;
end;

procedure TValueLabelEditor2.MenuItem1Click(Sender: TObject);
begin
  DoAddNewValueLabelSet(ftInteger);
end;

procedure TValueLabelEditor2.MenuItem2Click(Sender: TObject);
begin
  DoAddNewValueLabelSet(ftFloat);
end;

procedure TValueLabelEditor2.MenuItem3Click(Sender: TObject);
begin
  DoAddNewValueLabelSet(ftString);
end;

procedure TValueLabelEditor2.DoAddNewValueLabelSet(FieldType: TEpiFieldType);
begin
  FValueLabelSets.NewValueLabelSet(FieldType).Name := '(Untitled)';
  VLSetsTree.RootNodeCount := FValueLabelSets.Count;
  VLSetsTree.Refresh;
end;

procedure TValueLabelEditor2.SetValueLabelSets(AValue: TEpiValueLabelSets);
begin
  if FValueLabelSets = AValue then Exit;
  FValueLabelSets := AValue;

  VLSetsTree.RootNodeCount := FValueLabelSets.Count;
end;

function TValueLabelEditor2.ValueLabelSetFromNode(Node: PVirtualNode
  ): TEpiValueLabelSet;
begin
  result := FValueLabelSets[Node^.Index];
end;

procedure TValueLabelEditor2.DoShowHintMsg(Sender: TObject; Ctrl: TControl;
  const Msg: string);
var
  R: TRect;
  P: TPoint;
begin
  if not Assigned(FHintWindow) then
  begin
    FHintWindow := THintWindow.Create(Self);
    FHintWindow.HideInterval := 5*1000;
  end;

  if (Msg = '') or (Ctrl = nil) then
  begin
    FHintWindow.Hide;
    Exit;
  end;

  R := FHintWindow.CalcHintRect(0, Msg, nil);
  P := Ctrl.ClientToScreen(Point(Ctrl.Width + 5, 0));
  OffsetRect(R, P.X, P.Y);
  FHintWindow.ActivateHint(R, Msg);
end;

end.

