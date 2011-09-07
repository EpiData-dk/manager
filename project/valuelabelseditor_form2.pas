unit valuelabelseditor_form2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, Buttons, VirtualTrees, epivaluelabels;

type

  { TValuelabelEditor2 }

  TValuelabelEditor2 = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    ImageList1: TImageList;
    Panel2: TPanel;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ValueLabelNameEdit: TEdit;
    Label1: TLabel;
    Panel1: TPanel;
    Panel3: TPanel;
    VLST: TVirtualStringTree;
    procedure FormCreate(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure VLSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure VLSTInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure VLSTNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; const NewText: String);
  private
    { private declarations }
  public
    { public declarations }
  end;

implementation

{$R *.lfm}

type
  PVLRecord = ^TVLRecord;
  TVLRecord = record
    Value: string;
    VLabel: string;
    Missing: boolean;
  end;

procedure TValuelabelEditor2.FormCreate(Sender: TObject);
begin
  VLST.NodeDataSize := SizeOF(TVLRecord);
//  VLST.RootNodeCount := 0;
end;

procedure TValuelabelEditor2.ToolButton1Click(Sender: TObject);
begin
  VLST.RootNodeCount := VLST.RootNodeCount + 1;
end;

procedure TValuelabelEditor2.VLSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  VLRec: PVLRecord;
begin
  VLRec := Sender.GetNodeData(Node);
  with VLRec^ do
  begin
    case Column of
      1: CellText := Value;
      2: CellText := VLabel;
      3: CellText := ''; // Checkbox, do nothing
    end;
  end;
end;

procedure TValuelabelEditor2.VLSTInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  VLRec: PVLRecord;
begin
  Node^.CheckType := ctCheckBox;

  VLRec := Sender.GetNodeData(Node);
  with VLRec^ do
  begin
    Value := '';
    VLabel := '';
    Missing := false;
  end;
end;

procedure TValuelabelEditor2.VLSTNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; const NewText: String);
var
  VLRec: PVLRecord;
begin
  VLRec := Sender.GetNodeData(Node);
  with VLRec^ do
  begin
    Case Column of
      1: Value := NewText;
      2: VLabel := NewText;
      3: Missing := false;
    end;
  end;
end;

{ TValuelabelEditor2 }

end.

