unit valuelabelseditor_form2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, Buttons, VirtualTrees, epivaluelabels, epidatafilestypes;

type

  { TValuelabelEditor2 }

  TValuelabelEditor2 = class(TForm)
    CancelBtn: TBitBtn;
    OkBtn: TBitBtn;
    ImageList1: TImageList;
    Panel2: TPanel;
    ToolBar1: TToolBar;
    NewLineBtn: TToolButton;
    DelLineBtn: TToolButton;
    ValueLabelNameEdit: TEdit;
    Label1: TLabel;
    Panel1: TPanel;
    Panel3: TPanel;
    procedure OkBtnClick(Sender: TObject);
    procedure DelLineBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure NewLineBtnClick(Sender: TObject);
    procedure VLSTEditor(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; out EditLink: IVTEditLink);
  private
    { Virtual TreeView }
    VLST: TVirtualStringTree;
    procedure VLSTChecking(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var NewState: TCheckState; var Allowed: Boolean);
    procedure VLSTKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FocusChanging(Sender: TBaseVirtualTree; OldNode,
      NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
      var Allowed: Boolean);
    procedure FreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure GetNodeText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure InitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure SetNodeText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; const NewText: String);
    procedure SetValueLabelSets(AValue: TEpiValueLabelSets);
  private
    {EpiData Properties}
    FResultValueLabelSet: TEpiValueLabelSet;
    FValueLabelSets: TEpiValueLabelSets;
    FFieldType: TEpiFieldType;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; FieldType: TEpiFieldType);
    property  ValueLabelSets: TEpiValueLabelSets read FValueLabelSets write SetValueLabelSets;
    property  ResultValueLabelSet: TEpiValueLabelSet read FResultValueLabelSet;
  end;

implementation

{$R *.lfm}

uses
  LCLType;

type
  PVLRecord = ^TVLRecord;
  TVLRecord = record
    Value: string;
    VLabel: string;
    Missing: boolean;
  end;

procedure ResetVLRecord(VLRec: PVLRecord);
begin
  with VLRec^ do
  begin
    Value := '';
    VLabel := '';
    Missing := false;
  end;
end;

type

  { TValidatedStringEditLing }

  TValidatedStringEditLing = class(TStringEditLink, IVTEditLink)
  private
    FFieldType: TEpiFieldType;
  public
    function EndEdit: Boolean; override; stdcall;
    property FieldType: TEpiFieldType read FFieldType write FFieldType;
  end;

{ TValidatedStringEditLing }

function TValidatedStringEditLing.EndEdit: Boolean; stdcall;
var
  I: integer;
  F: Extended;
begin
  Result := not FStopping;

  if result and Edit.Modified then
  begin
    case FieldType of
      ftInteger:
        result := TryStrToInt(Edit.Text, I);
      ftFloat:
        result := TryStrToFloat(Edit.Text, F);
    end;
  end;

  Result := Result and inherited;
end;

  { TValuelabelEditor2 }

procedure TValuelabelEditor2.FormCreate(Sender: TObject);
begin
  VLST := TVirtualStringTree.Create(Self);
  with VLST do
  begin
    // Setup:
    Align := alClient;
    Parent := Panel2;
    Color := clNone;
    NodeDataSize := SizeOF(TVLRecord);
    WantTabs := true;

    // Events:
    OnInitNode  := @InitNode;
    OnFreeNode   := @FreeNode;
    OnGetText   := @GetNodeText;
    OnNewText    := @SetNodeText;
    OnFocusChanging  := @FocusChanging;
    OnKeyDown   := @VLSTKeyDown;
    OnChecking  := @VLSTChecking;
    OnCreateEditor  := @VLSTEditor;
  end;

  with VLST.TreeOptions do
  begin
    AnimationOptions := [];
    AutoOptions := [];
{   toCheckSupport,             // Show checkboxes/radio buttons.
    toEditable,                 // Node captions can be edited.
    toGridExtensions,           // Use some special enhancements to simulate and support grid behavior.
    toWheelPanning,             // Support for mouse panning (wheel mice only). This option and toMiddleClickSelect are
                                // mutal exclusive, where panning has precedence.
    toEditOnDblClick            // Editing mode can be entered with a double click}
    MiscOptions := [toCheckSupport, toEditable, toGridExtensions, toWheelPanning, toEditOnDblClick];
{   toShowHorzGridLines,       // Display horizontal lines to simulate a grid.
    toShowVertGridLines,       // Display vertical lines (depending on columns) to simulate a grid.
    toThemeAware,              // Draw UI elements (header, tree buttons etc.) according to the current theme if
                               // enabled (Windows XP+ only, application must be themed). }
    PaintOptions := [toShowHorzGridLines, toShowVertGridLines, toThemeAware];
    SelectionOptions := [toExtendedFocus, toRightClickSelect, toCenterScrollIntoView];
  end;

  with VLST.Header do
  begin
    Options := [hoAutoResize, hoColumnResize, hoDblClickResize, hoVisible];
    with Columns.Add do
    begin
      Text := 'Value';

      {   coAllowClick,            // Column can be clicked (must be enabled too).
          coDraggable,             // Column can be dragged.
          coEnabled,               // Column is enabled.
          coParentBidiMode,        // Column uses the parent's bidi mode.
          coParentColor,           // Column uses the parent's background color.
          coResizable,             // Column can be resized.
          coShowDropMark,          // Column shows the drop mark if it is currently the drop target.
          coVisible,               // Column is shown.
          coAutoSpring,            // Column takes part in the auto spring feature of the header (must be resizable too).
          coFixed,                 // Column is fixed and can not be selected or scrolled etc.
          coSmartResize,           // Column is resized to its largest entry which is in view (instead of its largest
                                   // visible entry).
          coAllowFocus,            // Column can be focused.
          coDisableAnimatedResize, // Column resizing is not animated.
          coWrapCaption,           // Caption could be wrapped across several header lines to fit columns width.
          coUseCaptionAlignment    // Column's caption has its own aligment.  }
      Options := [coAllowClick, coEnabled, coParentBidiMode, coParentColor, coResizable, coVisible, coAllowFocus];
      Width := 20;
    end;

    with Columns.Add do
    begin
      Text := 'Label';
      Options := [coAllowClick, coEnabled, coParentBidiMode, coParentColor, coResizable, coVisible, coAllowFocus];
      Width := 40;
    end;

    with Columns.Add do
    begin
      Alignment := taCenter;
      Text := 'Missing';
      Options := [coAllowClick, coEnabled, coParentBidiMode, coParentColor, coVisible];
      CheckBox := true;
      Width := 70;
    end;
    AutoSizeIndex := 1;
    Height := 25;
    MainColumn := 2;
  end;

//  VLST.RootNodeCount := 5;
end;

procedure TValuelabelEditor2.DelLineBtnClick(Sender: TObject);
var
  NewNode: PVirtualNode;
begin
  if not Assigned(VLST.FocusedNode) then exit;

  NewNode := VLST.GetNextSibling(VLST.FocusedNode);
  if not Assigned(NewNode) then
    NewNode := VLST.GetPreviousSibling(VLST.FocusedNode);

  VLST.DeleteSelectedNodes;

  if Assigned(NewNode) then
    VLST.FocusedNode := NewNode;
end;

procedure TValuelabelEditor2.OkBtnClick(Sender: TObject);
var
  Node: PVirtualNode;
  VL: TEpiCustomValueLabel;
begin
  ModalResult := mrNone;
  if not Assigned(FValueLabelSets) then exit;

  if not FValueLabelSets.ValidateRename(nil, ValueLabelNameEdit.Text) then
  begin
    ValueLabelNameEdit.SetFocus;
    Exit;
  end;


  Node := VLSt.GetFirstChild(nil);
  While Assigned(Node) do
  begin
    with PVLRecord(VLST.GetNodeData(Node))^ do
    begin
      // Validate node data?
    end;
    Node := VLSt.GetNextSibling(Node);
  end;

  FResultValueLabelSet := FValueLabelSets.NewValueLabelSet(FFieldType);
  FResultValueLabelSet.Name := ValueLabelNameEdit.Text;

  Node := VLSt.GetFirstChild(nil);
  While Assigned(Node) do
  begin
    VL := FResultValueLabelSet.NewValueLabel;
    with PVLRecord(VLST.GetNodeData(Node))^ do
    begin
      case FFieldType of
        ftInteger:  TEpiIntValueLabel(VL).Value := StrToInt(Value);
        ftFloat:    TEpiFloatValueLabel(VL).Value := StrToFloat(Value);
        ftString:   TEpiStringValueLabel(VL).Value := Value;
      end;
      VL.TheLabel.Text := VLabel;
      VL.IsMissingValue := Missing;
    end;
    Node := VLSt.GetNextSibling(Node);
  end;

  ModalResult := mrOk;
end;

procedure TValuelabelEditor2.NewLineBtnClick(Sender: TObject);
begin
  VLST.AddChild(nil);
end;

procedure TValuelabelEditor2.VLSTEditor(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
var
  EL: TValidatedStringEditLing;
begin
  if Column = 0 then
  begin
    EL := TValidatedStringEditLing.Create;
    EL.FieldType := FFieldType;
    EditLink := EL;
  end else begin
    EditLink := TStringEditLink.Create;
  end;
end;

procedure TValuelabelEditor2.VLSTChecking(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
var
  Data: PVLRecord;
begin
  Data := Sender.GetNodeData(Node);
  with Data^ do
    Missing := NewState in [csCheckedNormal, csCheckedPressed];
end;

procedure TValuelabelEditor2.VLSTKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_RETURN:
      begin
        if ssCtrl in Shift then
        begin
          VLST.FocusedNode := VLST.AddChild(nil);
          VLST.FocusedColumn := 0;
          VLST.Refresh;
        end;
        if (Shift = []) and (Assigned(VLST.FocusedNode)) then
          VLST.EditNode(VLST.FocusedNode, VLST.FocusedColumn);
      end;
  end;
end;

procedure TValuelabelEditor2.FocusChanging(Sender: TBaseVirtualTree; OldNode,
  NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
  var Allowed: Boolean);
begin
  if (NewColumn = 2) or (NewColumn = -1) then Allowed := false;
end;

procedure TValuelabelEditor2.FreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  ResetVLRecord(Sender.GetNodeData(Node));
end;

procedure TValuelabelEditor2.GetNodeText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  Data: PVLRecord;
begin
  Data := Sender.GetNodeData(Node);
  with Data^ do
  begin
    case Column of
      0: CellText := Value;
      1: CellText := VLabel;
      2: CellText := '';
    end;
  end;
end;

procedure TValuelabelEditor2.InitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  Node^.CheckType := ctCheckBox;
  ResetVLRecord(Sender.GetNodeData(Node));
end;

procedure TValuelabelEditor2.SetNodeText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; const NewText: String);
var
  Data: PVLRecord;
begin
  Data := Sender.GetNodeData(Node);
  with Data^ do
  begin
    case Column of
      0: Value := NewText;
      1: VLabel := NewText;
      2: ; // do nothing
    end;
  end;
end;

procedure TValuelabelEditor2.SetValueLabelSets(AValue: TEpiValueLabelSets);
begin
  if FValueLabelSets = AValue then Exit;
  FValueLabelSets := AValue;
end;

constructor TValuelabelEditor2.Create(TheOwner: TComponent;
  FieldType: TEpiFieldType);
begin
  inherited Create(TheOwner);
  FFieldType := FieldType;
end;

end.

