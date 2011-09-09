unit valuelabelseditor_form2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Buttons, VirtualTrees, epivaluelabels,
  epidatafilestypes, LCLType;

type

  { TValuelabelEditor2 }

  TValuelabelEditor2 = class(TForm)
    Button1: TButton;
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
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure DelLineBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure NewLineBtnClick(Sender: TObject);
    procedure ValueLabelNameEditEditingDone(Sender: TObject);
  private
    FHintWindow: THintWindow;
    function  DoAddNewLine: PVirtualNode;
    procedure  ShowHintMsg(Ctrl: TControl; Msg: String);
  private
    { Virtual TreeView }
    VLST: TVirtualStringTree;
    procedure VLSTEdited(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure VLSTSendPostEdit(Data: PtrInt);
    procedure VLSTEditor(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure VLSTUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
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
  private
    {EpiData Properties}
    FResultValueLabelSet: TEpiValueLabelSet;
    FValueLabelSets: TEpiValueLabelSets;
    FFieldType: TEpiFieldType;
    procedure SetValueLabelSets(AValue: TEpiValueLabelSets);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; FieldType: TEpiFieldType);
    property  ValueLabelSets: TEpiValueLabelSets read FValueLabelSets write SetValueLabelSets;
    property  ResultValueLabelSet: TEpiValueLabelSet read FResultValueLabelSet;
  end;

implementation

{$R *.lfm}

uses
  LCLIntf, LMessages, valuelabelseditor_form, epidocument;

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

  { TValidatedStringEditLink }

  TValidatedStringEditLink = class(TStringEditLink, IVTEditLink)
  private
    FEditor: TValuelabelEditor2;
  public
    function EndEdit: Boolean; override; stdcall;
    property Editor: TValuelabelEditor2 read FEditor write FEditor;
  end;

{ TValidatedStringEditLing }

function TValidatedStringEditLink.EndEdit: Boolean; stdcall;
var
  I: integer;
  F: Extended;
begin
  Result := not FStopping;

  if result then
  begin
    case Editor.FFieldType of
      ftInteger:
        result := TryStrToInt(Edit.Text, I);
      ftFloat:
        result := TryStrToFloat(Edit.Text, F);
    end;
    if not Result then
    begin
      FEditor.ShowHintMsg(Edit, Edit.Text + ' is not a valid value!');
      FTree.CancelEditNode;
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
    TabStop := true;

    // Events:
    OnInitNode      := @InitNode;
    OnFreeNode      := @FreeNode;
    OnGetText       := @GetNodeText;
    OnNewText       := @SetNodeText;
    OnFocusChanging := @FocusChanging;
    OnKeyDown       := @VLSTKeyDown;
    OnUTF8KeyPress  := @VLSTUTF8KeyPress;
    OnChecking      := @VLSTChecking;
    OnCreateEditor  := @VLSTEditor;
    OnEdited        := @VLSTEdited;
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
  Data: PVLRecord;
begin
  if not Assigned(VLST.FocusedNode) then exit;

  NewNode := VLST.GetNextSibling(VLST.FocusedNode);
  if not Assigned(NewNode) then
    NewNode := VLST.GetPreviousSibling(VLST.FocusedNode);


  Data := VLST.GetNodeData(VLST.FocusedNode);
  with Data^ do
    if MessageDlg('Warning',
         Format('Are you sure you want to delete "%s = %s"?',[Value, VLabel]),
         mtWarning, mbYesNo, 0, mbNo) = mrNo then exit;

  VLST.DeleteNode(VLST.FocusedNode);

  if Assigned(NewNode) then
  begin
    VLST.FocusedNode := NewNode;
    VLST.Selected[NewNode] := true;
  end;
  VLST.Refresh;
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

procedure TValuelabelEditor2.Button1Click(Sender: TObject);
begin
  GetValueLabelsEditor(TEpiDocument(FValueLabelSets.RootOwner)).Show;
  BringToFront;
end;

procedure TValuelabelEditor2.FormShow(Sender: TObject);
begin
  DoAddNewLine;
end;

procedure TValuelabelEditor2.NewLineBtnClick(Sender: TObject);
begin
  DoAddNewLine;
  VLST.SetFocus;
end;

procedure TValuelabelEditor2.ValueLabelNameEditEditingDone(Sender: TObject);
const
  InEditingDone: boolean = false;
begin
  if InEditingDone then exit;
  InEditingDone := true;

  VLST.SetFocus;
  VLST.FocusedNode := VLST.GetFirstSelected();

  InEditingDone := false;
end;

function TValuelabelEditor2.DoAddNewLine: PVirtualNode;
var
  Node: PVirtualNode;
begin
  VLST.BeginUpdate;

  Node := VLST.GetLast();

  Result := VLST.AddChild(nil);
  VLST.FocusedNode := Result;
  VLST.FocusedColumn := 0;
  VLST.Selected[Result] := true;
  if FFieldType in [ftFloat,ftInteger] then
  begin
    if Assigned(Node) then
      VLST.Text[Result, 0] := FloatToStr(StrToFloat(PVLRecord(VLST.GetNodeData(Node))^.Value) + 1)
    else
      VLST.Text[Result, 0] := '0';
  end;

  VLST.EndUpdate;
end;

procedure TValuelabelEditor2.ShowHintMsg(Ctrl: TControl; Msg: String);
var
  R: TRect;
  P: TPoint;
begin
  if not Assigned(FHintWindow) then
    FHintWindow := THintWindow.Create(Self);

  if (Ctrl = nil) or (Msg = '') then
  begin
    FHintWindow.Hide;
    Exit;
  end;

  R := FHintWindow.CalcHintRect(0, Msg, nil);
  P := Ctrl.ControlToScreen(Ctrl.BoundsRect.TopLeft);
  OffsetRect(R, P.X, P.Y);
  FHintWindow.ActivateHint(R, Msg);
end;

procedure TValuelabelEditor2.VLSTSendPostEdit(Data: PtrInt);
begin
  VLST.FocusedColumn := 1;
  VLST.EditNode(PVirtualNode(Data), 1);
end;

procedure TValuelabelEditor2.VLSTEdited(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  if Column = 0 then
    Application.QueueAsyncCall(@VLSTSendPostEdit, PtrInt(Node));
end;

procedure TValuelabelEditor2.VLSTEditor(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
var
  EL: TValidatedStringEditLink;
begin
  if Column = 0 then
  begin
    EL := TValidatedStringEditLink.Create;
    EL.Editor := Self;
    EditLink := EL;
  end else begin
    EditLink := TStringEditLink.Create;
  end;
end;

procedure TValuelabelEditor2.VLSTUTF8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
begin
  if UTF8Key = Char(VK_SPACE) then exit;
  if UTF8Key = Char(VK_RETURN) then exit;

  VLST.EditNode(VLST.FocusedNode, VLST.FocusedColumn);
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
  ShowHintMsg(nil, '');

  case Key of
    VK_RETURN:
      begin
        if ssCtrl in Shift then
        begin
          DoAddNewLine;
          Key := VK_UNKNOWN;
        end;
        if (Shift = []) and (Assigned(VLST.FocusedNode)) then
        begin
          VLST.EditNode(VLST.FocusedNode, VLST.FocusedColumn);
          Key := VK_UNKNOWN;
        end;
      end;
    VK_DELETE:
      begin
        DelLineBtn.Click;
      Key := VK_UNKNOWN;
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

