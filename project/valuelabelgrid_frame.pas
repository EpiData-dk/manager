unit valuelabelgrid_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, VirtualTrees, epivaluelabels,
  epidatafilestypes, LCLType, ComCtrls;

type

  { TValueLabelGridFrame }

  TValueLabelGridFrame = class(TFrame)
    DelLineBtn: TToolButton;
    ImageList1: TImageList;
    NewLineBtn: TToolButton;
    ToolBar1: TToolBar;
    procedure DelLineBtnClick(Sender: TObject);
    procedure NewLineBtnClick(Sender: TObject);
  private
    { StringTree privates }
    VLG: TVirtualStringTree;
    procedure DoAddLine;
    procedure VLGChecking(Sender: TBaseVirtualTree; Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
    procedure VLGEdited(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure VLGEditor(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure VLGFocusChanging(Sender: TBaseVirtualTree; OldNode, NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex; var Allowed: Boolean);
    procedure VLGFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VLGGetNodeText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure VLGInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure VLGKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure VLGSendPostEdit(Data: PtrInt);
    procedure VLGSetNodeText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; const NewText: String);
    procedure VLGUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
  private
    { Other frame parts }
    FValueLabelSet: TEpiValueLabelSet;
    function GetValueLabelSet: TEpiValueLabelSet;
    procedure SetValueLabelSet(AValue: TEpiValueLabelSet);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    property    ValueLabelSet: TEpiValueLabelSet read GetValueLabelSet write SetValueLabelSet;
  end; 

implementation

{$R *.lfm}

uses
  Graphics, Dialogs;

type
  PVLRecord = ^TVLRecord;
  TVLRecord = record
    Value: string;
    VLabel: string;
    Missing: boolean;
  end;

  { TValidatedStringEditLink }

  TValidatedStringEditLink = class(TStringEditLink, IVTEditLink)
  private
    FEditor: TValueLabelGridFrame;
  public
    function EndEdit: Boolean; override; stdcall;
    property Editor: TValueLabelGridFrame read FEditor write FEditor;
  end;

{ TValidatedStringEditLing }

function TValidatedStringEditLink.EndEdit: Boolean; stdcall;
var
  I: integer;
  F: Extended;
begin
  Result := not FStopping;
{
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
  end;           }

  Result := Result and inherited;
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

{ TValueLabelGridFrame }

procedure TValueLabelGridFrame.NewLineBtnClick(Sender: TObject);
begin
  DoAddLine;
  VLG.SetFocus;
end;

procedure TValueLabelGridFrame.DelLineBtnClick(Sender: TObject);
var
  NewNode: PVirtualNode;
  Data: PVLRecord;
begin
  if not Assigned(VLG.FocusedNode) then exit;

  NewNode := VLG.GetNextSibling(VLG.FocusedNode);
  if not Assigned(NewNode) then
    NewNode := VLG.GetPreviousSibling(VLG.FocusedNode);


  Data := VLG.GetNodeData(VLG.FocusedNode);
  with Data^ do
    if MessageDlg('Warning',
         Format('Are you sure you want to delete "%s = %s"?',[Value, VLabel]),
         mtWarning, mbYesNo, 0, mbNo) = mrNo then exit;

  VLG.DeleteNode(VLG.FocusedNode);

  if Assigned(NewNode) then
  begin
    VLG.FocusedNode := NewNode;
    VLG.Selected[NewNode] := true;
  end;
  VLG.Refresh;
end;

procedure TValueLabelGridFrame.DoAddLine;
var
  Node: PVirtualNode;
  Last: PVirtualNode;
begin
  VLG.BeginUpdate;

  Last := VLG.GetLast();
  Node := VLG.AddChild(nil);

  VLG.FocusedNode := Node;
  VLG.FocusedColumn := 0;
  VLG.Selected[Node] := true;
{  if FieldType in [ftFloat,ftInteger] then
  begin
    if Assigned(Node) then
      Text[Node, 0] := FloatToStr(StrToFloat(PVLRecord(GetNodeData(Last))^.Value) + 1)
    else
      Text[Node, 0] := '0';
  end;             }

  VLG.EndUpdate;
end;

procedure TValueLabelGridFrame.VLGChecking(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
var
  Data: PVLRecord;
begin
  Data := Sender.GetNodeData(Node);
  with Data^ do
    Missing := NewState in [csCheckedNormal, csCheckedPressed];
end;

procedure TValueLabelGridFrame.VLGEdited(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  if Column = 0 then Application.QueueAsyncCall(@VLGSendPostEdit, PtrInt(Node));
end;

procedure TValueLabelGridFrame.VLGEditor(Sender: TBaseVirtualTree;
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

procedure TValueLabelGridFrame.VLGFocusChanging(Sender: TBaseVirtualTree; OldNode,
  NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
  var Allowed: Boolean);
begin
  if (NewColumn = 2) or (NewColumn = -1) then Allowed := false;
end;

procedure TValueLabelGridFrame.VLGFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  ResetVLRecord(Sender.GetNodeData(Node));
end;

procedure TValueLabelGridFrame.VLGGetNodeText(Sender: TBaseVirtualTree;
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

procedure TValueLabelGridFrame.VLGInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  Node^.CheckType := ctCheckBox;
end;

procedure TValueLabelGridFrame.VLGKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
//  ShowHintMsg(nil, '');

  case Key of
    VK_RETURN:
      begin
        if ssCtrl in Shift then
        begin
          DoAddLine;
          Key := VK_UNKNOWN;
        end;
        if (Shift = []) and (Assigned(VLG.FocusedNode)) then
        begin
          VLG.EditNode(VLG.FocusedNode, VLG.FocusedColumn);
          Key := VK_UNKNOWN;
        end;
      end;
    VK_DELETE:
      begin
//        DelLineBtn.Click;
        Key := VK_UNKNOWN;
      end;
  end;
end;

procedure TValueLabelGridFrame.VLGSendPostEdit(Data: PtrInt);
begin
  VLG.FocusedColumn := 1;
  VLG.EditNode(PVirtualNode(Data), 1);
end;

procedure TValueLabelGridFrame.VLGSetNodeText(Sender: TBaseVirtualTree;
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

procedure TValueLabelGridFrame.VLGUTF8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
begin
  if UTF8Key = Char(VK_SPACE) then exit;
  if UTF8Key = Char(VK_RETURN) then exit;

  VLG.EditNode(VLG.FocusedNode, VLG.FocusedColumn);
end;

procedure TValueLabelGridFrame.SetValueLabelSet(AValue: TEpiValueLabelSet);
begin
  if FValueLabelSet = AValue then Exit;
  FValueLabelSet := AValue;
end;


function TValueLabelGridFrame.GetValueLabelSet: TEpiValueLabelSet;
begin

end;

constructor TValueLabelGridFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  VLG := TVirtualStringTree.Create(Self);
  with VLG do
  begin
    Align := alClient;
    Parent := Self;
    Color := clNone;
    NodeDataSize := SizeOF(TVLRecord);
    WantTabs := true;
    TabStop := true;

    // Events:
    OnInitNode      := @VLGInitNode;
    OnFreeNode      := @VLGFreeNode;
    OnGetText       := @VLGGetNodeText;
    OnNewText       := @VLGSetNodeText;
    OnFocusChanging := @VLGFocusChanging;
    OnKeyDown       := @VLGKeyDown;
    OnUTF8KeyPress  := @VLGUTF8KeyPress;
    OnChecking      := @VLGChecking;
    OnCreateEditor  := @VLGEditor;
    OnEdited        := @VLGEdited;
  end;

  with VLG.TreeOptions do
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

  with VLG.Header do
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
end;


end.

