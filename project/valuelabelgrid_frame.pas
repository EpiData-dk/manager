unit valuelabelgrid_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, VirtualTrees, epivaluelabels,
  epidatafilestypes, LCLType, ComCtrls, design_types;

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
    FVLG: TVirtualStringTree;
    procedure DoAddLine;
    procedure VLGChecking(Sender: TBaseVirtualTree; Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
    procedure VLGEdited(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure VLGEditor(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure VLGFocusChanging(Sender: TBaseVirtualTree; OldNode, NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex; var Allowed: Boolean);
    procedure VLGGetNodeText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure VLGInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure VLGKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure VLGSendPostEdit(Data: PtrInt);
    procedure VLGSetNodeText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; const NewText: String);
    procedure VLGUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
  private
    { Other frame parts }
    FOnShowHintMsg: TDesignFrameShowHintEvent;
    FValueLabelSet: TEpiValueLabelSet;
    procedure SetValueLabelSet(AValue: TEpiValueLabelSet);
    function  ValueLabelFromNode(Node: PVirtualNode): TEpiCustomValueLabel;
    procedure DoShowHintMsg(Ctrl: TControl; Const Msg: String);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    function    ValidateGridEntries: boolean;
    property    ValueLabelSet: TEpiValueLabelSet read FValueLabelSet write SetValueLabelSet;
    property    VLG: TVirtualStringTree read FVLG;
    property    OnShowHintMsg: TDesignFrameShowHintEvent read FOnShowHintMsg write FOnShowHintMsg;
  end; 

implementation

{$R *.lfm}

uses
  Graphics, Dialogs, LCLProc, epimiscutils;

type
  PEpiValueLabel = ^TEpiCustomValueLabel;

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

  if result then
  begin
    case Editor.FValueLabelSet.LabelType of
      ftInteger:
        result := TryStrToInt(Edit.Text, I);
      ftFloat:
        result := TryStrToFloat(Edit.Text, F);
    end;
    if not Result then
    begin
      FEditor.DoShowHintMsg(Edit, Edit.Text + ' is not a valid ' + LowerCase(EpiTypeNames[Editor.FValueLabelSet.LabelType]));
      FTree.CancelEditNode;
    end;
  end;

  Result := Result and inherited;
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
  VL: TEpiCustomValueLabel;
begin
  if not Assigned(VLG.FocusedNode) then exit;

  NewNode := VLG.GetNextSibling(VLG.FocusedNode);
  if not Assigned(NewNode) then
    NewNode := VLG.GetPreviousSibling(VLG.FocusedNode);


  VL := ValueLabelFromNode(VLG.FocusedNode);
  with VL do
    if MessageDlg('Warning',
         Format('Are you sure you want to delete "%s = %s"?',[ValueAsString, TheLabel.Text]),
         mtWarning, mbYesNo, 0, mbNo) = mrNo then exit;

  VLG.DeleteNode(VLG.FocusedNode);
  VL.Free;

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
  Node := VLG.AddChild(nil, FValueLabelSet.NewValueLabel);

  if FValueLabelSet.LabelType in [ftFloat,ftInteger] then
  begin
    if Assigned(Last) then
      VLG.Text[Node, 0] := FloatToStr(StrToFloat(ValueLabelFromNode(Last).ValueAsString) + 1)
    else
      VLG.Text[Node, 0] := '0';
  end;

  VLG.FocusedNode := Node;
  VLG.FocusedColumn := 0;
  VLG.Selected[Node] := true;

  VLG.EndUpdate;
end;

procedure TValueLabelGridFrame.VLGChecking(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var NewState: TCheckState; var Allowed: Boolean);
begin
  ValueLabelFromNode(Node).IsMissingValue := NewState in [csCheckedNormal, csCheckedPressed];
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

procedure TValueLabelGridFrame.VLGGetNodeText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  VL: TEpiCustomValueLabel;
begin
  VL := ValueLabelFromNode(Node);
  with VL do
  begin
    case Column of
      0: CellText := ValueAsString;
      1: CellText := TheLabel.Text;
      2: CellText := '';
    end;
  end;
end;

procedure TValueLabelGridFrame.VLGInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  P, //: PtrUInt;
  D: Pointer;
begin
  Node^.CheckType := ctCheckBox;
  if not Assigned(ValueLabelFromNode(Node)) then
  begin
    Pointer(Sender.GetNodeData(Node)^) := FValueLabelSet[Node^.Index];
    if ValueLabelFromNode(Node).IsMissingValue then
      Node^.CheckState := csCheckedNormal;
  end;
end;

procedure TValueLabelGridFrame.VLGKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  DoShowHintMsg(nil, '');

  case Key of
    VK_RETURN:
      begin
        if ssCtrl in Shift then
        begin
          DoAddLine;
          Key := VK_UNKNOWN;
        end;

        if (Shift = []) then
        begin
          if not Assigned(VLG.FocusedNode) then
            VLG.FocusedNode := VLG.GetFirstSelected(false);

          VLG.EditNode(VLG.FocusedNode, VLG.FocusedColumn);
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

procedure TValueLabelGridFrame.VLGSendPostEdit(Data: PtrInt);
begin
  VLG.FocusedColumn := 1;
  VLG.EditNode(PVirtualNode(Data), 1);
end;

procedure TValueLabelGridFrame.VLGSetNodeText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; const NewText: String);
var
  VL: TEpiCustomValueLabel;
begin
  VL := ValueLabelFromNode(Node);
  with VL do
  begin
    case Column of
      0: case FValueLabelSet.LabelType of
           ftInteger:     TEpiIntValueLabel(VL).Value := StrToInt(NewText);
           ftFloat:       TEpiFloatValueLabel(Vl).Value := StrToFloat(NewText);
           ftString:      TEpiStringValueLabel(VL).Value := NewText;
           ftUpperString: TEpiStringValueLabel(VL).Value := UTF8UpperCase(NewText);
         end;
      1: TheLabel.Text := NewText;
      2: ; // do nothing
    end;
  end;
end;

procedure TValueLabelGridFrame.VLGUTF8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
begin
  if UTF8Key = Char(VK_SPACE) then exit;
  if UTF8Key = Char(VK_RETURN) then exit;

  if not Assigned(VLG.FocusedNode) then
    VLG.FocusedNode := VLG.GetFirstSelected(false);

  VLG.EditNode(VLG.FocusedNode, VLG.FocusedColumn);
end;

procedure TValueLabelGridFrame.SetValueLabelSet(AValue: TEpiValueLabelSet);
begin
  if FValueLabelSet = AValue then Exit;
  FValueLabelSet := AValue;

  VLG.Clear;
  VLG.RootNodeCount := FValueLabelSet.Count;
end;

function TValueLabelGridFrame.ValueLabelFromNode(Node: PVirtualNode
  ): TEpiCustomValueLabel;
begin
  Result := TEpiCustomValueLabel(VLG.GetNodeData(Node)^);
end;

procedure TValueLabelGridFrame.DoShowHintMsg(Ctrl: TControl; const Msg: String);
begin
  if Assigned(OnShowHintMsg) then
    OnShowHintMsg(Self, Ctrl, Msg);
end;

constructor TValueLabelGridFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FVLG := TVirtualStringTree.Create(Self);
  with VLG do
  begin
    Align := alClient;
    Parent := Self;
    Color := clNone;
    NodeDataSize := SizeOF(PEpiValueLabel);
    WantTabs := true;
    TabStop := true;

    // Events:
    OnInitNode      := @VLGInitNode;
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
    MiscOptions := [toCheckSupport, toEditable, toGridExtensions, toWheelPanning, toEditOnDblClick];
    PaintOptions := [toShowHorzGridLines, toShowVertGridLines, toThemeAware];
    SelectionOptions := [toExtendedFocus, toRightClickSelect, toCenterScrollIntoView];
  end;

  with VLG.Header do
  begin
    Options := [hoAutoResize, hoColumnResize, hoDblClickResize, hoVisible];
    with Columns.Add do
    begin
      Text := 'Value';
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

function TValueLabelGridFrame.ValidateGridEntries: boolean;
begin
  Result := true;
end;

end.

