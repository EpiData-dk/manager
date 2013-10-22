unit valuelabelseditor_form2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Buttons, StdCtrls, ComCtrls, Menus, VirtualTrees, epidatafilestypes,
  valuelabelgrid_frame, epivaluelabels, manager_messages, LMessages, epicustombase;

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
    AddBtn: TToolButton;
    DelBtn: TToolButton;
    VLSetsTree: TVirtualStringTree;
    procedure DelBtnClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure AddBtnClick(Sender: TObject);
    procedure VLSetsTreeFocusChanging(Sender: TBaseVirtualTree; OldNode,
      NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
      var Allowed: Boolean);
    procedure VLSetsTreeGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure VLSetsTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure VLSetsTreeKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure VLSetsTreeNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; const NewText: String);
  private
    { private declarations }
    FGridFrame: TValueLabelGridFrame;
    FValueLabelSets: TEpiValueLabelSets;
    procedure DoAddNewValueLabelSet(FieldType: TEpiFieldType);
    procedure DoDeleteValueLabelSet(Node: PVirtualNode);
    procedure SetValueLabelSets(AValue: TEpiValueLabelSets);
    function  ValueLabelSetFromNode(Node: PVirtualNode): TEpiValueLabelSet;
    procedure LMEditNode(var Message: TLMessage); message LM_VLEDIT_STARTEDIT;
    procedure LMCheckFocusedNode(var Message: TLMessage); message LM_VLEDIT_FOCUSCHECK;
  private
    { ValueLabelSet(s) Hook / Update }
    FLocalUpdating: boolean;
    procedure UpdateVLSetsTree;
    procedure ValueLabelsHook(Sender: TObject; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
  private
    { Hint }
    FHintWindow: THintWindow;
    procedure DoShowHintMsg(Sender: TObject; Ctrl: TControl; Const Msg: string);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property ValueLabelSets: TEpiValueLabelSets read FValueLabelSets write SetValueLabelSets;
  end;


procedure ShowValueLabelEditor2(ValueLabelSet: TEpiValueLabelSets);
procedure CloseValueLabelEditor2;
procedure RestoreDefaultPosValueLabelEditor2;

implementation

{$R *.lfm}

uses
  Main, settings2_var, settings2, LCLIntf, LCLType;

var
  Editor: TValueLabelEditor2 = nil;

procedure ShowValueLabelEditor2(ValueLabelSet: TEpiValueLabelSets);
begin
  if not Assigned(Editor) then
    Editor := TValueLabelEditor2.Create(MainForm);
  Editor.ValueLabelSets := ValueLabelSet;
  Editor.Show;
end;

procedure CloseValueLabelEditor2;
begin
  FreeAndNil(Editor);
end;

procedure RestoreDefaultPosValueLabelEditor2;
var
  F: TForm;
begin
  if Assigned(Editor) then
    F := Editor
  else
    F := TForm.Create(nil);

  with F do
  begin
    LockRealizeBounds;
    Width := 700;
    Height := 700;
    Left := 100;
    Top := 100;
    UnlockRealizeBounds;
  end;
  SaveFormPosition(F, F.ClassName);

  if F <> Editor then F.Free;
end;

{ TValueLabelEditor2 }

procedure TValueLabelEditor2.AddBtnClick(Sender: TObject);
begin
  DoAddNewValueLabelSet(ftInteger);
end;

procedure TValueLabelEditor2.VLSetsTreeFocusChanging(Sender: TBaseVirtualTree;
  OldNode, NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
  var Allowed: Boolean);
begin
  if OldNode = NewNode then exit;

  if Assigned(OldNode) then
    Allowed := FGridFrame.ValidateGridEntries;

  if not Allowed then exit;

  FGridFrame.ValueLabelSet := ValueLabelSetFromNode(NewNode);

  PostMessage(Self.Handle, LM_VLEDIT_FOCUSCHECK, WPARAM(NewNode), 0);
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

procedure TValueLabelEditor2.VLSetsTreeKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  DoShowHintMsg(nil, nil, '');

  case Key of
    VK_RETURN:
      begin
        PostMessage(Self.Handle, LM_VLEDIT_STARTEDIT, WPARAM(VLSetsTree.FocusedNode), 0);
        Key := VK_UNKNOWN;
      end;
    VK_DELETE:
      begin
        DoDeleteValueLabelSet(VLSetsTree.FocusedNode);
        Key := VK_UNKNOWN;
      end;
  end;
end;

procedure TValueLabelEditor2.VLSetsTreeNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; const NewText: String);
var
  S: String;
begin
  FLocalUpdating := true;

  if Trim(NewText) = '' then
  begin
    DoShowHintMsg(nil, VLSetsTree, 'Value label name must not be empty!');
    Exit;
  end;

  if ValueLabelSetFromNode(Node).Name = NewText then exit;

  S := ValueLabelSetFromNode(Node).Name;
  ValueLabelSetFromNode(Node).Name := NewText;
  if S = ValueLabelSetFromNode(Node).Name then
    DoShowHintMsg(nil, VLSetsTree, 'Value label name already used!');
  FLocalUpdating := false;
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
    TabOrder := Panel1.TabOrder + 1;
  end;
end;

procedure TValueLabelEditor2.FormDeactivate(Sender: TObject);
begin
  if (Assigned(FValueLabelSets)) and
     (Assigned(FGridFrame.ValueLabelSet)) and
     (not FGridFrame.ValidateGridEntries) then
  begin
    Self.SetFocus;
  end;
end;

procedure TValueLabelEditor2.FormShow(Sender: TObject);
begin
  if ManagerSettings.SaveWindowPositions then
    LoadFormPosition(Self, Self.ClassName);
end;

procedure TValueLabelEditor2.DelBtnClick(Sender: TObject);
begin
  DoDeleteValueLabelSet(VLSetsTree.FocusedNode);
end;

procedure TValueLabelEditor2.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  if Assigned(FGridFrame) then
    CanClose := FGridFrame.ValidateGridEntries;

  if ManagerSettings.SaveWindowPositions then
    SaveFormPosition(Self, Self.ClassName);
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
var
  Node: PVirtualNode;
  i: Integer;
  VLSet: TEpiValueLabelSet;
begin
  if not FGridFrame.ValidateGridEntries then exit;
  FLocalUpdating := true;

  VLSet := FValueLabelSets.NewValueLabelSet(FieldType);
  With VLSet do
  begin
    Name := '(Untitled)';

    i := 1;
    while (name = '') do
    begin
      Name := format('(Untitled%d)', [i]);
      inc(i);
    end;
  end;

  Node := VLSetsTree.AddChild(nil);
  VLSetsTree.FocusedNode := Node;
  FGridFrame.NewLineBtn.Click;

  FLocalUpdating := False;
  PostMessage(Self.Handle, LM_VLEDIT_STARTEDIT, WParam(Node), 0);
end;

procedure TValueLabelEditor2.DoDeleteValueLabelSet(Node: PVirtualNode);
var
  VL: TEpiValueLabelSet;
  NewNode: PVirtualNode;
begin
  if not Assigned(Node) then exit;
  FLocalUpdating := true;

  VL := ValueLabelSetFromNode(Node);
  if MessageDlg('Warning',
    format('Are you sure you want to delete "%s"?', [VL.Name]),
    mtWarning, mbYesNo, 0, mbNo) = mrNo then exit;

  NewNode := VLSetsTree.GetNextSibling(Node);
  if not Assigned(NewNode) then
    NewNode := VLSetsTree.GetPreviousSibling(Node);

  VLSetsTree.DeleteNode(Node);
  VL.Free;

  VLSetsTree.FocusedNode := NewNode;
  if Assigned(NewNode) then
    VLSetsTree.Selected[NewNode] := true
  else
    FGridFrame.ValueLabelSet := nil;

  FLocalUpdating := false;
end;

procedure TValueLabelEditor2.SetValueLabelSets(AValue: TEpiValueLabelSets);
var
  i: Integer;
begin
  if FValueLabelSets = AValue then Exit;

  if Assigned(FValueLabelSets) then
  begin // Unregister old hooks.
    FValueLabelSets.UnRegisterOnChangeHook(@ValueLabelsHook);
    for i := 0 to FValueLabelSets.Count - 1 do
      FValueLabelSets[i].UnRegisterOnChangeHook(@ValueLabelsHook);
  end;

  FValueLabelSets := AValue;

  if Assigned(FValueLabelSets) then
  begin // Register hook in new ValueLabelSets
    FValueLabelSets.RegisterOnChangeHook(@ValueLabelsHook, true);
    for i := 0 to FValueLabelSets.Count - 1 do
      FValueLabelSets[i].RegisterOnChangeHook(@ValueLabelsHook, true);
  end;

  VLSetsTree.RootNodeCount := FValueLabelSets.Count;
end;

function TValueLabelEditor2.ValueLabelSetFromNode(Node: PVirtualNode
  ): TEpiValueLabelSet;
begin
  Result := nil;
  if Assigned(Node) then
    result := FValueLabelSets[Node^.Index];
end;

procedure TValueLabelEditor2.LMEditNode(var Message: TLMessage);
var
  Node: PVirtualNode;
begin
  Node := PVirtualNode(Message.WParam);
  VLSetsTree.Selected[Node] := true;
  VLSetsTree.EditNode(Node, -1);
end;

procedure TValueLabelEditor2.LMCheckFocusedNode(var Message: TLMessage);
var
  Node: PVirtualNode;
begin
  Node := PVirtualNode(Message.WParam);

  if VLSetsTree.FocusedNode <> Node then
    VLSetsTree.FocusedNode := Node;

  if not VLSetsTree.Selected[Node] then
    VLSetsTree.Selected[Node] := true;
end;

procedure TValueLabelEditor2.UpdateVLSetsTree;
begin
  if FLocalUpdating then exit;

  VLSetsTree.BeginUpdate;
  VLSetsTree.Clear;
  VLSetsTree.RootNodeCount := FValueLabelSets.Count;
  VLSetsTree.EndUpdate;
end;

procedure TValueLabelEditor2.ValueLabelsHook(Sender: TObject;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  if Sender is TEpiValueLabelSet then
  begin
    if EventGroup = eegCustomBase then
    case TEpiCustomChangeEventType(EventType) of
      ecceDestroy: Exit;
      ecceUpdate,
      ecceName:    UpdateVLSetsTree;
    end;
    Exit;
  end;

  if Sender is TEpiValueLabelSets then
  begin
    if EventGroup = eegCustomBase then
    case TEpiCustomChangeEventType(EventType) of
      ecceDestroy: Exit;
      ecceAddItem:
        begin
          TEpiValueLabelSet(Data).RegisterOnChangeHook(@ValueLabelsHook, true);
          UpdateVLSetsTree;
        end;
      ecceDelItem:
        begin
          TEpiValueLabelSet(Data).UnRegisterOnChangeHook(@ValueLabelsHook);
          UpdateVLSetsTree;
        end;
    end;
  end;
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

constructor TValueLabelEditor2.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FLocalUpdating := false;
end;

destructor TValueLabelEditor2.Destroy;
var
  i: Integer;
begin
  if Assigned(FValueLabelSets) then
  begin // Unregister old hooks.
    FValueLabelSets.UnRegisterOnChangeHook(@ValueLabelsHook);
    for i := 0 to FValueLabelSets.Count - 1 do
      FValueLabelSets[i].UnRegisterOnChangeHook(@ValueLabelsHook);
  end;

  inherited Destroy;
  Editor := nil;
end;

end.

