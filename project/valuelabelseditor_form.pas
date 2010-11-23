unit valuelabelseditor_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, ExtDlgs, Grids, ActnList, Menus, epivaluelabels,
  epidocument, epidatafilestypes, epicustombase, LMessages;

type

  TValueLabelValidateEntryFunc =
    function (sender: TObject; aCol, aRow: Integer; const OldValue: string;
      var NewValue: String): boolean of object;

  { TValueLabelGrid }

  TValueLabelGrid = class(TStringGrid)
  private
    FOnValidateEntry: TValueLabelValidateEntryFunc;
  protected
    function ValidateEntry(const ACol, ARow: Integer; const OldValue: string;
       var NewValue: string): boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    property OnValidateEntry: TValueLabelValidateEntryFunc read FOnValidateEntry write FOnValidateEntry;
  end;

  { TValueLabelEditor }

  TValueLabelEditor = class(TForm)
    ImageList1: TImageList;
    InsertGridRowAction: TAction;
    DeleteGridRowAction: TAction;
    GridActionList: TActionList;
    NewStringValueLabelSetAction: TAction;
    NewFloatValueLabelSetAction: TAction;
    NewIntValueLabelSetAction: TAction;
    DeleteValueLabelSets: TAction;
    NewIntValueLabelSetMenuItem: TMenuItem;
    NewStringValueLabelSetMenuItem: TMenuItem;
    NewFloatValueLabelSetMenuItem: TMenuItem;
    ValueLabelEditorStatusBar: TStatusBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    TreeViewActionList: TActionList;
    Panel1: TPanel;
    NewValueLabelSetTypeDropDown: TPopupMenu;
    Splitter1: TSplitter;
    ToolBar1: TToolBar;
    ValueLabelSetTreeView: TTreeView;
    procedure DeleteGridRowExecute(Sender: TObject);
    procedure DeleteGridRowUpdate(Sender: TObject);
    procedure DeleteValueLabelSetsExecute(Sender: TObject);
    procedure DeleteValueLabelSetsUpdate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
    procedure InsertGridRowExecute(Sender: TObject);
    procedure NewFloatValueLabelSetActionExecute(Sender: TObject);
    procedure NewIntValueLabelSetActionExecute(Sender: TObject);
    procedure NewStringValueLabelSetActionExecute(Sender: TObject);
    procedure ValueLabelEditorStatusBarResize(Sender: TObject);
    procedure ValueLabelSetTreeViewDblClick(Sender: TObject);
    procedure ValueLabelSetTreeViewEdited(Sender: TObject; Node: TTreeNode;
      var S: string);
    procedure ValueLabelSetTreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure ValueLabelSetTreeViewChanging(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
    procedure ValueLabelSetTreeViewEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure ValueLabelSetTreeViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FDblClickedValueLabelSet: TEpiValueLabelSet;
    { private declarations }
    FValueLabelsGrid: TValueLabelGrid;
    FValueLabelSets: TEpiValueLabelSets;
    FCurrentVLSet: TEpiValueLabelSet;
    FHintWindow: THintWindow;
    constructor Create(TheOwner: TComponent); override;
    destructor  Destroy; override;
    function    DoNewValueLabelSet(Ft: TEpiFieldType): TEpiValueLabelSet;
    procedure   UpdateGridCells;
    procedure   ValueLabelsGridCheckboxToggled(sender: TObject; aCol, aRow: Integer; aState: TCheckboxState);
    procedure   ValueLabelsGridColRowMoved(Sender: TObject; IsColumn: Boolean; sIndex, tIndex: Integer);
    function    ValueLabelsGridValidateEntry(sender: TObject; aCol, aRow: Integer; const OldValue: string; var NewValue: String): boolean;
    procedure   ValueLabelsGridEditingDone(Sender: TObject);
    procedure   ValueLabelsGridKeyPress(Sender: TObject; var Key: Char);
    procedure   ValueLabelsGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure   DisplayHint(Const S: string; Const Ctrl: TControl; Const Pos: TPoint);
    procedure   CalculateStatusbar;
    procedure   UpdateStatusbar;
    procedure   SetEpiDocument(EpiDoc: TEpiDocument);
    procedure   UpdateTreeView;
    procedure   ValueLabelSetHook(Sender: TObject; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
    procedure   ValueLabelSetsHook(Sender: TObject; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
  public
    { public declarations }
    class procedure RestoreDefaultPos;
    property    ValueLabelsGrid: TValueLabelGrid read FValueLabelsGrid;
    property    DblClickedValueLabelSet: TEpiValueLabelSet read FDblClickedValueLabelSet;
  end;

function GetValueLabelsEditor(EpiDoc: TEpiDocument): TValueLabelEditor;

implementation

{$R *.lfm}

uses
  project_frame, math, LCLType, main, settings2, settings2_var;

var
  TheValueLabelEditor: TValueLabelEditor = nil;

function GetValueLabelsEditor(EpiDoc: TEpiDocument): TValueLabelEditor;
begin
  if not Assigned(TheValueLabelEditor) then
    TheValueLabelEditor := TValueLabelEditor.Create(MainForm);
  TheValueLabelEditor.SetEpiDocument(EpiDoc);
  result := TheValueLabelEditor;
end;

{ TValueLabelEditor }

procedure TValueLabelEditor.ValueLabelSetTreeViewEdited(Sender: TObject;
  Node: TTreeNode; var S: string);
var
  VLSet: TEpiValueLabelSet;
  R: TRect;
begin
  VLSet := TEpiValueLabelSet(Node.Data);

  // Do not allow blanks.
  if S = '' then
  begin
    R := Node.DisplayRect(false);
    DisplayHint('Blank names not allowed', ValueLabelSetTreeView, Point(R.Left, R.Bottom + 3));
    S := Node.Text;
    exit;
  end;

  VLSet.Name := S;
  // Setting the name validates internally and if not possible, the old name
  // is still used, hence update 'S'.
  if VLSet.Name <> S then
  begin
    R := Node.DisplayRect(false);
    DisplayHint('Name must be unique', ValueLabelSetTreeView, Point(R.Left, R.Bottom + 3));
    S := VLSet.Name;
  end;
  Node.Text := S;
  ValueLabelSetTreeView.CustomSort(nil);
end;

procedure TValueLabelEditor.ValueLabelSetTreeViewChange(Sender: TObject;
  Node: TTreeNode);
var
  TreeView: TTreeView absolute Sender;
  i: Integer;
begin
  // Happens after the change is done.
  if csDestroying in TreeView.ComponentState then exit;

  if not Assigned(Node) then
  begin
    ValueLabelsGrid.RowCount := 1;
    FCurrentVLSet := nil;
    exit;
  end;

  FCurrentVLSet := TEpiValueLabelSet(Node.Data);
  UpdateGridCells;
end;

procedure TValueLabelEditor.ValueLabelSetTreeViewChanging(Sender: TObject;
  Node: TTreeNode; var AllowChange: Boolean);
var
  TreeView: TTreeView absolute Sender;
begin
  // Happens before change is done.
  if csDestroying in TreeView.ComponentState then exit;
end;

procedure TValueLabelEditor.ValueLabelSetTreeViewEditing(Sender: TObject;
  Node: TTreeNode; var AllowEdit: Boolean);
begin
  FHintWindow.Hide;
end;

procedure TValueLabelEditor.ValueLabelSetTreeViewKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  FHintWindow.Hide;
  if (Key = VK_INSERT) and (Shift = []) then
  begin
    ValueLabelSetTreeView.Selected.EditText;
    Key := 0;
  end;
end;

procedure TValueLabelEditor.ValueLabelsGridCheckboxToggled(sender: TObject;
  aCol, aRow: Integer; aState: TCheckboxState);
begin
  // Missing Value.
  if aCol = 4 then
  begin
    FCurrentVLSet[aRow - 1].IsMissingValue := (aState = cbChecked);
    Exit;
  end;
end;

procedure TValueLabelEditor.ValueLabelsGridColRowMoved(Sender: TObject;
  IsColumn: Boolean; sIndex, tIndex: Integer);
var
  Item: TEpiCustomItem;
  i: LongInt;
begin
  Dec(sIndex); Dec(tIndex);
  Item := FCurrentVLSet.DeleteItem(sIndex);
  FCurrentVLSet.InsertItem(tIndex, Item);
  for i := Math.min(sIndex,tIndex) to Math.Max(sIndex,tIndex) do
    FCurrentVLSet[i].Order := i+1;

  UpdateGridCells;
end;

function TValueLabelEditor.ValueLabelsGridValidateEntry(sender: TObject; aCol,
  aRow: Integer; const OldValue: string; var NewValue: String): boolean;
var
  R: TRect;
  P: TPoint;
  S: String;
begin
  // Value change
  result := true;
  if aCol = 1 then
  begin
    // NewValue must not exists!
    try
      S := 'Incorrect value.';
      if (NewValue <> OldValue) and
         (FCurrentVLSet.ValueLabelExists[NewValue])
      then begin
        S := 'Value already exists.';
        abort;
      end;

      case FCurrentVLSet.LabelType of
        ftInteger: TEpiIntValueLabel(FCurrentVLSet[aRow - 1]).Value := StrToInt(NewValue);
        ftFloat:   TEpiFloatValueLabel(FCurrentVLSet[aRow - 1]).Value := StrToFloat(NewValue);
        ftString:  TEpiStringValueLabel(FCurrentVLSet[aRow - 1]).Value := NewValue;
      end;
    except
      R := ValueLabelsGrid.CellRect(aCol, aRow);
      DisplayHint(S, ValueLabelsGrid, Point(R.Left, R.Bottom + 3));
      NewValue := OldValue;
      result := false;
    end;
    Exit;
  end;

  // Label change
  if aCol = 2 then
  begin
    FCurrentVLSet[aRow - 1].TheLabel.Text := NewValue;
    Exit;
  end;

  // Label (tranlation) change
  if aCol = 3 then
  begin
    // TODO : Language
    FCurrentVLSet[aRow - 1].TheLabel.TextLang['fr'] := NewValue;
    Exit;
  end;
end;

procedure TValueLabelEditor.ValueLabelsGridEditingDone(Sender: TObject);
begin
  UpdateGridCells;
end;

constructor TValueLabelEditor.Create(TheOwner: TComponent);
var
  Cl: TGridColumn;
begin
  inherited Create(TheOwner);

  FValueLabelsGrid := TValueLabelGrid.Create(Self);
  with ValueLabelsGrid do
  begin
    ColCount := 1;
    RowCount := 1;

    Cl := Columns.Add;
    Cl.Title.Caption := 'Value';

    Cl := Columns.Add;
    Cl.Title.Caption := 'Label (catagory)';

    Cl := Columns.Add;
    Cl.Title.Caption := 'Label';
    Cl.Visible := false;

    Cl := Columns.Add;
    Cl.Title.Caption := 'Missing Value';
    Cl.ButtonStyle := cbsCheckboxColumn;

    OnCheckboxToggled := @ValueLabelsGridCheckboxToggled;
    OnColRowMoved     := @ValueLabelsGridColRowMoved;
    OnValidateEntry   := @ValueLabelsGridValidateEntry;
    OnEditingDone     := @ValueLabelsGridEditingDone;
    OnKeyPress        := @ValueLabelsGridKeyPress;
    OnKeyDown         := @ValueLabelsGridKeyDown;

    UseXORFeatures := true;
    TitleStyle     := tsStandard;
    Options        := Options + [goEditing, goRowMoving, goColSizing];
    AutoAdvance    := aaRightDown;
    Align          := alClient;
    Parent         := Self;
  end;

  FHintWindow := THintWindow.Create(Self);
  FHintWindow.HideInterval := 10 * 1000; // TODO : Adjust hint-timeout in settings.
  FHintWindow.AutoHide := true;
end;

destructor TValueLabelEditor.Destroy;
var
  B: Boolean;
begin
  B:=true;
  FormCloseQuery(nil, B); // enforces save of position in case program is closed.
  inherited Destroy;
end;

function TValueLabelEditor.DoNewValueLabelSet(Ft: TEpiFieldType
  ): TEpiValueLabelSet;
var
  i: Integer;
begin
  result := FValueLabelSets.NewValueLabelSet(Ft);
  result.name := '(untitled)';
  i := 1;
  while (result.name = '') do
  begin
    result.Name := format('(untitled%d)', [i]);
    inc(i);
  end;
  ValueLabelSetTreeView.Selected := ValueLabelSetTreeView.Items.FindNodeWithData(result);
  ValueLabelSetTreeView.CustomSort(nil);

  // Set manually as the ValueLabelSetTreeViewChange has not been fired at this point!
  FCurrentVLSet := result;

  // Add at least on line.
  InsertGridRowAction.Execute;
  ValueLabelSetTreeView.Selected.EditText;
  UpdateStatusbar;
end;

procedure TValueLabelEditor.UpdateGridCells;
var
  i: Integer;
begin
  if not Assigned(FCurrentVLSet) then exit;
  with ValueLabelsGrid do
  begin
    BeginUpdate;
    RowCount := FCurrentVLSet.Count + 1;
    for i := 0 to FCurrentVLSet.Count - 1 do
    begin
      Cells[0, i + 1] := IntToStr(FCurrentVLSet[i].Order);
      Cells[1, i + 1] := FCurrentVLSet[i].ValueAsString;
      Cells[2, i + 1] := FCurrentVLSet[i].TheLabel.Text;
//      Cells[3, i + 1] := FCurrentVLSet[i].TheLabel.TextLang['fr'];
      Cells[4, i + 1] := BoolToStr(FCurrentVLSet[i].IsMissingValue, '1', '0');
    end;
    AutoSizeColumns;
    EndUpdate;
  end;
end;

procedure TValueLabelEditor.SetEpiDocument(EpiDoc: TEpiDocument);
var
  i: Integer;
begin
  if EpiDoc.ValueLabelSets <> FValueLabelSets then
  begin
    // Unregister previous hooks.
    if Assigned(FValueLabelSets) then
    begin
      FValueLabelSets.UnRegisterOnChangeHook(@ValueLabelSetsHook);
      for i := 0 to FValueLabelSets.Count -1 do
        FValueLabelSets[i].UnRegisterOnChangeHook(@ValueLabelSetHook);
    end;

    FValueLabelSets := EpiDoc.ValueLabelSets;
    FValueLabelSets.RegisterOnChangeHook(@ValueLabelSetsHook, true);
    for i := 0 to FValueLabelSets.Count -1 do
      FValueLabelSets[i].RegisterOnChangeHook(@ValueLabelSetHook, True);
  end;

  UpdateTreeView;
end;

procedure TValueLabelEditor.UpdateTreeView;
var
  i: Integer;
  Node: TTreeNode;
  PreSelected: TObject;
begin
  if Assigned(ValueLabelSetTreeView.Selected) then
    PreSelected := TObject(ValueLabelSetTreeView.Selected.Data)
  else
    PreSelected := nil;

  ValueLabelSetTreeView.BeginUpdate;
  ValueLabelSetTreeView.SortType := stText;
  ValueLabelSetTreeView.Items.Clear;
  for i := 0 to FValueLabelSets.Count - 1 do
  begin
    Node := ValueLabelSetTreeView.Items.AddObject(nil, FValueLabelSets[i].Name, FValueLabelSets[i]);
    case FValueLabelSets[i].LabelType of
      ftInteger: Node.ImageIndex := 0;
      ftFloat:   Node.ImageIndex := 1;
      ftString:  Node.ImageIndex := 2;
    end;
  end;
  ValueLabelSetTreeView.CustomSort(nil);
  ValueLabelSetTreeView.EndUpdate;
  ValueLabelSetTreeView.Selected := ValueLabelSetTreeView.Items.FindNodeWithData(PreSelected);

  ValueLabelsGrid.Cells[0,0] := 'Order';
  UpdateStatusbar;
end;

procedure TValueLabelEditor.ValueLabelSetHook(Sender: TObject;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  if (EventGroup = eegValueLabels) and (EventType = Word(evceName)) then
    UpdateTreeView;
end;

procedure TValueLabelEditor.ValueLabelSetsHook(Sender: TObject;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  if (EventGroup = eegCustomBase) and (EventType = Word(ecceAddItem)) then
  begin
    TEpiValueLabelSet(Data).RegisterOnChangeHook(@ValueLabelSetHook, true);
    UpdateTreeView;
  end;

  if (EventGroup = eegCustomBase) and (EventType = Word(ecceDelItem)) then
  begin
    TEpiValueLabelSet(Data).UnRegisterOnChangeHook(@ValueLabelSetHook);
    UpdateTreeView;
  end;
end;

class procedure TValueLabelEditor.RestoreDefaultPos;
var
  Aform: TForm;
begin
  Aform := TForm.Create(nil);
  Aform.Width := 800;
  Aform.Height := 600;
  Aform.top := (Screen.Monitors[0].Height - Aform.Height) div 2;
  Aform.Left := (Screen.Monitors[0].Width - Aform.Width) div 2;
  SaveFormPosition(Aform, 'ValueLabelEditorForm');
  AForm.free;
end;

procedure TValueLabelEditor.DeleteValueLabelSetsExecute(Sender: TObject);
var
  Node: TTreeNode;
  VLSet: TEpiValueLabelSet;
  NNode: TTreeNode;
  Data: Pointer;
begin
  Node := ValueLabelSetTreeView.Selected;
  NNode := Node.GetNext;
  if not Assigned(NNode) then
    NNode := Node.GetPrev;
  if Assigned(NNode) then
    Data := NNode.Data
  else
    Data := nil;


  VLSet := TEpiValueLabelSet(Node.Data);

  {$IFNDEF EPI_DEBUG}
  if not (ssShift in GetKeyShiftState) then
  begin
    if MessageDlg('Warning',
      format('Are you sure you want to delete "%s"?', [VLSet.Name]),
      mtWarning, mbYesNo, 0, mbNo) = mrNo then exit;
  end;
  {$ENDIF}

  FValueLabelSets.RemoveItem(VLSet);
  VLSet.Free;
  ValueLabelSetTreeView.Selected := ValueLabelSetTreeView.Items.FindNodeWithData(Data);
end;

procedure TValueLabelEditor.DeleteValueLabelSetsUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled :=
    (ValueLabelSetTreeView.Focused) and
    (ValueLabelSetTreeView.Items.Count > 0);
end;

procedure TValueLabelEditor.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  if ManagerSettings.SaveWindowPositions then
    SaveFormPosition(Self, 'ValueLabelEditorForm');
end;

procedure TValueLabelEditor.DeleteGridRowExecute(Sender: TObject);
var
  Idx: LongInt;
  i: LongInt;
begin
  with ValueLabelsGrid do
  begin
    Idx := Row;

    {$IFNDEF EPI_DEBUG}
    if not (ssShift in GetKeyShiftState) then
    begin
      if MessageDlg('Warning',
        Format('Are you sure you want to delete "%s = %s"?',[Cells[1, Idx], Cells[2, Idx]]),
        mtWarning, mbYesNo, 0, mbNo) = mrNo then exit;
    end;
    {$ENDIF}

    BeginUpdate;
    DeleteRow(Idx);
    EndUpdate;
  end;
  Dec(Idx);
  FCurrentVLSet.DeleteItem(Idx).Free;
  for i := Idx to FCurrentVLSet.Count -1 do
    FCurrentVLSet[i].Order := i;
  UpdateGridCells;
end;

procedure TValueLabelEditor.DeleteGridRowUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled :=
    (Assigned(FCurrentVLSet)) and
    (ValueLabelsGrid.Focused) and
    (ValueLabelsGrid.RowCount > 1);
end;

procedure TValueLabelEditor.FormShow(Sender: TObject);
var
  A: TCollectionItem;
begin
  if ManagerSettings.SaveWindowPositions then
    LoadFormPosition(Self, 'ValueLabelEditorForm');

  if ValueLabelSetTreeView.Items.Count > 0 then
    ValueLabelSetTreeView.Selected := ValueLabelSetTreeView.Items[0];
  UpdateStatusbar;
  ValueLabelsGrid.AutoSizeColumns;
  FDblClickedValueLabelSet := nil;
end;

procedure TValueLabelEditor.ValueLabelsGridKeyPress(Sender: TObject;
  var Key: Char);
begin
  if not Assigned(FCurrentVLSet) then exit;

  if (FCurrentVLSet.LabelType = ftFloat) and (Key in ['.', ','])  then
    Key := DecimalSeparator;
end;

procedure TValueLabelEditor.ValueLabelsGridKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  FHintWindow.Hide;
end;

procedure TValueLabelEditor.DisplayHint(const S: string; const Ctrl: TControl;
  const Pos: TPoint);
var
  P: TPoint;
  R: TRect;
begin
  P := Ctrl.ClientToScreen(Pos);
  R := FHintWindow.CalcHintRect(0, S, nil);
  OffsetRect(R, P.X, P.Y);
  FHintWindow.ActivateHint(R, S);
end;

procedure TValueLabelEditor.CalculateStatusbar;
var
  i: Integer;
  W: LongInt;
begin
  with ValueLabelEditorStatusBar do
  begin
    W := ClientWidth div Panels.Count;
    for i := 0 to Panels.Count - 1 do
      Panels[i].Width := W;
  end;
  UpdateStatusbar;
end;

procedure TValueLabelEditor.UpdateStatusbar;
var
  I, F, S: Integer;
  j: Integer;
begin
  I := 0; F := 0; S := 0;
  for j := 0 to FValueLabelSets.Count - 1 do
  case FValueLabelSets[j].LabelType of
    ftInteger: Inc(I);
    ftFloat:   Inc(F);
    ftString:  Inc(S);
  end;

  with ValueLabelEditorStatusBar do
  begin
    Panels[0].Text := Format('Integer: %d', [I]);
    Panels[1].Text := Format('Float: %d', [F]);
    Panels[2].Text := Format('String: %d', [S]);
  end;
end;

procedure TValueLabelEditor.InsertGridRowExecute(Sender: TObject);
var
  VL: TEpiCustomValueLabel;
  F: Extended;
  I: Integer;
begin
  if not Assigned(FCurrentVLSet) then exit;

  // This helps the grid not to f*ck up when pressing "ins" during grid-entry.
  if ValueLabelsGrid.EditorMode then
    ValueLabelsGrid.EditorMode := false;

  VL := FCurrentVLSet.NewValueLabel;
  if FCurrentVLSet.Count > 1 then
    case FCurrentVLSet.LabelType of
      ftInteger:
        begin
          I := TEpiIntValueLabel(FCurrentVLSet[FCurrentVLSet.Count-2]).Value + 1;
          while FCurrentVLSet.ValueLabelExists[I] do Inc(i);
          TEpiIntValueLabel(VL).Value := I;
        end;
      ftFloat:
        begin
          F := TEpiFloatValueLabel(FCurrentVLSet[FCurrentVLSet.Count-2]).Value + 1;
          while FCurrentVLSet.ValueLabelExists[F] do F := F + 1;
          TEpiFloatValueLabel(VL).Value := F;
        end;
    end;

  with ValueLabelsGrid do
  begin
    if not Focused then SetFocus;
    BeginUpdate;
    InsertColRow(false, RowCount);
    Cells[0, RowCount-1] := IntToStr(VL.Order);
    Cells[1, RowCount-1] := VL.ValueAsString;
    Cells[4, RowCount-1] := '0';
    EndUpdate;
    Col := 1;
    Row := RowCount - 1;
  end;
end;

procedure TValueLabelEditor.NewFloatValueLabelSetActionExecute(Sender: TObject
  );
begin
  DoNewValueLabelSet(ftFloat);
end;

procedure TValueLabelEditor.NewIntValueLabelSetActionExecute(Sender: TObject);
begin
  DoNewValueLabelSet(ftInteger);
end;

procedure TValueLabelEditor.NewStringValueLabelSetActionExecute(Sender: TObject
  );
begin
  DoNewValueLabelSet(ftString);
end;

procedure TValueLabelEditor.ValueLabelEditorStatusBarResize(Sender: TObject);
begin
  CalculateStatusbar;
end;

procedure TValueLabelEditor.ValueLabelSetTreeViewDblClick(Sender: TObject);
var
  P: TPoint;
  N: TTreeNode;
begin
  if (fsModal in FormState) then
  begin
    P := ValueLabelSetTreeView.ScreenToClient(Mouse.CursorPos);
    N := ValueLabelSetTreeView.GetNodeAt(P.X, P.Y);
    if Assigned(N) then
      FDblClickedValueLabelSet := TEpiValueLabelSet(N.Data);
    ModalResult := mrOk;
  end;
end;

{ TValueLabelGrid }

function TValueLabelGrid.ValidateEntry(const ACol, ARow: Integer;
  const OldValue: string; var NewValue: string): boolean;
begin
  if assigned(OnValidateEntry) then
    result := OnValidateEntry(Self, ACol, ARow, OldValue, NewValue)
  else
    result := inherited ValidateEntry(ACol, ARow, OldValue, NewValue);
end;

procedure TValueLabelGrid.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (Col = 4) then
  begin
    if (Row = (RowCount - 1)) then
      TValueLabelEditor(Parent).InsertGridRowAction.Execute
    else begin
      Row := Row + 1;
      Col := 0;
    end;
    Key := 0;
  end;
  inherited KeyDown(Key, Shift);
end;

end.

