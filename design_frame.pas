unit design_frame;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, ComCtrls, ActnList,
  Controls, Buttons, ExtCtrls, Dialogs, Menus, StdCtrls, UEpiDataFile,
  FieldEdit, Design_Field_Frame, AVL_Tree, LCLType, design_autoalign_form,
  UDataFileTypes;

type

  { TDesignFrame }

  TDesignFrame = class(TFrame)
    SaveFileAsAction: TAction;
    SaveFileAction: TAction;
    OpenFileAction: TAction;
    ImportStructureAction: TAction;
    NewYMDTodayFieldMenu: TMenuItem;
    NewMDYTodayFieldMenu: TMenuItem;
    NewDMYTodayFieldMenu: TMenuItem;
    TodayDateSubMenu: TMenuItem;
    NewSoundexFieldMenu: TMenuItem;
    NewUpperFieldMenu: TMenuItem;
    NewCryptFieldMenu: TMenuItem;
    StringSubMenu: TMenuItem;
    NewAutoIDMenu: TMenuItem;
    NewYMDFieldAction: TAction;
    NewMDYFieldAction: TAction;
    NewDMYFieldAction: TAction;
    AutoAlignLeftAdjustMenuItem: TMenuItem;
    AutoAlignEqualSpaceMenuItem: TMenuItem;
    NewLabelFieldAction: TAction;
    NewStringFieldAction: TAction;
    FontDialog1: TFontDialog;
    NewFloatFieldAction: TAction;
    NewIntFieldAction: TAction;
    DesignFrameActionList: TActionList;
    FieldToolBar: TToolBar;
    FieldToolBarImageList: TImageList;
    IntFieldBtn: TToolButton;
    EditFieldMenuItem: TMenuItem;
    FieldPopUp: TPopupMenu;
    DeleteFieldMenuItem: TMenuItem;
    OtherFieldsPopup: TPopupMenu;
    SaveDialog1: TSaveDialog;
    SelectorButton: TToolButton;
    FloatFieldBtn: TToolButton;
    ClearToolBtn: TToolButton;
    SaveToolBtn: TToolButton;
    FontSelectBtn: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    StringFieldBtn: TToolButton;
    LabelFieldBtn: TToolButton;
    OpenToolBtn: TToolButton;
    AutoAlignBtn: TToolButton;
    DMYFieldBtn: TToolButton;
    OtherFieldBtn: TToolButton;
    ImportStructureBtn: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    MDYFieldBtn: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    YMDFieldBtn: TToolButton;
    procedure FrameUnDock(Sender: TObject; Client: TControl;
      NewTarget: TWinControl; var Allow: Boolean);
    procedure ImportStructureActionExecute(Sender: TObject);
    procedure NewOtherFieldClick(Sender: TObject);
    procedure AutoAlignBtnClick(Sender: TObject);
    procedure ClearToolBtnClick(Sender: TObject);
    procedure DeleteFieldMenuItemClick(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure EditFieldMenuItemClick(Sender: TObject);
    procedure FontSelectBtnClick(Sender: TObject);
    procedure FrameDockDrop(Sender: TObject; Source: TDragDockObject; X,
      Y: Integer);
    procedure FrameMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure NewDMYFieldActionExecute(Sender: TObject);
    procedure NewFloatFieldActionExecute(Sender: TObject);
    procedure NewIntFieldActionExecute(Sender: TObject);
    procedure NewLabelFieldActionExecute(Sender: TObject);
    procedure NewMDYFieldActionExecute(Sender: TObject);
    procedure NewStringFieldActionExecute(Sender: TObject);
    procedure NewYMDFieldActionExecute(Sender: TObject);
    procedure OpenFileActionExecute(Sender: TObject);
    procedure SaveFileActionExecute(Sender: TObject);
    procedure SaveFileAsActionExecute(Sender: TObject);
    procedure ToolBtnClick(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
  private
    { private declarations }
    ActiveButton: TToolButton;
    FActiveDatafile: TEpiDataFile;
    FComponentXTree: TAVLTree;
    FComponentYTree: TAVLTree;
    FModified: boolean;
    FOldCaption: String;
    ClickedField: TFieldEdit;
    ClickedLabel: TFieldLabel;
    function NewFieldEdit(AField: TEpiField): TFieldEdit;
    function NewQuestionLabel(AField: TEpiField): TFieldLabel;
    procedure SetModified(const AValue: Boolean);
    procedure StartFieldDock(Sender: TObject; var DragObject: TDragDockObject);
    procedure EndFieldDock(Sender, Target: TObject; X,Y: Integer);
    procedure FieldMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure EndLabelDock(Sender, Target: TObject; X,Y: Integer);
    function  GetLowestControlPosition(Var LowestCtrl: TControl): TPoint;
    procedure GetNearestControls(Const FindCtrl: TControl; var NearestCtrlX, NearestCtrlY: TControl);
    function  FindNewAutoControlPostion: TPoint;
    procedure AutoAlignFields(ActiveControl: TControl; AlignProps: TAutoAlignRecord);
    function  BackupFile(FileName: string; BackupExt: string = '.old'): boolean;
    procedure RemoveDeadSpace;
    procedure NewShortCutFieldAction(aBtn: TToolButton);
  protected
    property  ComponentYTree: TAVLTree read FComponentYTree;
    property  ComponentXTree: TAVLTree read FComponentXTree;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateAllFields;
    property  ActiveDataFile: TEpiDataFile read FActiveDataFile;
    property  Modified: Boolean read FModified write SetModified;
  end;

implementation

uses
  main, graphics,
  types, math, settings, design_label_form,
  UEpiDataGlobals, UImportExport, UQesHandler, UEpiUtils;

function YCmp(Item1, Item2: Pointer): Integer;
var
  Ctrl1, Ctrl2: TControl;
begin
  // This is the same item and result should
  // always be 0.
  Result := Item1 - Item2;
  if Result = 0 then
    Exit;

  Ctrl1 := TControl(Item1);
  Ctrl2 := TControl(Item2);

  if Ctrl1.Top > Ctrl2.Top then
    result := 1
  else if Ctrl1.Top < Ctrl2.Top then
    result := -1
  else
    if Ctrl1.Left > Ctrl2.Left then
      result := 1
    else if Ctrl1.Left < Ctrl2.Left then
      result := -1
    else
      result := 0;
end;

function XCmp(Item1, Item2: Pointer): Integer;
var
  Ctrl1, Ctrl2: TControl;
  Field1, Field2: TEpiField;
begin
  // This is the same item and result should
  // always be 0.
  Result := Item1 - Item2;
  if Result = 0 then
    Exit;

  Ctrl1 := TControl(Item1);
  Ctrl2 := TControl(Item2);

  if Ctrl1.Left > Ctrl2.Left then
    result := 1
  else if Ctrl1.Left < Ctrl2.Left then
    result := -1
  else
    if Ctrl1.Top > Ctrl2.Top then
      result := 1
    else if Ctrl1.Top < Ctrl2.Top then
      result := -1
    else
      result := 0;
end;

{ TDesignFrame }

function TDesignFrame.NewFieldEdit(AField: TEpiField): TFieldEdit;
var
  FieldForm: TFieldCreateForm;
  Pt: TPoint;
begin
  Result := TFieldEdit.Create(Self);
  Result.Align := alNone;
  Result.DragMode := dmAutomatic;
  Result.DragKind := dkDock;
  Result.OnStartDock := @StartFieldDock;
  Result.OnEndDock := @EndFieldDock;
  Result.AutoSelect := false;
  Result.AutoSize := false;
  Result.PopupMenu := FieldPopUp;
  Result.OnMouseDown := @FieldMouseDown;
  Result.Parent := Self;
  Result.Field := AField;

  if VertScrollBar.Visible then
    VertScrollBar.Position := VertScrollBar.Range - VertScrollBar.Page;

  ComponentYTree.Add(Result);
  ComponentXTree.Add(Result);
end;

function TDesignFrame.NewQuestionLabel(AField: TEpiField): TFieldLabel;
var
  LabelForm: TCreateLabelForm;
  Pt: TPoint;
begin
  Result := TFieldLabel.Create(Self);
  Result.Align := alNone;
  Result.DragMode := dmAutomatic;
  Result.DragKind := dkDock;
  Result.OnEndDock := @EndLabelDock;
  Result.PopupMenu := FieldPopUp;
  Result.OnMouseDown := @FieldMouseDown;
  Result.Parent := Self;
  Result.Field := AField;

  ComponentYTree.Add(Result);
  ComponentXTree.Add(Result);
end;

procedure TDesignFrame.SetModified(const AValue: Boolean);
begin
  if AValue = FModified then exit;

  FModified := AValue;
  if AValue then
  begin
    FOldCaption := TTabSheet(Parent).Caption;
    TTabSheet(Parent).Caption := '*' + TTabSheet(Parent).Caption;
  end
  else
    TTabSheet(Parent).Caption := FOldCaption;
end;

procedure TDesignFrame.StartFieldDock(Sender: TObject;
  var DragObject: TDragDockObject);
var
  S: string;
begin
  DragObject := TFieldDockObject.Create(TControl(Sender));
  ComponentYTree.Remove(Sender);
  ComponentXTree.Remove(Sender);
end;

procedure TDesignFrame.EndFieldDock(Sender, Target: TObject; X, Y: Integer);
begin
  with TFieldEdit(Sender) do
  begin
    Align := alNone;

    // Using to positional controls of the Edit since its position is updated correctly in the
    // FrameDockDrop event.
    Field.FieldX := Left;
    Field.FieldY := Top;
    Field.LabelX := VariableLabel.Left;
    Field.LabelY := VariableLabel.Top;
  end;
  // Only add to component tree if it is being placed on the actual form.
  // - it could be placed outside in a custom form.
  // - "nil" = dock was abort using eg. ESC.
  if (Target is TDesignFrame) or (not Assigned(Target)) then
  begin
    ComponentYTree.Add(Sender);
    ComponentXTree.Add(Sender);
  end;
end;

procedure TDesignFrame.FieldMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
//  if Button <> mbRight then exit;

  if (Sender is TFieldEdit) then
  begin
    ClickedField := TFieldEdit(Sender);
    ClickedLabel := nil;
    EditFieldMenuItem.Caption := 'Edit Field';
    DeleteFieldMenuItem.Caption := 'Delete Field';
  end;
  if (Sender is TFieldLabel) then
  begin
    ClickedLabel := TFieldLabel(Sender);
    ClickedField := nil;
    EditFieldMenuItem.Caption := 'Edit Label';
    DeleteFieldMenuItem.Caption := 'Delete Label';
  end;
end;

procedure TDesignFrame.EndLabelDock(Sender, Target: TObject; X, Y: Integer);
begin
  if not (Sender is TFieldLabel) then exit;

  with TFieldLabel(Sender) do
  begin
    Align := alNone;

    // Using to positional controls of the Edit since its position is updated correctly in the
    // FrameDockDrop event
    Field.FieldX := Left;
    Field.FieldY := Top;
    Field.LabelX := Left;
    Field.LabelY := Top;
  end;
end;

function TDesignFrame.GetLowestControlPosition(var LowestCtrl: TControl): TPoint;
var
  Hit, Prd: TAVLTreeNode;
begin
  Result := Point(ManagerSettings.DefaultRightPostion, FieldToolBar.Height + 5);
  LowestCtrl := nil;
  if ComponentYTree.Count = 0 then exit;

  Hit := ComponentYTree.FindHighest;
  LowestCtrl := TControl(Hit.Data);

  Prd := ComponentYTree.FindPrecessor(Hit);
  // If the pred. has same top value, then is by sorting ord it must have a
  // smaller left value.
  while Assigned(Prd) do
  begin
    if (TControl(Prd.Data).Top = LowestCtrl.Top) then
    begin
      LowestCtrl := TControl(Prd.Data);
      Prd := ComponentYTree.FindPrecessor(Prd);
    end else
      Prd := nil;
  end;
  Result := Point(LowestCtrl.Left, LowestCtrl.Top);
end;

procedure TDesignFrame.GetNearestControls(const FindCtrl: TControl;
  var NearestCtrlX, NearestCtrlY: TControl);
var
  Hit, Prd, Scc: TAVLTreeNode;
  HDy, PDy, SDy: Integer;

begin
  // Finding the nearest it either an exact hit,
  // a little above or below - hence we need to look
  // both ways.
  Hit := ComponentYTree.FindNearest(FindCtrl);
  Prd := ComponentYTree.FindPrecessor(Hit);
  Scc := ComponentYTree.FindSuccessor(Hit);
  PDY := MaxInt;
  SDy := MaxInt;

  HDy := Abs(TControl(Hit.Data).Top - FindCtrl.Top);
  if Assigned(Prd) then
    PDy := Abs(TControl(Prd.Data).Top - FindCtrl.Top);
  if Assigned(Scc) then
    SDy := Abs(TControl(Scc.Data).Top - FindCtrl.Top);
  if PDy < HDy then
  begin
    Hit := Prd;
    if SDy < PDy then
      Hit := Scc;
  end else
    if SDY < HDy then
      Hit := Scc;
  NearestCtrlY := TControl(Hit.Data);

  // Same story for finding X component.
  Hit := ComponentXTree.FindNearest(FindCtrl);
  Prd := ComponentXTree.FindPrecessor(Hit);
  Scc := ComponentXTree.FindSuccessor(Hit);
  PDY := MaxInt;
  SDy := MaxInt;

  HDy := Abs(TControl(Hit.Data).Left - FindCtrl.Left);
  if Assigned(Prd) then
    PDy := Abs(TControl(Prd.Data).Left - FindCtrl.Left);
  if Assigned(Scc) then
    SDy := Abs(TControl(Scc.Data).Left - FindCtrl.Left);
  if PDy < HDy then
  begin
    Hit := Prd;
    if SDy < PDy then
      Hit := Scc;
  end else
    if SDY < HDy then
      Hit := Scc;
  NearestCtrlX := TControl(Hit.Data);
end;

function TDesignFrame.FindNewAutoControlPostion: TPoint;
var
  Ctrl: TControl;
begin
  Result := GetLowestControlPosition(Ctrl);
  if Assigned(Ctrl) then
    Result.Y += (Ctrl.Height + ManagerSettings.SpaceBetweenFields);
end;

procedure TDesignFrame.AutoAlignFields(ActiveControl: TControl;
  AlignProps: TAutoAlignRecord);
var
  MaxVariableLabelWidth,
  MaxFieldNameWidth: Integer;
  MinY, MaxY, Spacing: Integer;
  FieldNo, FieldCount: Integer;
  i, PrevTop: Integer;
  AdjustedFieldLeft: LongInt;
  TreeNode: TAVLTreeNode;
  NewYTree: TAVLTree;
  NewXTree: TAVLTree;
  Dx: Integer;
begin
  // Init vars.
  MaxVariableLabelWidth := 0;
  MaxFieldNameWidth := 0;
  MinY := MaxInt;
  MaxY := 0;
  FieldCount := 0;
  PrevTop := -MaxInt;

  // Information collection pass.
  TreeNode := ComponentYTree.FindLowest;
  while Assigned(TreeNode) do
  begin
    if (TControl(TreeNode.Data) is TFieldEdit) then
    with TFieldEdit(TreeNode.Data) do
    begin
      MaxVariableLabelWidth := Max(MaxVariableLabelWidth, VariableLabel.Width);
      MaxFieldNameWidth     := Max(MaxFieldNameWidth, FieldNameLabel.Width);
    end;

    if PrevTop <> TControl(TreeNode.Data).Top then
      Inc(FieldCount);
    PrevTop := TControl(TreeNode.Data).Top;

    MinY := Min(MinY, TControl(TreeNode.Data).Top);
    MaxY := Max(MaxY, TControl(TreeNode.Data).Top);

    TreeNode := ComponentYTree.FindSuccessor(TreeNode);
  end;
  MinY := Max(FieldToolBar.Height + 5, MinY);

  // Spacing between "top" point of TEditFields, adjusted for overlapping components.
  Spacing := Max((MaxY - MinY) div (FieldCount - 1), ActiveControl.Height + ManagerSettings.SpaceBetweenFields);

  AdjustedFieldLeft := Max(ActiveControl.Left,
    MaxFieldNameWidth + MaxVariableLabelWidth + 10);

  FieldNo := 0;
  PrevTop := -MaxInt;
  NewYTree := TAVLTree.Create(@YCmp);
  NewXTree := TAVLTree.Create(@XCmp);
  TreeNode := ComponentYTree.FindLowest;
  while Assigned(TreeNode) do
  begin
    if (TControl(TreeNode.Data) is TFieldEdit) then
    with TFieldEdit(TreeNode.Data) do
    begin
      // Field positioning.
      Dx := 0;
      if AlignProps.DefaultAlign then
      begin
        Dx := (Field.FieldX - AdjustedFieldLeft);
        Field.FieldX := AdjustedFieldLeft;
      end;
      if AlignProps.EqualVertSpace then
      begin
        if PrevTop = Top then
          Dec(FieldNo);
        PrevTop := Top;
        Field.FieldY := MinY + FieldNo * Spacing;
        Field.LabelY := Field.FieldY + (Height - VariableLabel.Height);
      end;
      case AlignProps.LabelsAlign of
        alLeft:  Field.LabelX := Field.FieldX - (MaxVariableLabelWidth + 5);
        alRight: Field.LabelX := Field.FieldX - (VariableLabel.Width + 5);
        alNone:  Field.LabelX := Field.LabelX - Dx;
      end;
    end;

    if (TControl(TreeNode.Data) is TFieldLabel) then
    with TFieldLabel(TreeNode.Data) do
    begin
      if AlignProps.EqualVertSpace then
      begin
        Field.FieldY := MinY + FieldNo * Spacing;
        Field.LabelY := Field.FieldY;
      end;
      case AlignProps.LabelsAlign of
        alLeft:
          begin
            Field.FieldX := AdjustedFieldLeft - (MaxVariableLabelWidth + 5);
            Field.LabelX := AdjustedFieldLeft - (MaxVariableLabelWidth + 5);
          end;
      end;
    end;

    inc(FieldNo);
    NewYTree.Add(TreeNode.Data);
    NewXTree.Add(TreeNode.Data);
    TreeNode := ComponentYTree.FindSuccessor(TreeNode);
  end;

  FreeAndNil(FComponentYTree);
  FComponentYTree := NewYTree;
  FreeAndNil(FComponentXTree);
  FComponentXTree := NewXTree;
end;

function TDesignFrame.BackupFile(FileName: string; BackupExt: string): boolean;
begin
  result := false;
  try
    if FileExistsUTF8(FileName) then
    begin
      if FileExistsUTF8(FileName + BackupExt) then
        DeleteFileUTF8(FileName + BackupExt);
      CopyFile(FileName, FileName + BackupExt);
    end;
    result := true;
  except
  end;
end;

procedure TDesignFrame.RemoveDeadSpace;
var
  NewYTree, NewXTree: TAVLTree;
  Curr, Old, Prev, Next: TAVLTreeNode;
  CurField, NextField, PrevField, OldField: TEpiField;
  Dy, Zy, Ly: Integer;
begin
  NewYTree := TAVLTree.Create(@YCmp);
  NewXTree := TAVLTree.Create(@XCmp);
  Curr := ComponentYTree.FindLowest;
  Next := ComponentYTree.FindSuccessor(Curr);
  while Assigned(Next) do
  begin
    if (TControl(Curr.Data) is TFieldEdit) then
      CurField := TFieldEdit(Curr.Data).Field
    else
      CurField := TFieldLabel(Curr.Data).Field;

    if (TControl(Next.Data) is TFieldEdit) then
      NextField := TFieldEdit(Next.Data).Field
    else
      NextField := TFieldLabel(Next.Data).Field;
    Dy := TControl(Curr.Data).Height + ManagerSettings.SpaceBetweenFields;

    if (NextField.FieldY > (CurField.FieldY + Dy)) then
    begin
      NewYTree.Add(Curr.Data);
      NewXTree.Add(Curr.Data);

      Old := Next;
      OldField := NextField;

      Prev := Next;
      Next := ComponentYTree.FindSuccessor(Next);
      while Assigned(Next) do
      begin
        PrevField := NextField;
        if (TControl(Next.Data) is TFieldEdit) then
          NextField := TFieldEdit(Next.Data).Field
        else
          NextField := TFieldLabel(Next.Data).Field;
        Zy := TControl(Prev.Data).Height + ManagerSettings.SpaceBetweenFields;

        if (NextField.FieldY >= (OldField.FieldY + Zy)) or
           (NextField.FieldX < PrevField.FieldX) then
        begin
          if (Old <> Prev) then
          begin
            NewYTree.Remove(Prev.Data);
            NewXTree.Remove(Prev.Data);
          end;
          Curr := Prev;
          Break;
        end;
        Ly := NextField.FieldY - (CurField.FieldY + Zy);
        NextField.FieldY := CurField.FieldY + Zy;
        NextField.LabelY := NextField.LabelY - Abs(Ly);

        NewYTree.Add(Next.Data);
        NewXTree.Add(Next.Data);
        Prev := Next;
        Next := ComponentYTree.FindSuccessor(Next);
      end;

      Ly := OldField.FieldY - (CurField.FieldY + Dy);
      OldField.FieldY := CurField.FieldY + Dy;
      OldField.LabelY := OldField.LabelY - Abs(Ly);
      if (Old <> Curr) then
      begin
        NewYTree.Add(Old.Data);
        NewXTree.Add(Old.Data);
      end;
      Continue;
    end;

    NewYTree.Add(Curr.Data);
    NewXTree.Add(Curr.Data);

    Curr := Next;
    if Assigned(Next) then
      Next := ComponentYTree.FindSuccessor(Next);
  end;

  if not Assigned(NewYTree.Find(Curr.Data)) then
  begin
    NewYTree.Add(Curr.Data);
    NewXTree.Add(Curr.Data);
  end;

  FreeAndNil(FComponentYTree);
  FComponentYTree := NewYTree;
  FreeAndNil(FComponentXTree);
  FComponentXTree := NewXTree;
end;

procedure TDesignFrame.NewShortCutFieldAction(aBtn: TToolButton);
var
  Pt: TPoint;
begin
  ActiveButton := aBtn;
  Pt := FindNewAutoControlPostion;
  FrameMouseDown(nil, mbLeft, GetKeyShiftState, Pt.X, Pt.Y);
end;

constructor TDesignFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ActiveButton := SelectorButton;
  FActiveDatafile := TEpiDataFile.Create();
  ClickedField := nil;
  Modified := false;
  FComponentYTree := TAVLTree.Create(@YCmp);
  FComponentXTree := TAVLTree.Create(@XCmp);
end;

destructor TDesignFrame.Destroy;
begin
  if Assigned(FActiveDatafile) then
   FreeAndNil(FActiveDatafile);
  FreeAndNil(FComponentYTree);
  FreeAndNil(FComponentXTree);
  inherited Destroy;
end;

procedure TDesignFrame.UpdateAllFields;
var
  i: Integer;
begin
  for i := ControlCount -1 downto  0 do
  begin
    if not (Controls[i] is TFieldEdit) then continue;

    if ManagerSettings.ShowFieldNamesInLabel then
      TFieldEdit(Controls[i]).FieldNameLabel.Parent := Self
    else
      TFieldEdit(Controls[i]).FieldNameLabel.Parent := nil;
  end;
end;


procedure TDesignFrame.ToolBtnClick(Sender: TObject);
begin
  if not (Sender is TToolButton) then exit;
  ActiveButton.Down := false;
  TToolButton(Sender).Down := true;
  ActiveButton := TToolButton(Sender);
end;

procedure TDesignFrame.ToolButton3Click(Sender: TObject);
var
  FS: TFileStream;
  ss: Int64;

  procedure WriteReportToStream(Tree: TAVLTree; s: TStream; var StreamSize: int64);
  var h: string;

    procedure WriteStr(const Txt: string);
    begin
      if s<>nil then
        s.Write(Txt[1],length(Txt));
      inc(StreamSize,length(Txt));
    end;

    procedure WriteTreeNode(ANode: TAVLTreeNode; const Prefix: string);
    var b: string;
    begin
      if ANode=nil then exit;
      WriteTreeNode(ANode.Right,Prefix+'  ');
      b:=Prefix; //Prefix+HexStr(PtrInt(ANode.Data),SizeOf(PtrInt)*2)+'    ';
      if TControl(ANode.Data) is TFieldEdit then
        b += Format('  Name="%8s"',
               [TFieldEdit(ANode.Data).Field.FieldName]);
      if TControl(ANode.Data) is TFieldLabel then
        b += Format('  Name="%10s"',
               [TFieldLabel(ANode.Data).Field.FieldName]);
//          +'  Self='+HexStr(PtrInt(ANode),SizeOf(PtrInt)*2)
//          +'  Parent='+HexStr(PtrInt(ANode.Parent),SizeOf(PtrInt)*2)
//          +'  Balance='+IntToStr(ANode.Balance)
       b +=Format('  Top="%3d"  Left="%3d"', [TControl(ANode.Data).Top, TControl(ANode.Data).Left])
         +LineEnding;
      WriteStr(b);
      WriteTreeNode(ANode.Left,Prefix+'  ');
    end;

  // TAVLTree.WriteReportToStream
  begin
    h:='Consistency: '+IntToStr(Tree.ConsistencyCheck)+' ---------------------'+LineEnding;
    WriteStr(h);
    WriteTreeNode(Tree.Root,'  ');
    h:='-End-Of-AVL-Tree---------------------'+LineEnding;
    WriteStr(h);
  end;

begin
  fs := TFileStream.Create('/tmp/componenttree.dump', fmCreate);
  if not (Sender is TAVLTree) then
    Sender := ComponentXTree;
  WriteReportToStream(TAVLTree(Sender), fs, ss);
  fs.free;
end;


procedure TDesignFrame.FrameDockDrop(Sender: TObject; Source: TDragDockObject;
  X, Y: Integer);
var
  Dx, Dy: Integer;
  XCtrl, YCtrl: TControl;
  AvlNode: TAVLTreeNode;
begin
  Source.Control.Left := X - Source.DockOffset.X;
  Source.Control.Top := Y - Source.DockOffset.Y;



  if (not (ssShift in GetKeyShiftState)) and
     ManagerSettings.SnapFields and
     (Source.Control is TFieldEdit) and
     (ComponentYTree.Count > 0) then
  begin
    GetNearestControls(Source.Control, XCtrl, YCtrl);
    Dx := Source.Control.Left - XCtrl.Left;
    Dy := Source.Control.Top - YCtrl.Top;

    if Abs(Dx) <= ManagerSettings.SnappingThresHold then
      Source.Control.Left := XCtrl.Left;
    if Abs(Dy) <= ManagerSettings.SnappingThresHold then
      Source.Control.Top := YCtrl.Top;
  end;

  Modified := True;
end;

procedure TDesignFrame.ClearToolBtnClick(Sender: TObject);
var
  Comp: TControl;
  Field: TEpiField;
  i: Integer;
begin
  Field := nil;
  for i := ControlCount - 1 downto  0 do
  begin
    Comp := Controls[i];

    if not ((Comp is TFieldEdit) or
      (Comp is TFieldLabel)) then continue;

    if (Comp is TFieldEdit) then
      Field := TFieldEdit(Comp).Field
    else
      Field := TFieldLabel(Comp).Field;
    ActiveDatafile.RemoveField(Field, true);
    RemoveControl(Comp);
    ComponentYTree.Remove(Comp);
    ComponentXTree.Remove(Comp);
    FreeAndNil(Comp);
    Modified := true;
  end;
end;

procedure TDesignFrame.AutoAlignBtnClick(Sender: TObject);
var
  AutoAlignForm: TAutoAlignForm;
  AutoAlignRes: TAutoAlignRecord;
  Res: TModalResult;
  Pt: TPoint;
begin
  AutoAlignForm := TAutoAlignForm.Create(self);
  With TToolButton(Sender) do
    Pt := FieldToolBar.ClientToScreen(Point(Left, Top + Height + 1));
  AutoAlignForm.Left := Pt.X;
  AutoAlignForm.Top  := Pt.Y;
  Res := AutoAlignForm.ShowModal;
  AutoAlignRes := AutoAlignForm.AlignProperties;
  FreeAndNil(AutoAlignForm);

  if Res = mrCancel then Exit;

  if AutoAlignRes.RemoveEmptySpace then
    RemoveDeadSpace;

  if AutoAlignRes.DefaultAlign and (not Assigned(ClickedField)) then
    Exit;

  if Assigned(ClickedField) then
    AutoAlignFields(ClickedField, AutoAlignRes);

  Modified := true;
end;

procedure TDesignFrame.DeleteFieldMenuItemClick(Sender: TObject);
var
  TmpField: TEpiField;
  TmpCtrl: TControl;
begin
  if Assigned(ClickedField) then
  begin
    TmpCtrl := ClickedField;
    TmpField := ClickedField.Field;
  end else
  if Assigned(ClickedLabel) then
  begin
    TmpCtrl := ClickedLabel;
    TmpField := ClickedLabel.Field;
  end else
    exit;

  {$IFNDEF EPI_DEBUG}
  if (TmpField.Size > 0) and
     (MessageDlg('Field contains data.' + LineEnding +
      'Are you sure you want to delete?', mtWarning, mbYesNo, 0) = mrNo) then
    exit;
  {$ENDIF}

  Modified := true;
  ActiveDatafile.RemoveField(TmpField, true);
  RemoveControl(TmpCtrl);
  ComponentYTree.Remove(TmpCtrl);
  ComponentXTree.Remove(TmpCtrl);
  FreeAndNil(TmpCtrl);
end;

procedure TDesignFrame.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  MainForm.StatusBar2.Panels[1].Text := Format(
    'Mouse - X: %d Y: %d',
    [X, Y]);

  MainForm.Label5.Caption := Format(
    'Y Component Tree.Count = %d' + LineEnding +
    'X Component Tree.Count = %d',
    [ComponentYTree.Count, ComponentXTree.Count]
  );
end;

procedure TDesignFrame.EditFieldMenuItemClick(Sender: TObject);
var
  FieldForm: TFieldCreateForm;
  LabelForm: TCreateLabelForm;
  Pt: TPoint;
begin
  if Assigned(ClickedField) then
  With ClickedField do
  begin
    Pt := Self.ClientToScreen(Point(Left + Width, Top + Height));
    FieldForm := TFieldCreateForm.Create(Self, ActiveDatafile, Field.FieldType, false);
    FieldForm.Left := Pt.X;
    FieldForm.Top  := Pt.Y;
    FieldForm.ReadField(Field);
    if FieldForm.ShowModal = mrCancel then exit;
    FieldForm.WriteField(Field);
    FreeAndNil(FieldForm);
    Self.Modified := true;
  end;
  if Assigned(ClickedLabel) then
  With ClickedLabel do
  begin
    Pt := Self.ClientToScreen(Point(Left + Width, Top + Height));
    LabelForm := TCreateLabelForm.Create(Self, ActiveDatafile);
    LabelForm.Left := Pt.X;
    LabelForm.Top  := Pt.Y;
    LabelForm.LabelEdit.Text := Field.VariableLabel;
    if LabelForm.ShowModal = mrCancel then exit;
    Caption :=  LabelForm.LabelEdit.Text;
    Field.VariableLabel := LabelForm.LabelEdit.Text;
    FreeAndNil(LabelForm);
    Self.Modified := true;
  end;
end;

procedure TDesignFrame.FontSelectBtnClick(Sender: TObject);
begin
  if FontDialog1.Execute then
  begin
    Font := FontDialog1.Font;
    Repaint;
  end;
end;

procedure TDesignFrame.FrameMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  TmpField: TEpiField;
  Ctrl: TControl;
  CreateForm: TForm;
  Pt: TPoint;
begin
  if Button <> mbLeft then exit;
  if ActiveButton = SelectorButton then Exit;

  CreateForm := nil;
  try
    // The IFDEF is needed since scrollbar handling (apparently) is different on windows and linux/mac.
    Pt := ClientToScreen(Point(X, Y{$IFNDEF WINDOWS} - VertScrollBar.Position{$ENDIF}));

    if ActiveButton = LabelFieldBtn then
    begin
      CreateForm := TCreateLabelForm.Create(Self, ActiveDataFile);
      CreateForm.Top := Min(Pt.Y, Screen.Height - CreateForm.Height - 5);
      CreateForm.Left := Min(Pt.X, Screen.Width - CreateForm.Width - 5);
      if CreateForm.ShowModal = mrCancel then
        exit;

      TmpField := TEpiField.CreateField(TFieldType(ActiveButton.Tag), ActiveDataFile.Size);
      with TmpField do
      begin
        FieldName := TCreateLabelForm(CreateForm).GetFieldName;
        VariableLabel := TCreateLabelForm(CreateForm).LabelEdit.Text;
        FieldX := X;
        FieldY := Y;
        LabelX := X;
        LabelY := Y;
      end;
      ActiveDatafile.AddField(TmpField);
      NewQuestionLabel(TmpField);
    end else begin
      if not (ssShift in Shift) then
      begin
        CreateForm := TFieldCreateForm.Create(Self, ActiveDatafile, TFieldType(ActiveButton.Tag));
        CreateForm.Top := Min(Pt.Y, Screen.Height - CreateForm.Height - 5);
        CreateForm.Left := Min(Pt.X, Screen.Width - CreateForm.Width - 5);
        if CreateForm.ShowModal = mrCancel then
          Exit;
      end;

      TmpField := TEpiField.CreateField(TFieldType(ActiveButton.Tag), ActiveDataFile.Size);
      if (ssShift in Shift) then
        TFieldCreateForm.AutoCreateField(ActiveDataFile, TmpField)
      else with TmpField do
      begin
        FieldName       := TFieldCreateForm(CreateForm).FieldNameEdit.Text;
        VariableLabel   := TFieldCreateForm(CreateForm).LabelEdit.Text;
        FieldLength     := StrToInt(TFieldCreateForm(CreateForm).FieldLengthEdit.Text);
        if FieldType = ftFloat then
          FieldDecimals := StrToInt(TFieldCreateForm(CreateForm).FieldDecimalSizeEdit.Text);
      end;
      TmpField.FieldX := X;
      TmpField.FieldY := Y;

      ActiveDatafile.AddField(TmpField);

      NewFieldEdit(TmpField);
    end;
    Modified := true;
    ToolBtnClick(SelectorButton);
  finally
    if Assigned(CreateForm) then FreeAndNil(CreateForm);
  end;
end;

procedure TDesignFrame.NewOtherFieldClick(Sender: TObject);
begin
  if not (Sender is TMenuItem) then exit;
  OtherFieldBtn.Tag := TMenuItem(Sender).Tag;
  ToolBtnClick(OtherFieldBtn);
end;

procedure TDesignFrame.ImportStructureActionExecute(Sender: TObject);
var
  QES: TQesHandler;
  Dlg: TOpenDialog;
  TmpEdit: TFieldEdit;
  i: Integer;
  TmpField: TEpiField;
  TmpDF: TEpiDataFile;
  Importer: TEpiImportExport;
  Pt: TPoint;
  AutoAlignProps: TAutoAlignRecord;
begin
  Dlg := TOpenDialog.Create(nil);
  Dlg.Filter := GetEpiDialogFilter(True, True, True, True, False, True, True,
    True, True, False);

  if not Dlg.Execute then exit;

  TmpDf := nil;
  Importer := TEpiImportExport.Create;
  Importer.OnProgress := @MainForm.ShowProgress;
  Importer.OnClipBoardRead := @MainForm.ReadClipBoard;
  Importer.Import(Dlg.FileName, TmpDF, dftNone);

  GetLowestControlPosition(TmpEdit);

  for i := 0 to TmpDF.NumFields - 1 do
  begin
    Pt := FindNewAutoControlPostion;
    TmpField := TmpDF[i].Clone(ActiveDataFile, false);
    ActiveDataFile.AddField(TmpField);
    TmpField.FieldX := Pt.X;
    TmpField.FieldY := Pt.Y;
    TmpField.LabelX := 0;
    TmpField.LabelY := 0;

    if TmpField.FieldType = ftQuestion then
      NewQuestionLabel(TmpField)
    else
      NewFieldEdit(TmpField);
  end;

  Modified := true;
end;

procedure TDesignFrame.FrameUnDock(Sender: TObject; Client: TControl;
  NewTarget: TWinControl; var Allow: Boolean);
begin
  if (Sender = NewTarget) then
    Allow := true
  else
    Allow := false;
end;

procedure TDesignFrame.NewDMYFieldActionExecute(Sender: TObject);
begin
  NewShortCutFieldAction(DMYFieldBtn);
end;

procedure TDesignFrame.NewFloatFieldActionExecute(Sender: TObject);
begin
  NewShortCutFieldAction(FloatFieldBtn);
end;

procedure TDesignFrame.NewIntFieldActionExecute(Sender: TObject);
begin
  NewShortCutFieldAction(IntFieldBtn);
end;

procedure TDesignFrame.NewLabelFieldActionExecute(Sender: TObject);
begin
  NewShortCutFieldAction(LabelFieldBtn);
end;

procedure TDesignFrame.NewMDYFieldActionExecute(Sender: TObject);
begin
  NewShortCutFieldAction(MDYFieldBtn);
end;

procedure TDesignFrame.NewStringFieldActionExecute(Sender: TObject);
begin
  NewShortCutFieldAction(StringFieldBtn);
end;

procedure TDesignFrame.NewYMDFieldActionExecute(Sender: TObject);
begin
  NewShortCutFieldAction(YMDFieldBtn);
end;

procedure TDesignFrame.OpenFileActionExecute(Sender: TObject);
var
  Dlg: TOpenDialog;
  Import: TEpiImportExport;
  Fn: String;
  Dummy: boolean;
  i: Integer;
  TmpField: TEpiField;
  TmpEdit: TFieldEdit;
  TmpLabel: TFieldLabel;
  AutoAlignProps: TAutoAlignRecord;
  Pt: TPoint;
begin
  {$IFNDEF EPI_DEBUG}
  if (ActiveDatafile.NumFields > 0) and
     (MessageDlg('Dataform is not saved.' + LineEnding + 'Continue?', mtWarning, mbYesNo, 0) = mrNo) then
    exit;
  {$ENDIF}

  Dlg := TOpenDialog.Create(nil);
  Dlg.Filter := GetEpiDialogFilter(True, True, True, True,
    False, True, True, False, True, True);

  if Dlg.Execute then
  begin
    ClearToolBtn.Click;
    FreeAndNil(FActiveDatafile);

    Fn := Dlg.FileName;

    Import := TEpiImportExport.Create;
    Import.OnProgress := @MainForm.ShowProgress;
    Import.OnClipBoardRead := @MainForm.ReadClipBoard;
    ImportTxtGuess.FieldNaming := fnAuto;
    Import.Import(Fn, FActiveDatafile, dftNone);
    FreeAndNil(Import);

    TmpEdit := nil;
    for i := 0 to ActiveDatafile.NumFields - 1 do
    begin
      TmpField := ActiveDatafile[i];
      if ActiveDataFile.DatafileType <> dftEpiDataXml then
      begin
        Pt := FindNewAutoControlPostion;
        TmpField.FieldX := Pt.X;
        TmpField.FieldY := Pt.Y;
        TmpField.LabelX := 0;
        TmpField.LabelY := 0;
      end;

      if TmpField.FieldType = ftQuestion then
        NewQuestionLabel(TmpField)
      else
        if not Assigned(TmpEdit) then
          TmpEdit := NewFieldEdit(TmpField)
        else
          NewFieldEdit(TmpField);
    end;

    MainForm.PageControl1.ActivePage.Caption := ExtractFileName(Fn);
    MainForm.PageControl1.Hint := ExpandFileNameUTF8(Fn);
    MainForm.PageControl1.ShowHint := true;

    AutoAlignProps.DefaultAlign := true;
    AutoAlignProps.LabelsAlign := alLeft;
    AutoAlignProps.EqualVertSpace := true;
    AutoAlignProps.RemoveEmptySpace := false;
    if ActiveDatafile.DatafileType <> dftEpiDataXml then
      AutoAlignFields(TmpEdit, AutoAlignProps);

    Modified := false;
  end;
  FreeAndNil(Dlg);
end;

procedure TDesignFrame.SaveFileActionExecute(Sender: TObject);
begin
  if ActiveDataFile.FileName = '' then
  begin
    SaveFileAsAction.Execute;
    Exit;
  end;

  ActiveDataFile.Save(ActiveDataFile.FileName, []);
  Modified := false;
  MainForm.StatusBar2.Panels[0].Text := 'Saving complete: ' + ActiveDataFile.FileName;
end;

procedure TDesignFrame.SaveFileAsActionExecute(Sender: TObject);
var
  Fn: String;
begin
  if SaveDialog1.Execute then
  begin
    Fn := SaveDialog1.FileName;

    if CompareFileExt(Fn, '.recxml') <> 0 then
      Fn := Fn + '.recxml';

    BackupFile(Fn);

    ActiveDatafile.Save(Fn, []);

    Modified := false;

    MainForm.PageControl1.ActivePage.Caption := ExtractFileName(Fn);
    MainForm.PageControl1.Hint := ExpandFileNameUTF8(Fn);
    MainForm.PageControl1.ShowHint := true;
    MainForm.StatusBar2.Panels[0].Text := 'Saving complete: ' + Fn;
  end;
end;

initialization
  {$I design_frame.lrs}

end.

