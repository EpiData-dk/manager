unit design_frame;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, ComCtrls, ActnList, Controls,
  Buttons, ExtCtrls, Dialogs, Menus, StdCtrls, UEpiDataFile, FieldEdit,
  Design_Field_Frame, AVL_Tree, LCLType, StdActns, design_autoalign_form,
  UDataFileTypes, datafile_documentation_form;

type

  { TDesignFrame }

  TDesignFrame = class(TFrame)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    MovePageDownAction: TAction;
    MoveDownAction: TAction;
    MoveUpAction: TAction;
    MovePageUpAction: TAction;
    MoveFirstAction: TAction;
    MoveLastAction: TAction;
    NewDataMenu: TMenuItem;
    Panel1: TPanel;
    PasteAsLabel: TEditPaste;
    MenuItem1: TMenuItem;
    NewDateFieldAction: TAction;
    NewYMDFieldMenu: TMenuItem;
    NewMDYFieldMenu: TMenuItem;
    NewOtherFieldAction: TAction;
    EditCompAction: TAction;
    DocumentFileAction: TAction;
    AlignAction: TAction;
    NewDMYFieldMenu: TMenuItem;
    NewDataFormBtn: TToolButton;
    DateFieldPopupMenu: TPopupMenu;
    DesignerPopup: TPopupMenu;
    SaveFileAsAction: TAction;
    SaveFileAction: TAction;
    OpenFileAction: TAction;
    ImportStructureAction: TAction;
    NewYMDTodayFieldMenu: TMenuItem;
    NewMDYTodayFieldMenu: TMenuItem;
    NewDMYTodayFieldMenu: TMenuItem;
    ShowStructureBtn: TToolButton;
    TodayDateSubMenu: TMenuItem;
    NewSoundexFieldMenu: TMenuItem;
    NewUpperFieldMenu: TMenuItem;
    NewCryptFieldMenu: TMenuItem;
    StringSubMenu: TMenuItem;
    NewAutoIDMenu: TMenuItem;
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
    OtherFieldsToolBtnPopup: TPopupMenu;
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
    DateFieldBtn: TToolButton;
    OtherFieldBtn: TToolButton;
    ImportStructureBtn: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    procedure DocumentFileActionExecute(Sender: TObject);
    procedure EditCompActionExecute(Sender: TObject);
    procedure ImportStructureActionExecute(Sender: TObject);
    procedure MoveDownActionExecute(Sender: TObject);
    procedure MoveFirstActionExecute(Sender: TObject);
    procedure MoveLastActionExecute(Sender: TObject);
    procedure MovePageDownActionExecute(Sender: TObject);
    procedure MovePageUpActionExecute(Sender: TObject);
    procedure MoveUpActionExecute(Sender: TObject);
    procedure NewDataMenuClick(Sender: TObject);
    procedure NewDateFieldMenuClick(Sender: TObject);
    procedure NewOtherFieldClick(Sender: TObject);
    procedure NewDateFieldActionExecute(Sender: TObject);
    procedure AutoAlignBtnClick(Sender: TObject);
    procedure ClearToolBtnClick(Sender: TObject);
    procedure DeleteFieldMenuItemClick(Sender: TObject);
    procedure EditFieldMenuItemClick(Sender: TObject);
    procedure FontSelectBtnClick(Sender: TObject);
    procedure NewFloatFieldActionExecute(Sender: TObject);
    procedure NewIntFieldActionExecute(Sender: TObject);
    procedure NewLabelFieldActionExecute(Sender: TObject);
    procedure NewStringFieldActionExecute(Sender: TObject);
    procedure OpenFileActionExecute(Sender: TObject);
    procedure PasteAsLabelExecute(Sender: TObject);
    procedure SaveFileActionExecute(Sender: TObject);
    procedure SaveFileAsActionExecute(Sender: TObject);
    procedure ToolBtnClick(Sender: TObject);
  private
    { private declarations }
    ActiveButton: TToolButton;
    FActiveDatafile: TEpiDataFile;
    FActiveDocumentationForm: TDatafileDocumentationForm;
    FComponentXTree: TFieldAVLTree;
    FComponentYTree: TFieldAVLTree;
    FModified: boolean;
    FOldCaption: String;
    SelectedControl: TControl;
    FDesignerBox: TScrollBox;
    function NewFieldEdit(AField: TEpiField): TFieldEdit;
    function NewQuestionLabel(AField: TEpiField): TFieldLabel;
    function  BackupFile(FileName: string; BackupExt: string = '.old'): boolean;
    procedure NewShortCutFieldAction(aBtn: TToolButton);
    procedure SetModified(const AValue: Boolean);
    procedure FieldMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ShowOnMainStatusBar(Msg: string; Const Index: integer);
    procedure DataFileChange(Sender: TObject;
      EventType: TEpiDataFileChangeEventType; OldValue: EpiVariant);
    procedure EnterSelectControl(Sender: TObject);
    procedure ExitSelectControl(Sender: TObject);
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    function  NewCompTree(TreeType: TFieldAVLTreeType; CmpMethod: TListSortCompare): TFieldAVLTree;
  private
    // Docking.
    // - Field:
    procedure StartFieldDock(Sender: TObject; var DragObject: TDragDockObject);
    procedure EndFieldDock(Sender, Target: TObject; X,Y: Integer);
    procedure EndLabelDock(Sender, Target: TObject; X,Y: Integer);
    // - Designer events:
    procedure DesignerUnDock(Sender: TObject; Client: TControl;
      NewTarget: TWinControl; var Allow: Boolean);
    procedure DesignerDockDrop(Sender: TObject; Source: TDragDockObject; X,
      Y: Integer);
    procedure DesignerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DesignerMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    // Aligning.
    // - helper functions.
    function  GetHighestFieldPosition(Var HighestField: TControl): TPoint;
    function  GetLowestControlPosition(Var LowestCtrl: TControl): TPoint;
    procedure GetNearestControls(Const FindCtrl: TControl; var NearestCtrlX, NearestCtrlY: TControl);
    function  FindNewAutoControlPostion(NewCtrlClass: TClass): TPoint;
    // - Auto align functions.
    procedure RemoveDeadSpace;
    procedure DefaultAlignFields(ActiveControl: TControl; Const ColumnCount: Integer = 1);
    procedure EqualSpace(StartCtrl, EndCtrl: TControl);
    procedure LabelsAlignment(StartCtrl, EndCtrl: TControl; aAlign: TAlign);
  protected
    property  ComponentYTree: TFieldAVLTree read FComponentYTree;
    property  ComponentXTree: TFieldAVLTree read FComponentXTree;
    property  ActiveDocumentationForm: TDatafileDocumentationForm
      read FActiveDocumentationForm write FActiveDocumentationForm;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateAllFields;
    procedure UpdateNonInteractiveVisuals;
    property  ActiveDataFile: TEpiDataFile read FActiveDataFile;
    property  Modified: Boolean read FModified write SetModified;
    property  DesignerBox: TScrollBox read FDesignerBox;
  end;

implementation

uses
  main, graphics,
  types, math, settings, design_label_form,
  UEpiDataGlobals, UImportExport, UQesHandler, UEpiUtils,
  Clipbrd, UStringUtils, ManagerProcs, LMessages;

function SortFields(Item1, Item2: Pointer): integer;
var
  Field1: TEpiField absolute Item1;
  Field2: TEpiField absolute Item2;
begin
  result := 0;
  if Field1 = Field2 then
    exit;

  if Field1.FieldY < Field2.FieldY then
    result := -1
  else if Field1.FieldY > Field2.FieldY then
    result := 1
  else
    if Field1.FieldX < Field2.FieldX then
      result := -1
    else if Field1.FieldX > Field2.FieldX then
      result := 1
end;

function YCmp(Item1, Item2: Pointer): Integer;
var
  Ctrl1: TControl absolute Item1;
  Ctrl2: TControl absolute Item2;
begin
  // This is the same item and result should
  // always be 0.
  WriteLn(Format('Items: %p - %p', [Item1, Item2]));
  Writeln(Format('Compare: %s - %s',
    [(Ctrl1 as IFieldControl).Field.FieldName, (Ctrl2 as IFieldControl).Field.FieldName])
  );
  Result := Item1 - Item2;
  if Result = 0 then
    Exit;

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
  Ctrl1: TControl absolute Item1;
  Ctrl2: TControl absolute Item2;
begin
  // This is the same item and result should
  // always be 0.
  Result := Item1 - Item2;
  if Result = 0 then
    Exit;

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
  Result := TFieldEdit.Create(DesignerBox);
  Result.Align := alNone;
  Result.DragMode := dmAutomatic;
  Result.DragKind := dkDock;
  Result.OnStartDock := @StartFieldDock;
  Result.OnEndDock := @EndFieldDock;
  Result.AutoSelect := false;
  Result.AutoSize := false;
  Result.PopupMenu := FieldPopUp;
  Result.OnMouseDown := @FieldMouseDown;
  Result.Parent := DesignerBox;
  Result.Field := AField;
  Result.ReadOnly := true;
  Result.OnEnter := @EnterSelectControl;
  Result.OnExit := @ExitSelectControl;
  Result.Color:= clMenuBar;
  Result.OnKeyDown := @EditKeyDown;
  Result.TabStop := false;
  EnterSelectControl(Result);

  ComponentYTree.Add(Result);
  ComponentXTree.Add(Result);
end;

function TDesignFrame.NewQuestionLabel(AField: TEpiField): TFieldLabel;
var
  LabelForm: TCreateLabelForm;
  Pt: TPoint;
begin
  Result := TFieldLabel.Create(DesignerBox);
  Result.Align := alNone;
  Result.DragMode := dmAutomatic;
  Result.DragKind := dkDock;
  Result.OnStartDock := @StartFieldDock;
  Result.OnEndDock := @EndLabelDock;
  Result.PopupMenu := FieldPopUp;
  Result.OnMouseDown := @FieldMouseDown;
  Result.Font.Style := [fsBold];
  Result.Parent := DesignerBox;
  Result.Field := AField;
  EnterSelectControl(Result);

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
  DragObject := TDragDockObject.Create(TControl(Sender));
  ComponentYTree.Remove(Sender);
  ComponentXTree.Remove(Sender);
end;

procedure TDesignFrame.EndFieldDock(Sender, Target: TObject; X, Y: Integer);
begin
  with TFieldEdit(Sender) do
  begin
    Align := alNone;

    // Using to positional controls of the Edit since its position is updated correctly in the
    // DesignerDockDrop event.
    Field.FieldX := Left;
    Field.FieldY := Top;
    Field.LabelX := VariableLabel.Left;
    Field.LabelY := VariableLabel.Top;
  end;
  // Only add to component tree if it is being placed on the actual form.
  // - it could be placed outside in a custom form.
  // - "nil" = dock was abort using eg. ESC.
  if (Target = DesignerBox) or (not Assigned(Target)) then
  begin
    ComponentYTree.Add(TFieldEdit(Sender));
    ComponentXTree.Add(TFieldEdit(Sender));
  end;
end;

procedure TDesignFrame.FieldMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  EnterSelectControl(Sender);

  if (Sender is TFieldEdit) then
  begin
    EditFieldMenuItem.Caption := 'Edit Field';
    DeleteFieldMenuItem.Caption := 'Delete Field';
  end;
  if (Sender is TFieldLabel) then
  begin
    EditFieldMenuItem.Caption := 'Edit Label';
    DeleteFieldMenuItem.Caption := 'Delete Label';
  end;
end;

procedure TDesignFrame.ShowOnMainStatusBar(Msg: string; const Index: integer);
begin
  Mainform.ShowOnStatusBar(Msg, Index);
end;

procedure TDesignFrame.UpdateNonInteractiveVisuals;
var
  s: string;
begin
  S := '';
  if Modified then
    s := '*';
  if ActiveDatafile.DatafileType <> dftNone then
  begin
    MainForm.PageControl1.ActivePage.Caption := S +
      ExtractFileName(ActiveDataFile.FileName);
    MainForm.PageControl1.Hint :=
      ExpandFileNameUTF8(ActiveDataFile.FileName);
    MainForm.PageControl1.ShowHint := true;
  end else begin
    MainForm.PageControl1.ActivePage.Caption := S +
      'Untitled';
    if MainForm.PageControl1.PageCount > 1 then
      MainForm.PageControl1.ActivePage.Caption := S +
        'Untitled ' + IntToStr(MainForm.PageControl1.PageCount);
  end;

  ShowOnMainStatusBar(
    Format(
      'Fields: %d - %d',
      [ActiveDatafile.NumFields, ActiveDatafile.DataFields.Count]
    ), 1
  );
  ShowOnMainStatusBar(
    Format(
      'Records: %d',
      [ActiveDatafile.Size]
    ), 2
  );
end;

procedure TDesignFrame.DataFileChange(Sender: TObject;
  EventType: TEpiDataFileChangeEventType; OldValue: EpiVariant);
begin
  UpdateNonInteractiveVisuals;
end;

procedure TDesignFrame.EnterSelectControl(Sender: TObject);
begin
  if Assigned(SelectedControl) then
    if SelectedControl is TFieldEdit then
      SelectedControl.Color:= clMenuBar
    else
      SelectedControl.Color := DesignerBox.Color;

  SelectedControl := TControl(Sender);
  TFieldEdit(SelectedControl).Color := ManagerSettings.SelectedControlColour;
  if SelectedControl is TFieldEdit then
    TFieldEdit(SelectedControl).SetFocus;

  if (DesignerBox.VertScrollBar.Position > SelectedControl.Top) then
    DesignerBox.VertScrollBar.Position := SelectedControl.Top - 5;
  if (SelectedControl.Top + SelectedControl.Height) > (DesignerBox.VertScrollBar.Position + DesignerBox.ClientHeight) then
    DesignerBox.VertScrollBar.Position := SelectedControl.Top + SelectedControl.Height - DesignerBox.ClientHeight + 5;

//  MainForm.Label2.Caption := 'Selected Control: ' + SelectedControl.Name;
end;

procedure TDesignFrame.ExitSelectControl(Sender: TObject);
begin
  SelectedControl.Color:= clMenuBar;
  SelectedControl := nil;
end;

procedure TDesignFrame.EndLabelDock(Sender, Target: TObject; X, Y: Integer);
begin
  with TFieldLabel(Sender) do
  begin
    Align := alNone;

    // Using to positional controls of the Edit since its position is updated correctly in the
    // DesignerDockDrop event
    Field.FieldX := Left;
    Field.FieldY := Top;
    Field.LabelX := Left;
    Field.LabelY := Top;
  end;
  // Only add to component tree if it is being placed on the actual form.
  // - it could be placed outside in a custom form.
  // - "nil" = dock was abort using eg. ESC.
  if (Target = DesignerBox) or (not Assigned(Target)) then
  begin
    ComponentYTree.Add(TFieldLabel(Sender));
    ComponentXTree.Add(TFieldLabel(Sender));
  end;
end;

function TDesignFrame.GetLowestControlPosition(var LowestCtrl: TControl): TPoint;
var
  Hit, Prd: TAVLTreeNode;
begin
  Result := Point(ManagerSettings.DefaultRightPostion, 5);
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

function TDesignFrame.FindNewAutoControlPostion(NewCtrlClass: TClass): TPoint;
var
  Ctrl: TControl;
  Dist: Integer;
begin
  Result := GetLowestControlPosition(Ctrl);
  if Assigned(Ctrl) then
  begin
    Dist := ManagerSettings.SpaceBtwFieldLabel;
    if (NewCtrlClass = TFieldEdit) and (Ctrl is TFieldEdit) then
      Dist := ManagerSettings.SpaceBtwFieldField;
    if (NewCtrlClass = TFieldLabel) and (Ctrl is TFieldLabel) then
      Dist := ManagerSettings.SpaceBtwLabelLabel;
    Result.Y += (Ctrl.Height + Dist);
  end;
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
{  NewYTree := TAVLTree.Create(@YCmp);
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
  FComponentXTree := NewXTree;          }
end;

procedure TDesignFrame.DefaultAlignFields(ActiveControl: TControl;
  const ColumnCount: Integer);
var
  NewXTree, NewYTree: TFieldAVLTree;
  TheLeft, TheTop: Integer;
  Dx: Integer;
  Dy: Integer;
  Curr,Prev: TAVLTreeNode;
  Dist: Integer;
begin
  if ActiveControl = nil then
    GetHighestFieldPosition(ActiveControl);
  if not
    Assigned(ActiveControl) then exit;

  TheLeft := ActiveControl.Left;
  TheTop  := 5;

  NewYTree := NewCompTree(attY, @YCmp);
  NewXTree := NewCompTree(attX, @XCmp);

  Curr := ComponentYTree.FindLowest;
  Prev := Curr;
  Self.LockRealizeBounds;
  while Assigned(Curr) do
  begin
    Dist := ManagerSettings.SpaceBtwFieldLabel;
    if (TControl(Curr.Data) is TFieldEdit) then
    begin
      Dx := TFieldEdit(Curr.Data).Field.FieldX -
        TFieldEdit(Curr.Data).Field.LabelX;
      Dy := TFieldEdit(Curr.Data).Field.FieldY -
        TFieldEdit(Curr.Data).Field.LabelY;
      TFieldEdit(Curr.Data).Field.FieldX := TheLeft;
      TFieldEdit(Curr.Data).Field.FieldY := TheTop;
      TFieldEdit(Curr.Data).Field.LabelX := TheLeft - Dx;
      TFieldEdit(Curr.Data).Field.LabelY := TheTop - Dy;
      if (TControl(Curr.Data) is TFieldEdit) then
        Dist := ManagerSettings.SpaceBtwFieldField;
    end else begin
      TFieldLabel(Curr.Data).Field.FieldY := TheTop;
      TFieldLabel(Curr.Data).Field.LabelY := TheTop;
      if (TControl(Curr.Data) is TFieldLabel) then
        Dist := ManagerSettings.SpaceBtwLabelLabel;
    end;
    TheTop += TControl(Curr.Data).Height + Dist;

    NewXTree.Add(IFieldControl(Curr.Data));
    NewYTree.Add(IFieldControl(Curr.Data));
    Prev := Curr;
    Curr := ComponentYTree.FindSuccessor(Curr);
  end;
  Self.UnlockRealizeBounds;

  FreeAndNil(FComponentXTree);
  FComponentXTree := NewXTree;
  FreeAndNil(FComponentYTree);
  FComponentYTree := NewYTree;
end;

procedure TDesignFrame.EqualSpace(StartCtrl, EndCtrl: TControl);
var
  NewYTree, NewXTree: TFieldAVLTree;
  Curr: TAVLTreeNode;
  CmpCount: Integer;
  PrevTop: LongInt;
  Spacing: Integer;
  Dy: Integer;
begin
  if not Assigned(StartCtrl) then
    GetHighestFieldPosition(StartCtrl);
  if not Assigned(StartCtrl) then
    Exit;

  if not Assigned(EndCtrl) then
    EndCtrl := TControl(ComponentYTree.FindHighest.Data);

  CmpCount := 1;
  PrevTop := StartCtrl.Top;
  Curr := ComponentYTree.Find(StartCtrl);
  while Assigned(Curr) do
  begin
    if TControl(Curr.Data).Top > PrevTop then
      Inc(CmpCount);
    PrevTop := TControl(Curr.Data).Top;
    if TControl(Curr.Data) = EndCtrl then break;
    Curr := ComponentYTree.FindSuccessor(Curr);
  end;

  // This occurs if the last field is selected.
  if CmpCount = 1 then exit;

  // Spacing between "top" point of TEditFields, adjusted for overlapping components.
  Spacing := Max((EndCtrl.Top - StartCtrl.Top) div (CmpCount - 1),
                  StartCtrl.Height + ManagerSettings.SpaceBtwFieldField);

  NewYTree := NewCompTree(attY, @YCmp);
  NewXTree := NewCompTree(attX, @XCmp);

  CmpCount := 0;
  PrevTop := -1;
  Curr := ComponentYTree.Find(StartCtrl);

  Self.LockRealizeBounds;
  while Assigned(Curr) do
  begin
    if PrevTop = TControl(Curr.Data).Top then
      Dec(CmpCount);
    PrevTop := TControl(Curr.Data).Top;

    if TControl(Curr.Data) is TFieldEdit then
    with TFieldEdit(Curr.Data) do
    begin
      Dy := Field.FieldY - Field.LabelY;
      Field.FieldY := StartCtrl.Top + CmpCount * Spacing;
      Field.LabelY := Field.FieldY - Dy;
    end else with TFieldLabel(Curr.Data) do
    begin
      Field.FieldY := StartCtrl.Top + CmpCount * Spacing;
      Field.LabelY := Field.FieldY;
    end;

    NewXTree.Add(IFieldControl(Curr.Data));
    NewYTree.Add(IFieldControl(Curr.Data));

    if (TControl(Curr.Data) = EndCtrl) then
      Exit;
    Inc(CmpCount);
    Curr := ComponentYTree.FindSuccessor(Curr);
  end;
  Self.UnLockRealizeBounds;

  FreeAndNil(FComponentXTree);
  FComponentXTree := NewXTree;
  FreeAndNil(FComponentYTree);
  FComponentYTree := NewYTree;
end;

procedure TDesignFrame.LabelsAlignment(StartCtrl, EndCtrl: TControl;
  aAlign: TAlign);
var
  NewYTree, NewXTree: TFieldAVLTree;
  Curr: TAVLTreeNode;
  MaxVariableLabelWidth: Integer;
  MaxFieldNameWidth: Integer;
  MinLeft: Integer;
  NewLabelLeft: Integer;
begin
  if not (aAlign in [alLeft, alRight]) then exit;

  if not Assigned(StartCtrl) then
    GetHighestFieldPosition(StartCtrl);
  if not Assigned(StartCtrl) then
    Exit;

  if not Assigned(EndCtrl) then
    EndCtrl := TControl(ComponentYTree.FindHighest.Data);

  // Information collection pass.
  MaxVariableLabelWidth := 0;
  MaxFieldNameWidth     := 0;
  MinLeft               := MaxInt;
  Curr := ComponentYTree.Find(StartCtrl);
  while Assigned(Curr) do
  begin
    if (TControl(Curr.Data) is TFieldEdit) then
    with TFieldEdit(Curr.Data) do
    begin
      MaxVariableLabelWidth := Max(MaxVariableLabelWidth, VariableLabel.Width);
      MaxFieldNameWidth     := Max(MaxFieldNameWidth, FieldNameLabel.Width);
      MinLeft               := Min(MinLeft, Field.FieldX);
    end;

    if (TControl(Curr.Data) = EndCtrl) then
      Break;
    Curr := ComponentYTree.FindSuccessor(Curr);
  end;

  NewLabelLeft := MinLeft - (MaxVariableLabelWidth + 5) -
                            (MaxFieldNameWidth + 5);

  if NewLabelLeft < (MaxFieldNameWidth + 5) then
  begin
   NewLabelLeft := MaxFieldNameWidth + 5;
   MinLeft := MaxVariableLabelWidth + 5 + MaxFieldNameWidth + 5;
  end;

  NewYTree := NewCompTree(attY, @YCmp);
  NewXTree := NewCompTree(attX, @XCmp);

  Curr := ComponentYTree.Find(StartCtrl);
  Self.LockRealizeBounds;
  while Assigned(Curr) do
  begin
    if TControl(Curr.Data) is TFieldEdit then
    with TFieldEdit(Curr.Data) do
    begin
      case aAlign of
        alLeft:
          begin
            Field.FieldX := Max(Field.FieldX, MinLeft);
            Field.LabelX := NewLabelLeft;
          end;
        alRight:
          Field.LabelX := Field.FieldX - (VariableLabel.Width + 5);
      end;
    end else
    with TFieldLabel(Curr.Data) do
    begin
      case aAlign of
        alLeft:
          begin
            Field.FieldX := NewLabelLeft;
            Field.LabelX := NewLabelLeft;
          end;
        alRight:
          begin
            Field.FieldX := Max(Field.FieldX, MinLeft);
            Field.LabelX := Max(Field.FieldX, MinLeft);
          end;
      end;
    end;

    NewXTree.Add(IFieldControl(Curr.Data));
    NewYTree.Add(IFieldControl(Curr.Data));

    if (TControl(Curr.Data) = EndCtrl) then
      Exit;
    Curr := ComponentYTree.FindSuccessor(Curr);
  end;

  FreeAndNil(FComponentXTree);
  FComponentXTree := NewXTree;
  FreeAndNil(FComponentYTree);
  FComponentYTree := NewYTree;
end;

procedure TDesignFrame.NewShortCutFieldAction(aBtn: TToolButton);
var
  Pt: TPoint;
begin
  ActiveButton := aBtn;
  if ActiveButton = LabelFieldBtn then
    Pt := FindNewAutoControlPostion(TFieldLabel)
  else
    Pt := FindNewAutoControlPostion(TFieldEdit);
  DesignerMouseDown(nil, mbLeft, GetKeyShiftState, Pt.X, Pt.Y);
end;

function TDesignFrame.GetHighestFieldPosition(var HighestField: TControl): TPoint;
var
  Curr: TAVLTreeNode;
begin
  HighestField := nil;
  Result := Point(0,0);

  Curr := ComponentYTree.FindLowest;
  while Assigned(Curr) do
  begin
    if (TControl(Curr.Data) is TFieldEdit) then
    begin
      HighestField := TControl(Curr.Data);
      Result := Point(HighestField.Left, HighestField.Top);
      Break;
    end;
    Curr := ComponentYTree.FindSuccessor(Curr);
  end;
end;

constructor TDesignFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ActiveButton := SelectorButton;
  FActiveDatafile := TEpiDataFile.Create();
  FActiveDatafile.RegisterOnChangeHook(@DataFileChange);
  FActiveDocumentationForm := nil;
  SelectedControl := nil;
  Modified := false;
  FComponentYTree := NewCompTree(attY, @YCmp);
  FComponentXTree := NewCompTree(attX, @XCmp);

  // Manager settings specifics.
  DateFieldBtn.Tag := Ord(ManagerSettings.DefaultDateType);
  case ManagerSettings.DefaultDateType of
    ftEuroDate: DateFieldBtn.ImageIndex := 10;
    ftDate:     DateFieldBtn.ImageIndex := 11;
    ftYMDDate:  DateFieldBtn.ImageIndex := 12;
  end;

  {$IFDEF EPI_DEBUG}
  Panel1.Visible := true;
  {$ELSE EPI_DEBUG}
  Panel1.Visible := false;
  {$ENDIF}

  // Designer box creation and setup.
  // - (This is subject to change if we find a better component than
  //    the form or a scrollbox).
  FDesignerBox             := TScrollBox.Create(Self);
  FDesignerBox.Name        := 'DesingerBox';
  FDesignerBox.Parent      := Self;
  FDesignerBox.Align       := alClient;
  FDesignerBox.DockSite    := true;
  FDesignerBox.OnDockDrop  := @DesignerDockDrop;
  FDesignerBox.OnUnDock    := @DesignerUnDock;
  FDesignerBox.OnMouseDown := @DesignerMouseDown;
  FDesignerBox.OnMouseMove := @DesignerMouseMove;
  FDesignerBox.Color       := clWhite;
  FDesignerBox.AutoScroll  := true;
  FDesignerBox.PopupMenu   := DesignerPopup;
end;

destructor TDesignFrame.Destroy;
begin
  inherited Destroy;

  if Assigned(FActiveDatafile) then
    FreeAndNil(FActiveDatafile);
  FreeAndNil(FComponentYTree);
  FreeAndNil(FComponentXTree);
end;

procedure TDesignFrame.UpdateAllFields;
var
  i: Integer;
begin
  DateFieldBtn.Tag := Ord(ManagerSettings.DefaultDateType);
  case ManagerSettings.DefaultDateType of
    ftEuroDate: DateFieldBtn.ImageIndex := 10;
    ftDate:     DateFieldBtn.ImageIndex := 11;
    ftYMDDate:  DateFieldBtn.ImageIndex := 12;
  end;

  for i := DesignerBox.ControlCount -1 downto  0 do
  with DesignerBox do
  begin
    if not (Controls[i] is TFieldEdit) then continue;

    TFieldEdit(Controls[i]).ForceVisualUpdate;
    if ManagerSettings.ShowFieldNamesInLabel then
      TFieldEdit(Controls[i]).FieldNameLabel.Parent := DesignerBox
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

procedure TDesignFrame.DesignerDockDrop(Sender: TObject; Source: TDragDockObject;
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
  ActiveDataFile.BeginUpdate;
  with DesignerBox do
  begin
    for i := ControlCount - 1 downto  0 do
    begin
      Comp := Controls[i];

      if not ((Comp is TFieldEdit) or
        (Comp is TFieldLabel)) then continue;

      RemoveControl(Comp);
      ComponentYTree.Remove(Comp);
      ComponentXTree.Remove(Comp);
      FreeAndNil(Comp);
    end;
  end;
  ActiveDataFile.Reset;
  ActiveDataFile.RegisterOnChangeHook(@DataFileChange);
  // Handle in case there is an open documentation form.
  if Assigned(ActiveDocumentationForm) then
    ActiveDocumentationForm.Datafile := ActiveDataFile;
  ActiveDataFile.EndUpdate;  // Forced updatenonvisual due to registered hook.

  SelectedControl := nil;

  Modified := false;
end;

procedure TDesignFrame.AutoAlignBtnClick(Sender: TObject);
var
  AutoAlignForm: TAutoAlignForm;
  AutoAlignRes: TAutoAlignRecord;
  Res: TModalResult;
  Pt: TPoint;
begin
  with AutoAlignBtn do
    Pt := FieldToolBar.ClientToScreen(Point(Left, Top + Height + 1));
  AutoAlignForm := TAutoAlignForm.Create(self);
  AutoAlignForm.Left := Pt.X;
  AutoAlignForm.Top  := Pt.Y;
  Res := AutoAlignForm.ShowModal;
  AutoAlignRes := AutoAlignForm.AlignProperties;
  FreeAndNil(AutoAlignForm);

  if Res = mrCancel then Exit;

  case AutoAlignRes.AlignMethod of
    aamRemoveSpace: RemoveDeadSpace;
    aamEqualSpace:  EqualSpace(SelectedControl, nil);
    aamDefault:     DefaultAlignFields(SelectedControl);
  end;

  if AutoAlignRes.LabelsAlign <> alNone then
    LabelsAlignment(SelectedControl, nil, AutoAlignRes.LabelsAlign);

  Modified := true;
end;

procedure TDesignFrame.DeleteFieldMenuItemClick(Sender: TObject);
var
  TmpField: TEpiField;
begin
  Writeln('TDesignFrame.DeleteFieldMenuItemClick: A');
  if not Assigned(SelectedControl) then exit;

  Writeln('TDesignFrame.DeleteFieldMenuItemClick: B');
  if (SelectedControl is TFieldEdit) then
    TmpField := TFieldEdit(SelectedControl).Field
  else
    TmpField := TFieldLabel(SelectedControl).Field;

  Writeln('TDesignFrame.DeleteFieldMenuItemClick: C');
  {$IFNDEF EPI_DEBUG}
  if (TmpField.FieldType <> ftQuestion) and (TmpField.Size > 0) and
     (MessageDlg('Field contains data.' + LineEnding +
      'Are you sure you want to delete?', mtWarning, mbYesNo, 0) = mrNo) then
    exit;
  {$ENDIF}

  Writeln('TDesignFrame.DeleteFieldMenuItemClick: D');
  DesignerBox.RemoveControl(SelectedControl);
  Writeln('TDesignFrame.DeleteFieldMenuItemClick: E');
  ComponentYTree.Remove(SelectedControl);
  Writeln('TDesignFrame.DeleteFieldMenuItemClick: F');
  ComponentXTree.Remove(SelectedControl);
  Writeln('TDesignFrame.DeleteFieldMenuItemClick: G');
  FreeAndNil(SelectedControl);
  Writeln('TDesignFrame.DeleteFieldMenuItemClick: H');
  ActiveDatafile.RemoveField(TmpField, true);
  Writeln('TDesignFrame.DeleteFieldMenuItemClick: I');
  Modified := true;
end;

procedure TDesignFrame.DesignerMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  ShowOnMainStatusBar(Format('Mouse - X: %d Y: %d',[X, Y]), 4);

  Label5.Caption := Format(
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
  if not Assigned(SelectedControl) then exit;

  with SelectedControl do
    Pt := DesignerBox.ClientToScreen(Point(Left + Width, Top + Height));

  if (SelectedControl is TFieldEdit) then
  With TFieldEdit(SelectedControl) do
  begin
    FieldForm := TFieldCreateForm.Create(DesignerBox, ActiveDatafile, Field.FieldType, false);
    FieldForm.Left := Pt.X;
    FieldForm.Top  := Pt.Y;
    FieldForm.ReadField(Field);
    if FieldForm.ShowModal = mrCancel then exit;
    FieldForm.WriteField(Field);
    FreeAndNil(FieldForm);
    Self.Modified := true;
  end else
  With TFieldLabel(SelectedControl) do
  begin
    LabelForm := TCreateLabelForm.Create(DesignerBox, ActiveDatafile);
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

procedure TDesignFrame.DesignerMouseDown(Sender: TObject; Button: TMouseButton;
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
    Pt := DesignerBox.ClientToScreen(Point(X, Y{$IFNDEF WINDOWS} - DesignerBox.VertScrollBar.Position{$ENDIF}));

    if ActiveButton = LabelFieldBtn then
    begin
      CreateForm := TCreateLabelForm.Create(DesignerBox, ActiveDataFile);
      CreateForm.Top := Min(Pt.Y, Screen.Height - CreateForm.Height - 5);
      CreateForm.Left := Min(Pt.X, Screen.Width - CreateForm.Width - 5);
      if CreateForm.ShowModal = mrCancel then
      begin
        ToolBtnClick(SelectorButton);
        exit;
      end;
      TmpField := TEpiField.CreateField(TFieldType(ActiveButton.Tag), ActiveDataFile.Size);
      with TmpField do
      begin
        FieldName := TCreateLabelForm.GetFieldName(ActiveDataFile);
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
        CreateForm := TFieldCreateForm.Create(DesignerBox, ActiveDatafile, TFieldType(ActiveButton.Tag));
        CreateForm.Top := Min(Pt.Y, Screen.Height - CreateForm.Height - 5);
        CreateForm.Left := Min(Pt.X, Screen.Width - CreateForm.Width - 5);
        if CreateForm.ShowModal = mrCancel then
        begin
          ToolBtnClick(SelectorButton);
          exit;
        end;
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

    if (DesignerBox.VertScrollBar.Visible) then
      DesignerBox.VertScrollBar.Position := DesignerBox.VertScrollBar.Range - DesignerBox.VertScrollBar.Page;

    Modified := true;
    ToolBtnClick(SelectorButton);
  finally
    if Assigned(CreateForm) then FreeAndNil(CreateForm);
  end;
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
  FieldNo: Integer;
begin
  Dlg := TOpenDialog.Create(nil);
  Dlg.InitialDir := ManagerSettings.WorkingDirUTF8;
  Dlg.Filter := GetEpiDialogFilter(True, True, True, True, False, True, True,
    True, True, False);

  if not Dlg.Execute then exit;

  TmpDf := nil;
  Importer := TEpiImportExport.Create;
  Importer.OnProgress := @MainForm.ShowProgress;
  Importer.OnClipBoardRead := @MainForm.ReadClipBoard;
  Importer.Import(Dlg.FileName, TmpDF, dftNone);

  GetLowestControlPosition(TmpEdit);

  ActiveDataFile.BeginUpdate;
  for i := 0 to TmpDF.NumFields - 1 do
  begin
    TmpField := TmpDF[i].Clone(ActiveDataFile, false);
    if ActiveDataFile.FieldExists(TmpField.FieldName) then
      if TmpField.FieldType = ftQuestion then
        TmpField.FieldName := NextLabelName(ActiveDataFile)
      else
        TmpField.FieldName := NextFieldName(ActiveDataFile);
    ActiveDataFile.AddField(TmpField);

    if TmpField.FieldType = ftQuestion then
      Pt := FindNewAutoControlPostion(TFieldLabel)
    else
      Pt := FindNewAutoControlPostion(TFieldEdit);

    TmpField.FieldX := Pt.X;
    TmpField.FieldY := Pt.Y;
    TmpField.LabelX := 0;
    TmpField.LabelY := 0;

    if TmpField.FieldType = ftQuestion then
      NewQuestionLabel(TmpField)
    else
      NewFieldEdit(TmpField);
  end;
  ActiveDataFile.EndUpdate;

  Modified := true;
end;

procedure TDesignFrame.MoveDownActionExecute(Sender: TObject);
var
  Node: TAVLTreeNode;
begin
  Node := (SelectedControl as IFieldControl).YTreeNode;
  Node := ComponentYTree.FindSuccessor(Node);
  if not Assigned(Node) then
    Node := ComponentYTree.FindLowest;
  EnterSelectControl(TControl(Node.Data));
end;

procedure TDesignFrame.MoveFirstActionExecute(Sender: TObject);
var
  Node: TAVLTreeNode;
begin
  Node := ComponentYTree.FindLowest;

  while Assigned(Node) and (not (TControl(Node.Data) is TFieldEdit)) do
    Node := ComponentYTree.FindSuccessor(Node);

  if Assigned(Node) then
    EnterSelectControl(TControl(Node.Data));
end;

procedure TDesignFrame.MoveLastActionExecute(Sender: TObject);
var
  Node: TAVLTreeNode;
begin
  Node := ComponentYTree.FindHighest;

  while Assigned(Node) and (not (TControl(Node.Data) is TFieldEdit)) do
    Node := ComponentYTree.FindPrecessor(Node);

  if Assigned(Node) then
    EnterSelectControl(TControl(Node.Data));
end;

procedure TDesignFrame.MovePageDownActionExecute(Sender: TObject);
var
  Node: TAVLTreeNode;
  Dx: Integer;
  NextNode: TAVLTreeNode;
begin
  if ComponentYTree.Count <= 2 then exit;

  Node := (SelectedControl as IFieldControl).YTreeNode;
  if Node = ComponentYTree.FindHighest then exit;

  Dx := 0;
  NextNode := ComponentYTree.FindSuccessor(Node);
  if (TControl(NextNode.Data).Top + TControl(NextNode.Data).Height) > (DesignerBox.VertScrollBar.Position + DesignerBox.ClientHeight) then
    Dx := DesignerBox.ClientHeight;
  while (Assigned(Node)) and
    ((TControl(Node.Data).Top + TControl(Node.Data).Height) < (DesignerBox.VertScrollBar.Position + DesignerBox.ClientHeight + Dx)) do
    Node := ComponentYTree.FindSuccessor(Node);
  if Assigned(Node) then
    Node := ComponentYTree.FindPrecessor(Node)
  else
    Node := ComponentYTree.FindHighest;
  EnterSelectControl(TControl(Node.Data));
end;

procedure TDesignFrame.MovePageUpActionExecute(Sender: TObject);
var
  Node: TAVLTreeNode;
  Dx: Integer;
begin
  if ComponentYTree.Count <= 2 then exit;

  Node := (SelectedControl as IFieldControl).YTreeNode;
  if Node = ComponentYTree.FindLowest then exit;

  Dx := 0;
  if TControl(ComponentYTree.FindPrecessor(Node).Data).Top < (DesignerBox.VertScrollBar.Position) then
    Dx := DesignerBox.ClientHeight;
  while (Assigned(Node)) and (TControl(Node.Data).Top > (DesignerBox.VertScrollBar.Position  - Dx)) do
    Node := ComponentYTree.FindPrecessor(Node);
  if Assigned(Node) then
    Node := ComponentYTree.FindSuccessor(Node)
  else
    Node := ComponentYTree.FindLowest;
  EnterSelectControl(TControl(Node.Data));
end;

procedure TDesignFrame.MoveUpActionExecute(Sender: TObject);
var
  Node: TAVLTreeNode;
begin
  Node := (SelectedControl as IFieldControl).YTreeNode;
  Node := ComponentYTree.FindPrecessor(Node);
  if not Assigned(Node) then
    Node := ComponentYTree.FindHighest;
  EnterSelectControl(TControl(Node.Data));
end;

procedure TDesignFrame.NewDataMenuClick(Sender: TObject);
begin
   DatefieldPopupMenu.popup();
end;

procedure TDesignFrame.NewDateFieldMenuClick(Sender: TObject);
begin
  if not (Sender is TMenuItem) then exit;
  DateFieldBtn.Tag := TMenuItem(Sender).Tag;
  ManagerSettings.DefaultDateType := TFieldType(DateFieldBtn.Tag);
  DateFieldBtn.ImageIndex := TMenuItem(Sender).ImageIndex;
  ToolBtnClick(DateFieldBtn);
end;

procedure TDesignFrame.NewOtherFieldClick(Sender: TObject);
begin
  if not (Sender is TMenuItem) then exit;
  OtherFieldBtn.Tag := TMenuItem(Sender).Tag;
  ToolBtnClick(OtherFieldBtn);
end;

procedure TDesignFrame.NewDateFieldActionExecute(Sender: TObject);
begin
  NewShortCutFieldAction(DateFieldBtn);
end;

procedure TDesignFrame.DocumentFileActionExecute(Sender: TObject);
begin
  ActiveDataFile.SortFields(@SortFields);
  if not Assigned(FActiveDocumentationForm) then
    FActiveDocumentationForm := TDatafileDocumentationForm.Create(Self, ActiveDataFile);
  ActiveDocumentationForm.Show;
end;

procedure TDesignFrame.EditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Node: TAVLTreeNode;
  rct: TRect;
  pt: TPoint;
  Dx: Integer;
  KeyMsg: TLMKey;
begin
  if Key = VK_RETURN then
  begin
    EditFieldMenuItem.Click;
    Key := VK_UNKNOWN;
  end;

  // Movement actions.
  // ..going up: (Up, Shift+Tab, PgUp, Ctrl+Home)
  // - Single step = Up, shift + Tab
  if (Key = VK_UP) or ((Key = VK_TAB) and (ssShift in Shift)) then
  begin
    MoveUpAction.Execute;
    Key := VK_UNKNOWN
  end;
  // - Page step = PageUp (VK_PRIOR)
  if (Key = VK_PRIOR) then
  begin
    MovePageUpAction.Execute;
    Key := VK_UNKNOWN;
  end;
  // - Top step = Ctrl + Home
  if (Key = VK_HOME) and (ssCtrl in Shift) then
  begin
    MoveFirstAction.Execute;
    Key := VK_UNKNOWN;
  end;
  // ..going down: (Down, Tab, PgDown, Ctrl+End)
  // - Single step = Down, Tab
  if (Key = VK_DOWN) or (Key = VK_TAB) then
  begin
    MoveDownAction.Execute;
    Key := VK_UNKNOWN
  end;
  // - Page step = PageDown (VK_NEXT)
  if (Key = VK_NEXT) then
  begin
    MovePageDownAction.Execute;
    Key := VK_UNKNOWN;
  end;
  // - Bottom step = Ctrl + End
  if (Key = VK_END) and (ssCtrl in Shift) then
  begin
    MoveLastAction.Execute;
    Key := VK_UNKNOWN;
  end;

  // Ugly dirty way of capturing shortcuts involving keys.
  // -- send to mainform, it automatically propagetes down through action lists..
  if Key <> VK_UNKNOWN then
  begin
    KeyMsg.Msg := LM_KEYDOWN;
    KeyMsg.KeyData := ShortCut(0, Shift);
    if (ssAlt in Shift) then
      KeyMsg.KeyData := KeyMsg.KeyData or $20000000;
    KeyMsg.CharCode := Key;
    KeyMsg.Result := 0;
    if MainForm.IsShortcut(KeyMsg) then
      Key := VK_UNKNOWN;
  end;
end;

function TDesignFrame.NewCompTree(TreeType: TFieldAVLTreeType;
  CmpMethod: TListSortCompare): TFieldAVLTree;
begin
  Result := TFieldAVLTree.Create(CmpMethod);
  Result.AVLTreeType := TreeType;
end;

procedure TDesignFrame.EditCompActionExecute(Sender: TObject);
begin
  if not Assigned(SelectedControl) then exit;
  EditFieldMenuItem.Click;
end;

procedure TDesignFrame.DesignerUnDock(Sender: TObject; Client: TControl;
  NewTarget: TWinControl; var Allow: Boolean);
begin
  if (NewTarget = DesignerBox) then
    Allow := true
  else
    Allow := false;
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

procedure TDesignFrame.NewStringFieldActionExecute(Sender: TObject);
begin
  NewShortCutFieldAction(StringFieldBtn);
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
     (MessageDlg('Dataform was not saved.' + LineEnding + 'Close Form?', mtWarning, mbYesNo, 0) = mrNo) then
    exit;
  {$ENDIF}

  Dlg := TOpenDialog.Create(nil);
  Dlg.InitialDir := ManagerSettings.WorkingDirUTF8;
  Dlg.Filter := GetEpiDialogFilter(True, True, True, True,
    False, True, True, True, True, True);

  if Dlg.Execute then
  begin
    ClearToolBtn.Click;
    FreeAndNil(FActiveDatafile);

    Fn := Dlg.FileName;
    Import := TEpiImportExport.Create;
    Import.OnProgress := @MainForm.ShowProgress;
    Import.OnClipBoardRead := @MainForm.ReadClipBoard;
    Import.FieldNaming := ManagerSettings.FieldNamingStyle;
    Import.Import(Fn, FActiveDatafile, dftNone);
    FActiveDatafile.RegisterOnChangeHook(@DataFileChange);
    FreeAndNil(Import);

    Application.ProcessMessages;
    Self.LockRealizeBounds;

    TmpEdit := nil;
    ActiveDataFile.BeginUpdate;
    for i := 0 to ActiveDatafile.NumFields - 1 do
    begin
      MainForm.ShowProgress(Self, ((i+1)*100) div ActiveDataFile.NumFields, '');
      TmpField := ActiveDatafile[i];
      if ActiveDataFile.DatafileType <> dftEpiDataXml then
      begin
        if TmpField.FieldType = ftQuestion then
          Pt := FindNewAutoControlPostion(TFieldLabel)
        else
          Pt := FindNewAutoControlPostion(TFieldEdit);
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
    EnterSelectControl(TmpEdit);
    TmpEdit.SetFocus;
    ActiveDataFile.EndUpdate;
    Self.UnLockRealizeBounds;

    UpdateNonInteractiveVisuals;

    if ActiveDatafile.DatafileType <> dftEpiDataXml then
      LabelsAlignment(SelectedControl, Nil, alLeft);

    Modified := false;
  end;
  FreeAndNil(Dlg);
end;

procedure TDesignFrame.PasteAsLabelExecute(Sender: TObject);
var
  Cbl: TStringList;
  Pt: TPoint;
  i: Integer;
  TmpField: TEpiField;
begin
  Cbl := TStringList.Create;
  try
    MainForm.ReadClipBoard(Cbl);

    for i := 0 to Cbl.Count - 1 do
    begin
      if Trim(Cbl[i]) = '' then continue;

      Pt := FindNewAutoControlPostion(TFieldLabel);
      TmpField := TEpiField.CreateField(ftQuestion, ActiveDataFile.Size);
      with TmpField do
      begin
        FieldName := TCreateLabelForm.GetFieldName(ActiveDataFile);
        VariableLabel := Trim(Cbl[i]);
        FieldX := Pt.X;
        FieldY := Pt.Y;
        LabelX := Pt.X;
        LabelY := Pt.Y;
      end;
      ActiveDatafile.AddField(TmpField);
      NewQuestionLabel(TmpField);
    end;

    if (DesignerBox.VertScrollBar.Visible) and (true) then
      DesignerBox.VertScrollBar.Position := DesignerBox.VertScrollBar.Range - DesignerBox.VertScrollBar.Page;

  finally
    Cbl.Free;
  end;
end;

procedure TDesignFrame.SaveFileActionExecute(Sender: TObject);
begin
  if ActiveDataFile.DatafileType <> dftEpiDataXml then
  begin
    SaveFileAsAction.Execute;
    Exit;
  end;

  ActiveDataFile.SortFields(@SortFields);
  ActiveDataFile.Save(ActiveDataFile.FileName, []);
  Modified := false;
  ShowOnMainStatusBar('Saving complete: ' + ActiveDataFile.FileName, 0);
end;

procedure TDesignFrame.SaveFileAsActionExecute(Sender: TObject);
var
  Fn: String;
  Dlg: TSaveDialog;
begin
  Dlg := TSaveDialog.Create(Self);
  try
    Dlg.Filter := GetEpiDialogFilter(true, false, false,
      false, false, false, false, false, false, false);
    Dlg.Options := [ofDontAddToRecent, ofOverwritePrompt];
    Dlg.InitialDir := ManagerSettings.WorkingDirUTF8;
    if Dlg.Execute then
    begin
      Fn := Dlg.FileName;

      if CompareFileExt(Fn, '.recxml') <> 0 then
        Fn := Fn + '.recxml';

      BackupFile(Fn);

      ActiveDataFile.SortFields(@SortFields);
      ActiveDatafile.Save(Fn, []);

      Modified := false;

      MainForm.PageControl1.ActivePage.Caption := ExtractFileName(Fn);
      MainForm.PageControl1.Hint := ExpandFileNameUTF8(Fn);
      MainForm.PageControl1.ShowHint := true;
      ShowOnMainStatusBar('Saving complete: ' + Fn, 0);
    end;
  finally
    Dlg.Free;
  end;
end;

initialization
  {$I design_frame.lrs}

end.

