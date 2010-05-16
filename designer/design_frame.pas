unit design_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, FileUtil, LResources, Forms, ComCtrls, Controls,
  ActnList, ExtCtrls, StdCtrls, Menus, epidatafiles, epidatafilestypes,
  design_field, design_heading, design_section, epicustombase,
  design_custombase, AVL_Tree ;

type

  { TDesignFrame }

  TMouseState = record
    Down: boolean;
    Button: TMouseButton;
    Shift: TShiftState;
  end;

  TDesignFrame = class(TFrame)
    MoveEndAction: TAction;
    MoveHomeAction: TAction;
    MovePgDnAction: TAction;
    MovePgUpAction: TAction;
    MoveDownAction: TAction;
    MoveUpAction: TAction;
    Button2: TButton;
    Button3: TButton;
    DateFieldPopupMenu: TPopupMenu;
    ImportDataFileAction: TAction;
    Label6: TLabel;
    LoadDataFileAction: TAction;
    ExportDataformAction: TAction;
    DeleteControlAction: TAction;
    EditControlAction: TAction;
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    EditPopupMenuItem: TMenuItem;
    DeletePopupMenuItem: TMenuItem;
    NewTimeNowFieldMenu: TMenuItem;
    NewTimeFieldMenu: TMenuItem;
    TimeSubMenu: TMenuItem;
    NewAutoIncMenu: TMenuItem;
    NewCryptFieldMenu: TMenuItem;
    NewDMYFieldMenu: TMenuItem;
    NewDMYTodayFieldMenu: TMenuItem;
    NewMDYFieldMenu: TMenuItem;
    NewMDYTodayFieldMenu: TMenuItem;
    NewSectionAction: TAction;
    NewHeadingAction: TAction;
    NewSoundexFieldMenu: TMenuItem;
    NewUpperFieldMenu: TMenuItem;
    NewYMDFieldAction: TAction;
    NewMDYFieldAction: TAction;
    NewDMYFieldAction: TAction;
    NewStringFieldAction: TAction;
    NewFloatFieldAction: TAction;
    NewIntFieldAction: TAction;
    DesignerActionList: TActionList;
    DesignerImageList: TImageList;
    DesignerToolBar: TToolBar;
    IntToolButton: TToolButton;
    NewYMDFieldMenu: TMenuItem;
    NewYMDTodayFieldMenu: TMenuItem;
    OtherFieldsPopup: TPopupMenu;
    Panel1: TPanel;
    EpiControlPopUpMenu: TPopupMenu;
    SelectorToolButton: TToolButton;
    Splitter1: TSplitter;
    EditToolButton: TToolButton;
    Divider1: TToolButton;
    FloatToolButton: TToolButton;
    StringSubMenu: TMenuItem;
    StringToolButton: TToolButton;
    DateToolButton: TToolButton;
    OtherToolButton: TToolButton;
    Divider6: TToolButton;
    Divider3: TToolButton;
    Divider0: TToolButton;
    DeleteToolButton: TToolButton;
    Divider5: TToolButton;
    LoadToolButton: TToolButton;
    Divider2: TToolButton;
    HeadingToolButton: TToolButton;
    SectionToolButton: TToolButton;
    ImportToolButton: TToolButton;
    ExportToolButton: TToolButton;
    Divider4: TToolButton;
    TodayDateSubMenu: TMenuItem;
    TestToolButton: TToolButton;
    procedure   Button1Click(Sender: TObject);
    procedure   Button2Click(Sender: TObject);
    procedure   Button3Click(Sender: TObject);
    procedure   DeleteControlActionExecute(Sender: TObject);
    procedure   EditControlActionExecute(Sender: TObject);
    procedure   LoadDataFileActionExecute(Sender: TObject);
    procedure   MoveDownActionExecute(Sender: TObject);
    procedure   MoveUpActionExecute(Sender: TObject);
    procedure   NewDateFieldMenuClick(Sender: TObject);
    procedure   NewOtherFieldMenuClick(Sender: TObject);
    procedure   ToggleToolBtn(Sender: TObject);
  private
    { common private }
    FDataFile: TEpiDataFile;
    FDesignerBox: TScrollBox;
    FActiveButton: TToolButton;
    FLeftMouseDown:  TPoint;
    FLeftMouseUp:  TPoint;
    FRightMouseDown: TPoint;
    FRightMouseUp: TPoint;
    procedure   ResetMousePos;
    function    ShowForm(EpiControl: TEpiCustomControlItem;
      Pos: TPoint): TModalResult;
    procedure   ShowEpiControlPopup(Sender: TControl; Pos: TPoint);

  private
    { Docksite controls - methods }
    // - mouse
    FMouseState: TMouseState;
    FActiveDockSite: TWinControl;       // For debugging
    FShowPanel: TPanel; // For showing the area for a new section.
    procedure   DrawShowPanel(X, Y: Integer);
    procedure   DockSiteMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure   DockSiteMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure   DockSiteMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    // - docking
    procedure   DockSiteDockOver(Sender: TObject; Source: TDragDockObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure   DockSiteDockDrop(Sender: TObject; Source: TDragDockObject; X,
      Y: Integer);
    procedure   DockSiteUnDock(Sender: TObject; Client: TControl;
      NewTarget: TWinControl; var Allow: Boolean);

  private
    { Design controls - methods }
    FActiveControl: TControl;
    function    NewDesignControl(AClass: TControlClass;
      AParent: TWinControl; Pos: TPoint;
      EpiControl: TEpiCustomControlItem): TControl;
    function    NewSectionControl(StartPos, EndPos: TPoint;
      EpiControl: TEpiCustomControlItem): TControl;
    // - Dock Events.
    procedure   DesignControlStartDock(Sender: TObject; var DragObject: TDragDockObject);
    // - Mouse events.
    procedure   DesignControlMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure   DesignControlMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    // - Key controls.
  private
    procedure   DesignKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure   EnterControl(Sender: TObject);
    procedure   ExitControl(Sender: TObject);
  private
    { Position handling }
    function    FindNewPosition(ParentControl: TWinControl;
      AClass: TControlClass): TPoint;
  private
    { Epidata Core Objects }
    FActiveSection: TEpiSection;
    function    NewField(FieldType: TEpiFieldType): TEpiField;
    function    NewHeading: TEpiHeading;
    function    NewSection: TEpiSection;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; ADataFile: TEpiDataFile);
    property    DataFile: TEpiDataFile read FDataFile;
  end; 

implementation

{$R *.lfm}

uses
  Graphics, Clipbrd, epidocument, epiadmin, math, import_form, LMessages,
  LCLType, main;

type

  { TScrollBoxEx }

  TScrollBoxEx = class(TScrollBox, IDesignEpiControl, IPositionHandler)
  private
    FEpiControl: TEpiCustomControlItem;
    FXTree:     TAVLTree;
    FYTree:     TAVLTree;
  protected
    function    GetEpiControl: TEpiCustomControlItem;
    function    GetXTreeNode: TAVLTreeNode;
    function    GetYTreeNode: TAVLTreeNode;
    procedure   SetEpiControl(const AValue: TEpiCustomControlItem);
    procedure   SetXTreeNode(const AValue: TAVLTreeNode);
    procedure   SetYTreeNode(const AValue: TAVLTreeNode);
    function    GetXTree: TAVLTree;
    function    GetYTree: TAVLTree;
  public
    constructor Create(AOwner: TComponent); override;
    property    EpiControl: TEpiCustomControlItem read GetEpiControl write SetEpiControl;
    property    XTreeNode: TAVLTreeNode read GetXTreeNode write SetXTreeNode;
    property    YTreeNode: TAVLTreeNode read GetYTreeNode write SetYTreeNode;
    property    XTree: TAVLTree read GetXTree;
    property    YTree: TAVLTree read GetYTree;
  end;

  TDesignDockObject = class(TDragDockObject)
  private
    FOldDockSite: TWinControl;
  end;


  TControlEx = class(TControl);

{ TScrollBoxEx }

function TScrollBoxEx.GetEpiControl: TEpiCustomControlItem;
begin
  result := FEpiControl;
end;

function TScrollBoxEx.GetXTreeNode: TAVLTreeNode;
begin
  // Not used
end;

function TScrollBoxEx.GetYTreeNode: TAVLTreeNode;
begin
  // Not used
end;

procedure TScrollBoxEx.SetEpiControl(const AValue: TEpiCustomControlItem);
begin
  if FEpiControl = AValue then exit;
  FEpiControl := AValue;
end;

procedure TScrollBoxEx.SetXTreeNode(const AValue: TAVLTreeNode);
begin
  // Not used
end;

procedure TScrollBoxEx.SetYTreeNode(const AValue: TAVLTreeNode);
begin
  // Not used
end;

function TScrollBoxEx.GetXTree: TAVLTree;
begin
  result := FXTree;
end;

function TScrollBoxEx.GetYTree: TAVLTree;
begin
  result := FYTree;
end;

constructor TScrollBoxEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FXTree := TAVLTree.Create(@XTreeSort);
  FYTree := TAVLTree.Create(@YTreeSort);
end;


{ TDesignFrame }

procedure TDesignFrame.ToggleToolBtn(Sender: TObject);
begin
  if not (Sender is TToolButton) then exit;
  FActiveButton.Down := false;

  if FActiveButton = OtherToolButton then
    OtherToolButton.ImageIndex := 7;

  TToolButton(Sender).Down := true;
  FActiveButton := TToolButton(Sender);
end;

procedure TDesignFrame.ResetMousePos;
var
  Pt: TPoint;
begin
  Pt := Point(-1, -1);
  FLeftMouseDown := Pt;
  FLeftMouseUp := Pt;
  FRightMouseDown := Pt;
  FRightMouseUp := Pt;
end;

procedure TDesignFrame.Button1Click(Sender: TObject);
begin
  Clipboard.AsText := DataFile.SaveToXml('', 0);
end;

procedure TDesignFrame.DesignKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  KeyMsg: TLMKey;
  Ctrl: TControl absolute Sender;
  Handler: IPositionHandler;
begin
  if Key = VK_RETURN then
  begin
    EditToolButton.Click;
    Key := VK_UNKNOWN;
  end;

  Handler := nil;
  if Supports(Ctrl.Parent, IPositionHandler) then
    Handler := (Ctrl.Parent as IPositionHandler);

  // Movement actions.
  // ..going up: (Up, Shift+Tab, PgUp, Ctrl+Home)
  // - Single step = Up, shift + Tab
  if (Key = VK_UP) or ((Key = VK_TAB) and (ssShift in Shift)) then
  begin
    MoveUpAction.Execute;
    Key := VK_UNKNOWN
  end;
{  // - Page step = PageUp (VK_PRIOR)
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
  end;}
  // ..going down: (Down, Tab, PgDown, Ctrl+End)
  // - Single step = Down, Tab
  if (Key = VK_DOWN) or (Key = VK_TAB) then
  begin
    MoveDownAction.Execute;
    Key := VK_UNKNOWN
  end;
{  // - Page step = PageDown (VK_NEXT)
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
                    }
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

procedure TDesignFrame.Button2Click(Sender: TObject);
begin
  Clipboard.AsText :=  WriteTree((FActiveDockSite as IPositionHandler).XTree);
end;

procedure TDesignFrame.Button3Click(Sender: TObject);
begin
  Clipboard.AsText :=  WriteTree((FActiveDockSite as IPositionHandler).YTree);
end;

procedure TDesignFrame.DeleteControlActionExecute(Sender: TObject);
var
  EpiCtrl: TEpiCustomControlItem;
  LocalCtrl: TControl;
begin
  EpiCtrl := (FActiveControl as IDesignEpiControl).EpiControl;

  // TODO : Find next control in line.
  LocalCtrl := FActiveControl;
  ExitControl(FActiveControl);

  // TODO : Show warning when containing data.
  EpiCtrl.Free;  // This also removes the epicontrol from it's parent/list.
  LocalCtrl.Free;

  EnterControl(FDesignerBox);
end;


procedure TDesignFrame.EditControlActionExecute(Sender: TObject);
var
  EpiCtrl: TEpiCustomControlItem;
  Pt: TPoint;
begin
  EpiCtrl := (FActiveControl as IDesignEpiControl).EpiControl;
  if (FRightMouseUp.X = -1) then
    Pt := FActiveControl.Parent.ClientToScreen(Point(EpiCtrl.Left, EpiCtrl.Top))
  else
    Pt := FRightMouseUp;
  ShowForm(EpiCtrl, Pt);
end;


procedure TDesignFrame.LoadDataFileActionExecute(Sender: TObject);
var
  Frm: TImportForm;
begin
  Frm := TImportForm.Create(Self);
  Frm.ShowModal;
end;

procedure TDesignFrame.MoveDownActionExecute(Sender: TObject);
var
  YTree: TAVLTree;
  Ctrl: TControl;
  Node: TAVLTreeNode;
begin
  if not Assigned(FActiveControl) then exit;

  if FActiveControl = FDesignerBox then
  begin
    if (FActiveControl as IPositionHandler).YTree.Count = 0 then exit;
    Node := (FActiveControl as IPositionHandler).YTree.FindHighest;
  end else
    Node := (FActiveControl as IDesignEpiControl).YTreeNode;

  Ctrl := TControl(Node.Data);

  if (Ctrl is TDesignSection) and ((Ctrl as IPositionHandler).YTree.Count > 0) then
    Node := (Ctrl as IPositionHandler).YTree.FindLowest
  else
    Node := (Ctrl.Parent as IPositionHandler).YTree.FindSuccessor(Node);

  if Assigned(Node) then
  begin
    EnterControl(TControl(Node.Data));
    Exit;
  end;

  // Node is nil, find parent.
  if (Ctrl.Parent is TDesignSection) then
  begin
    Ctrl := Ctrl.Parent;
    YTree := (Ctrl.Parent as IPositionHandler).YTree;
    Node := YTree.FindSuccessor((Ctrl as IDesignEpiControl).YTreeNode);
    if Assigned(Node) then
    begin
      EnterControl(TControl(Node.Data));
      Exit;
    end;
  end;
  // We are at the bottom - select top most (lowest in AVL tree) component.
  Node := (Ctrl.Parent as IPositionHandler).YTree.FindLowest;
  EnterControl(TControl(Node.Data));
end;

procedure TDesignFrame.MoveUpActionExecute(Sender: TObject);
var
  YTree: TAVLTree;
  Ctrl: TControl;
  Node: TAVLTreeNode;
begin
  if not Assigned(FActiveControl) then exit;

  if FActiveControl = FDesignerBox then
  begin
    if (FActiveControl as IPositionHandler).YTree.Count = 0 then exit;
    Node := (FActiveControl as IPositionHandler).YTree.FindHighest;
  end else
    Node := (FActiveControl as IDesignEpiControl).YTreeNode;

  Ctrl := TControl(Node.Data);
  if (Ctrl is TDesignSection) and ((Ctrl as IPositionHandler).YTree.Count > 0) then
    Node := (Ctrl as IPositionHandler).YTree.FindHighest
  else
    Node := (Ctrl.Parent as IPositionHandler).YTree.FindPrecessor(Node);

  if Assigned(Node) then
  begin
    EnterControl(TControl(Node.Data));
    Exit;
  end;

  // Node is nil, find parent.
  if (Ctrl.Parent is TDesignSection) then
  begin
    Ctrl := Ctrl.Parent;
    YTree := (Ctrl.Parent as IPositionHandler).YTree;
    Node := YTree.FindPrecessor((Ctrl as IDesignEpiControl).YTreeNode);
    if Assigned(Node) then
    begin
      EnterControl(TControl(Node.Data));
      Exit;
    end;
  end;
  Node := (Ctrl.Parent as IPositionHandler).YTree.FindHighest;
  EnterControl(TControl(Node.Data));
end;

procedure TDesignFrame.NewDateFieldMenuClick(Sender: TObject);
begin
  if not (Sender is TMenuItem) then exit;

  DateToolButton.Tag := TMenuItem(Sender).Tag;
  DateToolButton.ImageIndex := TMenuItem(Sender).ImageIndex;
  ToggleToolBtn(DateToolButton);
end;

procedure TDesignFrame.NewOtherFieldMenuClick(Sender: TObject);
begin
  if not (Sender is TMenuItem) then exit;

  OtherToolButton.Tag := TMenuItem(Sender).Tag;
  OtherToolButton.ImageIndex := TMenuItem(Sender).ImageIndex;
  ToggleToolBtn(OtherToolButton);
end;


function TDesignFrame.ShowForm(EpiControl: TEpiCustomControlItem; Pos: TPoint
  ): TModalResult;
var
  Form: TDesignCustomForm;
begin
  if EpiControl is TEpiField then
    Form := TDesignFieldForm.Create(Self);
  if EpiControl is TEpiHeading then
    Form := TDesignHeadingForm.Create(Self);
  if EpiControl is TEpiSection then
    Form := TDesignSectionForm.Create(Self);
  Form.EpiControl := EpiControl;
  Form.Left := Pos.X;
  Form.Top := Pos.Y;
  result := Form.ShowModal;
end;

function TDesignFrame.NewDesignControl(AClass: TControlClass;
  AParent: TWinControl; Pos: TPoint; EpiControl: TEpiCustomControlItem
  ): TControl;
var
  Ctrl: TControlEx;
begin
  Result := AClass.Create(AParent);
  Ctrl := TControlEx(Result);

  (Ctrl as IDesignEpiControl).EpiControl := EpiControl;
  with Ctrl do
  begin
    OnMouseDown := @DesignControlMouseDown;
    OnMouseUp   := @DesignControlMouseUp;
    OnStartDock := @DesignControlStartDock;
    if Result is TWinControl then
      TWinControl(Result).OnKeyDown   := @DesignKeyDown;
    Parent      := AParent;
  end;

  with EpiControl do
  begin
    BeginUpdate;
    Left := Pos.X;
    Top  := Pos.Y;
    EndUpdate;
  end;
  EnterControl(Result);
  AddToPositionHandler((AParent as IPositionHandler), Result);
end;

function TDesignFrame.NewSectionControl(StartPos, EndPos: TPoint;
  EpiControl: TEpiCustomControlItem): TControl;
var
  Ctrl: TDesignSection;
  EpiSection: TEpiSection absolute EpiControl;
const
  MinSectionWidth = 200;
  MinSectionHeight = 100;
begin
  Ctrl := TDesignSection.Create(FDesignerBox);
  Result := Ctrl;

  Ctrl.EpiControl := EpiSection;
  with Ctrl do
  begin
    OnMouseDown := @DockSiteMouseDown;
    OnMouseMove := @DockSiteMouseMove;
    OnMouseUp   := @DockSiteMouseUp;
    OnDockDrop  := @DockSiteDockDrop;
    OnUnDock    := @DockSiteUnDock;
    OnDockOver  := @DockSiteDockOver;
    OnStartDock := @DesignControlStartDock;
    OnKeyDown   := @DesignKeyDown;
  end;

  with EpiSection do
  begin
    BeginUpdate;
    Left := StartPos.X;
    Top  := StartPos.Y;
    Width := Max(Abs(StartPos.X - EndPos.X), MinSectionWidth);
    Height := Max(Abs(StartPos.Y - EndPos.Y), MinSectionHeight);
    EndUpdate;
  end;
  Ctrl.Parent := FDesignerBox;

  // Forces designer box last in docksite list, because retrieving docksite
  // is not implemented fully in the LCL.
  FDesignerBox.DockSite := false;
  FDesignerBox.DockSite := true;

  EnterControl(Result);
  AddToPositionHandler((FDesignerBox as IPositionHandler), Result);
end;

procedure TDesignFrame.DesignControlStartDock(Sender: TObject;
  var DragObject: TDragDockObject);
var
  Ctrl: TControl absolute Sender;
begin
  DragObject := TDesignDockObject.Create(Ctrl);
  TDesignDockObject(DragObject).FOldDockSite := Ctrl.Parent;
end;

procedure TDesignFrame.ShowEpiControlPopup(Sender: TControl; Pos: TPoint);
begin
  EpiControlPopUpMenu.PopUp(Pos.X, Pos.Y);
end;

procedure TDesignFrame.DrawShowPanel(X, Y: Integer);
var
  Pt1: TPoint;
  Pt2: TPoint;
begin
  Pt1 := FDesignerBox.ScreenToClient(FLeftMouseDown);
  Pt2 := Point(X, Y);
  FShowPanel.Left := Min(Pt1.X, Pt2.X);
  FShowPanel.Top := Min(Pt1.Y, Pt2.Y);
  FShowPanel.Width := Abs(Pt1.X - Pt2.X);
  FShowPanel.Height := Abs(Pt1.Y - Pt2.Y);
end;

function TDesignFrame.FindNewPosition(ParentControl: TWinControl;
  AClass: TControlClass): TPoint;
begin
  result := Point(20,20);
end;

function TDesignFrame.NewField(FieldType: TEpiFieldType): TEpiField;
begin
  result := FActiveSection.NewField(FieldType);
end;

function TDesignFrame.NewHeading: TEpiHeading;
begin
  result := FActiveSection.NewHeading;
end;

function TDesignFrame.NewSection: TEpiSection;
begin
  result := DataFile.NewSection;
end;

procedure TDesignFrame.DockSiteMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  WinSender: TWinControl absolute Sender;
begin
  FActiveSection := TEpiSection((Sender as IDesignEpiControl).EpiControl);
  FActiveDockSite := WinSender;

  case Button of
    mbLeft: FLeftMouseDown := WinSender.ClientToScreen(Point(X, Y));
    mbRight: FRightMouseDown := WinSender.ClientToScreen(Point(X, Y));
  end;

  FMouseState.Down := true;
  FMouseState.Button := Button;
  FMouseState.Shift := Shift;

  EnterControl(Sender);

  if FActiveButton.Index <> 11 then exit;
  FShowPanel := TPanel.Create(FDesignerBox);
  FShowPanel.Color := clWhite;
  FShowPanel.BorderStyle := bsSingle;
  FShowPanel.BorderWidth := 1;
  FShowPanel.BevelOuter := bvNone;
  FShowPanel.BevelInner := bvNone;
  DrawShowPanel(X+1, Y+1);
  FShowPanel.Parent := FDesignerBox;
end;

procedure TDesignFrame.DockSiteMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  WinSender: TWinControl absolute Sender;
  ParentPt: TPoint;
  EpiControl: TEpiCustomControlItem;
  panel: TPanel;
begin
  case Button of
    mbLeft: FLeftMouseUp := WinSender.ClientToScreen(Point(X, Y));
    mbRight: FRightMouseUp := WinSender.ClientToScreen(Point(X, Y));
  end;

  FMouseState.Down := false;

  // Right button activates popup-menu
  if Button = mbRight then
  begin
    ShowEpiControlPopup(WinSender, FRightMouseUp);
    Exit;
  end;

  ParentPt := WinSender.ScreenToClient(FLeftMouseUp);

  case FActiveButton.Index of
    // Dividers... should never get here:
    0, 2, 8, 10, 12, 15, 19:
      Exit;

    // Selector
    1: Exit;

    // Integer, Float, String, Date (all dates), Others.
    3,4,5,6,7:
      begin
        EpiControl := NewField(TEpiFieldType(FActiveButton.Tag));
        if ShowForm(EpiControl, FLeftMouseUp) = mrOK then
          NewDesignControl(TDesignField, WinSender, ParentPt, EpiControl)
        else
          EpiControl.Free;
      end;
    // Heading
    9:
      begin
        EpiControl := NewHeading;
        if ShowForm(EpiControl, FLeftMouseUp) = mrOK then
          NewDesignControl(TDesignHeading, WinSender, ParentPt, EpiControl)
        else
          EpiControl.Free;
      end;
    // Section
    11:
      begin
        // Sections can only be created on the designer.
        if not (Sender = FDesignerBox) then exit;
        EpiControl := NewSection;
        if ShowForm(EpiControl, FLeftMouseDown) = mrOK then
          NewSectionControl(WinSender.ScreenToClient(FLeftMouseDown),
            WinSender.ScreenToClient(FLeftMouseUp), EpiControl)
        else
          EpiControl.Free;
        FShowPanel.Free;
      end;

    // TEST BUTTON!
    20:
      begin
        panel := TPanel.Create(FDesignerBox);
        panel.Top := FLeftMouseUp.Y;
        Panel.Left := FLeftMouseUp.X;
        panel.Width := abs(FLeftMouseDown.X - panel.left);
        panel.Height := abs(FLeftMouseDown.Y - panel.Top);
        panel.Parent := FDesignerBox;
        panel.OnMouseDown := @DockSiteMouseDown;
        panel.OnMouseMove := @DockSiteMouseMove;
      end;
  end;
  ToggleToolBtn(SelectorToolButton);
end;

procedure TDesignFrame.DockSiteMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  TopLeft: TPoint;
  S: String;
begin
  S := '';
  WriteStr(S, TWinControl(Sender).Focused);
  Label6.Caption := Format(
    'Focused: %s' + LineEnding +
    'XTree: %d' + LineEnding +
    'YTree: %d',
    [S,
     (Sender as IPositionHandler).XTree.Count,
     (Sender as IPositionHandler).YTree.Count
    ]
  );

  // Used to paint box when creating the section.
  if not (
    (FActiveButton = SectionToolButton) and
    (FMouseState.Down) and
    (FMouseState.Button = mbLeft)) then exit;

  TopLeft := FDesignerBox.ScreenToClient(FLeftMouseDown);
  Label4.Caption := Format(
    'Top:    %d, Left:  %d' + LineEnding +
    'Bottom: %d, Right: %d',
    [TopLeft.X, TopLeft.Y, X, Y]);
  Label5.Caption := Format(
    'Moving mouse over (%s:%s)', [TControl(Sender).Name, Sender.ClassName]);

  DrawShowPanel(X, Y);
//  FDesignerBox.Canvas.Rectangle(TopLeft.X, TopLeft.Y, X, Y);
end;

procedure TDesignFrame.DockSiteDockOver(Sender: TObject;
  Source: TDragDockObject; X, Y: Integer; State: TDragState; var Accept: Boolean
  );
var
  S: String;
begin
  // Sender = the site that is moved over.
  // Source.control = the control that was dragged to sende.
  // State = indicator of what's going on.
  // Accept = our feedback to the dragmanager if we accept this site to dock onto.
  Accept := true;
  Label1.Caption := Format('Sender: %s', [TControl(Sender).Name]);
  Label2.Caption := Format('Control: %s', [Source.Control.Name]);
  Label3.Caption := Format('X:%d  Y:%d', [X, Y]);
  WriteStr(S, State);
  Label4.Caption := S;
end;

procedure TDesignFrame.DockSiteDockDrop(Sender: TObject;
  Source: TDragDockObject; X, Y: Integer);
var
  NSection, OSection: TEpiSection;
  EpiControl: TEpiCustomControlItem;
begin
  // Sender = the site being dragged onto.
  // Source.control = the control being dragged.

  RemoveFromPositionHandler(
    (TDesignDockObject(Source).FOldDockSite as IPositionHandler),
    Source.Control);

  EpiControl := (Source.Control as IDesignEpiControl).EpiControl;
  EpiControl.BeginUpdate;
  EpiControl.Left := X - Source.DockOffset.X;
  EpiControl.Top  := Y - Source.DockOffset.Y;
  EpiControl.EndUpdate;

  AddToPositionHandler((Sender as IPositionHandler),
    Source.Control);

  // Sanity checks:
  // - sections do not need to be relocated in the Core structure.
  if EpiControl is TEpiSection then exit;

  // - if old and new section is the same do nothing.
  NSection := TEpiSection((Sender as IDesignEpiControl).EpiControl);
  OSection := TEpiSection(EpiControl.Owner.Owner);
  if NSection = OSection then exit;

  // Remove from old parent
  TEpiCustomList(EpiControl.Owner).RemoveItem(EpiControl);
  // Insert into new
  if EpiControl is TEpiField then
    NSection.Fields.AddItem(EpiControl)
  else
    NSection.Headings.AddItem(EpiControl);
end;

procedure TDesignFrame.DockSiteUnDock(Sender: TObject; Client: TControl;
  NewTarget: TWinControl; var Allow: Boolean);
begin
  // Sender = the site that is being undocked from.
  // Client = the control that was dragged to another position.
  // NewTarget = the target we try dock the client onto (can be the same the sender)
  // Allow = our feedback to the dragmanager if we want this undock to happen.

  Allow := false;
  // NewTarget = false: trying to release the client outside of program window.
  if not Assigned(NewTarget) then exit;

  // This effectively prevents dragging section onto section.
  if (NewTarget.ClassType = Client.ClassType) then exit;

  // No sure if this is needed, but better safe than sorry. Basically we prevent
  // anything from docking into other controls that a section and the scrollbox.
  if (NewTarget = FDesignerBox) or
    (NewTarget is TDesignSection) then Allow := true;
end;

procedure TDesignFrame.DesignControlMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  CtrlSender: TControl absolute Sender;
begin
  case Button of
    mbLeft: FLeftMouseDown := CtrlSender.ClientToScreen(Point(X, Y));
    mbRight: FRightMouseDown := CtrlSender.ClientToScreen(Point(X, Y));
  end;

  EnterControl(Sender);
end;

procedure TDesignFrame.DesignControlMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  CtrlSender: TControl absolute Sender;
begin
  case Button of
    mbLeft: FLeftMouseUp := CtrlSender.ClientToScreen(Point(X, Y));
    mbRight: FRightMouseUp := CtrlSender.ClientToScreen(Point(X, Y));
  end;

  // Right button activates popup-menu
  if Button = mbRight then
  begin
    ShowEpiControlPopup(CtrlSender, FRightMouseUp);
    Exit;
  end;
end;

{procedure TDesignFrame.MoveDown(Sender: TControl;
  PositionHandler: IPositionHandler);

  function DoMoveDown: TAVLTreeNode;
  begin
    Result := (Sender as IDesignEpiControl).YTreeNode;
    Result := PositionHandler.YTree.FindSuccessor(Result);
  end;

var
  Node: TAVLTreeNode;
begin
  if not Assigned(Sender) then exit;

  if (Sender is TDesignSection) then
    Node := (Sender as IPositionHandler).YTree.FindLowest
  else
    Node := DoMoveDown;

  if Assigned(Node) then
  begin
    EnterControl(TControl(Node.Data));
    Exit;
  end;

  // Node is nil, find parent.
  if (Sender.Parent is TDesignSection) then
  begin
    Sender := Sender.Parent;
    PositionHandler := (Sender.Parent as IPositionHandler);
    Node := DoMoveDown;
    if Assigned(Node) then
    begin
      EnterControl(TControl(Node.Data));
      Exit;
    end;
  end;
  // We are at the bottom - select top most (lowest in AVL tree) component.
  Node := PositionHandler.YTree.FindLowest;
  EnterControl(TControl(Node.Data));
end;         }

procedure TDesignFrame.EnterControl(Sender: TObject);
begin
  ExitControl(nil);
  FActiveControl := TControl(Sender);
  DeleteControlAction.Enabled := (Sender <> FDesignerBox);
  if (Sender is TWinControl) then
    TWinControl(Sender).SetFocus;
  FActiveControl.Color := $00B6F5F5;
end;

procedure TDesignFrame.ExitControl(Sender: TObject);
begin
  if not Assigned(FActiveControl) then exit;
  FActiveControl.Color := clBtnFace;
  FActiveControl := nil;
end;

constructor TDesignFrame.Create(TheOwner: TComponent; ADataFile: TEpiDataFile);
var
  LocalAdm: TEpiAdmin;
  Grp: TEpiGroup;
begin
  inherited Create(TheOwner);
  FDataFile := ADataFile;
  Name := FDataFile.Id;
  FActiveButton := SelectorToolButton;
  FActiveSection := ADataFile.MainSection;
  DragManager.DragThreshold := 5;
  DragManager.DragImmediate := False;

  // Designer box creation and setup.
  // - (This is subject to change if we find a better component than
  //    the form or a scrollbox).
  FDesignerBox             := TScrollBoxEx.Create(Self);
  FDesignerBox.Name        := 'DesignerBox';
  FDesignerBox.Parent      := Self;
  FDesignerBox.Align       := alClient;
  FDesignerBox.DockSite    := true;
  FDesignerBox.Color       := clWhite;
  FDesignerBox.AutoScroll  := true;
  FDesignerBox.OnMouseUp   := @DockSiteMouseUp;
  FDesignerBox.OnMouseMove := @DockSiteMouseMove;
  FDesignerBox.OnMouseDown := @DockSiteMouseDown;
  FDesignerBox.OnDockDrop  := @DockSiteDockDrop;
  FDesignerBox.OnUnDock    := @DockSiteUnDock;
  FDesignerBox.OnDockOver  := @DockSiteDockOver;
  FDesignerBox.OnKeyDown   := @DesignKeyDown;
  (FDesignerBox as IDesignEpiControl).EpiControl := ADataFile.MainSection;

  FActiveDockSite := FDesignerBox;

  EnterControl(FDesignerBox);

  // DEBUGGING!!!!
  LocalAdm := TEpiDocument(FDataFile.RootOwner).Admin;
  Grp := LocalAdm.NewGroup;
  Grp.Name.Text := 'Group 1';
  Grp.Rights := [earCreate, earRead, earUpdate, earDelete, earVerify,
    earStructure, earTranslate, earUsers, earPassword];
  Grp := LocalAdm.NewGroup;
  Grp.Name.Text := 'Group 2';
  Grp.Rights := [earCreate, earRead, earUpdate, earDelete, earVerify,
    earStructure, earTranslate, earUsers, earPassword];
  Grp := LocalAdm.NewGroup;
  Grp.Name.Text := 'Group 3';
  Grp.Rights := [earCreate, earRead, earUpdate, earDelete, earVerify,
    earStructure, earTranslate, earUsers, earPassword];
end;

end.

