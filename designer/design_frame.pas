unit design_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, FileUtil, LResources, Forms, ComCtrls, Controls,
  ActnList, ExtCtrls, StdCtrls, Menus, epidatafiles, epidatafilestypes,
  design_field, design_heading, design_section, epicustombase,
  design_custombase, AVL_Tree , LCLType;

type

  { TDesignFrame }

  TMouseState = record
    Down: boolean;
    Button: TMouseButton;
    Shift: TShiftState;
  end;

  TDesignFrame = class(TFrame)
    PasteAsQesMenuItem: TMenuItem;
    PasteAsQESAction: TAction;
    DeleteAllControlsAction: TAction;
    NewUpperCaseMenu: TMenuItem;
    PasteAsFloatMenuItem: TMenuItem;
    PasteAsStringMenuItem: TMenuItem;
    PasteAsStringAction: TAction;
    PasteAsFloatAction: TAction;
    PasteAsHeadingMenuItem: TMenuItem;
    PasteAsHeadingAction: TAction;
    DockingSitePopUpMenuDeleteItem: TMenuItem;
    DockingSitePopUpMenuEditItem: TMenuItem;
    DockingSitePopUpMenuDivider1: TMenuItem;
    PasteAsIntAction: TAction;
    DockingSitePopUpMenu: TPopupMenu;
    MoveEndAction: TAction;
    MoveHomeAction: TAction;
    MovePgDnAction: TAction;
    MovePgUpAction: TAction;
    MoveDownAction: TAction;
    MoveUpAction: TAction;
    DateFieldPopupMenu: TPopupMenu;
    AddStructureAction: TAction;
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
    DesignerStatusBar: TStatusBar;
    PasteAsIntMenuItem: TMenuItem;
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
    DesignControlPopUpMenu: TPopupMenu;
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
    DeleteAllToolButton: TToolButton;
    procedure AddStructureActionExecute(Sender: TObject);
    procedure   Button1Click(Sender: TObject);
    procedure   Button2Click(Sender: TObject);
    procedure   Button3Click(Sender: TObject);
    procedure   DeleteAllControlsActionExecute(Sender: TObject);
    procedure   DeleteControlActionExecute(Sender: TObject);
    procedure   EditControlActionExecute(Sender: TObject);
    procedure   FrameResize(Sender: TObject);
    procedure   LoadDataFileActionExecute(Sender: TObject);
    procedure   MoveDownActionExecute(Sender: TObject);
    procedure   MoveEndActionExecute(Sender: TObject);
    procedure   MoveHomeActionExecute(Sender: TObject);
    procedure   MovePgDnActionExecute(Sender: TObject);
    procedure   MovePgUpActionExecute(Sender: TObject);
    procedure   MoveUpActionExecute(Sender: TObject);
    procedure   NewDateFieldMenuClick(Sender: TObject);
    procedure   NewDMYFieldActionExecute(Sender: TObject);
    procedure   NewFloatFieldActionExecute(Sender: TObject);
    procedure   NewHeadingActionExecute(Sender: TObject);
    procedure   NewIntFieldActionExecute(Sender: TObject);
    procedure   NewMDYFieldActionExecute(Sender: TObject);
    procedure   NewOtherFieldMenuClick(Sender: TObject);
    procedure   NewStringFieldActionExecute(Sender: TObject);
    procedure   NewYMDFieldActionExecute(Sender: TObject);
    procedure   PasteAsFloatActionExecute(Sender: TObject);
    procedure   PasteAsHeadingActionExecute(Sender: TObject);
    procedure   PasteAsIntActionExecute(Sender: TObject);
    procedure   PasteAsQESActionExecute(Sender: TObject);
    procedure   PasteAsStringActionExecute(Sender: TObject);
    procedure   TestToolButtonClick(Sender: TObject);
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
    function    DesignControlTop(LocalCtrl: TControl): Integer;
    function    ShowForm(EpiControl: TEpiCustomControlItem;
      Pos: TPoint): TModalResult;
    procedure   ShowEpiControlPopup(Sender: TControl; Pos: TPoint);
  private
    { Import/Export/Paste }
    FLastRecYPos: Integer;
    FLastRecCtrl: TControl;
    procedure   DoPostImportAlignment(ParentControl: TWinControl;
      StartControl, EndControl: TControl);
    procedure   ImportHook(Sender: TObject; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
    procedure   PasteAsField(FieldType: TEpiFieldType);
  private
    { Docksite methods }
    // - mouse
    FMouseState: TMouseState;
    FActiveDockSite: TWinControl;
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
    function    NewShortCutFieldControl(Ft: TEpiFieldType;
      AParent: TWinControl): TControl;
    function    NewShortCutHeadingControl(AParent: TWinControl): TControl;
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
    procedure   DesignKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure   EnterControl(Sender: TObject);
    procedure   ExitControl(Sender: TObject);
    procedure   DeleteControl(Sender: TObject);
    procedure   DeleteAllControls;
  private
    { Position handling }
    procedure   FindNearestControls(ParentControl: TWinControl;
      Control: TControl; var XCtrl: TControl; var YCtrl: TControl);
    function    FindLowestDesignControl(ParentControl: TWinControl;
      var Control: TControl): TPoint;
    function    FindNewPosition(ParentControl: TWinControl;
      AClass: TControlClass): TPoint;
  private
    { Epidata Core Objects }
    FActiveSection: TEpiSection;
    function    NewField(FieldType: TEpiFieldType): TEpiField;
    function    NewHeading: TEpiHeading;
    function    NewSection: TEpiSection;
  private
    { Visual presentation }
    procedure   VisualFeedbackHook(Sender: TObject; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
  private
    FModified: Boolean;
    FOnModified: TNotifyEvent;
    { Property methods }
    procedure   SetDataFile(const AValue: TEpiDataFile);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent);
    procedure   UpdateFrame;
    property    DataFile: TEpiDataFile read FDataFile write SetDataFile;
  end;

implementation

{$R *.lfm}

uses
  Graphics, Clipbrd, epidocument, epiadmin, math, import_form, LMessages,
  main, settings, epiimport, LCLProc, dialogs, epimiscutils, epistringutils,
  managerprocs, epiqeshandler;

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

function TDesignFrame.DesignControlTop(LocalCtrl: TControl): Integer;
begin
  if LocalCtrl.Parent = FDesignerBox then
    exit(LocalCtrl.Top);

  result := LocalCtrl.Top + LocalCtrl.Parent.Top;
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
  // - Page step = PageUp (VK_PRIOR)
  if (Key = VK_PRIOR) then
  begin
    MovePgUpAction.Execute;
    Key := VK_UNKNOWN;
  end;
  // - Top step = Ctrl + Home
  if (Key = VK_HOME) and (ssCtrl in Shift) then
  begin
    MoveHomeAction.Execute;
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
    MovePgDnAction.Execute;
    Key := VK_UNKNOWN;
  end;
  // - Bottom step = Ctrl + End
  if (Key = VK_END) and (ssCtrl in Shift) then
  begin
    MoveEndAction.Execute;
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

procedure TDesignFrame.Button2Click(Sender: TObject);
begin
  Clipboard.AsText :=  WriteTree((FActiveDockSite as IPositionHandler).XTree);
end;

procedure TDesignFrame.Button3Click(Sender: TObject);
begin
  Clipboard.AsText :=  WriteTree((FActiveDockSite as IPositionHandler).YTree);
end;

procedure TDesignFrame.DeleteAllControlsActionExecute(Sender: TObject);
begin
  {$IFNDEF EPI_DEBUG}
  if MessageDlg('Warning', 'Are you sure you want to clear dataform?',
    mtWarning, mbYesNo, 0, mbNo) = mrNo then exit;
  {$ENDIF}

  DeleteAllControls;
end;

procedure TDesignFrame.DeleteControlActionExecute(Sender: TObject);
var
  EpiCtrl: TEpiCustomControlItem;
  LocalCtrl: TControl;
  Node, NNode: TAVLTreeNode;
  YTree: TAVLTree;
begin
  Node := nil;

  {$IFNDEF EPI_DEBUG}
  if not (ssShift in GetKeyShiftState) then
  begin
    if MessageDlg('Warning', 'Are you sure you want to delete?',
      mtWarning, mbYesNo, 0, mbNo) = mrNo then exit;


    if FActiveControl is TDesignSection then
    begin
      YTree := (FActiveControl as IPositionHandler).YTree;
      Node := Ytree.FindLowest;
      while Assigned(Node) do
      begin
        if TControl(Node.Data) is TDesignField then break;
        Node := YTree.FindSuccessor(Node);
      end;
    end else if FActiveControl is TDesignField then
      Node := (FActiveControl as IDesignEpiControl).YTreeNode;

    // Issue warning for last data-containing control will reset size to 0 (zero).
    if (DataFile.Size > 0) and (
         ((DataFile.Fields.Count = 1) and (FActiveControl is TDesignField)) or
         ((FActiveControl is TDesignSection) and
           (TEpiSection(TDesignSection(FActiveControl).EpiControl).Fields.Count = DataFile.Fields.Count))
       ) then
    begin
      if MessageDlg('Warning', 'Last field/section containing data is being deleted.' + LineEnding +
                    'This will reset datafile to 0 (zero) records. Are you sure you want to delete?',
                    mtWarning, mbYesNo, 0, mbNo) = mrNo then
        exit;
    end else
    if (Assigned(Node)) and
       (DataFile.Size > 0) and
       (MessageDlg('Warning', 'Field(s) contains data.' + LineEnding +
        'Are you sure you want to delete?', mtWarning, mbYesNo, 0, mbNo) = mrNo) then
      exit;
  end;  // ssShift!
  {$ENDIF}

  DeleteControl(nil);

  if DataFile.Fields.Count = 0 then
    DataFile.Size := 0;
end;


procedure TDesignFrame.EditControlActionExecute(Sender: TObject);
var
  EpiCtrl: TEpiCustomControlItem;
  Pt: TPoint;
begin
  EpiCtrl := (FActiveControl as IDesignEpiControl).EpiControl;
  Pt := FActiveControl.Parent.ClientToScreen(Point(EpiCtrl.Left, EpiCtrl.Top));
  ShowForm(EpiCtrl, Pt);
end;

procedure TDesignFrame.FrameResize(Sender: TObject);
begin
  With DesignerStatusBar do
    Panels[4].Width := DesignerStatusBar.Width - (Panels[0].Width + Panels[1].Width + Panels[2].Width +
      Panels[3].Width + Panels[5].Width);
end;

procedure TDesignFrame.AddStructureActionExecute(Sender: TObject);
var
  Dlg: TOpenDialog;
  Importer: TEpiImport;
  Ext: String;
  Fn: String;
begin
  Dlg := TOpenDialog.Create(Self);
  Dlg.InitialDir := ManagerSettings.WorkingDirUTF8;
  Dlg.Filter := GetEpiDialogFilter(false, true, false, false, false,
    true, false, true, true, false);
  if not Dlg.Execute then exit;

  DataFile.MainSection.Fields.RegisterOnChangeHook(@ImportHook, false);
  DataFile.MainSection.Headings.RegisterOnChangeHook(@ImportHook, false);

  FLastRecYPos := -1;
  FLastRecCtrl := nil;
  Importer := TEpiImport.Create;
  Fn := Dlg.FileName; //ImportForm.ImportFileEdit.FileName;
  Ext := ExtractFileExt(UTF8LowerCase(Fn));

  if ext = '.rec' then
    Importer.ImportRec(Fn, FDataFile, false)
  else if ext = '.dta' then
    Importer.ImportStata(Fn, FDataFile, false)
  else if ext = '.qes' then
    Importer.ImportQES(Fn, FDataFile, nil);

  Importer.Free;

  DataFile.MainSection.Fields.UnRegisterOnChangeHook(@ImportHook);
  DataFile.MainSection.Headings.UnRegisterOnChangeHook(@ImportHook);
  Dlg.Free;
end;

procedure TDesignFrame.LoadDataFileActionExecute(Sender: TObject);
var
  ImportForm: TImportForm;
  Importer: TEpiImport;
  Fn: String;
  Ext: String;
  Dlg: TOpenDialog;
begin
  {$IFNDEF EPI_DEBUG}
  if (DataFile.Size > 0) or
     (DataFile.Fields.Count > 0) or
     (DataFile.Headings.Count > 0) or
     (DataFile.Sections.Count > 1) then
  begin
    if MessageDlg('Warning',
      'Dataform is not empty. Loading a data file will clear dataform.' + LineEnding +
      'Are you sure you want to proceed?', mtWarning, mbYesNo, 0, mbNo) <> mrYes then exit;
  end;
  {$ENDIF}

{  ImportForm := TImportForm.Create(Self);
  ImportForm.DataFile := FDataFile;

  if ImportForm.ShowModal <> mrOK then exit;  }

  Dlg := TOpenDialog.Create(Self);
  Dlg.InitialDir := ManagerSettings.WorkingDirUTF8;
  Dlg.Filter := GetEpiDialogFilter(false, true, false, false, false,
    true, false, true, true, false);
  if not Dlg.Execute then exit;

  DeleteAllControls;

  NewIntFieldAction.ShortCut := ShortCut(VK_V, [ssCtrl]);

  FDataFile.MainSection.Fields.RegisterOnChangeHook(@ImportHook, true);
  FDataFile.MainSection.Headings.RegisterOnChangeHook(@ImportHook, true);

  FLastRecYPos := -1;
  FLastRecCtrl := nil;
  Importer := TEpiImport.Create;
  Fn := Dlg.FileName; //ImportForm.ImportFileEdit.FileName;
  Ext := ExtractFileExt(UTF8LowerCase(Fn));

  if ext = '.rec' then
    Importer.ImportRec(Fn, FDataFile, true)
  else if ext = '.dta' then
    Importer.ImportStata(Fn, FDataFile, true)
  else if ext = '.qes' then
    Importer.ImportQES(Fn, FDataFile, nil);
  Importer.Free;

  FDataFile.MainSection.Fields.UnRegisterOnChangeHook(@ImportHook);
  FDataFile.MainSection.Headings.UnRegisterOnChangeHook(@ImportHook);

  Dlg.Free;
//  ImportForm.Free;
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

procedure TDesignFrame.MoveEndActionExecute(Sender: TObject);
var
  TheComponent: TControl;
  TheParent: TWinControl;
  YTree: TAVLTree;
  Node: TAVLTreeNode;
begin
  if not Assigned(FActiveControl) then exit;
  if FActiveControl = FDesignerBox then
  begin
    if (FActiveControl as IPositionHandler).YTree.Count = 0 then exit;
    Node := (FActiveControl as IPositionHandler).YTree.FindHighest;
    TheComponent := TControl(Node.Data);
  end else
    TheComponent := FActiveControl;
  TheParent := TheComponent.Parent;
  YTree := (TheParent as IPositionHandler).YTree;
  Node := (TheComponent as IDesignEpiControl).YTreeNode;

  if Node = YTree.FindHighest then
  begin
    if TheParent = FDesignerBox then exit;

    TheComponent := TheParent;
    TheParent := TheComponent.Parent;
    YTree := (TheParent as IPositionHandler).YTree;
  end;

  Node := YTree.FindHighest;
  if Assigned(Node) then
    EnterControl(TControl(Node.Data));
end;

procedure TDesignFrame.MoveHomeActionExecute(Sender: TObject);
var
  TheComponent: TControl;
  TheParent: TWinControl;
  YTree: TAVLTree;
  Node: TAVLTreeNode;
begin
  if not Assigned(FActiveControl) then exit;
  if FActiveControl = FDesignerBox then
  begin
    if (FActiveControl as IPositionHandler).YTree.Count = 0 then exit;
    Node := (FActiveControl as IPositionHandler).YTree.FindLowest;
    TheComponent := TControl(Node.Data);
  end else
    TheComponent := FActiveControl;
  TheParent := TheComponent.Parent;
  YTree := (TheParent as IPositionHandler).YTree;
  Node := (TheComponent as IDesignEpiControl).YTreeNode;

  if Node = YTree.FindLowest then
  begin
    if TheParent = FDesignerBox then exit;

    TheComponent := TheParent;
    TheParent := TheComponent.Parent;
    YTree := (TheParent as IPositionHandler).YTree;
  end;

  Node := YTree.FindLowest;
  if Assigned(Node) then
    EnterControl(TControl(Node.Data));
end;

procedure TDesignFrame.MovePgDnActionExecute(Sender: TObject);
var
  Node: TAVLTreeNode;
  Dx: Integer;
  YTree: TAVLTree;
  TheParent: TWinControl;
  TheComponent: TControl;
begin
  if not Assigned(FActiveControl) then exit;
  if FActiveControl = FDesignerBox then
  begin
    if (FActiveControl as IPositionHandler).YTree.Count = 0 then exit;
    Node := (FActiveControl as IPositionHandler).YTree.FindLowest;
    TheComponent := TControl(Node.Data);
  end else
    TheComponent := FActiveControl;
  TheParent := TheComponent.Parent;
  YTree := (TheParent as IPositionHandler).YTree;
  Node := (TheComponent as IDesignEpiControl).YTreeNode;

  if Node = YTree.FindHighest then
  begin
    // Lowest component in the wincontrol, move out if we are in a section, do nothing
    // if we are at the bottom of the screen (on designer box).
    if TheParent = FDesignerBox then exit;

    TheComponent := TheParent;
    TheParent := TheComponent.Parent;
    YTree := (TheParent as IPositionHandler).YTree;
    Node := (TheComponent as IDesignEpiControl).YTreeNode;
  end;

  Dx := 0;
  Node := YTree.FindSuccessor(Node);
  // This will only occur if we are at the very bottom of the designerbox.
  if not Assigned(Node) then exit;

  if (DesignControlTop(TControl(Node.Data)) + TControl(Node.Data).Height) > (FDesignerBox.VertScrollBar.Position + FDesignerBox.VertScrollBar.Page) then
    Dx := FDesignerBox.ClientHeight;
  while (Assigned(Node)) and
    ((DesignControlTop(TControl(Node.Data)) + TControl(Node.Data).Height) < (FDesignerBox.VertScrollBar.Position + FDesignerBox.VertScrollBar.Page + Dx)) do
    Node := YTree.FindSuccessor(Node);
  if Assigned(Node) then
    Node := YTree.FindPrecessor(Node)
  else
    Node := YTree.FindHighest;

  EnterControl(TControl(Node.Data));
end;

procedure TDesignFrame.MovePgUpActionExecute(Sender: TObject);
var
  Node: TAVLTreeNode;
  Dx: Integer;
  YTree: TAVLTree;
  TheParent: TWinControl;
  TheComponent: TControl;
begin
  if not Assigned(FActiveControl) then exit;
  if FActiveControl = FDesignerBox then
  begin
    if (FActiveControl as IPositionHandler).YTree.Count = 0 then exit;
    Node := (FActiveControl as IPositionHandler).YTree.FindHighest;
    TheComponent := TControl(Node.Data);
  end else
    TheComponent := FActiveControl;
  TheParent := TheComponent.Parent;
  YTree := (TheParent as IPositionHandler).YTree;
  Node := (TheComponent as IDesignEpiControl).YTreeNode;

  if Node = YTree.FindLowest then
  begin
    // Top most component the section, move out if we are in a section, do nothing
    // if we are at the top of the screen (on designer box).
    if TheParent = FDesignerBox then exit;

    TheComponent := TheParent;
    TheParent := TheComponent.Parent;
    YTree := (TheParent as IPositionHandler).YTree;
    Node := (TheComponent as IDesignEpiControl).YTreeNode;
  end;

  Dx := 0;
  Node := YTree.FindPrecessor(Node);
  // This will only occur if we are at the very top of the designerbox.
  if not Assigned(Node) then exit;

  if DesignControlTop(TControl(Node.Data)) < (FDesignerBox.VertScrollBar.Position) then
    Dx := FDesignerBox.ClientHeight;
  while (Assigned(Node)) and (DesignControlTop(TControl(Node.Data)) > (FDesignerBox.VertScrollBar.Position  - Dx)) do
    Node := YTree.FindPrecessor(Node);
  if Assigned(Node) then
    Node := YTree.FindSuccessor(Node)
  else
    Node := YTree.FindLowest;

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

procedure TDesignFrame.NewDMYFieldActionExecute(Sender: TObject);
begin
  NewShortCutFieldControl(ftDMYDate, FActiveDockSite);
end;

procedure TDesignFrame.NewFloatFieldActionExecute(Sender: TObject);
begin
  NewShortCutFieldControl(ftFloat, FActiveDockSite);
end;

procedure TDesignFrame.NewHeadingActionExecute(Sender: TObject);
begin
  NewShortCutHeadingControl(FActiveDockSite);
end;

procedure TDesignFrame.NewIntFieldActionExecute(Sender: TObject);
begin
  NewShortCutFieldControl(ftInteger, FActiveDockSite);
end;

procedure TDesignFrame.NewMDYFieldActionExecute(Sender: TObject);
begin
  NewShortCutFieldControl(ftMDYDate, FActiveDockSite);
end;


procedure TDesignFrame.NewOtherFieldMenuClick(Sender: TObject);
begin
  if not (Sender is TMenuItem) then exit;

  OtherToolButton.Tag := TMenuItem(Sender).Tag;
  OtherToolButton.ImageIndex := TMenuItem(Sender).ImageIndex;
  ToggleToolBtn(OtherToolButton);
end;

procedure TDesignFrame.NewStringFieldActionExecute(Sender: TObject);
begin
  NewShortCutFieldControl(ftString, FActiveDockSite);
end;

procedure TDesignFrame.NewYMDFieldActionExecute(Sender: TObject);
begin
  NewShortCutFieldControl(ftYMDDate, FActiveDockSite);
end;

procedure TDesignFrame.PasteAsFloatActionExecute(Sender: TObject);
begin
  PasteAsField(ftFloat);
end;

procedure TDesignFrame.PasteAsHeadingActionExecute(Sender: TObject);
var
  Cbl: TStringList;
  i: Integer;
  Pt: TPoint;
  H: TEpiHeading;
begin
  Cbl := TStringList.Create;
  try
    ReadClipBoard(Cbl);

    for i := 0 to Cbl.Count - 1 do
    begin
      if Trim(Cbl[i]) = '' then continue;

      Pt := FindNewPosition(FActiveDockSite, TDesignHeading);
      H := NewHeading;
      H.Caption.Text := Trim(Cbl[i]);
      NewDesignControl(TDesignHeading, FActiveDockSite, Pt, H);
    end;
  finally
    Cbl.Free;
  end;
end;

procedure TDesignFrame.PasteAsIntActionExecute(Sender: TObject);
begin
  PasteAsField(ftInteger);
end;

procedure TDesignFrame.PasteAsQESActionExecute(Sender: TObject);
var
  QH: TQesHandler;
  Cbl: TStringList;
begin
  Cbl := TStringList.Create;
  try
    ReadClipBoard(Cbl);

    FLastRecYPos := -1;
    FLastRecCtrl := nil;

    FActiveSection.Headings.RegisterOnChangeHook(@ImportHook);
    FActiveSection.Fields.RegisterOnChangeHook(@ImportHook);

    QH := TQesHandler.Create;
    QH.FieldPrefix := ManagerSettings.FieldNamePrefix;
    QH.QesToDatafile(Cbl, FDataFile, FActiveSection);
    QH.Free;

    FActiveSection.Headings.UnRegisterOnChangeHook(@ImportHook);
    FActiveSection.Fields.UnRegisterOnChangeHook(@ImportHook);
  finally
    Cbl.Free;
  end;
end;

procedure TDesignFrame.PasteAsStringActionExecute(Sender: TObject);
begin
  PasteAsField(ftString);
end;

procedure TDesignFrame.TestToolButtonClick(Sender: TObject);
begin
  MainForm.FlipChildren(true);
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
  Form.Left := Min(Pos.X, Screen.Width - Form.Width - 5);
  Form.Top := Min(Pos.Y, Screen.Height - Form.Height - 5);
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

{  // Forces designer box last in docksite list, because retrieving docksite
  // is not implemented fully in the LCL.
  FDesignerBox.DockSite := false;
  FDesignerBox.DockSite := true;      }

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
  if Supports(Sender, IPositionHandler) then
    DockingSitePopUpMenu.PopUp(Pos.X, Pos.Y)
  else
    DesignControlPopUpMenu.PopUp(Pos.X, Pos.Y);
end;

procedure TDesignFrame.DoPostImportAlignment(ParentControl: TWinControl; StartControl,
  EndControl: TControl);
var
  YTree: TAVLTree;
  MinLeft: Integer;
  MaxQuestionWidth: Integer;
  MaxNameWidth: Integer;
  NewLeft: Integer;
  Curr: TAVLTreeNode;
  LastTop: Integer;
begin
{  YTree := (ParentControl as IPositionHandler).YTree;

  if not Assigned(StartControl) then
    StartControl := TControl(YTree.FindLowest.Data);

  if not Assigned(EndControl) then
    EndControl := TControl(YTree.FindHighest.Data);

  // Information collection pass.
  MaxQuestionWidth := 0;
  MaxNameWidth     := 0;
  LastTop          := -1;
  MinLeft          := MaxInt;

  Curr := (StartControl as IDesignEpiControl).YTreeNode;
  while Assigned(Curr) do
  begin
    if LastTop <> TControl(Curr.Data).Top then
    begin
      if (TControl(Curr.Data) is TDesignField) then
      with TDesignField(Curr.Data) do
      begin
        MaxQuestionWidth := Max(MaxQuestionWidth, QuestionLabel.Width);
        MaxNameWidth     := Max(MaxNameWidth,     NameLabel.Width);
      end;

      MinLeft := Min(MinLeft, (TControl(Curr.Data).Left));
    end;
    LastTop := TControl(Curr.Data).Top;
    if (TControl(Curr.Data) = EndControl) then
      Break;
    Curr := YTree.FindSuccessor(Curr);
  end;

  NewLeft := MinLeft - (MaxQuestionWidth + 5) -
                       (MaxNameWidth + 5);

  if NewLeft < (MaxNameWidth + 5) then
  begin
    NewLeft := MaxNameWidth + 5;
    MinLeft := MaxQuestionWidth + 5 + MaxNameWidth + 5;
  end;

  Curr := (StartControl as IDesignEpiControl).YTreeNode;
  while Assigned(Curr) do
  with (TControl(Curr.Data) as IDesignEpiControl) do
  begin
    if EpiControl is TEpiField then
      EpiControl.Left := MinLeft
    else
      EpiControl.Left := NewLeft;

    if (TControl(Curr.Data) = EndControl) then
      Break;
    Curr := YTree.FindSuccessor(Curr);
  end;   }
end;

procedure TDesignFrame.ImportHook(Sender: TObject; EventGroup: TEpiEventGroup;
  EventType: Word; Data: Pointer);
var
  Cls: TControlClass;
  Pt: TPoint;
begin
  if (Sender is TEpiFields) then
  begin
    if (EventGroup = eegCustomBase) and (EventType = Ord(ecceAddItem)) then
      TEpiField(Data).RegisterOnChangeHook(@ImportHook, false);
    exit;
  end;

  if (Sender is TEpiHeadings) then
  begin
    if (EventGroup = eegCustomBase) and (EventType = Ord(ecceAddItem)) then
      TEpiHeading(Data).RegisterOnChangeHook(@ImportHook, false);
    exit;
  end;

  if (Sender is TEpiCustomControlItem) and (EventGroup = eegCustomBase) and (EventType = Ord(ecceUpdate)) then
  begin
    TEpiCustomControlItem(Sender).UnRegisterOnChangeHook(@ImportHook);

    if (Sender is TEpiField) then
    with TEpiField(Sender) do
    begin
      if Name = '' then
        Name := ManagerSettings.FieldNamePrefix + IntToStr(DataFile.Fields.Count);
    end;

    Cls := TDesignField;
    if Sender is TEpiHeading then
      Cls := TDesignHeading;
    Pt := FindNewPosition(FDesignerBox, Cls);

    if (not (FLastRecYPos = -1)) and (FLastRecYPos = TEpiCustomControlItem(Sender).Top) then
    begin
      Pt.Y := FLastRecCtrl.Top;
      Pt.X := FLastRecCtrl.Left + FLastRecCtrl.Width + 10;
    end;

    FLastRecYPos := TEpiCustomControlItem(Sender).Top;
    FLastRecCtrl := NewDesignControl(Cls, FDesignerBox, Pt, TEpiCustomControlItem(Sender));
  end;
end;

procedure TDesignFrame.PasteAsField(FieldType: TEpiFieldType);
var
  Cbl: TStringList;
  i: Integer;
  Pt: TPoint;
  TmpField: TEpiField;
begin
  Cbl := TStringList.Create;
  try
    ReadClipBoard(Cbl);

    for i := 0 to Cbl.Count - 1 do
    begin
      if Trim(Cbl[i]) = '' then continue;

      Pt := FindNewPosition(FActiveDockSite, TDesignField);
      TmpField := NewField(FieldType);
      with TmpField do
      begin
        Name := FirstWord(Cbl[i]);
{        case ManagerSettings.FieldNamingStyle of
          fnFirstWord: FieldName := FirstWord(Cbl[i]);
          fnAuto:      FieldName := AutoFieldName(Cbl[i]);
        end;                       }
//        FieldName := DataFile.CreateUniqueFieldName(FieldName);
        Question.Caption.Text := Trim(Cbl[i]);
      end;
      NewDesignControl(TDesignField, FActiveDockSite, Pt, TmpField);
    end;
  finally
    Cbl.Free;
  end;
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
var
  Control: TControl;
  Dist: LongInt;
begin
  result := FindLowestDesignControl(ParentControl, Control);
  if not Assigned(Control) then exit;

  Dist := ManagerSettings.SpaceBtwFieldLabel;
  if (AClass = TDesignField) and (Control is TDesignField) then
    Dist := ManagerSettings.SpaceBtwFieldField;
  if (AClass = TDesignHeading) and (Control is TDesignHeading) then
    Dist := ManagerSettings.SpaceBtwLabelLabel;
  Result.Y += (Control.Height + Dist);
end;

function TDesignFrame.NewField(FieldType: TEpiFieldType): TEpiField;
begin
  result := FActiveSection.NewField(FieldType);

  with result do
  begin
    Name := ManagerSettings.FieldNamePrefix + IntToStr(DataFile.Fields.Count);
    Case FieldType of
      ftBoolean:
        Length := 1;
      ftInteger, ftAutoInc:
        Length := ManagerSettings.IntFieldLength;
      ftFloat:
        begin
          Length := ManagerSettings.FloatIntLength;
          Decimals := ManagerSettings.FloatDecimalLength;
        end;
      ftDMYDate, ftDMYToday,
      ftMDYDate, ftMDYToday,
      ftYMDDate, ftYMDToday:
        Length := 10;
      ftTime, ftTimeNow:
        Length := 8;
      ftString, ftUpperString:
        Length := ManagerSettings.StringFieldLength;
    end;
  end;
end;

function TDesignFrame.NewHeading: TEpiHeading;
begin
  result := FActiveSection.NewHeading;
  result.Caption.Text := '(Untitled)';
end;

function TDesignFrame.NewSection: TEpiSection;
begin
  result := DataFile.NewSection;
end;

procedure TDesignFrame.VisualFeedbackHook(Sender: TObject;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
var
  SdrList: TEpiCustomList absolute Sender;
begin
  if TEpiCustomChangeEventType(EventType) in [ecceAddItem, ecceDelItem, ecceUpdate] then
  begin
    if Sender is TEpiSections then
      DesignerStatusBar.Panels.Items[0].Text := 'Sections: ' + IntToStr(SdrList.Count - 1);

    if Sender is TEpiFields then
      DesignerStatusBar.Panels.Items[1].Text := 'Fields: ' + IntToStr(SdrList.Count);

    if Sender is TEpiHeadings then
      DesignerStatusBar.Panels.Items[2].Text := 'Headings: ' + IntToStr(SdrList.Count);
  end;
  if (Sender is TEpiDataFile) and (not (ebsDestroying in TEpiDataFile(Sender).State)) then
    DesignerStatusBar.Panels[3].Text := Format('Records: %d', [DataFile.Size]);
end;

procedure TDesignFrame.SetDataFile(const AValue: TEpiDataFile);
var
  i: Integer;
  TheParent: TWinControl;
  j: Integer;
begin
  if DataFile = AValue then exit;
  FDataFile := AValue;

  Name := DataFile.Id;
  FActiveSection := DataFile.MainSection;
  (FDesignerBox as IDesignEpiControl).EpiControl := DataFile.MainSection;

  // Register the visual feedback hook.
  DataFile.BeginUpdate;
  DataFile.RegisterOnChangeHook(@VisualFeedbackHook);
  DataFile.Sections.RegisterOnChangeHook(@VisualFeedbackHook);
  DataFile.Fields.RegisterOnChangeHook(@VisualFeedbackHook);
  DataFile.Headings.RegisterOnChangeHook(@VisualFeedbackHook);

  with DataFile do
  begin
    if not ((Fields.Count = 0) and (Headings.Count = 0)) then
    begin
      for i := 0 to Sections.Count - 1 do
      begin
        if Section[i] <> MainSection then
          TheParent := TWinControl(NewSectionControl(Point(Section[i].Left, Section[i].Top),
            Point(Section[i].Left+Section[i].Width, Section[i].Top+Section[i].Height), Section[i]))
        else
          TheParent := FDesignerBox;

        with Section[i] do
        begin
          for j := 0 to Fields.Count - 1 do
            NewDesignControl(TDesignField, TheParent, Point(Field[j].Left, Field[j].Top), Field[j]);
          for j := 0 to Headings.Count - 1 do
            NewDesignControl(TDesignHeading, TheParent, Point(Heading[j].Left, Heading[j].Top), Heading[j]);
        end;
      end;
    end;
  end;
  DataFile.EndUpdate;
end;

procedure TDesignFrame.DockSiteMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  WinSender: TWinControl absolute Sender;
begin
  case Button of
    mbLeft: FLeftMouseDown := WinSender.ClientToScreen(Point(X, Y));
    mbRight: FRightMouseDown := WinSender.ClientToScreen(Point(X, Y));
  end;

  FMouseState.Down := true;
  FMouseState.Button := Button;
  FMouseState.Shift := Shift;

  EnterControl(Sender);

  if FActiveButton.Index <> 11 then exit;
  // For drawing the area where the new section is going to be.
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

  DesignerStatusBar.Panels[5].Text := Format('Mouse - X: %d Y: %d',[X, Y]);

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


  // TODO : Draw lines when close to a snapping component.
end;

procedure TDesignFrame.DockSiteDockDrop(Sender: TObject;
  Source: TDragDockObject; X, Y: Integer);
var
  NSection, OSection: TEpiSection;
  EpiControl: TEpiCustomControlItem;
  XCtrl, YCtrl: TControl;
  Dx: Integer;
  Dy: Integer;
begin
  // Sender = the site being dragged onto.
  // Source.control = the control being dragged.

  RemoveFromPositionHandler(
    (TDesignDockObject(Source).FOldDockSite as IPositionHandler),
    Source.Control);
  EpiControl := (Source.Control as IDesignEpiControl).EpiControl;

  // X,Y is the mouse position, not the control top/left - so use DockOffset to
  // get the right coordinates.
  // Insert new coordinates already now, they are needed in the FindNearestControls
  // algorithm (lookup in AVL tree is based on the EpiControl values). But defer the
  // update to we know the exact position!
  EpiControl.BeginUpdate;
  EpiControl.Left := X - Source.DockOffset.X;
  EpiControl.Top  := Y - Source.DockOffset.Y;

  // Snapping:
  if (not (ssShift in GetKeyShiftState)) and
     (ManagerSettings.SnapFields)        and
     ((Sender as IPositionHandler).YTree.Count > 0) then
  with Source do
  begin
    FindNearestControls(TWinControl(Sender), Control, XCtrl, YCtrl);
    Dx := EpiControl.Left - XCtrl.Left;
    // Snapping distance according to bottoms.
    Dy := Abs(EpiControl.Top - YCtrl.Top) - Abs(YCtrl.Height - Control.Height);

    if Abs(Dx) <= ManagerSettings.SnappingThresHold then
      EpiControl.Left := XCtrl.Left;
    // Align bottoms.
    if Abs(Dy) <= ManagerSettings.SnappingThresHold then
      EpiControl.Top := YCtrl.Top + (YCtrl.Height - Control.Height);
  end;
  EpiControl.EndUpdate;

  AddToPositionHandler((Sender as IPositionHandler),
    Source.Control);

  // Sanity checks:
  // - sections do not need to be relocated in the Core structure.
  if EpiControl is TEpiSection then  exit;

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

function TDesignFrame.NewShortCutFieldControl(Ft: TEpiFieldType;
  AParent: TWinControl): TControl;
var
  Pt: TPoint;
  Field: TEpiField;
begin
  Field := NewField(Ft);
  Pt := FindNewPosition(AParent, TDesignField);
  if (not (ssShift in GetKeyShiftState)) and (ShowForm(Field, AParent.ClientToScreen(Pt)) <> mrOk) then
  begin
    Field.Free;
    Exit;
  end;
  NewDesignControl(TDesignField, AParent, Pt, Field);
end;

function TDesignFrame.NewShortCutHeadingControl(AParent: TWinControl): TControl;
var
  Pt: TPoint;
  Heading: TEpiHeading;
begin
  Heading := NewHeading;
  Pt := FindNewPosition(AParent, TDesignHeading);
  if (not (ssShift in GetKeyShiftState)) and (ShowForm(Heading, AParent.ClientToScreen(Pt)) <> mrOk) then
  begin
    Heading.Free;
    Exit;
  end;
  NewDesignControl(TDesignHeading, AParent, Pt, Heading);
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

procedure TDesignFrame.EnterControl(Sender: TObject);
var
  S: String;
  EpiControl: TEpiCustomControlItem;
begin
  ExitControl(nil);
  if Not Assigned(Sender) then
    Sender := FDesignerBox;

  FActiveControl := TControl(Sender);
  DeleteControlAction.Enabled := (Sender <> FDesignerBox);
  if (Sender is TWinControl) then
    TWinControl(Sender).SetFocus;

  EpiControl := (FActiveControl as IDesignEpiControl).EpiControl;

  if EpiControl is TEpiSection then
    FActiveSection := TEpiSection(EpiControl)
  else
    FActiveSection := TEpiSection(EpiControl.Owner.Owner);

  if Supports(Sender, IPositionHandler) then
    FActiveDockSite := TWinControl(Sender)
  else
    FActiveDockSite := TControl(Sender).Parent;

  S := '';
  if Sender is TDesignField then
  with TEpiField(EpiControl) do begin
    if FieldType in BoolFieldTypes then
      S := 'F(b): ';
    if FieldType in IntFieldTypes then
      S := 'F(i): ';
    if FieldType in FloatFieldTypes then
      S := 'F(f): ';
    if FieldType in DateFieldTypes then
      S := 'F(d): ';
    if FieldType in TimeFieldTypes then
      S := 'F(t): ';
    if FieldType in StringFieldTypes then
      S := 'F(s): ';
    S += Name;
    if Question.Caption.Text <> '' then
      S += '(' + Question.Caption.Text + ')';
  end;
  if Sender is TDesignHeading then
    S := 'H: ' + TEpiHeading(EpiControl).Caption.Text;
  if Sender is TDesignSection then
    S := 'S: ' + TEpiSection(EpiControl).Name.Text;
  DesignerStatusBar.Panels[4].Text := S;

  if DesignControlTop(FActiveControl) < FDesignerBox.VertScrollBar.Position then
    FDesignerBox.VertScrollBar.Position := DesignControlTop(FActiveControl) - 5;

  if DesignControlTop(FActiveControl) > (FDesignerBox.VertScrollBar.Position + FDesignerBox.VertScrollBar.Page) then
    FDesignerBox.VertScrollBar.Position := DesignControlTop(FActiveControl) - FDesignerBox.VertScrollBar.Page + FActiveControl.Height + 5;
  FActiveControl.Color := $00B6F5F5;
end;

procedure TDesignFrame.ExitControl(Sender: TObject);
begin
  if not Assigned(FActiveControl) then exit;
  FActiveControl.Color := clWhite;
  if FActiveControl is TDesignHeading then
    TDesignHeading(FActiveControl).ParentColor := true;
  FActiveControl := nil;

  DesignerStatusBar.Panels[4].Text := '';
end;

procedure TDesignFrame.DeleteControl(Sender: TObject);
var
  EpiCtrl: TEpiCustomControlItem;
  LocalCtrl: TControl;
  Node, NNode: TAVLTreeNode;
  YTree: TAVLTree;
begin
  LocalCtrl := FActiveControl;
  EpiCtrl := (LocalCtrl as IDesignEpiControl).EpiControl;
  Node    := (LocalCtrl as IDesignEpiControl).YTreeNode;
  YTree   := (LocalCtrl.Parent as IPositionHandler).YTree;
  ExitControl(nil);

  NNode := YTree.FindSuccessor(Node);
  if Assigned(NNode) then
    EnterControl(TControl(NNode.Data))
  else
    NNode := YTree.FindPrecessor(Node);
  if Assigned(NNode) then
    EnterControl(TControl(NNode.Data))
  else
    EnterControl(nil);

  RemoveFromPositionHandler(LocalCtrl.Parent as IPositionHandler, LocalCtrl);
  EpiCtrl.Free;  // This also removes the epicontrol from it's parent/list.
  LocalCtrl.Free;
end;

procedure TDesignFrame.DeleteAllControls;
var
  YTree: TAVLTree;
  Node: TAVLTreeNode;
  LocalCtrl: TControl;
  EpiCtrl: TEpiCustomControlItem;
begin
  YTree := TScrollBoxEx(FDesignerBox).YTree;
  Node  := YTree.FindLowest;

  FDesignerBox.BeginUpdateBounds;
  ExitControl(nil);
  while Assigned(Node) do
  begin
    LocalCtrl := TControl(Node.Data);
    EpiCtrl := (LocalCtrl as IDesignEpiControl).EpiControl;

    EpiCtrl.Free;
    LocalCtrl.Free;
    Node := YTree.FindSuccessor(Node);
  end;
  YTree.Clear;
  TScrollBoxEx(FDesignerBox).XTree.Clear;

  EnterControl(FDesignerBox);

  DataFile.Size := 0;
  FDesignerBox.EndUpdateBounds;
end;

procedure TDesignFrame.FindNearestControls(ParentControl: TWinControl;
  Control: TControl; var XCtrl: TControl; var YCtrl: TControl);
var
  Hit, Prd, Scc: TAVLTreeNode;
  HDy, PDy, SDy: Integer;
  YTree, XTree: TAVLTree;
  EpiCtrl: TEpiCustomControlItem;
begin
  YTree := (ParentControl as IPositionHandler).YTree;
  XTree := (ParentControl as IPositionHandler).XTree;
  EpiCtrl := (Control as IDesignEpiControl).EpiControl;

  // Finding the nearest it either an exact hit,
  // a little above or below - hence we need to look
  // both ways.
  Hit := YTree.FindNearest(Control);
  Prd := YTree.FindPrecessor(Hit);
  Scc := YTree.FindSuccessor(Hit);
  PDY := MaxInt;
  SDy := MaxInt;

  HDy := Abs(TControl(Hit.Data).Top - EpiCtrl.Top);
  if Assigned(Prd) then
    PDy := Abs(TControl(Prd.Data).Top - EpiCtrl.Top);
  if Assigned(Scc) then
    SDy := Abs(TControl(Scc.Data).Top - EpiCtrl.Top);
  if PDy < HDy then
  begin
    Hit := Prd;
    if SDy < PDy then
      Hit := Scc;
  end else
    if SDY < HDy then
      Hit := Scc;
  YCtrl := TControl(Hit.Data);

  // Same story for finding X component.
  Hit := XTree.FindNearest(Control);
  Prd := XTree.FindPrecessor(Hit);
  Scc := XTree.FindSuccessor(Hit);
  PDY := MaxInt;
  SDy := MaxInt;

  HDy := Abs(TControl(Hit.Data).Left - EpiCtrl.Left);
  if Assigned(Prd) then
    PDy := Abs(TControl(Prd.Data).Left - EpiCtrl.Left);
  if Assigned(Scc) then
    SDy := Abs(TControl(Scc.Data).Left - EpiCtrl.Left);
  if PDy < HDy then
  begin
    Hit := Prd;
    if SDy < PDy then
      Hit := Scc;
  end else
    if SDY < HDy then
      Hit := Scc;
  XCtrl := TControl(Hit.Data);
end;

function TDesignFrame.FindLowestDesignControl(ParentControl: TWinControl;
  var Control: TControl): TPoint;
var
  YTree: TAVLTree;
  Hit: TAVLTreeNode;
  Prd: TAVLTreeNode;
begin
  // Initialization
  Result := Point(ManagerSettings.DefaultRightPostion, 5);
  Control := nil;

  // Look in the tree for lowest component (highest value in tree).
  YTree := (ParentControl as IPositionHandler).YTree;
  if YTree.Count = 0 then exit;

  Hit := YTree.FindHighest;
  Control := TControl(Hit.Data);

  Prd := YTree.FindPrecessor(Hit);
  // If the pred. has same top value, then by sorting ord it must have a
  // smaller left value.
  while Assigned(Prd) do
  begin
    if (TControl(Prd.Data).Top = Control.Top) then
    begin
      Control := TControl(Prd.Data);
      Prd := YTree.FindPrecessor(Prd);
    end else
      Prd := nil;
  end;
  Result := Point(Control.Left, Control.Top);
end;

constructor TDesignFrame.Create(TheOwner: TComponent);
var
  LocalAdm: TEpiAdmin;
  Grp: TEpiGroup;
  TmpEpiSection: TEpiSection;
  TmpCtrlSection: TWinControl;
  TmpHeading: TEpiHeading;
  Heading: TEpiHeading;
  i: Integer;
  Pt: TPoint;
begin
  inherited Create(TheOwner);

  // ==================================
  // Essetial things are created first!
  // ==================================
  DateToolButton.Tag := Ord(ManagerSettings.DefaultDateType);
  DateToolButton.ImageIndex := Ord(ManagerSettings.DefaultDateType);
  FActiveButton := SelectorToolButton;

  // TODO : Move to main - the DragManager is global to the whole program and
  // should only be set once.
  DragManager.DragThreshold := 5;
  DragManager.DragImmediate := False;

  // ==================================
  // Designer box creation and setup.
  // - (This is subject to change if we find a better component than
  //    the form or a scrollbox).
  // ==================================
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
  FActiveDockSite := FDesignerBox;

  // ========================
  // Statusbar setup
  // ========================
  with DesignerStatusBar do
  begin
    BeginUpdate;
    Panels[0].Width := {$IFDEF WINDOWS}  70 {$ELSE}  90 {$ENDIF};  // Sections
    Panels[1].Width := {$IFDEF WINDOWS}  70 {$ELSE}  80 {$ENDIF};  // Fields
    Panels[2].Width := {$IFDEF WINDOWS}  90 {$ELSE} 105 {$ENDIF};  // Headings
    Panels[3].Width := {$IFDEF WINDOWS} 100 {$ELSE} 110 {$ENDIF};  // Records
    // Width for name is set on resize.
    Panels[5].Width := {$IFDEF WINDOWS} 130 {$ELSE} 150 {$ENDIF};  // Mouse
    Panels[5].Alignment := taRightJustify;
    EndUpdate;
  end;
  {$IFNDEF EPI_DEBUG}
  Splitter1.Enabled := false;
  Splitter1.Visible := False;
  Panel1.Enabled := false;
  Panel1.Visible := false;
  TestToolButton.Enabled := false;
  TestToolButton.Visible := false;
  {$ENDIF}
end;

procedure TDesignFrame.UpdateFrame;

  procedure Recurse(YTree: TAVLTree);
  var
    Ctrl: TControl;
    Node: TAVLTreeNode;
  begin
    Node := YTree.FindLowest;
    while Assigned(Node) do
    begin
      Ctrl := TControl(Node.Data);

      if Ctrl is TDesignSection then
        Recurse((Ctrl as IPositionHandler).YTree)
      else if Ctrl is TDesignField then
      with TDesignField(Ctrl) do
      begin
        if ManagerSettings.ShowFieldBorder then
          BorderStyle := bsSingle
        else
          BorderStyle := bsNone;

        if ManagerSettings.ShowFieldNamesInLabel then
          NameLabel.Parent := QuestionLabel.Parent
        else
          NameLabel.Parent := nil;
      end;
      Node := YTree.FindSuccessor(Node);
    end;
  end;

var
  CtrlV: TShortCut;
begin
  Recurse((FDesignerBox as IPositionHandler).YTree);

  PasteAsQESAction.ShortCut     := 0;
  PasteAsHeadingAction.ShortCut := 0;
  PasteAsIntAction.ShortCut     := 0;
  PasteAsFloatAction.ShortCut   := 0;
  PasteAsStringAction.ShortCut  := 0;

  CtrlV := ShortCut(VK_V, [ssCtrl]);
  Case ManagerSettings.PasteSpecialType of
    0: PasteAsQESAction.ShortCut     := CtrlV;
    1: PasteAsHeadingAction.ShortCut := CtrlV;
    2: PasteAsIntAction.ShortCut     := CtrlV;
    3: PasteAsFloatAction.ShortCut   := CtrlV;
    4: PasteAsStringAction.ShortCut  := CtrlV;
  end;
end;

end.

