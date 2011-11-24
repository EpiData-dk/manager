unit design_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, FileUtil, LResources, Forms, ComCtrls, Controls,
  ActnList, ExtCtrls, StdCtrls, Menus, epidatafiles, epidatafilestypes,
  epicustombase, AVL_Tree , LCLType, LMessages, StdActns, design_controls,
  epidocument, epivaluelabels, manager_messages;

type

  { TDesignFrame }

  TMouseState = record
    Down: boolean;
    Button: TMouseButton;
    Shift: TShiftState;
  end;

  TDesignFrame = class(TFrame)
    MoreSpaceAction: TAction;
    DeleteControlFastAction: TAction;
    NewBooleanMenu: TMenuItem;
    NewHeadingFastAction: TAction;
    NewYMDFieldFastAction: TAction;
    NewMDYFieldFastAction: TAction;
    NewDMYFieldFastAction: TAction;
    NewStringFieldFastAction: TAction;
    NewFloatFieldFastAction: TAction;
    NewIntFieldFastAction: TAction;
    CopyControlPopupMenuItem: TMenuItem;
    CopyControlMenuItem: TMenuItem;
    ToolBarPanel: TPanel;
    PopupMenuDivider2: TMenuItem;
    PopupMenuDivider1: TMenuItem;
    PasteControPopupMenuItem: TMenuItem;
    PasteControlMenuItem: TMenuItem;
    PasteControlAction: TAction;
    CopyControlAction: TAction;
    CurrentSectionPanel: TPanel;
    KeyLabel: TLabel;
    ExtendedLabel: TLabel;
    KeyPanel: TPanel;
    ExtendedPanel: TPanel;
    Label7: TLabel;
    Label8: TLabel;
    ProjectToolBar: TToolBar;
    OpenProjectToolBtn: TToolButton;
    ProjectDivider1: TToolButton;
    SaveProjectToolBtn: TToolButton;
    SaveProjectAsToolBtn: TToolButton;
    ToolButton1: TToolButton;
    ValueLabelLabel: TLabel;
    RangeLabel: TLabel;
    ValueLabelPanel: TPanel;
    FieldNameLabel: TLabel;
    FieldTypeLabel: TLabel;
    FieldNamePanel: TPanel;
    DefaultValueLabel: TLabel;
    FieldTypePanel: TPanel;
    DefaultValuePanel: TPanel;
    RecordStaticLabel: TLabel;
    FieldsStaticLabel: TLabel;
    SectionsLabel: TLabel;
    RecordsLabel: TLabel;
    FieldsLabel: TLabel;
    SectionsPanel: TPanel;
    SectionsStaticLabel: TLabel;
    CurrentSectionLabel: TLabel;
    RecordsPanel: TPanel;
    FieldsPanel: TPanel;
    StatusBarPanel: TPanel;
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
    ImportDataFileAction: TAction;
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
    RangePanel: TPanel;
    procedure   AddStructureActionExecute(Sender: TObject);
    procedure   Button1Click(Sender: TObject);
    procedure   Button2Click(Sender: TObject);
    procedure   Button3Click(Sender: TObject);
    procedure   ControlActionUpdate(Sender: TObject);
    procedure   CopyControlActionExecute(Sender: TObject);
    procedure   CopyControlActionUpdate(Sender: TObject);
    procedure   DeleteAllControlsActionExecute(Sender: TObject);
    procedure   DeleteControlActionExecute(Sender: TObject);
    procedure   DeleteControlActionUpdate(Sender: TObject);
    procedure   DeleteControlFastActionExecute(Sender: TObject);
    procedure DesignerActionListUpdate(AAction: TBasicAction;
      var Handled: Boolean);
    procedure   EditControlActionExecute(Sender: TObject);
    procedure   FrameResize(Sender: TObject);
    procedure   ImportDataFileActionExecute(Sender: TObject);
    procedure   MoreSpaceActionExecute(Sender: TObject);
    procedure   MoveDownActionExecute(Sender: TObject);
    procedure   MoveEndActionExecute(Sender: TObject);
    procedure   MoveHomeActionExecute(Sender: TObject);
    procedure   MovePgDnActionExecute(Sender: TObject);
    procedure   MovePgUpActionExecute(Sender: TObject);
    procedure   MoveUpActionExecute(Sender: TObject);
    procedure   NewDateFieldMenuClick(Sender: TObject);
    procedure   NewDMYFieldActionExecute(Sender: TObject);
    procedure   NewDMYFieldFastActionExecute(Sender: TObject);
    procedure   NewFloatFieldActionExecute(Sender: TObject);
    procedure   NewFloatFieldFastActionExecute(Sender: TObject);
    procedure   NewHeadingActionExecute(Sender: TObject);
    procedure   NewHeadingFastActionExecute(Sender: TObject);
    procedure   NewIntFieldActionExecute(Sender: TObject);
    procedure   NewIntFieldFastActionExecute(Sender: TObject);
    procedure   NewMDYFieldActionExecute(Sender: TObject);
    procedure   NewMDYFieldFastActionExecute(Sender: TObject);
    procedure   NewOtherFieldMenuClick(Sender: TObject);
    procedure   NewSectionActionExecute(Sender: TObject);
    procedure   NewStringFieldActionExecute(Sender: TObject);
    procedure   NewStringFieldFastActionExecute(Sender: TObject);
    procedure   NewYMDFieldActionExecute(Sender: TObject);
    procedure   NewYMDFieldFastActionExecute(Sender: TObject);
    procedure   PasteAsFloatActionExecute(Sender: TObject);
    procedure   PasteAsHeadingActionExecute(Sender: TObject);
    procedure   PasteAsIntActionExecute(Sender: TObject);
    procedure   PasteAsStringActionExecute(Sender: TObject);
    procedure   PasteControlActionExecute(Sender: TObject);
    procedure   PasteControlActionUpdate(Sender: TObject);
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
    FDesignControlForm: TDesignControlsForm;
    FHintWindow:  THintWindow;
    procedure   ResetMousePos;
    function    DesignControlTop(LocalCtrl: TControl): Integer;
    function    ShowForm(EpiControl: TEpiCustomControlItem;
      Pos: TPoint; ForceShow: boolean = true): TModalResult;
    procedure   ShowEpiControlPopup(Sender: TControl; Pos: TPoint);
    procedure   ShowHintMsg(Sender: TObject; Ctrl: TControl; const Msg: string);
    procedure   DataFileHook(Sender: TObject; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
  private
    { Import/Export/Paste }
    FLastRecYPos: Integer;
    FLastRecCtrl: TControl;
    procedure   DoPostImportAlignment(ParentControl: TWinControl;
      StartControl, EndControl: TControl);
    procedure   ImportHook(Sender: TObject; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
    procedure   PasteAsField(FieldType: TEpiFieldType);
    procedure   PasteEpiDoc(Const ImportDoc: TEpiDocument; RenameVL, RenameFields: boolean);
    procedure   RecPasswordRequest(Sender: TObject; var Login: string; var Password: string);
  private
    { Docksite methods }
    // - mouse
    FMouseState: TMouseState;
    FActiveDockSite: TWinControl;
    FShowPanel: TPanel; // For showing the area for a new section.
    procedure   DesignBoxMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);  // not really valid for all dock-sites, but place here for convinience.
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
    function    NewShortCutFieldControl(Ft: TEpiFieldType; AParent: TWinControl; ForceShowForm: Boolean): TControl;
    function    NewShortCutHeadingControl(AParent: TWinControl; ForceShowForm: Boolean): TControl;
    function    NewDesignControl(AClass: TControlClass; AParent: TWinControl; Pos: TPoint; EpiControl: TEpiCustomControlItem): TControl;
    function    NewSectionControl(StartPos, EndPos: TPoint; EpiControl: TEpiCustomControlItem): TControl;
    // - Dock Events.
    procedure   DesignControlStartDock(Sender: TObject; var DragObject: TDragDockObject);
    // - Mouse events.
    procedure   DesignControlMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure   DesignControlMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure   EnterControl(Sender: TObject);
    procedure   ExitControl(Sender: TObject);
    procedure   DeleteControl(Sender: TObject; ForceDelete: Boolean);
    procedure   DoDeleteControl(Sender: TObject);
    procedure   DeleteAllControls;
    // - Custom message, used for deleting controls.
    procedure   LMDesignerDel(var Msg: TLMessage); message LM_DESIGNER_DEL;
    procedure   LMDesignerDelAll(var Msg: TLMessage); message LM_DESIGNER_DELALL;
  private
    { Position handling }
    procedure   FindNearestControls(ParentControl: TWinControl;
      Control: TControl; var XCtrl: TControl; var YCtrl: TControl);
    function    FindLowestDesignControl(ParentControl: TWinControl;
      var Control: TControl; AClass: TControlClass): TPoint;
    function    FindNewPosition(ParentControl: TWinControl;
      AClass: TControlClass): TPoint;
    procedure   ComputeScrollbarPosition(Const Ctrl: TControl);
  private
    { Epidata Core Objects }
    FActiveSection: TEpiSection;
    function    FieldNameHook: string;
    function    NewField(FieldType: TEpiFieldType): TEpiField;
    function    NewHeading: TEpiHeading;
    function    NewSection: TEpiSection;
  private
    { Visual presentation / Statusbars}
    procedure   EpiContolStatusbarUpdateHook(Sender: TObject; EventGrp: TEpiEventGroup; EventType: word; Data: Pointer);
    procedure   UpdateStatusbarControl(EpiControl: TEpiCustomControlItem);
    procedure   UpdateStatusbarSizes;
    procedure   UpdateShortCuts;
  private
    FModified: Boolean;
    FOnModified: TNotifyEvent;
    FImportedFileName: string;
    { Property methods }
    procedure   SetDataFile(const AValue: TEpiDataFile);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent);
    procedure   UpdateFrame;
    procedure   RestoreDefaultPos;
    property    DataFile: TEpiDataFile read FDataFile write SetDataFile;
    property    ImportedFileName: string read FImportedFileName;
  end;

implementation

{$R *.lfm}

uses
  Graphics, Clipbrd, epiadmin, math, import_form, LCLIntf,
  main, settings2_var, epiimport, LCLProc, dialogs, epimiscutils, epistringutils,
  managerprocs, copyobject, epiranges, design_types,
  import_structure_form, shortcuts;

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

procedure TDesignFrame.Button2Click(Sender: TObject);
begin
  Clipboard.AsText :=  WriteTree((FActiveDockSite as IPositionHandler).XTree);
end;

procedure TDesignFrame.Button3Click(Sender: TObject);
begin
  Clipboard.AsText :=  WriteTree((FActiveDockSite as IPositionHandler).YTree);
end;

procedure TDesignFrame.CopyControlActionExecute(Sender: TObject);
var
  CO: TCopyObject;
  EpiCtrl: TEpiCustomControlItem;
begin
  // Main section cannot be copied.
  // TODO : Fix when multiple controls can be selected.
  if FActiveControl = FDesignerBox then exit;
  NewCopyObject(FActiveControl);
end;

procedure TDesignFrame.CopyControlActionUpdate(Sender: TObject);
var
  S: String;
begin
  with TAction(Sender) do
  begin
    Enabled := Assigned(FActiveControl) and
               (not (FActiveControl = FDesignerBox));
    if Enabled then
      case DesignControlTypeFromControl(FActiveControl) of
        dctField:   S := 'Field';
        dctSection: S := 'Section';
        dctHeading: S := 'Heading';
      end
    else
      S := '(not allowed)';
    Caption := 'Copy ' + S;
  end;
end;

procedure TDesignFrame.DeleteAllControlsActionExecute(Sender: TObject);
begin
  {$IFNDEF EPI_DEBUG}
  if MessageDlg('Warning', 'Are you sure you want to clear dataform?',
    mtWarning, mbYesNo, 0, mbNo) = mrNo then exit;
  {$ENDIF}

  PostMessage(Self.Handle, LM_DESIGNER_DELALL, 0, 0);
end;

procedure TDesignFrame.DeleteControlActionExecute(Sender: TObject);
begin
  PostMessage(Self.Handle, LM_DESIGNER_DEL, WPARAM(Sender), 0);
end;

procedure TDesignFrame.ControlActionUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled :=
    (Assigned(FActiveControl));
end;

procedure TDesignFrame.DeleteControlActionUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled :=
    (Assigned(FActiveControl)) and
    (FActiveControl <> FDesignerBox);
end;

procedure TDesignFrame.DeleteControlFastActionExecute(Sender: TObject);
begin
  PostMessage(Self.Handle, LM_DESIGNER_DEL, WPARAM(Sender), 1);
end;

procedure TDesignFrame.DesignerActionListUpdate(AAction: TBasicAction;
  var Handled: Boolean);
begin
  // All designer actions require to be executed on the mainform.
  // - avoids executing "delete controls", etc. on eg. Field Properties Frame.
  if (Screen.ActiveCustomForm = MainForm) then
    DesignerActionList.State := asNormal
  else
    DesignerActionList.State := asSuspended;
end;

procedure TDesignFrame.NewSectionActionExecute(Sender: TObject);
begin
  //
end;

procedure TDesignFrame.EditControlActionExecute(Sender: TObject);
var
  EpiCtrl: TEpiCustomControlItem;
  Pt: TPoint;
begin
  EpiCtrl := (FActiveControl as IDesignEpiControl).EpiControl;
  Pt := FActiveControl.Parent.ClientToScreen(Point(EpiCtrl.Left + FActiveControl.Width, EpiCtrl.Top));
  ShowForm(EpiCtrl, Pt);
  UpdateStatusbarControl(EpiCtrl);
end;

procedure TDesignFrame.FrameResize(Sender: TObject);
begin
  UpdateStatusbarSizes;
end;

procedure TDesignFrame.AddStructureActionExecute(Sender: TObject);
var
  Dlg: TOpenDialog;
  ImpStructurForm: TImportStructureForm;
  i: Integer;
begin
  Dlg := nil;
  ImpStructurForm := nil;
  try
    Dlg := TOpenDialog.Create(Self);
    Dlg.InitialDir := ManagerSettings.WorkingDirUTF8;
    Dlg.Filter := GetEpiDialogFilter(dfImport + [dfCollection]);
    Dlg.Options := [ofAllowMultiSelect, ofFileMustExist, ofEnableSizing, ofViewDetail];
    if not Dlg.Execute then exit;

    ImpStructurForm := TImportStructureForm.Create(FDesignerBox, Dlg.Files);
    if ImpStructurForm.ShowModal = mrCancel then exit;

    // Prepare screen...
    Screen.Cursor := crHourGlass;
    Application.ProcessMessages;
    MainForm.BeginUpdatingForm;

    for i := 0 to ImpStructurForm.SelectedDocuments.Count - 1 do
      PasteEpiDoc(TEpiDocument(ImpStructurForm.SelectedDocuments.Objects[i]),
        ImpStructurForm.ValueLabelsRenameGrpBox.ItemIndex=1, ImpStructurForm.FieldsRenameGrpBox.ItemIndex=1);

    EnterControl(TControl((FDesignerBox as IPositionHandler).YTree.FindLowest.Data));
  finally
    MainForm.EndUpdatingForm;
    Screen.Cursor := crDefault;
    Application.ProcessMessages;

    Dlg.Free;
    ImpStructurForm.Free;
  end;
end;

procedure TDesignFrame.ImportDataFileActionExecute(Sender: TObject);
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
      'Proceed?', mtWarning, mbYesNo, 0, mbNo) <> mrYes then exit;
  end;
  {$ENDIF}

  Importer := nil;
  Dlg := nil;
  try
    Dlg := TOpenDialog.Create(Self);
    Dlg.InitialDir := ManagerSettings.WorkingDirUTF8;
    Dlg.Filter := GetEpiDialogFilter([dfDTA, dfREC, dfCollection]);
    if not Dlg.Execute then exit;

    DeleteAllControls;

    // Prepare screen...
    Screen.Cursor := crHourGlass;
    Application.ProcessMessages;
    MainForm.BeginUpdatingForm;

    // Set import hook
    FActiveSection := FDataFile.MainSection;
    FActiveSection.Fields.RegisterOnChangeHook(@ImportHook, true);
    FActiveSection.Headings.RegisterOnChangeHook(@ImportHook, true);

    // prepare import variables
    FLastRecYPos := -1;
    FLastRecCtrl := nil;
    Importer := TEpiImport.Create;
    Fn := Dlg.FileName;
    Ext := ExtractFileExt(UTF8LowerCase(Fn));

    // Do the import.
    if ext = '.rec' then
    begin
      try
        Importer.OnRequestPassword := @RecPasswordRequest;
        Importer.ImportRec(Fn, FDataFile, true)
      except
        on E: Exception do begin ShowMessage(E.Message); exit; end;
      end;
    end
    else if ext = '.dta' then
      Importer.ImportStata(Fn, TEpiDocument(FDataFile.RootOwner), FDataFile, true);
    FImportedFileName := fn;

    // Update Title with imported file description.
    TEpiDocument(FDataFile.RootOwner).Study.Title.Text := FDataFile.Caption.Text;

    EnterControl(TControl((FDesignerBox as IPositionHandler).YTree.FindLowest.Data));
  finally
    Importer.Free;
    MainForm.EndUpdatingForm;
    Screen.Cursor := crDefault;
    Application.ProcessMessages;
    FActiveSection.Fields.UnRegisterOnChangeHook(@ImportHook);
    FActiveSection.Headings.UnRegisterOnChangeHook(@ImportHook);
    Dlg.Free;
  end;
end;

procedure TDesignFrame.MoreSpaceActionExecute(Sender: TObject);
var
  Heading: TEpiHeading;
  Pt: TPoint;
begin
  Heading := NewHeading;
  Heading.Caption.Text := '_________ automatic extension _________';

  Pt := FindNewPosition(FDesignerBox, TDesignHeading);
  Pt.X := 50;
  Pt.Y := Pt.Y + (FDesignerBox.VertScrollBar.Page);

  NewDesignControl(TDesignHeading, FDesignerBox, Pt, Heading);
  ShowForm(Heading, FDesignerBox.ClientToScreen(Pt), False);
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
  NewShortCutFieldControl(ftDMYDate, FActiveDockSite, true);
end;

procedure TDesignFrame.NewDMYFieldFastActionExecute(Sender: TObject);
begin
  NewShortCutFieldControl(ftDMYDate, FActiveDockSite, false);
end;

procedure TDesignFrame.NewFloatFieldActionExecute(Sender: TObject);
begin
  NewShortCutFieldControl(ftFloat, FActiveDockSite, true);
end;

procedure TDesignFrame.NewFloatFieldFastActionExecute(Sender: TObject);
begin
  NewShortCutFieldControl(ftFloat, FActiveDockSite, false);
end;

procedure TDesignFrame.NewHeadingActionExecute(Sender: TObject);
begin
  NewShortCutHeadingControl(FActiveDockSite, true);
end;

procedure TDesignFrame.NewHeadingFastActionExecute(Sender: TObject);
begin
  NewShortCutHeadingControl(FActiveDockSite, false);
end;

procedure TDesignFrame.NewIntFieldActionExecute(Sender: TObject);
begin
  NewShortCutFieldControl(ftInteger, FActiveDockSite, true);
end;

procedure TDesignFrame.NewIntFieldFastActionExecute(Sender: TObject);
begin
  NewShortCutFieldControl(ftInteger, FActiveDockSite, false);
end;

procedure TDesignFrame.NewMDYFieldActionExecute(Sender: TObject);
begin
  NewShortCutFieldControl(ftMDYDate, FActiveDockSite, true);
end;

procedure TDesignFrame.NewMDYFieldFastActionExecute(Sender: TObject);
begin
  NewShortCutFieldControl(ftMDYDate, FActiveDockSite, false);
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
  NewShortCutFieldControl(ftString, FActiveDockSite, true);
end;

procedure TDesignFrame.NewStringFieldFastActionExecute(Sender: TObject);
begin
  NewShortCutFieldControl(ftString, FActiveDockSite, false);
end;

procedure TDesignFrame.NewYMDFieldActionExecute(Sender: TObject);
begin
  NewShortCutFieldControl(ftYMDDate, FActiveDockSite, true);
end;

procedure TDesignFrame.NewYMDFieldFastActionExecute(Sender: TObject);
begin
  NewShortCutFieldControl(ftYMDDate, FActiveDockSite, false);
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

procedure TDesignFrame.PasteAsStringActionExecute(Sender: TObject);
begin
  PasteAsField(ftString);
end;

procedure TDesignFrame.PasteControlActionExecute(Sender: TObject);
var
  CO: TCopyObject;
  EpiCtrl: TEpiCustomControlItem;
  OldEpiCtrl: TEpiCustomControlItem;
  OrgField: TEpiField;
  OrgSection: TEpiSection;
  ThisSection: TEpiSection;
  OrgHeading: TEpiHeading;
  i: Integer;
  Pt: TPoint;
  SectionCtrl: TWincontrol;
  J: Integer;
  TheName: String;
  FieldCount: LongInt;
begin
  if not Assigned(GetCopyObject) then exit;

  CO := GetCopyObject;

  try
    MainForm.BeginUpdatingForm;

    if (FActiveControl = FDesignerBox) or
       ((FActiveControl is TDesignSection) and
        (CO.CopyType in [ctField, ctHeading]))
    then
    begin
      // Creating new control in section/main.
      case CO.CopyType of
        ctField:
          begin
            OrgField := TFieldCopyObject(CO.Data).Field;

            // Create new field.
            EpiCtrl := NewField(OrgField.FieldType);

            // Copy properties.
            EpiCtrl.Assign(OrgField);
            // Since Valuelabels are NOT copied - do it manually.
            TEpiField(EpiCtrl).ValueLabelSet := OrgField.ValueLabelSet;

            // Place new field.
            Pt := FindNewPosition(FActiveDockSite, TDesignField);
            NewDesignControl(TDesignField, FActiveDockSite, Pt, EpiCtrl);
          end;
        ctSection:
          begin
            FieldCount := DataFile.Fields.Count;
            OrgSection := TSectionCopyObject(CO.Data).Section;
            EpiCtrl := NewSection;
            EpiCtrl.Assign(OrgSection);
            Pt := FindNewPosition(FActiveDockSite, TDesignSection);
            SectionCtrl := TWincontrol(NewSectionControl(Pt, Point(Pt.X+OrgSection.Width, Pt.Y+OrgSection.Height), EpiCtrl));

            // New design controls for ALL fields and headings.
            with TEpiSection(EpiCtrl) do
            begin
              for i := 0 to Fields.Count - 1 do
              begin
                OldEpiCtrl := Field[i];
                // Since Valuelabels are NOT copied - do it manually.
                TEpiField(OldEpiCtrl).ValueLabelSet := OrgSection.Field[i].ValueLabelSet;
                NewDesignControl(TDesignField, SectionCtrl, Point(OldEpiCtrl.Left, OldEpiCtrl.Top), OldEpiCtrl);
              end;
              for i := 0 to Headings.Count - 1 do
              begin
                OldEpiCtrl := Heading[i];
                NewDesignControl(TDesignHeading, SectionCtrl, Point(OldEpiCtrl.Left, OldEpiCtrl.Top), OldEpiCtrl);
              end;
            end;
          end;
        ctHeading:
          begin
            OrgHeading := THeadingCopyObject(CO.Data).Heading;
            EpiCtrl := NewHeading;
            EpiCtrl.Assign(OrgHeading);
            Pt := FindNewPosition(FActiveDockSite, TDesignHeading);
            NewDesignControl(TDesignHeading, FActiveDockSite, Pt, EpiCtrl);
          end;
        ctSelection: ;
      end;
      EnterControl(FActiveDockSite);
    end else begin
      // Copying properties.
      case CO.CopyType of
        ctField:
          begin
            OrgField := TFieldCopyObject(CO.Data).Field;
            EpiCtrl  := (FActiveControl as IDesignEpiControl).EpiControl;
            if TEpiField(EpiCtrl).FieldType <> Orgfield.FieldType then
            begin
              ShowHintMsg(nil, FActiveControl, 'Cannot copy properties:'+LineEnding+'Fields must of same type');
              exit;
            end;

            EpiCtrl.BeginUpdate;
            TheName := '';
            if TEpiField(EpiCtrl).Question.Text <> '' then
              TheName := TEpiField(EpiCtrl).Question.Text;
            EpiCtrl.Assign(OrgField);
            // Since Valuelabels are NOT copied - do it manually.
            TEpiField(EpiCtrl).ValueLabelSet := OrgField.ValueLabelSet;
            if TheName <> '' then
              TEpiField(EpiCtrl).Question.Text := TheName;
            EpiCtrl.EndUpdate;
          end;
        ctSection:
          begin
            OrgSection := TSectionCopyObject(CO.Data).Section;
            EpiCtrl    := (FActiveControl as IDesignEpiControl).EpiControl;
            ThisSection := TEpiSection(EpiCtrl);
            ThisSection.Caption.Assign(OrgSection.Caption);
            // TODO : Maybe change if CustomList changes assignment based on ItemOwner.
            for i := 0 to OrgSection.Groups.Count - 1 do
              ThisSection.Groups.AddItem(OrgSection.Groups[i]);
          end;
        ctHeading:
          begin
            OrgHeading := THeadingCopyObject(CO.Data).Heading;
            EpiCtrl    := (FActiveControl as IDesignEpiControl).EpiControl;
            EpiCtrl.Assign(OrgHeading);
          end;
        ctSelection: ;
      end;
      ShowForm(EpiCtrl, Point(0,0), false);
      UpdateStatusbarControl(EpiCtrl);
    end;
  finally
    MainForm.EndUpdatingForm;
  end;
end;

procedure TDesignFrame.PasteControlActionUpdate(Sender: TObject);
begin
  With TAction(Sender) do
  begin
    Caption := 'Paste';
    Enabled := Assigned(GetCopyObject) and
               CopyCompatibleTable[DesignControlTypeFromControl(FActiveControl), GetCopyObject.CopyType] and
               (not GetCopyObject.ContainsControl(FActiveControl));
    if Enabled then
      case GetCopyObject.CopyType of
        ctField:     begin
                       Caption := Caption + ' Field';
                     end;
        ctSection:   Caption := Caption + ' Section';
        ctHeading:   Caption := Caption + ' Heading';
        ctSelection: Caption := Caption + ' Selection';
      end
    else
      Caption := Caption + ' (not allowed)';
  end;
end;

procedure TDesignFrame.TestToolButtonClick(Sender: TObject);
var
  Field: TEpiField;
  Jumps: TEpiJumps;
  J: TEpiJump;
begin
{  MainForm.FlipChildren(true);
  if MainForm.MainMenu1.BidiMode = bdRightToLeft then
    MainForm.MainMenu1.BidiMode := bdLeftToRight
  else
    MainForm.MainMenu1.BidiMode := bdRightToLeft; }
  Field := TepiField((FActiveControl as IDesignEpiControl).EpiControl);
  Jumps := TEpiJumps.Create(Field);

  J := Jumps.NewJump;
  J.ResetType := jrLeaveAsIs;
  J.JumpType := jtSkipNextField;
  TEpiIntJump(J).JumpValue := 0;

  J := Jumps.NewJump;
  J.ResetType := jrSystemMissing;
  J.JumpType := jtExitSection;
  TEpiIntJump(J).JumpValue := 1;

  J := Jumps.NewJump;
  J.ResetType := jrMaxMissing;
  J.JumpType := jtSaveRecord;
  TEpiIntJump(J).JumpValue := 9;

  Field.Jumps := Jumps;
end;


function TDesignFrame.ShowForm(EpiControl: TEpiCustomControlItem; Pos: TPoint;
  ForceShow: boolean): TModalResult;
begin
  if (not Assigned(FDesignControlForm)) and (not ForceShow) then exit;

  if not Assigned(FDesignControlForm) then
  begin
    FDesignControlForm := TDesignControlsForm.Create(Self, DataFile.RootOwner);
    FDesignControlForm.OnShowHintMsg := @ShowHintMsg;
  end;

  if (not FDesignControlForm.Showing) and ForceShow then
  begin
    FDesignControlForm.Left := Min(Pos.X, Screen.Width - FDesignControlForm.Width - 5);
    FDesignControlForm.Top := Min(Pos.Y, Screen.Height - FDesignControlForm.Height - 5);
  end;
  FDesignControlForm.EpiControl := EpiControl;

  if ForceShow then
    FDesignControlForm.Show;
end;

function TDesignFrame.NewDesignControl(AClass: TControlClass;
  AParent: TWinControl; Pos: TPoint; EpiControl: TEpiCustomControlItem
  ): TControl;
var
  Ctrl: TControlEx;
begin
  MainForm.BeginUpdatingForm;

  Result := AClass.Create(AParent);
  Ctrl := TControlEx(Result);

  (Ctrl as IDesignEpiControl).EpiControl := EpiControl;
  with Ctrl do
  begin
    OnMouseDown := @DesignControlMouseDown;
    OnMouseUp   := @DesignControlMouseUp;
    OnStartDock := @DesignControlStartDock;
    Dock(AParent, Bounds(Pos.X, Pos.Y, Width, Height));
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

  MainForm.EndUpdatingForm;
end;

function TDesignFrame.NewSectionControl(StartPos, EndPos: TPoint;
  EpiControl: TEpiCustomControlItem): TControl;
var
  Ctrl: TDesignSection;
  EpiSection: TEpiSection absolute EpiControl;
const
  MinSectionWidth = 20;
  MinSectionHeight = 10;
begin
  MainForm.BeginUpdatingForm;

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
    Dock(FDesignerBox, Bounds(StartPos.X, StartPos.Y, Width, Height));
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
//  Ctrl.Parent := FDesignerBox;

  EnterControl(Result);
  AddToPositionHandler((FDesignerBox as IPositionHandler), Result);

  MainForm.EndUpdatingForm;
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
  if Supports(Sender, 'IPositionHandler') then
    DockingSitePopUpMenu.PopUp(Pos.X, Pos.Y)
  else
    DesignControlPopUpMenu.PopUp(Pos.X, Pos.Y);
end;

procedure TDesignFrame.ShowHintMsg(Sender: TObject; Ctrl: TControl;
  const Msg: string);
var
  R: TRect;
  P: TPoint;
begin
  if (Msg = '') and (Ctrl = nil) then
  begin
    FHintWindow.Hide;
    exit;
  end;

  R := FHintWindow.CalcHintRect(0, Msg, nil);
  P := Ctrl.ClientToScreen(Point(0,0));
  OffsetRect(R, P.X, P.Y + Ctrl.Height + 2);
  FHintWindow.ActivateHint(R, Msg);
end;

procedure TDesignFrame.DataFileHook(Sender: TObject;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  if (EventGroup = eegCustomBase) and (EventType = Word(ecceDestroy)) then exit;

  if (EventGroup = eegDataFiles) and (EventType = Word(edceSize)) then
    UpdateStatusbarControl((FActiveControl as IDesignEpiControl).EpiControl);
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
      ShowValueLabel := ManagerSettings.ShowValuelabelText;
    end;

    Cls := TDesignField;
    if Sender is TEpiHeading then
      Cls := TDesignHeading;
    Pt := FindNewPosition(FActiveDockSite, Cls);

    if (not (FLastRecYPos = -1)) and (FLastRecYPos = TEpiCustomControlItem(Sender).Top) then
    begin
      Pt.Y := FLastRecCtrl.Top;
      if (FLastRecCtrl is TDesignField) and (Sender is TEpiField) then
        Pt.X := FLastRecCtrl.Left + FLastRecCtrl.Width + 5 +                                   // This calculates right side of previous placed control (with 5px margin)
                FDesignerBox.Canvas.GetTextWidth(TEpiField(Sender).Question.Text) + 5 +        // This gives a rough estimate of the width of the Question text (5px margin)
                FDesignerBox.Canvas.GetTextWidth(TEpiField(Sender).Name) + 5                   // This gives a rough estimate of the width of the Name (if shown).
      else
        Pt.X := FLastRecCtrl.Left + FLastRecCtrl.Width + 10;
    end;

    FLastRecYPos := TEpiCustomControlItem(Sender).Top;
    FLastRecCtrl := NewDesignControl(Cls, FActiveDockSite, Pt, TEpiCustomControlItem(Sender));
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
      TmpField.Question.Text := Trim(Cbl[i]);
      NewDesignControl(TDesignField, FActiveDockSite, Pt, TmpField);
    end;
  finally
    Cbl.Free;
  end;
end;

procedure TDesignFrame.PasteEpiDoc(const ImportDoc: TEpiDocument; RenameVL,
  RenameFields: boolean);
var
  Pt: TPoint;
  i: Integer;
  j: Integer;
  VLSet: TEpiValueLabelSet;
  OldSet: TEpiValueLabelSet;
  OField: TEpiField;
  NField: TEpiField;
  TheSection: TEpiSection;

  function NewFieldName(Const OldName: string): string;
  var
    FC, i: integer;
    TheName: String;
  begin
    Result := OldName;
    if DataFile.ValidateRename(OldName) then exit;

    i := 0;
    repeat
      inc(i);
      Result := OldName + IntToStr(i);
    until DataFile.ValidateRename(Result);
  end;

  procedure CopyField(Const AField: TEpiField; AParent: TWinControl; Const AddTop: Integer);
  var
    NField, OField: TEpiField;
  begin
    OField := DataFile.Fields.FieldByName[AField.Name];
    if (not RenameFields) and
       Assigned(OField) and
       (AField.FieldType = OField.FieldType) then
      exit;

    NField := NewField(AField.FieldType);
    // Can be used later on...
    NField.ObjectData := PtrUInt(AField);
    AField.ObjectData := PtrUInt(NField);
    With NField do
    begin
      if not ValidateRename(AField.Name, true) then
        Name := NewFieldName(AField.Name);

      Question.Assign(AField.Question);
      Top            := Afield.Top + AddTop;
      Left           := Afield.Left;
      Length         := Afield.Length;
      Decimals       := AField.Decimals;
      EntryMode      := Afield.EntryMode;
      ConfirmEntry   := AField.ConfirmEntry;
      ShowValueLabel := AField.ShowValueLabel;

      if Assigned(AField.Ranges) then
      begin
        Ranges := TEpiRanges.Create(NField);
        Ranges.Assign(AField.Ranges);
      end;

      // Cannot copy jumps, since a jump-to field may not exists yet.

      // The ValuelabelSet have previously been assigned to new Document and hence,
      // stored the pointer to the new VLSet in ObjectData.
      if Assigned(AField.ValueLabelSet) then
        ValueLabelSet := TEpiValueLabelSet(AField.ValueLabelSet.ObjectData);
    end;
    NewDesignControl(TDesignField, AParent, Point(NField.Left, NField.Top), NField);
  end;

  procedure CopyHeading(Const AHeading: TEpiHeading; AParent: TWinControl; Const AddTop: Integer);
  var
    NHeading: TEpiHeading;
  begin
    NHeading := NewHeading;
    NHeading.Assign(AHeading);
    NHeading.Top := AHeading.Top + AddTop;
    NewDesignControl(TDesignHeading, AParent, Point(NHeading.Left, NHeading.Top), NHeading);
  end;

  procedure CopySection(Const ASection: TEpiSection; Const AddTop: integer);
  var
    NSection: TEpiSection;
    WinSection: TWinControl;
    OldActiveSection: TEpiSection;
    i,j: integer;
  begin
    NSection := NewSection;

    with NSection do
    begin
      Top  := ASection.Top + AddTop;
      Left := ASection.Left;
      Width := ASection.Width;
      Height := ASection.Height;
      Caption.Assign(ASection.Caption);

      WinSection := TWinControl(NewSectionControl(Point(Left, Top), Point(Left + Width, Top + Height), NSection));
    end;

    OldActiveSection := FActiveSection;
    FActiveSection := NSection;

    for i := 0 to ASection.Fields.Count -1 do
      CopyField(ASection.Fields[i], WinSection, 0);
    for i := 0 to ASection.Headings.Count -1 do
      CopyHeading(ASection.Heading[i], WinSection, 0);

    FActiveSection := OldActiveSection;
  end;

begin
  // Copy ValueLabels first...
  if ImportDoc.ValueLabelSets.Count > 0 then
  begin
    for i := 0 to ImportDoc.ValueLabelSets.Count - 1 do
    begin
      OldSet := ImportDoc.ValueLabelSets[i];
      VLSet  := DataFile.ValueLabels.GetValueLabelSetByName(OldSet.Name);

      // Rename vs. Keep options.
      if (not RenameVL) and
         Assigned(VLSet) and
         (VLSet.LabelType = OldSet.LabelType) then
      begin
        // Old set must carry a pointer to "new" set, else field copy will fail.
        OldSet.ObjectData := PtrUInt(VLSet);
        continue;
      end;

      VLSet := DataFile.ValueLabels.NewValueLabelSet(OldSet.LabelType);
      VLSet.Assign(OldSet);
      OldSet.ObjectData := PtrUInt(VLSet);
    end;
  end;

  Pt := FindNewPosition(FDesignerBox, TDesignField);
  FActiveSection := TEpiSection((FDesignerBox as IDesignEpiControl).EpiControl);

  // First place main section - it's easiest.
  TheSection := ImportDoc.DataFiles[0].MainSection;
  // Do fields...
  for i := 0 to TheSection.Fields.Count - 1 do
    CopyField(TheSection.Field[i], FDesignerBox, Pt.Y);
  // Do Headings...
  for i := 0 to TheSection.Headings.Count - 1 do
    CopyHeading(TheSection.Heading[i], FDesignerBox, Pt.Y);

  for i := 0 to ImportDoc.DataFiles[0].Sections.Count - 1 do
    if ImportDoc.DataFiles[0].Section[i] <> ImportDoc.DataFiles[0].MainSection then
      CopySection(ImportDoc.DataFiles[0].Section[i], Pt.Y);

  // Now all fields are copied - try to copy jumps.
  for i := 0 to ImportDoc.DataFiles[0].Fields.Count - 1 do
  begin
    if not Assigned(ImportDoc.DataFiles[0].Field[i].Jumps) then continue;

    OField := ImportDoc.DataFiles[0].Field[i];
    NField := TepiField(OField.ObjectData);

    NField.Jumps := TEpiJumps.Create(NField);
    // Simple assign work for non jump-to-field jumps.
    NField.Jumps.Assign(OField.Jumps);

    for j := 0 to OField.Jumps.Count -1 do
    with TEpiJump(OField.Jumps[j]) do
    begin
      if JumpType <> jtToField then continue;
      NField.Jumps[j].JumpToField := TEpiField(JumpToField.ObjectData);;
    end;
  end;
end;

procedure TDesignFrame.RecPasswordRequest(Sender: TObject; var Login: string;
  var Password: string);
begin
  Login := '';
  Password := PasswordBox('WARNING', 'File is encrypted.' + LineEnding + 'Enter password:');
end;

procedure TDesignFrame.DesignBoxMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  with FDesignerBox.VertScrollBar do
    Position := Position - WheelDelta;
  Handled := true;
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
  result := FindLowestDesignControl(ParentControl, Control, AClass);
  if not Assigned(Control) then exit;

  Dist := ManagerSettings.SpaceBtwFieldLabel;
  if (AClass = TDesignField) and (Control is TDesignField) then
    Dist := ManagerSettings.SpaceBtwFieldField;
  if (AClass = TDesignHeading) and (Control is TDesignHeading) then
    Dist := ManagerSettings.SpaceBtwLabelLabel;
  Result.Y += (Control.Height + Dist);
end;

procedure TDesignFrame.ComputeScrollbarPosition(const Ctrl: TControl);
var
  Delta: Integer;
  ControlTop: Integer;
begin
  Delta := FDesignerBox.Height div 4;
  ControlTop := DesignControlTop(Ctrl);

  With FDesignerBox.VertScrollBar do
  begin
    if ControlTop < (Position + Delta) then
      Position := ControlTop - Delta;

    if ControlTop > (Position + Page - Delta) then
      Position := ControlTop - Page + FActiveControl.Height + Delta;
  end;
end;

function TDesignFrame.FieldNameHook: string;
begin
  result := ManagerSettings.FieldNamePrefix;
end;

function TDesignFrame.NewField(FieldType: TEpiFieldType): TEpiField;
var
  TheName: String;
  I: Integer;
begin
  result := FActiveSection.NewField(FieldType);

  with result do
  begin
    ShowValueLabel := ManagerSettings.ShowValuelabelText;
    Case FieldType of
      ftInteger, ftAutoInc:
          Length := ManagerSettings.IntFieldLength;
      ftFloat:
        begin
          Length := ManagerSettings.FloatIntLength;
          Decimals := ManagerSettings.FloatDecimalLength;
        end;
      ftString, ftUpperString:
        Length := ManagerSettings.StringFieldLength;
    end;
  end;
  Result.RegisterOnChangeHook(@EpiContolStatusbarUpdateHook);
end;

function TDesignFrame.NewHeading: TEpiHeading;
begin
  result := FActiveSection.NewHeading;
  result.Caption.Text := '(Untitled)';
  Result.RegisterOnChangeHook(@EpiContolStatusbarUpdateHook);
end;

function TDesignFrame.NewSection: TEpiSection;
begin
  result := DataFile.NewSection;
  Result.RegisterOnChangeHook(@EpiContolStatusbarUpdateHook);
  Result.Fields.OnGetPrefix := @FieldNameHook;
end;

procedure TDesignFrame.EpiContolStatusbarUpdateHook(Sender: TObject;
  EventGrp: TEpiEventGroup; EventType: word; Data: Pointer);
begin
  if ((EventGrp = eegCustomBase) and (EventType = Word(ecceDestroy))) then exit;

  UpdateStatusbarControl(TEpiCustomControlItem(Sender));
end;

procedure TDesignFrame.UpdateStatusbarControl(EpiControl: TEpiCustomControlItem
  );
begin
  // New "statusbar"
  RecordsLabel.Caption := IntToStr(DataFile.Size);
  SectionsLabel.Caption := IntToStr(DataFile.Sections.Count);
  if FActiveSection = DataFile.MainSection then
    CurrentSectionLabel.Caption := 'main'
  else
    CurrentSectionLabel.Caption := FActiveSection.Caption.Text;
  FieldsLabel.Caption := IntToStr(DataFile.Fields.Count);
  FieldNameLabel.Caption := EpiControl.Name;

  if EpiControl is TEpiField then
  with TEpiField(EpiControl) do
  begin
    FieldTypeLabel.Caption    := EpiTypeNames[FieldType];
    DefaultValueLabel.Caption := BoolToStr(HasDefaultValue, DefaultValueAsString, '');
    if Assigned(ValueLabelSet) then
      ValueLabelLabel.Caption := ValueLabelSet.Name
    else
      ValueLabelLabel.Caption := '';
    RangeLabel.Caption        := BoolToStr(Assigned(Ranges), 'Range', '');
    KeyLabel.Caption          := ''; // TODO : Set when implement in core.
    ExtendedLabel.Caption     := BoolToStr(Assigned(Jumps),       'J', ' ') +
                                 BoolToStr(RepeatValue,           'R', ' ') +
                                 BoolToStr(EntryMode=emMustEnter, 'M', '')  +
                                 BoolToStr(EntryMode=emNoEnter,   'N', '')  +
                                 BoolToStr(EntryMode=emDefault,   ' ', '')  +
                                 BoolToStr(ConfirmEntry,          'F', ' ') +
                                 BoolToStr(Assigned(Calculation), 'C', ' ');
  end else begin
    FieldTypeLabel.Caption    := '';
    DefaultValueLabel.Caption := '';
    ValueLabelLabel.Caption   := '';
    RangeLabel.Caption        := '';
    KeyLabel.Caption          := '';
    ExtendedLabel.Caption     := '';
  end;

  UpdateStatusbarSizes;
end;

procedure TDesignFrame.UpdateStatusbarSizes;
const
  PanelBorder = 2;

  function TW(Lbl: TLabel): Integer;
  begin
    Result := StatusBarPanel.Canvas.TextWidth(Lbl.Caption);
  end;

begin
  RecordsPanel.Width  := RecordsLabel.Left + TW(RecordsLabel) + PanelBorder;
  SectionsPanel.Width := SectionsLabel.Left + TW(SectionsLabel) + PanelBorder;
  FieldsPanel.Width   := FieldsLabel.Left + TW(FieldsLabel) + PanelBorder;
end;

procedure TDesignFrame.UpdateShortCuts;
begin
  PasteAsHeadingAction.ShortCut := 0;
  PasteAsIntAction.ShortCut     := 0;
  PasteAsFloatAction.ShortCut   := 0;
  PasteAsStringAction.ShortCut  := 0;

  Case ManagerSettings.PasteSpecialType of
    0: PasteAsHeadingAction.ShortCut := D_PasteAs;
    1: PasteAsIntAction.ShortCut     := D_PasteAs;
    2: PasteAsFloatAction.ShortCut   := D_PasteAs;
    3: PasteAsStringAction.ShortCut  := D_PasteAs;
  end;

  // Designer Frame
  NewIntFieldAction.ShortCut := D_NewIntField;
  NewIntFieldFastAction.ShortCut := D_NewIntField_Fast;
  NewFloatFieldAction.ShortCut := D_NewFloatField;
  NewFloatFieldFastAction.ShortCut := D_NewFloatField_Fast;
  NewStringFieldAction.ShortCut := D_NewStringField;
  NewStringFieldFastAction.ShortCut := D_NewStringField_Fast;
  NewDMYFieldAction.ShortCut := D_NewDateField;
  NewDMYFieldFastAction.ShortCut := D_NewDateField_Fast;
  NewHeadingAction.ShortCut := D_NewHeading;
  NewHeadingFastAction.ShortCut := D_NewHeading_Fast;
  NewSectionAction.ShortCut := D_NewSection;
  EditControlAction.ShortCut := D_EditControl;
  DeleteControlAction.ShortCut := D_DeleteControl;
  DeleteControlFastAction.ShortCut := D_DeleteControl_Fast;
  ImportDataFileAction.ShortCut := D_ImportData;
  AddStructureAction.ShortCut := D_AddStructure;
  MoveHomeAction.ShortCut := D_MoveTop;
  MovePgUpAction.ShortCut := D_MoveSideUp;
  MoveUpAction.ShortCut := D_MoveControlUp;
  MoveDownAction.ShortCut := D_MoveControlDown;
  MovePgDnAction.ShortCut := D_MoveSideDown;
  MoveEndAction.ShortCut := D_MoveBottom;
  DeleteAllControlsAction.ShortCut := D_DeleteAllControl;
  CopyControlAction.ShortCut := D_CopyControl;
  PasteControlAction.ShortCut := D_PasteControl;
end;

procedure TDesignFrame.SetDataFile(const AValue: TEpiDataFile);
var
  i: Integer;
  TheParent: TWinControl;
  j: Integer;
  Ft: TEpiFieldType;
  W: Integer;
begin
  if DataFile = AValue then exit;
  FDataFile := AValue;

  Name := DataFile.Name;
  FActiveSection := DataFile.MainSection;
  (FDesignerBox as IDesignEpiControl).EpiControl := DataFile.MainSection;

  MainForm.BeginUpdatingForm;
  DataFile.BeginUpdate;
  with DataFile do
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
        Fields.OnGetPrefix := @FieldNameHook;
        for j := 0 to Fields.Count - 1 do
          NewDesignControl(TDesignField, TheParent, Point(Field[j].Left, Field[j].Top), Field[j]);
        for j := 0 to Headings.Count - 1 do
          NewDesignControl(TDesignHeading, TheParent, Point(Heading[j].Left, Heading[j].Top), Heading[j]);
      end;
    end;
  end;
  if not Assigned(FActiveControl) then
    EnterControl(FDesignerBox);
  DataFile.EndUpdate;
  MainForm.EndUpdatingForm;

  DataFile.RegisterOnChangeHook(@DataFileHook);

  W := 0;
  for Ft := Low(TEpiFieldType) to High(TEpiFieldType) do
    W := Max(W, StatusBarPanel.Canvas.TextWidth(EpiTypeNames[Ft]));
  FieldTypePanel.Width := FieldTypeLabel.Left + W + 2;
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

  if FActiveButton <> SectionToolButton then exit;
  // For drawing the area where the new section is going to be.
  FShowPanel := TPanel.Create(FDesignerBox);
  FShowPanel.Color := clWhite;
  FShowPanel.BorderStyle := bsSingle;
  FShowPanel.BorderWidth := 1;
  FShowPanel.BevelOuter := bvNone;
  FShowPanel.BevelInner := bvNone;
  {$IFNDEF DARWIN}
  DrawShowPanel(X+1, Y+1);
  {$ENDIF DARWIN}
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
    mbLeft: FLeftMouseUp := WinSender.ControlToScreen(Point(X, Y));
    mbRight: FRightMouseUp := WinSender.ControlToScreen(Point(X, Y));
  end;

  FMouseState.Down := false;

  // Right button activates popup-menu
  if Button = mbRight then
  begin
    ShowEpiControlPopup(WinSender, FRightMouseUp);
    Exit;
  end;

  ParentPt := Point(X, Y);
  if (WinSender = FDesignerBox) then
  begin
    ParentPt.X += FDesignerBox.HorzScrollBar.Position;
    ParentPt.Y += FDesignerBox.VertScrollBar.Position;
  end;

  case (FActiveButton.Index + 1) of
    // Dividers... should never get here:
    2, 8, 10, 12, 15, 19:
      Exit;

    // Selector
    1: Exit;

    // Integer, Float, String, Date (all dates), Others.
    3,4,5,6,7:
      begin
        EpiControl := NewField(TEpiFieldType(FActiveButton.Tag));
        NewDesignControl(TDesignField, WinSender, ParentPt, EpiControl);
        ShowForm(EpiControl, FLeftMouseUp);
      end;
    // Heading
    9:
      begin
        EpiControl := NewHeading;
        NewDesignControl(TDesignHeading, WinSender, ParentPt, EpiControl);
        ShowForm(EpiControl, FLeftMouseUp);
      end;
    // Section
    11:
      begin
        // Sections can only be created on the designer.
        if not (Sender = FDesignerBox) then exit;
        EpiControl := NewSection;
        NewSectionControl(WinSender.ScreenToClient(FLeftMouseDown),
          WinSender.ScreenToClient(FLeftMouseUp), EpiControl);
        ShowForm(EpiControl, FLeftMouseDown);
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

  Label7.Caption := Format('Mouse:  X=%d,Y=%d', [X, Y]);
  Label8.Caption := Format('Scollbar: Pos=%d', [FDesignerBox.VertScrollBar.Position]);

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
  OldDockSite: TWinControl;
begin
  // Sender = the site being dragged onto.
  // Source.control = the control being dragged.
  OldDockSite := TDesignDockObject(Source).FOldDockSite;

  RemoveFromPositionHandler(
    (OldDockSite as IPositionHandler),
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
    Dy := (EpiControl.Top + Control.Height) - (YCtrl.Top + YCtrl.Height);

    if Abs(Dx) <= ManagerSettings.SnappingThresHold then
      EpiControl.Left := XCtrl.Left;
    // Align bottoms.
    if Abs(Dy) <= ManagerSettings.SnappingThresHold then
      EpiControl.Top := YCtrl.Top + (YCtrl.Height - Control.Height);
  end;
  EpiControl.EndUpdate;

  AddToPositionHandler((Sender as IPositionHandler),
    Source.Control);

  // Dirty hack for placing vert. scrollbar correctly - but works.
  MainForm.BeginUpdatingForm;
  FDesignerBox.VertScrollBar.Position := 1;
  ComputeScrollbarPosition(Source.Control);
  MainForm.EndUpdatingForm;

  // Sanity checks:
  // - sections do not need to be relocated in the Core structure.
  if EpiControl is TEpiSection then  exit;

  // - if old and new section is the same do nothing.
  NSection := TEpiSection((Sender as IDesignEpiControl).EpiControl);
  OSection := TEpiSection(EpiControl.Owner.Owner);
  if NSection = OSection then exit;

  // Remove from old parent
  // - TComponent
  OldDockSite.RemoveComponent(Source.Control);
  // - EpiData Core
  TEpiCustomList(EpiControl.Owner).RemoveItem(EpiControl);

  // Insert into new
  // - EpiData Core
  if EpiControl is TEpiField then
    NSection.Fields.AddItem(EpiControl)
  else
    NSection.Headings.AddItem(EpiControl);
  // - TComponent
  TComponent(Sender).InsertComponent(Source.Control);
end;

procedure TDesignFrame.DockSiteUnDock(Sender: TObject; Client: TControl;
  NewTarget: TWinControl; var Allow: Boolean);
begin
  // Sender     = the site that is being undocked from.
  // Client     = the control that was dragged to another position.
  // NewTarget  = the target we try dock the client onto (can be the same the sender)
  // Allow      = our feedback to the dragmanager if we want this undock to happen.

  Allow := false;
  // NewTarget = false: trying to release the client outside of program window.
  if not Assigned(NewTarget) then exit;

  // This effectively prevents dragging section onto section.
  if (NewTarget.ClassType = Client.ClassType) then exit;

  // No sure if this is needed, but better safe than sorry. Basically we prevent
  // anything from docking into other controls than a section and the scrollbox.
  if (NewTarget = FDesignerBox) or
    (NewTarget is TDesignSection) then Allow := true;
end;

function TDesignFrame.NewShortCutFieldControl(Ft: TEpiFieldType;
  AParent: TWinControl; ForceShowForm: Boolean): TControl;
var
  Pt: TPoint;
  Field: TEpiField;
begin
  Field := NewField(Ft);
  Pt := FindNewPosition(AParent, TDesignField);

  NewDesignControl(TDesignField, AParent, Pt, Field);
  ShowForm(Field, AParent.ClientToScreen(Pt), ForceShowForm);
end;

function TDesignFrame.NewShortCutHeadingControl(AParent: TWinControl;
  ForceShowForm: Boolean): TControl;
var
  Pt: TPoint;
  Heading: TEpiHeading;
begin
  Heading := NewHeading;
  Pt := FindNewPosition(AParent, TDesignHeading);

  NewDesignControl(TDesignHeading, AParent, Pt, Heading);
  ShowForm(Heading, AParent.ClientToScreen(Pt), ForceShowForm);
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
  EpiControl: TEpiCustomControlItem;
begin
  // Validate active control in ControlDesign form - it could be in
  //  invalid state.
  if Assigned(FDesignControlForm) and
     (not FDesignControlForm.ValidateControl) then exit;

  ExitControl(nil);
  if Not Assigned(Sender) then
    Sender := FDesignerBox;

  FActiveControl := TControl(Sender);
  if (Sender is TWinControl) then
    TWinControl(Sender).SetFocus;

  EpiControl := (FActiveControl as IDesignEpiControl).EpiControl;

  if EpiControl is TEpiSection then
    FActiveSection := TEpiSection(EpiControl)
  else
    FActiveSection := TEpiSection(EpiControl.Owner.Owner);

  if Supports(Sender, 'IPositionHandler') then
    FActiveDockSite := TWinControl(Sender)
  else
    FActiveDockSite := TControl(Sender).Parent;

  // DO NOT reposition on the "main" section, this hinders correct placement of
  // new controls using the mouse, if eg. the box is scrolled some-what down.
  if FActiveControl <> FDesignerBox then
    ComputeScrollbarPosition(FActiveControl);
  FActiveControl.Color := $00B6F5F5;

  ShowForm(EpiControl, FActiveControl.ClientToScreen(Point(0,0)), false);
  UpdateStatusbarControl(EpiControl);
end;

procedure TDesignFrame.ExitControl(Sender: TObject);
begin
  if not Assigned(FActiveControl) then exit;
  FActiveControl.Color := clWhite;
  if FActiveControl is TDesignHeading then
    TDesignHeading(FActiveControl).ParentColor := true;
  FActiveControl := nil;
end;

procedure TDesignFrame.DeleteControl(Sender: TObject; ForceDelete: Boolean);
var
  EpiCtrl: TEpiCustomControlItem;
  LocalCtrl: TControl;
  Node, NNode: TAVLTreeNode;
  YTree: TAVLTree;
  S: String;
begin
  Node := nil;

  if not ForceDelete then
  begin
    EpiCtrl := (FActiveControl as IDesignEpiControl).EpiControl;
    if EpiCtrl is TEpiSection then
      S := 'Section: ';
    if EpiCtrl is TEpiField then
      S := 'Field: ';
    if EpiCtrl is TEpiHeading then
      S := 'Heading: ';
    S += EpiCtrl.Name;

    if MessageDlg('Warning', 'Are you sure you want to delete?' + LineEnding + S,
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
  end;  // ForceDelete

  DoDeleteControl(nil);

  if DataFile.Fields.Count = 0 then
    DataFile.Size := 0;
end;

procedure TDesignFrame.DoDeleteControl(Sender: TObject);
var
  EpiCtrl: TEpiCustomControlItem;
  LocalCtrl: TControl;
  Node, NNode: TAVLTreeNode;
  YTree: TAVLTree;
begin
  MainForm.BeginUpdatingForm;

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

  MainForm.EndUpdatingForm;
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
  EnterControl(FDesignerBox);

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

  DataFile.Size := 0;
  FDesignerBox.EndUpdateBounds;
end;

procedure TDesignFrame.LMDesignerDel(var Msg: TLMessage);
var
  Ctrl: TControl;
begin
  Ctrl := TControl(Msg.WParam);
  if Ctrl = FDesignerBox then exit;
  DeleteControl(Ctrl, Msg.LParam = 1);
  Msg.Result := 1;
end;

procedure TDesignFrame.LMDesignerDelAll(var Msg: TLMessage);
begin
  DeleteAllControls;
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
  var Control: TControl; AClass: TControlClass): TPoint;
var
  YTree: TAVLTree;
  Hit: TAVLTreeNode;
  Prd: TAVLTreeNode;
begin
  // Initialization
  if AClass = TDesignField then
    Result := Point(ManagerSettings.DefaultRightPosition, 5);
  if AClass = TDesignHeading then
    Result := Point(ManagerSettings.DefaultLabelPosition, 5);

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
  FHintWindow := THintWindow.Create(Self);
  FHintWindow.AutoHide := true;
  FHintWindow.HideInterval := 5 * 1000;

  DateToolButton.Tag := Ord(ManagerSettings.DefaultDateType);
  DateToolButton.ImageIndex := Ord(ManagerSettings.DefaultDateType);
  FActiveButton := SelectorToolButton;
  FImportedFileName := '';

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
  FDesignerBox.OnMouseWheel := @DesignBoxMouseWheel;
  FActiveDockSite := FDesignerBox;

  UpdateFrame;

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
var
  EpiCtrl: TEpiCustomControlItem;
begin
  UpdateShortCuts;

  DateToolButton.Tag := Ord(ManagerSettings.DefaultDateType);
  DateToolButton.ImageIndex := Ord(ManagerSettings.DefaultDateType);

  FDesignerBox.Invalidate;

  if Assigned(FActiveControl) then
  begin
    EpiCtrl := (FActiveControl as IDesignEpiControl).EpiControl;
    ShowForm(EpiCtrl, Point(EpiCtrl.Left, EpiCtrl.Top), false);
  end;
end;

procedure TDesignFrame.RestoreDefaultPos;
begin
  if Assigned(FDesignControlForm) then
    FDesignControlForm.RestoreDefaultPos;
  TImportStructureForm.RestoreDefaultPos;
end;

end.

