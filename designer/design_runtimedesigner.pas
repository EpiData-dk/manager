unit design_runtimedesigner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, ExtCtrls, StdCtrls,
  JvDesignSurface, epidatafiles, LMessages, ActnList, Menus,
  manager_messages, epidatafilestypes, design_properties_form, types,
  epicustombase, epidocument, epivaluelabels;

type

  { TRuntimeDesignFrame }

  TRuntimeDesignFrame = class(TFrame)
    ImportStructureAction: TAction;
    SelectNextAction: TAction;
    PasteAsDateMenuItem: TMenuItem;
    PasteAsFloatMenuItem: TMenuItem;
    PasteAsStringMenuItem: TMenuItem;
    PasteAsDateAction: TAction;
    PasteAsStringAction: TAction;
    PasteAsFloatAction: TAction;
    PasteAsIntMenuItem: TMenuItem;
    PasteAsIntAction: TAction;
    PasteAsHeadingMenuItem: TMenuItem;
    PopupMenuDivider3: TMenuItem;
    PasteAsHeadingAction: TAction;
    ClearSelectionAction: TAction;
    CutControlAction: TAction;
    OpenProjectToolBtn: TToolButton;
    PasteControlPopupMenuItem: TMenuItem;
    PasteControlAction: TAction;
    CopyControlAction: TAction;
    CopyControlPopupMenuItem: TMenuItem;
    DeleteAllAction: TAction;
    DeleteControlAction: TAction;
    DeletePopupMenuItem: TMenuItem;
    DesignControlPopUpMenu: TPopupMenu;
    EditControlAction: TAction;
    ActionList1: TActionList;
    Button1: TButton;
    CurrentSectionLabel: TLabel;
    CurrentSectionPanel: TPanel;
    DateToolButton: TToolButton;
    DefaultValueLabel: TLabel;
    DefaultValuePanel: TPanel;
    DeleteAllToolButton: TToolButton;
    DeleteToolButton: TToolButton;
    DesignerImageList: TImageList;
    DesignerToolBar: TToolBar;
    Divider1: TToolButton;
    Divider2: TToolButton;
    Divider3: TToolButton;
    Divider4: TToolButton;
    Divider5: TToolButton;
    Divider6: TToolButton;
    EditPopupMenuItem: TMenuItem;
    EditToolButton: TToolButton;
    ExportToolButton: TToolButton;
    ExtendedLabel: TLabel;
    ExtendedPanel: TPanel;
    FieldNameLabel: TLabel;
    FieldNamePanel: TPanel;
    FieldsLabel: TLabel;
    FieldsPanel: TPanel;
    FieldsStaticLabel: TLabel;
    FieldTypeLabel: TLabel;
    FieldTypePanel: TPanel;
    FloatToolButton: TToolButton;
    HeadingToolButton: TToolButton;
    ImportToolButton: TToolButton;
    IntToolButton: TToolButton;
    JvDesignScrollBox1: TJvDesignScrollBox;
    KeyLabel: TLabel;
    KeyPanel: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    LoadToolButton: TToolButton;
    NewAutoIncMenu: TMenuItem;
    NewBooleanMenu: TMenuItem;
    NewDMYTodayFieldMenu: TMenuItem;
    NewMDYTodayFieldMenu: TMenuItem;
    NewTimeFieldMenu: TMenuItem;
    NewTimeNowFieldMenu: TMenuItem;
    NewUpperCaseMenu: TMenuItem;
    NewYMDTodayFieldMenu: TMenuItem;
    OtherFieldsPopup: TPopupMenu;
    OtherToolButton: TToolButton;
    Panel1: TPanel;
    PasteControPopupMenuItem: TMenuItem;
    PopupMenuDivider1: TMenuItem;
    PopupMenuDivider2: TMenuItem;
    ProjectDivider1: TToolButton;
    ProjectToolBar: TToolBar;
    RangeLabel: TLabel;
    RangePanel: TPanel;
    RecordsLabel: TLabel;
    RecordsPanel: TPanel;
    RecordStaticLabel: TLabel;
    SaveProjectAsToolBtn: TToolButton;
    SaveProjectToolBtn: TToolButton;
    SectionsLabel: TLabel;
    SectionsPanel: TPanel;
    SectionsStaticLabel: TLabel;
    SectionToolButton: TToolButton;
    SelectorToolButton: TToolButton;
    Splitter1: TSplitter;
    StatusBarPanel: TPanel;
    StringToolButton: TToolButton;
    TestToolButton: TToolButton;
    TimeSubMenu: TMenuItem;
    TodayDateSubMenu: TMenuItem;
    ToolButton1: TToolButton;
    ValueLabelLabel: TLabel;
    ValueLabelPanel: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure ClearSelectionActionExecute(Sender: TObject);
    procedure CopyControlActionExecute(Sender: TObject);
    procedure CutCopyControlUpdate(Sender: TObject);
    procedure CutControlActionExecute(Sender: TObject);
    procedure DeleteAllActionExecute(Sender: TObject);
    procedure DeleteControlActionExecute(Sender: TObject);
    procedure DesignControlPopUpMenuPopup(Sender: TObject);
    procedure EditControlActionExecute(Sender: TObject);
    procedure FieldBtnClick(Sender: TObject);
    function FieldNamePrefix: string;
    procedure HeadingBtnClick(Sender: TObject);
    procedure ImportStructureActionExecute(Sender: TObject);
    procedure JvDesignScrollBox1MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure PasteAsDateActionExecute(Sender: TObject);
    procedure PasteAsFloatActionExecute(Sender: TObject);
    procedure PasteAsIntActionExecute(Sender: TObject);
    procedure PasteAsStringActionExecute(Sender: TObject);
    procedure PasteAsUpdate(Sender: TObject);
    procedure PasteControlActionExecute(Sender: TObject);
    procedure PasteAsHeadingActionExecute(Sender: TObject);
    procedure PasteControlActionUpdate(Sender: TObject);
    procedure SectionBtnClick(Sender: TObject);
    procedure SelecterBtnClick(Sender: TObject);
    procedure SelectNextActionExecute(Sender: TObject);
    procedure TestToolButtonClick(Sender: TObject);
  private
    FPopUpPoint: TPoint;
    FDatafile: TEpiDataFile;
    FActiveButton: TToolButton;
    FDesignPanel: TJvDesignPanel;
    FAddClass: string;
    FImportedFileName: string;
    FLastSelectedFieldType: TEpiFieldType;
    FPropertiesForm: TPropertiesForm;
    FSettingDataFile: boolean;
    FCreatingControl: boolean;
    FCheckingClipBoard: boolean;
    procedure PasteAsField(Ft: TEpiFieldType);
    function ControlFromEpiControl(EpiCtrl: TEpiCustomControlItem): TControl;
    function FindNewPostion(NewControl: TControlClass): TPoint;
    procedure GetAddClass(Sender: TObject; var ioClass: string);
    procedure SelectionChange(Sender: TObject);
    procedure DoToogleBtn(Sender: TObject);
    function DesignPanelAsJvObjectArray: TJvDesignObjectArray;
    procedure ApplyCommonCtrlSetting(Ctrl: TControl;
      EpiCtrl: TEpiCustomControlItem);
    procedure SectionsChangeEvent(Sender: TObject; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
    function ClipBoardHasText: boolean;
    procedure PasteEpiDoc(const ImportDoc: TEpiDocument; RenameVL, RenameFields: boolean);
  private
    { Design Controls with EpiControls }
    function NewDesignHeading(TopLeft: TPoint; Heading: TEpiHeading = nil;
      AParent: TWinControl = nil): TControl;
    function NewDesignField(TopLeft: TPoint; Field: TEpiField = nil;
      AParent: TWinControl = nil): TControl;
    function NewDesignSection(ARect: TRect; Section: TEpiSection = nil): TWinControl;
  protected
    function GetDataFile: TEpiDataFile;
    procedure SetDataFile(AValue: TEpiDataFile);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure   UpdateFrame;
    procedure   RestoreDefaultPos;
    function    IsShortCut(var Message: TLMKey): boolean;
    property DataFile: TEpiDataFile read GetDataFile write SetDataFile;
    property ImportedFileName: string read FImportedFileName;
  end;

implementation

{$R *.lfm}

uses
  JvDesignImp, design_types, design_designpanel,
  Graphics, design_designcontroller, design_designmessenger,
  main, epistringutils, JvDesignUtils, settings2_var,
  manager_globals, managerprocs, Clipbrd, math,
  Dialogs, import_structure_form, epimiscutils,
  design_control_section,
  design_control_field,
  design_control_heading;


{ TRuntimeDesignFrame }

procedure TRuntimeDesignFrame.DoToogleBtn(Sender: TObject);
begin
  if not (Sender is TToolButton) then exit;
  FActiveButton.Down := false;

  if FActiveButton = OtherToolButton then
    OtherToolButton.ImageIndex := 7;

  TToolButton(Sender).Down := true;
  FActiveButton := TToolButton(Sender);
end;

function TRuntimeDesignFrame.DesignPanelAsJvObjectArray: TJvDesignObjectArray;
begin
  SetLength(Result, 1);
  Result[0] := FDesignPanel;
end;

procedure TRuntimeDesignFrame.ApplyCommonCtrlSetting(Ctrl: TControl; EpiCtrl: TEpiCustomControlItem);
begin
  (Ctrl as IDesignEpiControl).EpiControl := EpiCtrl;
  Ctrl.PopupMenu := DesignControlPopUpMenu;
end;


procedure TRuntimeDesignFrame.GetAddClass(Sender: TObject; var ioClass: string);
begin
//  if FActiveButton = SelectorToolButton then exit;

  // Disallow Section-in-Section
  if (TDesignController(TJvDesignSurface(Sender).Controller).Clicked is TDesignSection) and
     (FActiveButton = SectionToolButton)
  then
    FAddClass := '';

  if FAddClass = 'TDesignField' then
    FLastSelectedFieldType := TEpiFieldType(FActiveButton.Tag);

  ioClass := FAddClass;

  if ioClass <> '' then
    FCreatingControl := true;

  SelectorToolButton.Click;
end;

procedure TRuntimeDesignFrame.FieldBtnClick(Sender: TObject);
begin
  FAddClass := 'TDesignField';
  DoToogleBtn(Sender);
end;

function TRuntimeDesignFrame.FieldNamePrefix: string;
begin
  Result := ManagerSettings.FieldNamePrefix;
end;

procedure TRuntimeDesignFrame.HeadingBtnClick(Sender: TObject);
begin
  FAddClass := 'TDesignHeading';
  DoToogleBtn(Sender);
end;

procedure TRuntimeDesignFrame.ImportStructureActionExecute(Sender: TObject);
var
  Dlg: TOpenDialog;
  ImpStructurForm: TImportStructureForm;
  i: Integer;
begin
  Dlg := nil;
  ImpStructurForm := nil;
  try
    Dlg := TOpenDialog.Create(Self);
    Dlg.Title := 'Add structure from existing file(s)';
    Dlg.InitialDir := ManagerSettings.WorkingDirUTF8;
    Dlg.Filter := GetEpiDialogFilter(dfImport + [dfCollection]);
    Dlg.Options := [ofAllowMultiSelect, ofFileMustExist, ofEnableSizing, ofViewDetail];
    if not Dlg.Execute then exit;

    ImpStructurForm := TImportStructureForm.Create(JvDesignScrollBox1, Dlg.Files);
    if ImpStructurForm.ShowModal = mrCancel then exit;

    // Prepare screen...
    Screen.Cursor := crHourGlass;
    Application.ProcessMessages;
    MainForm.BeginUpdatingForm;

    for i := 0 to ImpStructurForm.SelectedDocuments.Count - 1 do
      PasteEpiDoc(TEpiDocument(ImpStructurForm.SelectedDocuments.Objects[i]),
        ImpStructurForm.ValueLabelsRenameGrpBox.ItemIndex=1, ImpStructurForm.FieldsRenameGrpBox.ItemIndex=1);

  finally
    MainForm.EndUpdatingForm;
    Screen.Cursor := crDefault;
    Application.ProcessMessages;

    Dlg.Free;
    ImpStructurForm.Free;
  end;
end;

procedure TRuntimeDesignFrame.JvDesignScrollBox1MouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  with JvDesignScrollBox1.VertScrollBar do
    Position := Position - WheelDelta;
  Handled := true;
end;

procedure TRuntimeDesignFrame.PasteAsDateActionExecute(Sender: TObject);
begin
  PasteAsField(ftDMYDate);
end;

procedure TRuntimeDesignFrame.PasteAsFloatActionExecute(Sender: TObject);
begin
  PasteAsField(ftFloat);
end;

procedure TRuntimeDesignFrame.PasteAsIntActionExecute(Sender: TObject);
begin
  PasteAsField(ftInteger);
end;

procedure TRuntimeDesignFrame.PasteAsStringActionExecute(Sender: TObject);
begin
  PasteAsField(ftString);
end;

procedure TRuntimeDesignFrame.PasteAsUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled :=
    (ClipBoardHasText) and
    (FDesignPanel.Surface.Count = 1) and
    (csAcceptsControls in FDesignPanel.Surface.Selection[0].ControlStyle);
end;

procedure TRuntimeDesignFrame.PasteAsField(Ft: TEpiFieldType);
var
  Cbl: TStringList;
  P: TPoint;
  Controller: TDesignController;
  Surface: TJvDesignSurface;
  i: Integer;
  F: TDesignField;
begin
  Cbl := TStringList.Create;

  if (FPopUpPoint.X = -1) and
     (FPopUpPoint.Y = -1)
  then
    P := FindNewPostion(TDesignField)
  else
    P := FDesignPanel.ScreenToClient(FPopUpPoint);

  FLastSelectedFieldType := Ft;
  try
    MainForm.BeginUpdatingForm;
    ReadClipBoard(Cbl);
    for i := 0 to Cbl.Count -1 do
      begin
        if Trim(Cbl[i]) = '' then continue;

        F := TDesignField(NewDesignField(P));
        TEpiField(F.EpiControl).Question.Text := Cbl[i];

        Inc(P.Y, ManagerSettings.SpaceBtwFieldField + F.Height);
      end;
  finally
    Cbl.Free;
    FPopUpPoint := Point(-1, -1);
    MainForm.EndUpdatingForm;
  end;
end;

function TRuntimeDesignFrame.ControlFromEpiControl(
  EpiCtrl: TEpiCustomControlItem): TControl;


  function RecursiveFindControl(WinControl: TWinControl): TControl;
  var
    i: Integer;
  begin
    for i := 0 to WinControl.ControlCount - 1 do
    with WinControl do
      begin
        if Supports(Controls[i], IDesignEpiControl) and
           ((Controls[i] as IDesignEpiControl).EpiControl = EpiCtrl)
        then
          Exit(Controls[i]);

        if (Controls[i].InheritsFrom(TWinControl)) then
          Result := RecursiveFindControl(TWinControl(Controls[i]));

        if Assigned(Result) then
          Exit;
      end;

    Result := nil;
  end;

begin
  Result := RecursiveFindControl(JvDesignScrollBox1);
end;

function TRuntimeDesignFrame.FindNewPostion(NewControl: TControlClass): TPoint;
var
  CI: TEpiCustomControlItem;
  Dist: Integer;
begin
  CI := DataFile.ControlItem[DataFile.ControlItems.Count-1];
  if (CI is TEpiSection) then
    if CI = DataFile.MainSection then
      Result := Point(ManagerSettings.DefaultRightPosition, 20)
    else
      Result := Point(CI.Left, CI.Top + TEpiSection(CI).Height)
  else
    Result := Point(CI.Left, CI.Top + ControlFromEpiControl(CI).Height);

  Dist := ManagerSettings.SpaceBtwFieldLabel;
  if (NewControl = TDesignField) and (CI is TEpiField) then
    Dist := ManagerSettings.SpaceBtwFieldField;
  if (NewControl = TDesignHeading) and (CI is TEpiHeading) then
    Dist := ManagerSettings.SpaceBtwLabelLabel;

  Inc(Result.Y, Dist);
end;

procedure TRuntimeDesignFrame.PasteControlActionExecute(Sender: TObject);
var
  P: TPoint;
  R: TRect;
  i: Integer;
  O: TPoint;

  function TopLeftSelectionPoint: TPoint;
  var
    i: integer;
  begin
    Result.X := MaxInt;
    Result.Y := MaxInt;
    for i := 0 to FDesignPanel.Surface.Count - 1 do
    begin
      Result.X := Min(Result.X, FDesignPanel.Surface.Selection[i].Left);
      Result.Y := Min(Result.Y, FDesignPanel.Surface.Selection[i].Top);
    end;
  end;

begin
  if ClipBoardHasText then
    PasteAsHeadingAction.Execute
  else
  with FDesignPanel.Surface do
    begin
      MainForm.BeginUpdatingForm;
      P := SelectedContainer.ScreenToClient(FPopUpPoint);
      PasteComponents;

      O := TopLeftSelectionPoint;
      P := Point(P.X - O.X, P.Y - O.Y);
      for i := 0 to Count - 1 do
        begin
          R := Selection[i].BoundsRect;
          OffsetRect(R, P.X, P.Y);
          Selection[i].BoundsRect := R;
        end;

      MainForm.EndUpdatingForm;
    end;

  FPopUpPoint := Point(-1, -1);
end;

procedure TRuntimeDesignFrame.PasteAsHeadingActionExecute(Sender: TObject);
var
  Cbl: TStringList;
  P: TPoint;
  Controller: TDesignController;
  Surface: TJvDesignSurface;
  i: Integer;
  L: TDesignHeading;
begin
  Cbl := TStringList.Create;

  if (FPopUpPoint.X = -1) and
     (FPopUpPoint.Y = -1)
  then
    P := FindNewPostion(TDesignHeading)
  else
    P := FDesignPanel.ScreenToClient(FPopUpPoint);
  try
//    MainForm.BeginUpdatingForm;
    ReadClipBoard(Cbl);
    for i := 0 to Cbl.Count -1 do
      begin
        if Trim(Cbl[i]) = '' then continue;

        L := TDesignHeading(NewDesignHeading(P));
        TEpiHeading(L.EpiControl).Caption.Text := Cbl[i];

        Inc(P.Y, ManagerSettings.SpaceBtwLabelLabel + L.Height);
      end;
  finally
//    MainForm.EndUpdatingForm;
    Cbl.Free;
    FPopUpPoint := Point(-1, -1);
  end;
end;

procedure TRuntimeDesignFrame.PasteControlActionUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled :=
    (ClipBoardHasText or Clipboard.HasFormat(CF_Component)) and
    (FDesignPanel.Surface.Count = 1) and
    (csAcceptsControls in FDesignPanel.Surface.Selection[0].ControlStyle);
end;

procedure TRuntimeDesignFrame.SectionBtnClick(Sender: TObject);
begin
  FAddClass := 'TDesignSection';
  DoToogleBtn(Sender);
end;

procedure TRuntimeDesignFrame.SectionsChangeEvent(Sender: TObject;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  if not (EventGroup = eegCustomBase) then exit;

  case TEpiCustomChangeEventType(EventType) of
    ecceDestroy: ;
    ecceUpdate: ;
    ecceName: ;
    ecceSetItem,
    ecceAddItem:
      begin
        // New section was added!
        TEpiSection(Data).Fields.OnGetPrefix := @FieldNamePrefix;
      end;
    ecceDelItem: ;
    ecceSetTop: ;
    ecceSetLeft: ;
    ecceText: ;
  end;
end;

function TRuntimeDesignFrame.ClipBoardHasText: boolean;
begin
  FCheckingClipBoard := true;
  result := Clipboard.HasFormat(CF_Text);
  FCheckingClipBoard := false;
end;

procedure TRuntimeDesignFrame.PasteEpiDoc(const ImportDoc: TEpiDocument;
  RenameVL, RenameFields: boolean);
var
  i: Integer;
  VLSet: TEpiValueLabelSet;
  VLSetOld: TEpiValueLabelSet;
  NSection: TEpiSection;
  P: TPoint;
  Selected: TWinControl;
  C: TEpiCustomControlItem;
  j: Integer;

  function NewFieldName(Const OldName: string): string;
  var
    i: integer;
  begin
    Result := OldName;
    if DataFile.ValidateRename(OldName) then exit;

    i := 0;
    repeat
      inc(i);
      Result := OldName + IntToStr(i);
    until DataFile.ValidateRename(Result);
  end;

  procedure AssignField(F: TEpiField; S: TEpiSection = nil; AParent: TWinControl = nil);
  begin
    if (DataFile.Fields.ItemExistsByName(F.Name)) and
       (not RenameFields) and
       (F.FieldType = DataFile.Fields.FieldByName[F.Name].FieldType)
    then
      // Keep first, drop rest - strategy!
      Exit;

    // Rename if already present.
    if DataFile.Fields.ItemExistsByName(F.Name)
    then
      F.Name := NewFieldName(F.Name);

    if Assigned(S) then
    begin
      // Remove from old section...
      F.Section.Fields.RemoveItem(F);
      // Insert into new...
      S.Fields.AddItem(F);
    end;

    if (Assigned(F.ValueLabelSet)) and
       (F.ValueLabelSet.ObjectData <> 0)
    then
      F.ValueLabelSet := TEpiValueLabelSet(F.ValueLabelSet.ObjectData);

    NewDesignField(Point(F.Left, F.Top + P.Y), F, AParent);
  end;

  procedure AssignHeading(H: TEpiHeading; S: TEpiSection = nil; AParent: TWinControl = nil);
  begin
    // Rename if already present.
    if DataFile.Headings.ItemExistsByName(H.Name)
    then
      H.Name := DataFile.Headings.GetUniqueItemName(TEpiHeading);

    if Assigned(S) then
    begin
      // Remove from old section...
      H.Section.Headings.RemoveItem(H);
      // Insert into new...
      S.Headings.AddItem(H);
    end;

    NewDesignHeading(Point(H.Left, H.Top + P.Y), H, AParent);
  end;

  procedure AssignSection(S: TEpiSection);
  var
    i: integer;
    WinCtrl: TWinControl;
    OldPY: LongInt;
  begin
    // Rename if already present.
    if DataFile.Sections.ItemExistsByName(S.Name)
    then
      S.Name := DataFile.Sections.GetUniqueItemName(TEpiSection);

    S.DataFile.Sections.RemoveItem(S);
    DataFile.Sections.AddItem(S);

    WinCtrl := NewDesignSection(Bounds(S.Left, S.Top + P.Y, S.Width, S.Height), S);

    OldPY := P.Y;
    P.Y := 0;

    for i := 0 to S.Fields.Count - 1 do
      AssignField(S.Fields[i], nil, WinCtrl);

    for i := 0 to S.Headings.Count - 1 do
      AssignHeading(S.Heading[i], nil, WinCtrl);

    P.Y := OldPY;
  end;

begin
  ImportDoc.BeginUpdate;
  DataFile.BeginUpdate;

  for i := 0 to ImportDoc.ValueLabelSets.Count - 1 do
  begin
    VLSet := ImportDoc.ValueLabelSets[i];
    if (DataFile.ValueLabels.ItemExistsByName(VLSet.Name)) and
       (not RenameVL) and
       (VLSet.LabelType = DataFile.ValueLabels.GetValueLabelSetByName(VLSet.Name).LabelType)
    then
      begin
        VLSet.ObjectData := PtrUInt(DataFile.ValueLabels.GetValueLabelSetByName(VLSet.Name));
        Continue;
      end;

    // TODO : Remove from Old EpiDocument?
    DataFile.ValueLabels.AddItem(VLSet);
  end;

  P := FindNewPostion(TDesignField);
  // 1 Item = MainSection...
  if DataFile.ControlItems.Count = 1 then
    P.Y := 0;

  // Add all fields/headings belonging to ImportDoc to our DF.
  with ImportDoc.DataFiles[0] do
  begin
    for i := MainSection.Fields.Count -1 downto 0 do
      AssignField(MainSection.Field[i], DataFile.MainSection, FDesignPanel);


    for i := MainSection.Headings.Count -1 downto 0 do
      AssignHeading(MainSection.Heading[i], DataFile.MainSection, FDesignPanel);


    for i := Sections.Count - 1 downto 0 do
    if Section[i] <> MainSection then
      AssignSection(Section[i]);
  end;

  DataFile.EndUpdate;
  ImportDoc.EndUpdate;
end;

function TRuntimeDesignFrame.NewDesignHeading(TopLeft: TPoint;
  Heading: TEpiHeading; AParent: TWinControl): TControl;
var
  Controller: TDesignController;
  Surface: TJvDesignSurface;
begin
  Controller := TDesignController(FDesignPanel.Surface.Controller);
  Surface    := FDesignPanel.Surface;

  try
    if Assigned(Parent) then
      TopLeft := DesignClientToParent(TopLeft, AParent, FDesignPanel);

    FCreatingControl := not Assigned(Heading);
    Controller.SetDragRect(Rect(TopLeft.X, TopLeft.Y, 0, 0));
    Surface.AddClass := 'TDesignHeading';
    Surface.AddComponent;

    Result := TDesignHeading(Surface.Selection[0]);
    Result.PopupMenu := DesignControlPopUpMenu;

    if Assigned(Heading) then
      TDesignHeading(Result).EpiControl := Heading;
  finally
    Controller.ClearDragRect;
  end;
end;

function TRuntimeDesignFrame.NewDesignField(TopLeft: TPoint; Field: TEpiField;
  AParent: TWinControl): TControl;
var
  Controller: TDesignController;
  Surface: TJvDesignSurface;
begin
  Controller := TDesignController(FDesignPanel.Surface.Controller);
  Surface    := FDesignPanel.Surface;

  try
    if Assigned(Parent) then
      TopLeft := DesignClientToParent(TopLeft, AParent, FDesignPanel);

    FCreatingControl := not Assigned(Field);
    Controller.SetDragRect(Rect(TopLeft.X, TopLeft.Y, 0, 0));
    Surface.AddClass := 'TDesignField';
    Surface.AddComponent;

    Result := TDesignField(Surface.Selection[0]);
    Result.PopupMenu := DesignControlPopUpMenu;

    if Assigned(Field) then
      TDesignField(Result).EpiControl := Field;
  finally
    Controller.ClearDragRect;
  end;
end;

function TRuntimeDesignFrame.NewDesignSection(ARect: TRect; Section: TEpiSection
  ): TWinControl;
var
  Controller: TDesignController;
  Surface: TJvDesignSurface;
begin
  Controller := TDesignController(FDesignPanel.Surface.Controller);
  Surface    := FDesignPanel.Surface;

  try
    Surface.Select(FDesignPanel);
    FCreatingControl := not Assigned(Section);
    Controller.SetDragRect(ARect);
    Surface.AddClass := 'TDesignSection';
    Surface.AddComponent;

    Result := TDesignSection(Surface.Selection[0]);
    Result.PopupMenu := DesignControlPopUpMenu;

    if Assigned(Section) then
      TDesignSection(Result).EpiControl := Section;
  finally
    Controller.ClearDragRect;
  end;
end;

procedure TRuntimeDesignFrame.SelecterBtnClick(Sender: TObject);
begin
  FAddClass := '';
  DoToogleBtn(Sender);
end;

procedure TRuntimeDesignFrame.SelectNextActionExecute(Sender: TObject);
var
  EpiCtrl: TEpiCustomControlItem;
  Idx: Integer;
begin
  // TODO : Make generic for up/down.
  EpiCtrl := (FDesignPanel.Surface.Selection[0] as IDesignEpiControl).EpiControl;
  Idx := DataFile.ControlItems.IndexOf(EpiCtrl) +1;
  EpiCtrl := DataFile.ControlItem[Idx];
  FDesignPanel.Surface.Select(ControlFromEpiControl(EpiCtrl));
  FDesignPanel.Surface.SelectionChange;
  FDesignPanel.Surface.UpdateDesigner;
end;

procedure TRuntimeDesignFrame.TestToolButtonClick(Sender: TObject);
begin
  FPropertiesForm.Show;
end;

procedure TRuntimeDesignFrame.SelectionChange(Sender: TObject);
var
  Ctrl: TControl;
  EpiCtrl: TEpiCustomControlItem;
begin
  Label4.Caption := 'Selection Count: ' + IntToStr(FDesignPanel.Surface.Count);

  if (FCreatingControl) and
     (not FSettingDataFile)
  then
    begin
      MainForm.BeginUpdatingForm;
      Ctrl := FDesignPanel.Surface.Selection[0];

      if Ctrl is TDesignField then
        begin
          EpiCtrl := TEpiSection((Ctrl.Parent as IDesignEpiControl).EpiControl).NewField(FLastSelectedFieldType);
          case FLastSelectedFieldType of
            ftInteger,
            ftAutoInc:
              TEpiField(EpiCtrl).Length := ManagerSettings.IntFieldLength;
            ftFloat:
              begin
                TEpiField(EpiCtrl).Length := ManagerSettings.FloatIntLength +
                                             ManagerSettings.FloatDecimalLength + 1;
                TEpiField(EpiCtrl).Decimals := ManagerSettings.FloatDecimalLength;
              end;
            ftString,
            ftUpperString:
              TEpiField(EpiCtrl).Length := ManagerSettings.StringFieldLength;
          end;
        end;
      if Ctrl is TDesignHeading then
        begin
          EpiCtrl := TEpiSection((Ctrl.Parent as IDesignEpiControl).EpiControl).NewHeading;
          TEpiHeading(EpiCtrl).Caption.Text := '(untitled)';
        end;
      if Ctrl is TDesignSection then
        EpiCtrl := DataFile.NewSection;

      ApplyCommonCtrlSetting(Ctrl, EpiCtrl);
      FCreatingControl := false;

      MainForm.EndUpdatingForm;
    end;


  if Assigned(FPropertiesForm) and (not FSettingDataFile) then
    if FDesignPanel.Surface.Count = 0 then
      FPropertiesForm.UpdateSelection(DesignPanelAsJvObjectArray)
    else
      FPropertiesForm.UpdateSelection(FDesignPanel.Surface.Selected);
end;

procedure TRuntimeDesignFrame.EditControlActionExecute(Sender: TObject);
begin
  FPropertiesForm.Show;
  FPropertiesForm.SetFocus;
end;

procedure TRuntimeDesignFrame.DeleteControlActionExecute(Sender: TObject);
begin
  if FDesignPanel.Surface.Selector.IsSelected(FDesignPanel)
  then
    FDesignPanel.Surface.Selector.RemoveFromSelection(FDesignPanel);
  FDesignPanel.Surface.DeleteComponents;
end;

procedure TRuntimeDesignFrame.DesignControlPopUpMenuPopup(Sender: TObject);
var
  P: TPoint;
  Ctrl: TControl;
begin
  FPopUpPoint := TPopupMenu(Sender).PopupPoint;

  P := FDesignPanel.ScreenToClient(FPopUpPoint);
  with FDesignPanel.Surface do
  begin
    Ctrl := FindControl(P.X, P.Y);

    if not Selector.IsSelected(Ctrl) then
    begin
      Select(Ctrl);
      SelectionChange;
      UpdateDesigner;
    end;
  end;
end;

procedure TRuntimeDesignFrame.DeleteAllActionExecute(Sender: TObject);
begin
  FDesignPanel.Surface.Clear;
end;

procedure TRuntimeDesignFrame.CopyControlActionExecute(Sender: TObject);
begin
  GlobalCopyListClear;
  FDesignPanel.Surface.CopyComponents;
end;

procedure TRuntimeDesignFrame.CutCopyControlUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled :=
    (FDesignPanel.Surface.Count > 0) and
    (not FDesignPanel.Surface.Selector.IsSelected(FDesignPanel));
end;

procedure TRuntimeDesignFrame.Button1Click(Sender: TObject);
var
  Fs: TFileStream;
  S: String;
begin
  Fs := TFileStream.Create('/tmp/df.xml', fmCreate);
  S := DataFile.SaveToXml('', 0);
  Fs.Write(S[1], Length(S));
  Fs.Free;
end;

procedure TRuntimeDesignFrame.ClearSelectionActionExecute(Sender: TObject);
begin
  FDesignPanel.Surface.ClearSelection;
  FDesignPanel.Surface.SelectionChange;
end;

procedure TRuntimeDesignFrame.CutControlActionExecute(Sender: TObject);
begin
  GlobalCopyListClear;
  FDesignPanel.Surface.CutComponents;
end;

function TRuntimeDesignFrame.GetDataFile: TEpiDataFile;
begin
  result := FDatafile;
end;

procedure TRuntimeDesignFrame.SetDataFile(AValue: TEpiDataFile);
var
  i: Integer;
  Controller: TDesignController;
  Selected: TWinControl;
  S: TEpiSection;
  Surface: TJvDesignSurface;
  F: TEpiField;
  H: TEpiHeading;
  j: Integer;
  P: TPoint;
begin
  FDatafile := AValue;
  FDatafile.Sections.RegisterOnChangeHook(@SectionsChangeEvent, true);
  FDatafile.MainSection.Fields.OnGetPrefix := @FieldNamePrefix;
  (FDesignPanel as IDesignEpiControl).EpiControl := FDatafile.MainSection;

  FDesignPanel.Active := true;
  TJvDesignSelector(FDesignPanel.Surface.Selector).HandleWidth := 4;

  Controller := TDesignController(FDesignPanel.Surface.Controller);
  Surface    := FDesignPanel.Surface;

  FSettingDataFile := true;
  MainForm.BeginUpdatingForm;
  With DataFile do
  begin
    for i := 0 to Sections.Count - 1 do
    begin
      S := Section[i];

      if S <> MainSection then
        Selected := NewDesignSection(Bounds(S.Left, S.Top, S.Width, S.Height), S)
      else
        Selected := FDesignPanel;

      ApplyCommonCtrlSetting(Selected, S);

      for j := 0 to S.Fields.Count - 1 do
        begin
          F := S.Field[j];
          NewDesignField(Point(F.Left, F.Top), F, Selected);
        end;

      for j := 0 to S.Headings.Count - 1 do
        begin
          H := S.Heading[j];
          NewDesignHeading(Point(H.Left, H.Top), H, Selected);
        end;
    end;
  end;
  MainForm.EndUpdatingForm;

  Controller.ClearDragRect;
  Surface.Select(FDesignPanel);
  FSettingDataFile := false;
end;

constructor TRuntimeDesignFrame.Create(TheOwner: TComponent);
var
  ScrollBox: TJvDesignScrollBox;
begin
  inherited Create(TheOwner);
  FDesignPanel := TDesignPanel.Create(Self);
  FDesignPanel.OnGetAddClass := @GetAddClass;
  FDesignPanel.OnSelectionChange := @SelectionChange;
  FDesignPanel.Align := alClient;
  FDesignPanel.Color := clWhite;
  FDesignPanel.Parent := JvDesignScrollBox1;
  FActiveButton := SelectorToolButton;
  FDesignPanel.Surface.ControllerClass := TDesignController;
  FDesignPanel.Surface.MessengerClass := TDesignMessenger;

  FPropertiesForm := TPropertiesForm.Create(Self);
  FPropertiesForm.UpdateSelection(nil);

  FPopUpPoint := Point(-1, -1);
  FSettingDataFile := false;
end;

procedure TRuntimeDesignFrame.UpdateFrame;
begin
  //
end;

procedure TRuntimeDesignFrame.RestoreDefaultPos;
begin
  //
end;

function TRuntimeDesignFrame.IsShortCut(var Message: TLMKey): boolean;
begin
//  ActionList1.State := asNormal;

  result := ActionList1.IsShortCut(Message);

//  ActionList1.State := asSuspended;
  // Else ready for implementing a larger Short-cut editor.
end;

end.

