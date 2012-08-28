unit design_runtimedesigner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, ExtCtrls, StdCtrls,
  JvDesignSurface, epidatafiles, LMessages, ActnList, Menus,
  manager_messages, epidatafilestypes, design_properties_form, types,
  epicustombase, epidocument, epivaluelabels, design_types;

type

  { TRuntimeDesignFrame }

  TRuntimeDesignFrame = class(TFrame)
    SelectLastAction: TAction;
    SelectFirstAction: TAction;
    SelectPgUpAction: TAction;
    SelectPriorAction: TAction;
    SelectPgDnAction: TAction;
    DeleteControlFastAction: TAction;
    NewHeadingFastAction: TAction;
    NewHeadingAction: TAction;
    NewDateFieldFastAction: TAction;
    NewDateFieldAction: TAction;
    NewStringFieldFastAction: TAction;
    NewStringFieldAction: TAction;
    NewFloatFieldFastAction: TAction;
    NewFloatFieldAction: TAction;
    NewIntFieldFastAction: TAction;
    NewIntFieldAction: TAction;
    PrintDataFormAction: TAction;
    ViewDatasetAction: TAction;
    UndoAction: TAction;
    ImportAction: TAction;
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
    DesignerActionList: TActionList;
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
    procedure DesignerActionListExecute(AAction: TBasicAction;
      var Handled: Boolean);
    procedure DesignerActionListUpdate(AAction: TBasicAction;
      var Handled: Boolean);
    procedure EditControlActionExecute(Sender: TObject);
    procedure FieldBtnClick(Sender: TObject);
    function FieldNamePrefix: string;
    procedure HeadingBtnClick(Sender: TObject);
    procedure ImportActionExecute(Sender: TObject);
    procedure JvDesignScrollBox1MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure NewDateFieldActionExecute(Sender: TObject);
    procedure NewDateFieldFastActionExecute(Sender: TObject);
    procedure NewFloatFieldActionExecute(Sender: TObject);
    procedure NewFloatFieldFastActionExecute(Sender: TObject);
    procedure NewHeadingActionExecute(Sender: TObject);
    procedure NewHeadingFastActionExecute(Sender: TObject);
    procedure NewIntFieldActionExecute(Sender: TObject);
    procedure NewIntFieldFastActionExecute(Sender: TObject);
    procedure NewStringFieldActionExecute(Sender: TObject);
    procedure NewStringFieldFastActionExecute(Sender: TObject);
    procedure PasteAsDateActionExecute(Sender: TObject);
    procedure PasteAsFloatActionExecute(Sender: TObject);
    procedure PasteAsIntActionExecute(Sender: TObject);
    procedure PasteAsStringActionExecute(Sender: TObject);
    procedure PasteAsUpdate(Sender: TObject);
    procedure PasteControlActionExecute(Sender: TObject);
    procedure PasteAsHeadingActionExecute(Sender: TObject);
    procedure PasteControlActionUpdate(Sender: TObject);
    procedure PrintDataFormActionExecute(Sender: TObject);
    procedure SectionBtnClick(Sender: TObject);
    procedure SelecterBtnClick(Sender: TObject);
    procedure SelectFirstActionExecute(Sender: TObject);
    procedure SelectLastActionExecute(Sender: TObject);
    procedure SelectNextActionExecute(Sender: TObject);
    procedure SelectPgDnActionExecute(Sender: TObject);
    procedure SelectPgUpActionExecute(Sender: TObject);
    procedure SelectPriorActionExecute(Sender: TObject);
    procedure TestToolButtonClick(Sender: TObject);
    procedure UndoActionExecute(Sender: TObject);
    procedure ViewDatasetActionExecute(Sender: TObject);
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
  private
    { Import }
    procedure PasteEpiDoc(const ImportDoc: TEpiDocument;
      RenameVL, RenameFields: boolean;
      ImportData: boolean);
  private
    { Print }
    procedure DoPrintDataForm;
  private
    { Design Controls with EpiControls }
    function NewDesignHeading(TopLeft: TPoint; Heading: TEpiHeading = nil;
      AParent: TWinControl = nil): TControl;
    function NewDesignField(TopLeft: TPoint; Field: TEpiField = nil;
      AParent: TWinControl = nil): TControl;
    function NewDesignSection(ARect: TRect; Section: TEpiSection = nil): TWinControl;
    { Design control via shortcuts }
    function NewShortCutDesignField(Ft: TEpiFieldType;
      ShowPropertiesForm: boolean): TControl;
    function NewShortCutDesignHeading(ShowPropertiesForm: boolean): TControl;
  private
    { Other }
    procedure UpdateShortcuts;
    procedure SelectControl(AAction: TDesignSelectAction);
    procedure UpdateStatusbar(ControlList: TJvDesignObjectArray);
    procedure UpdateStatusbarSizes;
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
  JvDesignImp, design_designpanel,
  Graphics, design_designcontroller, design_designmessenger,
  main, epistringutils, JvDesignUtils, settings2_var,
  manager_globals, managerprocs, Clipbrd, math,
  Dialogs, import_structure_form, epimiscutils,
  datasetviewer_frame, fpvectorial, fpvutils, FPimage,
  LCLMessageGlue, LCLType, shortcuts,
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

procedure TRuntimeDesignFrame.ImportActionExecute(Sender: TObject);
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
    ImpStructurForm.ImportData := (DataFile.Size = 0);
    if ImpStructurForm.ShowModal = mrCancel then exit;

    // Prepare screen...
    Screen.Cursor := crHourGlass;
    Application.ProcessMessages;
    MainForm.BeginUpdatingForm;

    for i := 0 to ImpStructurForm.SelectedDocuments.Count - 1 do
      PasteEpiDoc(TEpiDocument(ImpStructurForm.SelectedDocuments.Objects[i]),
        ImpStructurForm.ValueLabelsRenameGrpBox.ItemIndex = 1,
        ImpStructurForm.FieldsRenameGrpBox.ItemIndex = 1,
        ImpStructurForm.ImportDataIndex = i
      );

    FDesignPanel.Surface.Select(FDesignPanel);
    FDesignPanel.Surface.UpdateDesigner;
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

procedure TRuntimeDesignFrame.NewDateFieldActionExecute(Sender: TObject);
begin
  NewShortCutDesignField(ftDMYDate, true);
end;

procedure TRuntimeDesignFrame.NewDateFieldFastActionExecute(Sender: TObject);
begin
  NewShortCutDesignField(ftDMYDate, false);
end;

procedure TRuntimeDesignFrame.NewFloatFieldActionExecute(Sender: TObject);
begin
  NewShortCutDesignField(ftFloat, true);
end;

procedure TRuntimeDesignFrame.NewFloatFieldFastActionExecute(Sender: TObject);
begin
  NewShortCutDesignField(ftFloat, false);
end;

procedure TRuntimeDesignFrame.NewHeadingActionExecute(Sender: TObject);
begin
  NewShortCutDesignHeading(true);
end;

procedure TRuntimeDesignFrame.NewHeadingFastActionExecute(Sender: TObject);
begin
  NewShortCutDesignHeading(false);
end;

procedure TRuntimeDesignFrame.NewIntFieldActionExecute(Sender: TObject);
begin
  NewShortCutDesignField(ftInteger, true);
end;

procedure TRuntimeDesignFrame.NewIntFieldFastActionExecute(Sender: TObject);
begin
  NewShortCutDesignField(ftInteger, false);
end;

procedure TRuntimeDesignFrame.NewStringFieldActionExecute(Sender: TObject);
begin
  NewShortCutDesignField(ftString, true);
end;

procedure TRuntimeDesignFrame.NewStringFieldFastActionExecute(Sender: TObject);
begin
  NewShortCutDesignField(ftString, false);
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
  begin
    case ManagerSettings.PasteSpecialType of
      0: PasteAsHeadingAction.Execute;
      1: PasteAsField(ftInteger);
      2: PasteAsField(ftFloat);
      3: PasteAsField(ftString);
    end;
  end
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

procedure TRuntimeDesignFrame.PrintDataFormActionExecute(Sender: TObject);
begin
  DoPrintDataForm;
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
  RenameVL, RenameFields: boolean; ImportData: boolean);
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

    if not ImportData then
      F.ResetData;

    if Assigned(S) then
    begin
      // Rename if already present.
      if (not S.ValidateRename(F.Name, false))
      then
        F.Name := NewFieldName(F.Name);

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
    if Assigned(S) then
    begin
      // Rename if already present.
      if (not S.ValidateRename(H.Name, false))
      then
        H.Name := DataFile.Headings.GetUniqueItemName(TEpiHeading);

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
    if (not DataFile.Sections.ValidateRename(S.Name, false))
    then
      S.Name := DataFile.Sections.GetUniqueItemName(TEpiSection);

    S.DataFile.Sections.RemoveItem(S);
    DataFile.Sections.AddItem(S);

    WinCtrl := NewDesignSection(Bounds(S.Left, S.Top + P.Y, S.Width, S.Height), S);

    // Do not off-set within sections.
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

  // Assume only one ImportDoc has ImportData=true
  if ImportData then
    DataFile.Size := ImportDoc.DataFiles[0].Size;

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
    FDesignPanel.Surface.Select(FDesignPanel);

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

procedure TRuntimeDesignFrame.DoPrintDataForm;
var
  FieldCount: Integer;
  VecDoc: TvVectorialDocument;
  Page: TvVectorialPage;
  F: TEpiField;
  i: Integer;
  L, R, T, B, W: Double;
  SectionCount: Integer;
  S: TEpiSection;
  HeadingCount: Integer;
  H: TEpiHeading;
  SaveDlg: TSaveDialog;
  FS: Integer;
  FN: String;
  J: Integer;

const
  PIXELS_PER_MILIMETER = 3.5433;
  A4_PIXEL_HEIGHT      = trunc(297 * PIXELS_PER_MILIMETER);
  A4_PIXEL_WIDTH       = trunc(210 * PIXELS_PER_MILIMETER);

const
  FieldHeigth =
    {$IFDEF WINDOWS}
      21
    {$ELSE}
      {$IFDEF DARWIN}
      22
      {$ELSE}
        {$IFDEF LINUX}
      27
        {$ENDIF}
    {$ENDIF}
  {$ENDIF};


  function FieldTop(F: TEpiField): integer; inline;
  begin
    Result := F.Top + F.Section.Top;
  end;

  function FieldLeft(F: TEpiField): integer; inline;
  begin
    Result := F.Left + F.Section.Left;
  end;

  function HeadingTop(H: TEpiHeading): integer; inline;
  begin
    Result := H.Top + H.Section.Top;
  end;

  function HeadingLeft(H: TEpiHeading): integer; inline;
  begin
    Result := H.Left + H.Section.Left;
  end;

begin
  SaveDlg := nil;
  VecDoc  := nil;
  try
    SaveDlg := TSaveDialog.Create(Self);
    SaveDlg.InitialDir := ManagerSettings.WorkingDirUTF8;
    SaveDlg.Filter := 'Scalable Vector Graphics (*.svg)|*.svg|' + GetEpiDialogFilter([dfAll]);
    if not SaveDlg.Execute then exit;

//    if not PrintDialog1.Execute then exit;

//    Printer.BeginDoc;

{    PIXELS_PER_MILIMETER := 1 / ((1 / Printer.YDPI) * 25.4001);
    A4_PIXEL_HEIGHT      := trunc(297 * PIXELS_PER_MILIMETER);
    A4_PIXEL_WIDTH       := trunc(210 * PIXELS_PER_MILIMETER); }

    VecDoc := TvVectorialDocument.Create;
    VecDoc.Width := 210;
    VecDoc.Height := 297;
    Page := VecDoc.AddPage();
    Page.Width := VecDoc.Width;
    Page.Height := VecDoc.Height;

//    Printer.PageHeight;
    FS := ManagerSettings.FieldFont.Size;
    FN := ManagerSettings.FieldFont.Name;
    if FS = 0 then FS := 10;

    FieldCount := DataFile.Fields.Count;
    for i := 0 to FieldCount -1 do
    begin
      F := DataFile.Field[i];

      L := (FieldLeft(F) - Canvas.TextWidth(F.Question.Text)) / PIXELS_PER_MILIMETER;
      T := CanvasTextPosToFPVectorial(
             FieldTop(F) + (FieldHeigth - Canvas.TextHeight(F.Question.Text)),
             A4_PIXEL_HEIGHT,
             Canvas.TextHeight(F.Question.Text)) / PIXELS_PER_MILIMETER;
      Page.AddText(L, T, 0, FN, FS, F.Question.Text);

      L := FieldLeft(F) / PIXELS_PER_MILIMETER;
      R := (FieldLeft(F) + (F.Length * Canvas.TextWidth('W'))) / PIXELS_PER_MILIMETER;
      T := CanvasCoordsToFPVectorial(FieldTop(F) + (FieldHeigth div 2), A4_PIXEL_HEIGHT) / PIXELS_PER_MILIMETER;
      B := CanvasCoordsToFPVectorial(FieldTop(F) + FieldHeigth, A4_PIXEL_HEIGHT)/ PIXELS_PER_MILIMETER;
      Page.StartPath(L, T);
      Page.AddLineToPath(L, B);
      Page.AddLineToPath(R, B);
      Page.AddLineToPath(R, T);
      Page.AddLineToPath(L, T, FPColor($FF, $FF, $FF, $FF));
      Page.EndPath();
    end;

    HeadingCount := DataFile.Headings.Count;
    for i := 0 to HeadingCount -1 do
    begin
      H := DataFile.Heading[i];

      L := HeadingLeft(H) / PIXELS_PER_MILIMETER;
      T := CanvasTextPosToFPVectorial(HeadingTop(H), A4_PIXEL_HEIGHT, Canvas.TextHeight(H.Caption.Text)) / PIXELS_PER_MILIMETER;

      Page.AddText(L, T, 0, FN, FS, H.Caption.Text);
    end;

    SectionCount := DataFile.Sections.Count;
    for i := 0 to SectionCount - 1 do
    begin
      S := DataFile.Section[i];
      if S = DataFile.MainSection then continue;

      L := S.Left / PIXELS_PER_MILIMETER;
      R := (S.Left + S.Width)  / PIXELS_PER_MILIMETER;
      T := CanvasCoordsToFPVectorial(S.Top, A4_PIXEL_HEIGHT)  / PIXELS_PER_MILIMETER;
      B := CanvasCoordsToFPVectorial(S.Top + S.Height, A4_PIXEL_HEIGHT)  / PIXELS_PER_MILIMETER;
      W := Canvas.TextWidth(S.Caption.Text) / PIXELS_PER_MILIMETER;

      Page.StartPath(L, T);
      Page.AddLineToPath(L, B);
      Page.AddLineToPath(R, B);
      Page.AddLineToPath(R, T);
      Page.AddLineToPath(Min(L + W, R), T);
      Page.AddLineToPath(L, T, FPColor($FF, $FF, $FF, $FF));
      Page.EndPath();

      L := (S.Left + 3) / PIXELS_PER_MILIMETER;
      T := CanvasTextPosToFPVectorial(S.Top, A4_PIXEL_HEIGHT, Canvas.TextHeight(S.Caption.Text) div 2) / PIXELS_PER_MILIMETER;
      Page.AddText(L, T, 0, FN, FS, S.Caption.Text);
    end;

//    fpvtocanvas.DrawFPVectorialToCanvas(Page, Canvas, 0, 12 * Floor(Page.Height), 1, -1);
//    Printer.EndDoc;
    VecDoc.WriteToFile(SaveDlg.FileName, vfSVG);
  finally
    SaveDlg.Free;
    VecDoc.Free;
  end;
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

function TRuntimeDesignFrame.NewShortCutDesignField(Ft: TEpiFieldType;
  ShowPropertiesForm: boolean): TControl;
var
  P: TPoint;
  KeyCode: Word;
begin
  P := FindNewPostion(TDesignField);
  FLastSelectedFieldType := ft;
  Result := NewDesignField(P, nil, FDesignPanel.Surface.SelectedContainer);

  if ShowPropertiesForm then
  begin
    KeyCode := VK_RETURN;
    LCLSendKeyDownEvent(Result, KeyCode, 0, true, false);
  end;
end;

function TRuntimeDesignFrame.NewShortCutDesignHeading(
  ShowPropertiesForm: boolean): TControl;
var
  P: TPoint;
  KeyCode: Word;
begin
  P := FindNewPostion(TDesignHeading);
  Result := NewDesignHeading(P, nil, FDesignPanel.Surface.SelectedContainer);

  if ShowPropertiesForm then
  begin
    KeyCode := VK_RETURN;
    LCLSendKeyDownEvent(Result, KeyCode, 0, true, false);
  end;
end;

procedure TRuntimeDesignFrame.UpdateShortcuts;
begin
  NewIntFieldAction.ShortCut           := D_NewIntField;
  NewIntFieldFastAction.ShortCut       := D_NewIntField_Fast;
  NewFloatFieldAction.ShortCut         := D_NewFloatField;
  NewFloatFieldFastAction.ShortCut     := D_NewFloatField_Fast;
  NewStringFieldAction.ShortCut        := D_NewStringField;
  NewStringFieldFastAction.ShortCut    := D_NewStringField_Fast;
  NewDateFieldAction.ShortCut          := D_NewDateField;
  NewDateFieldFastAction.ShortCut      := D_NewDateField_Fast;
  NewHeadingAction.ShortCut            := D_NewHeading;
  NewHeadingFastAction.ShortCut        := D_NewHeading_Fast;

//  NewSectionAction.ShortCut            := D_NewSection;
  EditControlAction.ShortCut           := D_EditControl;
  DeleteControlAction.ShortCut         := D_DeleteControl;
  DeleteControlFastAction.ShortCut     := D_DeleteControl_Fast;
  DeleteAllAction.ShortCut             := D_DeleteAllControl;
  ImportAction.ShortCut                := D_ImportData;
  CopyControlAction.ShortCut           := D_CopyControl;
  PasteControlAction.ShortCut          := D_PasteControl;

  SelectFirstAction.ShortCut           := D_MoveTop;
  SelectPgUpAction.ShortCut            := D_MoveSideUp;
  SelectPriorAction.ShortCut           := D_MoveControlUp;
  SelectNextAction.ShortCut            := D_MoveControlDown;
  SelectPgDnAction.ShortCut            := D_MoveSideDown;
  SelectLastAction.ShortCut            := D_MoveBottom;
end;

procedure TRuntimeDesignFrame.SelectControl(AAction: TDesignSelectAction);
var
  EpiCtrl: TEpiCustomControlItem;
  Idx: Integer;
  Surface: TJvDesignSurface;
  CTop: Integer;
  CBot: TScrollBarInc;
  Ctrl: TControl;
  SPage: TScrollBarInc;
  SPos: Integer;

  function RelativeTop(Ctrl: TControl): integer;
  begin
    Result := FDesignPanel.ScreenToControl(Ctrl.ControlToScreen(Point(0,0))).Y;
  end;

begin
  Surface := FDesignPanel.Surface;
  if Surface.Count = 0 then
    EpiCtrl := (FDesignPanel as IDesignEpiControl).EpiControl
  else
    EpiCtrl := (Surface.Selection[0] as IDesignEpiControl).EpiControl;
  Idx := DataFile.ControlItems.IndexOf(EpiCtrl);

  case AAction of
    dsaHome:
      if DataFile.ControlItems.Count = 1 then
        Idx := 0
      else
        Idx := 1;
    dsaPgUp:
      begin
        CBot := JvDesignScrollBox1.VertScrollBar.Position + JvDesignScrollBox1.VertScrollBar.Page;
        for Idx := DataFile.ControlItems.Count - 1 downto 0 do
        begin
          Ctrl := ControlFromEpiControl(DataFile.ControlItem[Idx]);
          if (RelativeTop(Ctrl) + Ctrl.Height) < CBot then
            Break;
        end;
      end;
    dsaPrior:
      Idx := Max(0, Idx-1);
    dsaNext:
      Idx := Min(DataFile.ControlItems.Count - 1, Idx +1);
    dsaPgDn:
      begin
        CTop := JvDesignScrollBox1.VertScrollBar.Position;
        for Idx := 0 to DataFile.ControlItems.Count - 1 do
          if RelativeTop(ControlFromEpiControl(DataFile.ControlItem[Idx])) > CTop then
            Break;
      end;
    dsaEnd:
      Idx := DataFile.ControlItems.Count - 1;
  end;

  EpiCtrl := DataFile.ControlItem[Idx];
  Ctrl := ControlFromEpiControl(EpiCtrl);
  FDesignPanel.Surface.Select(Ctrl);
  FDesignPanel.Surface.SelectionChange;
  FDesignPanel.Surface.UpdateDesigner;

  // TODO : Selected control within visual area!
  CTop := RelativeTop(Ctrl);
  CBot := CTop + Ctrl.Height;
  SPos := JvDesignScrollBox1.VertScrollBar.Position;
  SPage := JvDesignScrollBox1.VertScrollBar.Page;
  case AAction of
    dsaPrior:
      begin
        if (CBot < SPos) or
           (CTop > (SPos + SPage))
        then
          // Out of bounds completely => Center view.
        begin
          JvDesignScrollBox1.VertScrollBar.Position := CTop - (SPage div 2);
          Exit;
        end;

        if (CTop < (SPos + (SPage div 4))) then
        begin
          JvDesignScrollBox1.VertScrollBar.Position := CTop - (SPage div 4);
          Exit;
        end;
      end;
    dsaNext:
      begin
        if (CBot < SPos) or
           (CTop > (SPos + SPage))
        then
          // Out of bounds completely => Center view.
        begin
          JvDesignScrollBox1.VertScrollBar.Position := CTop - (SPage div 2);
          Exit;
        end;

        if (CBot > ((SPos + SPage) - (SPage div 4))) then
        begin
          JvDesignScrollBox1.VertScrollBar.Position := (CBot - SPage) + (SPage div 4);
          Exit;
        end;
      end;
  end;
end;

procedure TRuntimeDesignFrame.UpdateStatusbar(ControlList: TJvDesignObjectArray
  );
var
  EpiCtrl: TEpiCustomControlItem;
begin
  // New "statusbar"
  RecordsLabel.Caption := IntToStr(DataFile.Size);
  SectionsLabel.Caption := IntToStr(DataFile.Sections.Count);

  // TODO : Better statusbar with multiple selected controls!
  if Length(ControlList) = 1 then
    EpiCtrl := (ControlList[0] as IDesignEpiControl).EpiControl
  else
    EpiCtrl := nil;


  if Assigned(EpiCtrl) then
    if (EpiCtrl is TEpiSection) then
      CurrentSectionLabel.Caption := TEpiSection(EpiCtrl).Caption.Text
    else
      CurrentSectionLabel.Caption := TEpiSection(EpiCtrl.Owner.Owner).Caption.Text
  else
    CurrentSectionLabel.Caption := 'N/A';

  FieldsLabel.Caption := IntToStr(DataFile.Fields.Count);

  if Assigned(EpiCtrl) then
    FieldNameLabel.Caption := EpiCtrl.Name
  else
    FieldNameLabel.Caption := 'N/A';

  if EpiCtrl is TEpiField then
  with TEpiField(EpiCtrl) do
  begin
    FieldTypeLabel.Caption    := EpiTypeNames[FieldType];
    DefaultValueLabel.Caption := BoolToStr(HasDefaultValue, DefaultValueAsString, '');
    if Assigned(ValueLabelSet) then
      ValueLabelLabel.Caption := ValueLabelSet.Name
    else
      ValueLabelLabel.Caption := '';
    RangeLabel.Caption        := BoolToStr(Assigned(Ranges), 'Range', '');
    KeyLabel.Caption          := BoolToStr(DataFile.KeyFields.ItemExistsByName(Name), 'Key', '');
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

procedure TRuntimeDesignFrame.UpdateStatusbarSizes;
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

procedure TRuntimeDesignFrame.SelecterBtnClick(Sender: TObject);
begin
  FAddClass := '';
  DoToogleBtn(Sender);
end;

procedure TRuntimeDesignFrame.SelectFirstActionExecute(Sender: TObject);
begin
  with JvDesignScrollBox1.VertScrollBar do
    Position := 0;

  SelectControl(dsaHome);
end;

procedure TRuntimeDesignFrame.SelectLastActionExecute(Sender: TObject);
begin
  with JvDesignScrollBox1.VertScrollBar do
    Position := Range;

  SelectControl(dsaEnd);
end;

procedure TRuntimeDesignFrame.SelectNextActionExecute(Sender: TObject);
begin
  SelectControl(dsaNext);
end;

procedure TRuntimeDesignFrame.SelectPgDnActionExecute(Sender: TObject);
begin
  with JvDesignScrollBox1.VertScrollBar do
    Position := Position + Page;

  SelectControl(dsaPgDn);
end;

procedure TRuntimeDesignFrame.SelectPgUpActionExecute(Sender: TObject);
begin
  with JvDesignScrollBox1.VertScrollBar do
    Position := Position - Page;

  SelectControl(dsaPgUp);
end;

procedure TRuntimeDesignFrame.SelectPriorActionExecute(Sender: TObject);
begin
  SelectControl(dsaPrior);
end;

procedure TRuntimeDesignFrame.TestToolButtonClick(Sender: TObject);
begin
  FPropertiesForm.Show;
end;

procedure TRuntimeDesignFrame.UndoActionExecute(Sender: TObject);
begin
  GlobalCommandList.Undo;
end;

procedure TRuntimeDesignFrame.ViewDatasetActionExecute(Sender: TObject);
begin
  ShowDataSetViewerForm(
    Self,
    'View Dataset:',
    DataFile);
end;

procedure TRuntimeDesignFrame.SelectionChange(Sender: TObject);
var
  Ctrl: TControl;
  EpiCtrl: TEpiCustomControlItem;
  Selection: TJvDesignObjectArray;
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

  if FDesignPanel.Surface.Count = 0 then
    Selection := DesignPanelAsJvObjectArray
  else
    Selection := FDesignPanel.Surface.Selected;

  if Assigned(FPropertiesForm) and (not FSettingDataFile) then
    FPropertiesForm.UpdateSelection(Selection);

  UpdateStatusbar(Selection);
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

procedure TRuntimeDesignFrame.DesignerActionListExecute(AAction: TBasicAction;
  var Handled: Boolean);
begin
  if Screen.ActiveCustomForm <> MainForm then
    Handled := true;
end;

procedure TRuntimeDesignFrame.DesignerActionListUpdate(AAction: TBasicAction;
  var Handled: Boolean);
begin
  //
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

  UpdateFrame;

  FPopUpPoint := Point(-1, -1);
  FSettingDataFile := false;
end;

procedure TRuntimeDesignFrame.UpdateFrame;
begin
  UpdateShortcuts;
end;

procedure TRuntimeDesignFrame.RestoreDefaultPos;
begin
  //
end;

function TRuntimeDesignFrame.IsShortCut(var Message: TLMKey): boolean;
begin
  result :=
    // Only execute our actionlist if mainform is active!
    (Screen.ActiveCustomForm = MainForm) and
    (DesignerActionList.IsShortCut(Message));

  // Else ready for implementing a larger Short-cut editor.
end;

end.

