unit design_runtimedesigner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, PrintersDlgs, Forms, Controls, ComCtrls,
  ExtCtrls, StdCtrls, JvDesignSurface, epidatafiles, LMessages, ActnList, Menus,
  Buttons, manager_messages, epidatafilestypes, design_properties_form, types,
  epicustombase, epidocument, epivaluelabels, design_types,
  AnchorDocking;

type

  { TRuntimeDesignFrame }

  TRuntimeDesignFrame = class(TFrame)
    RenameControlsAction: TAction;
    RecodeDataAction: TAction;
    ClearDataAction: TAction;
    ExpandPageAction: TAction;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    ClearDataMenuItem: TMenuItem;
    MenuItem8: TMenuItem;
    SelectAllBoolsAction: TAction;
    SelectAllStringsAction: TAction;
    SelectAllFloatsAction: TAction;
    SelectAllIntsAction: TAction;
    ShowAlignFormAction: TAction;
    AlignRightAction: TAction;
    AlignTopAction: TAction;
    AlignBottomAction: TAction;
    AlignLeftAction: TAction;
    SelectAllAction: TAction;
    DateToolButton: TToolButton;
    DeleteAllToolButton: TToolButton;
    DeleteToolButton: TToolButton;
    DesignerToolBar: TToolBar;
    Divider1: TToolButton;
    Divider2: TToolButton;
    Divider3: TToolButton;
    Divider4: TToolButton;
    Divider5: TToolButton;
    Divider6: TToolButton;
    EditToolButton: TToolButton;
    ExportToolButton: TToolButton;
    FloatToolButton: TToolButton;
    HeadingToolButton: TToolButton;
    ImportToolButton: TToolButton;
    IntToolButton: TToolButton;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    OpenProjectToolBtn: TToolButton;
    OtherToolButton: TToolButton;
    ToolBarPanel: TPanel;
    PrintDialog1: TPrintDialog;
    ProjectDivider1: TToolButton;
    ProjectToolBar: TToolBar;
    RedoAction: TAction;
    NewTimeFieldFastAction: TAction;
    NewTimeFieldAction: TAction;
    SaveProjectAsToolBtn: TToolButton;
    SaveProjectToolBtn: TToolButton;
    SectionToolButton: TToolButton;
    SelectLastAction: TAction;
    SelectFirstAction: TAction;
    SelectorToolButton: TToolButton;
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
    StringToolButton: TToolButton;
    TestToolButton: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
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
    DefaultValueLabel: TLabel;
    DefaultValuePanel: TPanel;
    DesignerImageList: TImageList;
    EditPopupMenuItem: TMenuItem;
    ExtendedLabel: TLabel;
    ExtendedPanel: TPanel;
    FieldNameLabel: TLabel;
    FieldNamePanel: TPanel;
    FieldsLabel: TLabel;
    FieldsPanel: TPanel;
    FieldsStaticLabel: TLabel;
    FieldTypeLabel: TLabel;
    FieldTypePanel: TPanel;
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
    NewTimeNowFieldMenu: TMenuItem;
    NewUpperCaseMenu: TMenuItem;
    NewYMDTodayFieldMenu: TMenuItem;
    OtherFieldsPopup: TPopupMenu;
    Panel1: TPanel;
    PasteControPopupMenuItem: TMenuItem;
    PopupMenuDivider1: TMenuItem;
    PopupMenuDivider2: TMenuItem;
    RangeLabel: TLabel;
    RangePanel: TPanel;
    RecordsLabel: TLabel;
    RecordsPanel: TPanel;
    RecordStaticLabel: TLabel;
    SectionsLabel: TLabel;
    SectionsPanel: TPanel;
    SectionsStaticLabel: TLabel;
    Splitter1: TSplitter;
    StatusBarPanel: TPanel;
    TimeSubMenu: TMenuItem;
    TodayDateSubMenu: TMenuItem;
    ValueLabelLabel: TLabel;
    ValueLabelPanel: TPanel;
    procedure AlignBottomActionExecute(Sender: TObject);
    procedure AlignLeftActionExecute(Sender: TObject);
    procedure AlignRightActionExecute(Sender: TObject);
    procedure AlignTopActionExecute(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ClearDataActionExecute(Sender: TObject);
    procedure ClearDataActionUpdate(Sender: TObject);
    procedure ClearSelectionActionExecute(Sender: TObject);
    procedure CopyControlActionExecute(Sender: TObject);
    procedure CutCopyControlUpdate(Sender: TObject);
    procedure CutControlActionExecute(Sender: TObject);
    procedure DeleteAllActionExecute(Sender: TObject);
    procedure DeleteControlActionExecute(Sender: TObject);
    procedure DeleteControlFastActionExecute(Sender: TObject);
    procedure DesignControlPopUpMenuPopup(Sender: TObject);
    procedure DesignerActionListUpdate(AAction: TBasicAction;
      var Handled: Boolean);
    procedure EditControlActionExecute(Sender: TObject);
    procedure ExpandPageActionExecute(Sender: TObject);
    procedure ExportToolButtonClick(Sender: TObject);
    procedure FieldBtnClick(Sender: TObject);
    function FieldNamePrefix: string;
    procedure HeadingBtnClick(Sender: TObject);
    procedure ImportActionExecute(Sender: TObject);
    procedure NewDateFieldActionExecute(Sender: TObject);
    procedure NewDateFieldFastActionExecute(Sender: TObject);
    procedure NewFloatFieldActionExecute(Sender: TObject);
    procedure NewFloatFieldFastActionExecute(Sender: TObject);
    procedure NewHeadingActionExecute(Sender: TObject);
    procedure NewHeadingFastActionExecute(Sender: TObject);
    procedure NewIntFieldActionExecute(Sender: TObject);
    procedure NewIntFieldFastActionExecute(Sender: TObject);
    procedure NewOtherFieldClick(Sender: TObject);
    procedure NewStringFieldActionExecute(Sender: TObject);
    procedure NewStringFieldFastActionExecute(Sender: TObject);
    procedure NewTimeFieldActionExecute(Sender: TObject);
    procedure NewTimeFieldFastActionExecute(Sender: TObject);
    procedure PasteAsDateActionExecute(Sender: TObject);
    procedure PasteAsFloatActionExecute(Sender: TObject);
    procedure PasteAsIntActionExecute(Sender: TObject);
    procedure PasteAsStringActionExecute(Sender: TObject);
    procedure PasteAsUpdate(Sender: TObject);
    procedure PasteControlActionExecute(Sender: TObject);
    procedure PasteAsHeadingActionExecute(Sender: TObject);
    procedure PasteControlActionUpdate(Sender: TObject);
    procedure PrintDataFormActionExecute(Sender: TObject);
    procedure RecodeDataActionExecute(Sender: TObject);
    procedure RecodeDataActionUpdate(Sender: TObject);
    procedure RedoActionExecute(Sender: TObject);
    procedure RedoActionUpdate(Sender: TObject);
    procedure RenameControlsActionExecute(Sender: TObject);
    procedure SectionBtnClick(Sender: TObject);
    procedure SelectAllActionExecute(Sender: TObject);
    procedure SelectAllBoolsActionExecute(Sender: TObject);
    procedure SelectAllFloatsActionExecute(Sender: TObject);
    procedure SelectAllIntsActionExecute(Sender: TObject);
    procedure SelectAllStringsActionExecute(Sender: TObject);
    procedure SelecterBtnClick(Sender: TObject);
    procedure SelectFirstActionExecute(Sender: TObject);
    procedure SelectLastActionExecute(Sender: TObject);
    procedure SelectNextActionExecute(Sender: TObject);
    procedure SelectPgDnActionExecute(Sender: TObject);
    procedure SelectPgUpActionExecute(Sender: TObject);
    procedure SelectPriorActionExecute(Sender: TObject);
    procedure ShowAlignFormActionExecute(Sender: TObject);
    procedure TestToolButtonClick(Sender: TObject);
    procedure UndoActionExecute(Sender: TObject);
    procedure UndoActionUpdate(Sender: TObject);
    procedure ViewDatasetActionExecute(Sender: TObject);
  private
    FPopUpPoint: TPoint;
    FDatafile: TEpiDataFile;
    FActiveButton: TToolButton;
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
    procedure SectionsChangeEvent(Const Sender, Initiator: TEpiCustomBase;
      EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
    function ClipBoardHasText: boolean;
    function ClipBoardHasComponent: boolean;
    procedure LMDesignerAdd(var Msg: TLMessage); message LM_DESIGNER_ADD;
  private
    { Designer Panel/ScrollBox }
    FExtender: TWinControl;
    FDesignPanel: TJvDesignPanel;
    FDesignScrollBox: TJvDesignScrollBox;
    procedure DesignScrollBoxMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  public
    property Extender: TWinControl read FExtender write FExtender;
  private
    { Import }
    procedure PasteEpiDoc(const ImportDoc: TEpiDocument;
      RenameVL, RenameFields: boolean;
      ImportData: boolean);
  private
    { Print }
    procedure DoPrintDataForm;
    procedure PaintDesignPanel(Sender: TObject);
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
    { Hint }
    FHintWindow: THintWindow;
    procedure ShowHintMsg(Sender: TObject; Ctrl: TControl; const Msg: string);
  private
    { Select popup-submenu }
    procedure DoSelectFields(Const Field: TEpiField;
      Const SelectType: TDesignSelectType; Const Section: TEpiSection = nil);
    procedure SelectAllFieldsType(Sender: TObject);
    procedure SelectAllFieldsTypeInSection(Sender: TObject);
    procedure SelectAllFieldsLength(Sender: TObject);
    procedure SelectAllFieldsValueLabel(Sender: TObject);
    procedure SelectAllFieldsRange(Sender: TObject);
    procedure SelectAllFieldsDefaultValue(Sender: TObject);
    procedure SelectAllFieldsRepeat(Sender: TObject);
    { Align }
  private
    procedure DoAlignControls(Const AAlignMent: TDesignControlsAlignment;
      FixedDist: integer);
  public
    procedure AlignControls(Const AAlignMent: TDesignControlsAlignment;
      Const FixedDist: integer = -1);
  private
    { Other }
    procedure UpdateShortcuts;
    procedure UpdateControls;
    procedure UpdateInterface;
    procedure SelectControl(AAction: TDesignSelectAction);
    procedure UpdateStatusbar(ControlList: TJvDesignObjectArray);
    procedure UpdateStatusbarSizes;
    procedure DeleteControls(ForceDelete: boolean);
  protected
    function GetDataFile: TEpiDataFile;
    procedure SetDataFile(AValue: TEpiDataFile);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure   UpdateFrame;
    procedure   ShowPropertiesForm(NewControl: boolean);
    function    IsShortCut(var Message: TLMKey): boolean;
    function ValidateControls: boolean;
    property DataFile: TEpiDataFile read GetDataFile write SetDataFile;
    property ImportedFileName: string read FImportedFileName;
    property DesignPanel: TJvDesignPanel read FDesignPanel;
    property DesignScrollBar: TJvDesignScrollBox read FDesignScrollBox;
  public
    class procedure RestoreDefaultPos(F: TRuntimeDesignFrame);
  end;

implementation

{$R *.lfm}

uses
  JvDesignImp, design_designpanel,
  Graphics, design_designcontroller, design_designmessenger,
  main, epistringutils, JvDesignUtils, settings2_var,
  manager_globals, managerprocs, Clipbrd, math,
  Dialogs, import_structure_form, epimiscutils,
  dataset_form,
  LCLMessageGlue, LCLType, shortcuts, settings2,
  Printers, OSPrinters, strutils,
  design_control_section,
  design_control_field,
  design_control_heading,
  design_control_extender,
  align_form,
  recode_form,
  rename_form;

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
var
  Key: Word;
begin
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

    Screen.Cursor := crHourGlass;
    Application.ProcessMessages;
    ImpStructurForm := TImportStructureForm.Create(FDesignScrollBox, Dlg.Files);
    Screen.Cursor := crDefault;
    Application.ProcessMessages;

    ImpStructurForm.ImportData := (DataFile.Size = 0);
    if ImpStructurForm.ShowModal = mrCancel then exit;

    // Prepare screen...
    Screen.Cursor := crHourGlass;
    Application.ProcessMessages;
    try
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
    end;
  finally
    Screen.Cursor := crDefault;
    Application.ProcessMessages;

    Dlg.Free;
    ImpStructurForm.Free;
  end;
end;

procedure TRuntimeDesignFrame.DesignScrollBoxMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  with FDesignScrollBox.VertScrollBar do
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

procedure TRuntimeDesignFrame.NewOtherFieldClick(Sender: TObject);
begin
  FAddClass := 'TDesignField';
  OtherToolButton.Tag := TMenuItem(Sender).Tag;
  OtherToolButton.ImageIndex := TMenuItem(Sender).ImageIndex;
  DoToogleBtn(OtherToolButton);
end;

procedure TRuntimeDesignFrame.NewStringFieldActionExecute(Sender: TObject);
begin
  NewShortCutDesignField(ftString, true);
end;

procedure TRuntimeDesignFrame.NewStringFieldFastActionExecute(Sender: TObject);
begin
  NewShortCutDesignField(ftString, false);
end;

procedure TRuntimeDesignFrame.NewTimeFieldActionExecute(Sender: TObject);
begin
  NewShortCutDesignField(ftTime, true);
end;

procedure TRuntimeDesignFrame.NewTimeFieldFastActionExecute(Sender: TObject);
begin
  NewShortCutDesignField(ftTime, false);
end;

procedure TRuntimeDesignFrame.PasteAsDateActionExecute(Sender: TObject);
begin
//  PasteAsField(ftDMYDate);
  PasteAsField(ManagerSettings.DefaultDateType);
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
     (
      ((FDesignPanel.Surface.Count = 1) and
       (csAcceptsControls in FDesignPanel.Surface.Selection[0].ControlStyle)
      )
     or
      (FDesignPanel.Surface.Count = 0)
     );
end;

procedure TRuntimeDesignFrame.PasteAsField(Ft: TEpiFieldType);
var
  Cbl: TStringList;
  P: TPoint;
  Controller: TDesignController;
  Surface: TJvDesignSurface;
  i: Integer;
  F: TDesignField;
  H: Integer;
  Pt: TPoint;
begin
  Cbl := TStringList.Create;

  if (FPopUpPoint.X = -1) and
     (FPopUpPoint.Y = -1)
  then
    P := FindNewPostion(TDesignField)
  else
    P := FDesignPanel.ScreenToClient(FPopUpPoint);

  FLastSelectedFieldType := Ft;
  Surface := FDesignPanel.Surface;
  try
    MainForm.BeginUpdatingForm;
    ReadClipBoard(Cbl);
    for i := 0 to Cbl.Count -1 do
      begin
        if Trim(Cbl[i]) = '' then continue;

        F := TDesignField(NewDesignField(P));
        TEpiField(F.EpiControl).Question.Text := Cbl[i];
        Inc(P.Y, ManagerSettings.SpaceBtwFieldField + F.Height);

        Pt := Surface.ContainerToSelectedContainer(P);
        if (Surface.SelectedContainer <> FDesignPanel)  and
           ((Pt.Y + F.Height) > Surface.SelectedContainer.ClientHeight)
        then
          Dec(P.Y, F.Height + ManagerSettings.SpaceBtwFieldField + (Pt.Y - Surface.SelectedContainer.ClientHeight));
      end;
  finally
    Cbl.Free;
    FPopUpPoint := Point(-1, -1);
    MainForm.EndUpdatingForm;
  end;
end;

function TRuntimeDesignFrame.ControlFromEpiControl(
  EpiCtrl: TEpiCustomControlItem): TControl;
begin
  Result := TControl(EpiCtrl.FindCustomData(DesignControlCustomDataKey));
end;

function TRuntimeDesignFrame.FindNewPostion(NewControl: TControlClass): TPoint;
var
  CI: TEpiCustomControlItem;
  Dist: Integer;
  S: TEpiSection;
begin
  CI := DataFile.ControlItem[DataFile.ControlItems.Count-1];
  if (CI is TEpiSection) then
  begin
    if CI = DataFile.MainSection then
      Result := Point(ManagerSettings.DefaultRightPosition, 20)
    else
      Result := Point(CI.Left, CI.Top + TEpiSection(CI).Height)
  end else begin
    S := TEpiSection(Ci.Owner.Owner);
    if S <> DataFile.MainSection then
      Result := Point(S.Left, S.Top + S.Height)
    else
      Result := Point(CI.Left, CI.Top + ControlFromEpiControl(CI).Height);
  end;

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
  ASelectedContainer: TWinControl;

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
  // Pasting components have precedence over text-pasting.
  if ClipBoardHasComponent then
  begin
    with FDesignPanel.Surface do
      begin
        MainForm.BeginUpdatingForm;
        ASelectedContainer := SelectedContainer;
        P := SelectedContainer.ScreenToClient(FPopUpPoint);
        PasteComponents;

        O := TopLeftSelectionPoint;
        if (FPopUpPoint.X <> -1) or (FPopUpPoint.Y <> -1) then
          P := Point(P.X - O.X, P.Y - O.Y)
        else
          P := Point(20, 20);
        for i := 0 to Count - 1 do
          begin
            R := Selection[i].BoundsRect;
            OffsetRect(R, P.X, P.Y);
            if R.Bottom > ASelectedContainer.ClientHeight then
              OffsetRect(R, 0, -(R.Bottom - ASelectedContainer.ClientHeight));
            if R.Right > ASelectedContainer.ClientWidth then
              OffsetRect(R, -(R.Right - ASelectedContainer.ClientWidth), 0);
            Selection[i].BoundsRect := R;
          end;

        MainForm.EndUpdatingForm;
      end;
  end else
  if ClipBoardHasText then
  begin
    case ManagerSettings.PasteSpecialType of
      0: PasteAsHeadingAction.Execute;
      1: PasteAsField(ftInteger);
      2: PasteAsField(ftFloat);
      3: PasteAsField(ftString);
    end;
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
  Pt: TPoint;
begin
  Cbl := TStringList.Create;

  if (FPopUpPoint.X = -1) and
     (FPopUpPoint.Y = -1)
  then
    P := FindNewPostion(TDesignHeading)
  else
    P := FDesignPanel.ScreenToClient(FPopUpPoint);

  Surface := FDesignPanel.Surface;
  try
//    MainForm.BeginUpdatingForm;
    ReadClipBoard(Cbl);
    for i := 0 to Cbl.Count -1 do
      begin
        if Trim(Cbl[i]) = '' then continue;

        L := TDesignHeading(NewDesignHeading(P));
        TEpiHeading(L.EpiControl).Caption.Text := Cbl[i];

        Inc(P.Y, ManagerSettings.SpaceBtwLabelLabel + L.Height);

        Pt := Surface.ContainerToSelectedContainer(P);
        if (Surface.SelectedContainer <> FDesignPanel)  and
           ((Pt.Y + L.Height) > Surface.SelectedContainer.ClientHeight)
        then
          Dec(P.Y, L.Height + ManagerSettings.SpaceBtwLabelLabel + (Pt.Y - Surface.SelectedContainer.ClientHeight));
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
    (ClipBoardHasText or Clipboard.HasFormat(CF_Component)) {and
     (
      ((FDesignPanel.Surface.Count = 1) and
       (csAcceptsControls in FDesignPanel.Surface.Selection[0].ControlStyle)
      )
     or
      (FDesignPanel.Surface.Count = 0)
     )};
end;

procedure TRuntimeDesignFrame.PrintDataFormActionExecute(Sender: TObject);
begin
  DoPrintDataForm;
end;

procedure TRuntimeDesignFrame.RecodeDataActionExecute(Sender: TObject);
var
  RecodeForm: TRecodeForm;
begin
  if FDesignPanel.Surface.Selector.Count <> 1 then exit;

  RecodeForm := TRecodeForm.Create(self);
  RecodeForm.Field := TEpiField((FDesignPanel.Surface.Selection[0] as IDesignEpiControl).EpiControl);
  RecodeForm.ShowModal;
  RecodeForm.Free;
end;

procedure TRuntimeDesignFrame.RecodeDataActionUpdate(Sender: TObject);
var
  Surface: TJvDesignSurface;
  Obs: TJvDesignObjectArray;
  ActionEnable: Boolean;
begin
  Surface := FDesignPanel.Surface;
  Obs := Surface.Selected;

  ActionEnable := true;
  if DataFile.Size = 0 then ActionEnable := false;
  if Length(Obs) <> 1 then  ActionEnable := false;
  if (Length(Obs) > 0) and
     (not (Obs[0] is TDesignField))
  then
    ActionEnable := false;

  TAction(Sender).Enabled := ActionEnable;
end;

procedure TRuntimeDesignFrame.RedoActionExecute(Sender: TObject);
begin
  GlobalCommandList.ReDo;
end;

procedure TRuntimeDesignFrame.RedoActionUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := GlobalCommandList.CanRedo;
end;

procedure TRuntimeDesignFrame.RenameControlsActionExecute(Sender: TObject);
var
  F: TRenameForm;
begin
  F := TRenameForm.Create(self, DataFile);
  F.ShowModal;
  F.Free;
end;


procedure TRuntimeDesignFrame.SectionBtnClick(Sender: TObject);
begin
  FAddClass := 'TDesignSection';
  DoToogleBtn(Sender);
end;

procedure TRuntimeDesignFrame.SelectAllActionExecute(Sender: TObject);
var
  i: Integer;
  ctrl: TControl;
begin
  With FDesignPanel.Surface do
  begin
    ClearSelection;
    DisableAutoSizing;
    for i := 0 to FDesignPanel.ControlCount - 1 do
      if Supports(FDesignPanel.Controls[i], IDesignEpiControl) then
        Selector.AddToSelection(FDesignPanel.Controls[i]);
    EnableAutoSizing;
    SelectionChange;
  end;
end;

procedure TRuntimeDesignFrame.SelectAllBoolsActionExecute(Sender: TObject);
var
  F: TEpiField;
begin
  F := TEpiField.CreateField(nil, ftBoolean);
  DoSelectFields(F, dstType);
  F.Free;
end;

procedure TRuntimeDesignFrame.SelectAllFloatsActionExecute(Sender: TObject);
var
  F: TEpiField;
begin
  F := TEpiField.CreateField(nil, ftFloat);
  DoSelectFields(F, dstType);
  F.Free;
end;

procedure TRuntimeDesignFrame.SelectAllIntsActionExecute(Sender: TObject);
var
  F: TEpiField;
begin
  F := TEpiField.CreateField(nil, ftInteger);
  DoSelectFields(F, dstType);
  F.Free;
end;

procedure TRuntimeDesignFrame.SelectAllStringsActionExecute(Sender: TObject);
var
  F: TEpiField;
begin
  F := TEpiField.CreateField(nil, ftString);
  DoSelectFields(F, dstType);
  F.Free;
end;

procedure TRuntimeDesignFrame.SectionsChangeEvent(const Sender,
  Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
begin
  if not (Initiator = DataFile.Sections) then exit;
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

function TRuntimeDesignFrame.ClipBoardHasComponent: boolean;
begin
  FCheckingClipBoard := true;
  result := Clipboard.HasFormat(CF_Component);
  FCheckingClipBoard := false;
end;

procedure TRuntimeDesignFrame.LMDesignerAdd(var Msg: TLMessage);
begin
  // Hack - because we need the incomming field to be furthest
  // away from being the last entry in Datafile.CustomControls.
  // Otherwise FindNewPosition fails misarably.
  with TEpiField(Msg.WParam) do
  begin
    BeginUpdate;
    Top := 0;
    Left := 0;
    EndUpdate;
  end;
  NewDesignField(FindNewPostion(TDesignField), TEpiField(Msg.WParam), FDesignPanel);
end;

procedure TRuntimeDesignFrame.PasteEpiDoc(const ImportDoc: TEpiDocument;
  RenameVL, RenameFields: boolean; ImportData: boolean);
Const
  PasteEpiDocCustomDataKey = 'pasteepidoc-valuelabelset';
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
       (Assigned(F.ValueLabelSet.FindCustomData(PasteEpiDocCustomDataKey)))
    then
      F.ValueLabelSet := TEpiValueLabelSet(F.ValueLabelSet.RemoveCustomData(PasteEpiDocCustomDataKey));

    // Need to correct ShowValueLabel setting to match that of the setup.
    F.ShowValueLabel := ManagerSettings.ShowValuelabelText;

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
    F: TEpiField;
  begin
    // Rename if already present.
    if (not DataFile.Sections.ValidateRename(S.Name, false))
    then
      S.Name := DataFile.Sections.GetUniqueItemName(TEpiSection);

    S.DataFile.Sections.RemoveItem(S);

    // Section has beem detached -> to avoid possible renaming by Core
    // during AddItem, check for overlapping fieldnames now.
    for i := 0 to S.Fields.Count -1 do
    begin
      F := S.Fields[i];
      if (DataFile.Fields.ItemExistsByName(F.Name)) and
         (not RenameFields) and
         (F.FieldType = DataFile.Fields.FieldByName[F.Name].FieldType)
      then
        begin
          S.Fields.RemoveItem(F);
          F.Free;
        end;
    end;
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
        VLSet.AddCustomData(PasteEpiDocCustomDataKey, DataFile.ValueLabels.GetValueLabelSetByName(VLSet.Name));
        Continue;
      end;


    if DataFile.ValueLabels.ItemExistsByName(VLSet.Name) then
      VLSet.Name := DataFile.ValueLabels.GetUniqueItemName(TEpiValueLabelSet);
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
  ppix: Integer;
  ppiy: Integer;
  ppmmx: Int64;
  ppmmy: Int64;
  LeftMarg: Integer;
  TopMarg: Integer;
  BotMarg: Integer;
  pClientHeight: Integer;
  xscale: Extended;
  yscale: Extended;
  CI: TEpiCustomControlItem;
  ALeft: Integer;
  ARight: Integer;
  ATop: Integer;
  ABot: Integer;
  i: Integer;
  S: String;
  Sz: TSize;

  procedure SetFont(Const AFont: TFont);
  begin
    printer.canvas.Font.PixelsPerInch  := ppix;
    Printer.Canvas.Font.Name           := AFont.Name;
    Printer.Canvas.Font.Size           := AFont.Size;
    Printer.Canvas.Font.Style          := AFont.Style;
    printer.canvas.Font.PixelsPerInch  := ppix;
  end;

  function ControlItemTop(Const Item: TEpiCustomControlItem): Integer;
  begin
    if Item is TEpiSection then
      Result := Item.Top
    else
      Result := DesignPanel.ScreenToClient(ControlFromEpiControl(CI).ClientToScreen(Point(0,0))).Y;
  end;

  function ControlItemLeft(Const Item: TEpiCustomControlItem): Integer;
  begin
    if Item is TEpiSection then
      Result := Item.Left
    else
      Result := Item.Left + TEpiSection(Item.Owner.Owner).Left;
  end;

begin
  IF NOT PrintDialog1.Execute THEN Exit;
  WITH Printer DO
  BEGIN
//    FileName := '/tmp/tmp.ps';
    Title    := 'EpiData Manager - ' + TEpiDocument(DataFile.RootOwner).Study.Title.Text;
    ppix     := XDPI;                    //pixels pr inch X
    ppiy     := YDPI;                    //pixels pr inch Y
    ppmmx    := Round(ppix/25.4);        //pixels pr mm X
    ppmmy    := Round(ppiy/25.4);        //pixels pr mm Y
    LeftMarg := 0;                       //Sets left margin to 0 cm
    TopMarg  := 0;                       //Sets top margin to 0 cm
    BotMarg  := PageHeight;              //Sets bottom margin to 0 cm
    pClientHeight := BotMarg - TopMarg;

    xscale := ppix / GetParentForm(Self).PixelsPerInch;
    yscale := ppiy / GetParentForm(Self).PixelsPerInch;

    BeginDoc;

    i := 0;
    while i < DataFile.ControlItems.Count do
    begin
      CI := DataFile.ControlItem[i];
      if CI = DataFile.MainSection then
      begin
        inc(i);
        continue;
      end;

      ATop := (Round(ControlItemTop(CI) * yscale) - (PageNumber - 1) * pClientHeight) + TopMarg;
      ALeft := Round(ControlItemLeft(CI) * xscale) + LeftMarg;

      if (CI is TEpiSection) then
      begin
        SetFont(ManagerSettings.SectionFont);
        ABot := ATop + Round(TEpiSection(CI).Height * yscale);
      end;
      if (CI is TEpiHeading) then
      begin
        SetFont(ControlFromEpiControl(CI).Font);
        ABot := ATop + Canvas.TextHeight(TEpiHeading(CI).Caption.Text);
      end;
      if (CI is TEpiField) then
      begin
        SetFont(ManagerSettings.FieldFont);
        ABot := ATop + Round(ControlFromEpiControl(CI).Height * yscale);  // Canvas.TextHeight(TEpiField(CI).Name);
      end;

      // Check if we need to create a new page
      if ATop > BotMarg then
      begin
        NewPage;
        Continue;
      end;

      if CI is TEpiSection then
      with TEpiSection(CI) do
      begin
        SetFont(ManagerSettings.SectionFont);
        ARight := ALeft + Round(Width * xscale);

        Sz := Size(0,0);
        if Caption.Text <> '' then
        begin
          Sz := Canvas.TextExtent(Caption.Text);
          Canvas.TextOut(ALeft + Round(10 * xscale), ATop, Caption.Text);
        end;

        ATop := ATop + Round(Sz.cy * (2 / 3));

        // Draw box
        Canvas.MoveTo(ALeft + Round(5 * xscale), ATop);
        Canvas.LineTo(ALeft, ATop);
        Canvas.LineTo(ALeft, ABot);
        Canvas.LineTo(ARight, ABot);
        Canvas.LineTo(ARight, ATop);
        // .. line to caption text
        Canvas.LineTo(ALeft + Sz.cx + Round(15 * xscale), Atop);
      end;

      if CI is TEpiHeading then
      begin
        Canvas.TextOut(aLeft, ATop, TEpiHeading(CI).Caption.Text);
      end;

      if CI is TEpiField then
      with TEpiField(CI) do
      begin
        // Draw box
        ARight := ALeft + Round(ControlFromEpiControl(CI).Width * xscale);
        ATop := ABot - ((ABot - ATop) div 2);

        Canvas.MoveTo(ALeft, ATop);
        Canvas.LineTo(ALeft, ABot);
        Canvas.LineTo(ARight, ABot);
        Canvas.LineTo(ARight, ATop);

        IF trim(Question.Text)<>'' THEN
        BEGIN
          aLeft := ALeft - Round(5 * xscale) - Canvas.TextWidth(Question.Text);
          ATop := ABot - Canvas.TextHeight(Question.Text);
          Canvas.TextOut(aLeft, ATop, Question.Text);
        END;

        IF TEpiDocument(DataFile.RootOwner).ProjectSettings.ShowFieldNames then
        begin
          ALeft := ALeft - Round(5 * xscale) - Canvas.TextWidth(Name);
          ATop := ABot - Canvas.TextHeight(Question.Text);
          Canvas.TextOut(aLeft, ATop, Name);
        end;
      end;

      Inc(i);
    end;

    EndDoc;
  END;  //with printer
end;

procedure TRuntimeDesignFrame.PaintDesignPanel(Sender: TObject);
var
  Y: Integer;
  PPMM: Extended;
  A4_PIXEL_WIDTH: Int64;
  A4_PIXEL_HEIGHT: Int64;
  PgNum: Integer;
  X: Integer;
begin
  if not ManagerSettings.ShowA4GuideLines then exit;

  PPMM := GetParentForm(Self).PixelsPerInch / 25.400051;  // (Pixel / Inch) / (MM / Inch) =   (Pixel / MM)
  A4_PIXEL_HEIGHT  := Round(297 * PPMM);
  A4_PIXEL_WIDTH := Round(210 * PPMM);

  with TDesignPanel(Sender) do
  begin
    Y := FDesignScrollBox.VertScrollBar.Position;
    X := FDesignScrollBox.HorzScrollBar.Position;
    PgNum := (Y div A4_PIXEL_HEIGHT) + 1;
    Canvas.Pen.Style := psDot;
    Canvas.Pen.Color := clRed;
    // Vert. line
    Canvas.Line(
      A4_PIXEL_WIDTH, 0 + Y,        // X1, Y1
      A4_PIXEL_WIDTH, Height + Y    // X2, Y2
    );
    // Horz. line
    Canvas.Line(
      0 + X, A4_PIXEL_HEIGHT * PgNum,      // X1, Y1
      A4_PIXEL_WIDTH, A4_PIXEL_HEIGHT * PgNum   // X2, Y2
    );
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
    if Assigned(AParent) then
      TopLeft := DesignClientToParent(TopLeft, AParent, FDesignPanel);

    FCreatingControl := not Assigned(Field);
    Controller.SetDragRect(Rect(TopLeft.X, TopLeft.Y, 0, 0));
    Surface.AddClass := 'TDesignField';
    Surface.AddComponent;

    Result := TDesignField(Surface.Selection[0]);
    Result.PopupMenu := DesignControlPopUpMenu;
    TDesignField(Result).OnShowHint := @ShowHintMsg;

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
    Self.ShowPropertiesForm(true);
    //KeyCode := VK_RETURN;
    //LCLSendKeyDownEvent(Result, KeyCode, 0, true, false);
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
    Self.ShowPropertiesForm(true);
//    KeyCode := VK_RETURN;
//    LCLSendKeyDownEvent(Result, KeyCode, 0, true, false);
  end;
end;

procedure TRuntimeDesignFrame.ShowHintMsg(Sender: TObject; Ctrl: TControl;
  const Msg: string);
var
  R: TRect;
  P: TPoint;
begin
  if (Msg = '') and (Ctrl = nil) then
  begin
    FHintWindow.Hide;
    Exit;
  end;

  R := FHintWindow.CalcHintRect(0, Msg, nil);
  P := Ctrl.ClientToScreen(Point(0, Ctrl.Height + 2));
  OffsetRect(R, P.X, P.Y);
  FHintWindow.ActivateHint(R, Msg);
end;

procedure TRuntimeDesignFrame.DoSelectFields(const Field: TEpiField;
  const SelectType: TDesignSelectType; const Section: TEpiSection);
var
  i: Integer;
  CmpField: TEpiField;
  Selector: TJvDesignCustomSelector;
begin
  Selector := DesignPanel.Surface.Selector;
  Selector.ClearSelection;

  for i := 0 to FDatafile.Fields.Count -1 do
  begin
    CmpField := FDatafile.Fields[i];

    if not (CmpField.FieldType = Field.FieldType)  then continue;
    if Assigned(Section) and (CmpField.Section <> Section) then continue;

    case SelectType of
      dstType: // Do nothing - already checked
        ;
      dstLength:
        if Field.Length <> CmpField.Length then continue;
      dstValueLabel:
        if Field.ValueLabelSet <> CmpField.ValueLabelSet then continue;
      dstRange:
        begin
          if not Assigned(CmpField.Ranges) then continue;
          if (Field.Ranges[0].AsString[false] <> CmpField.Ranges[0].AsString[false]) or
             (Field.Ranges[0].AsString[true] <> CmpField.Ranges[0].AsString[true])
          then
            continue;
        end;
      dstDefaultValue:
        if Field.DefaultValueAsString <> CmpField.DefaultValueAsString then continue;
      dstRepeat:
        if Field.RepeatValue <> CmpField.RepeatValue then continue;
    end;

    Selector.AddToSelection(ControlFromEpiControl(CmpField));
  end;
  DesignPanel.Surface.SelectionChange;
end;

procedure TRuntimeDesignFrame.SelectAllFieldsType(Sender: TObject);
begin
  if not (Sender is TMenuItem) then exit;

  DoSelectFields(TEpiField(TMenu(Sender).Tag), dstType);
end;

procedure TRuntimeDesignFrame.SelectAllFieldsTypeInSection(Sender: TObject);
begin
  if not (Sender is TMenuItem) then exit;

  DoSelectFields(TEpiField(TMenu(Sender).Tag), dstType, TEpiField(TMenu(Sender).Tag).Section);
end;

procedure TRuntimeDesignFrame.SelectAllFieldsLength(Sender: TObject);
begin
  if not (Sender is TMenuItem) then exit;

  DoSelectFields(TEpiField(TMenu(Sender).Tag), dstLength);
end;

procedure TRuntimeDesignFrame.SelectAllFieldsValueLabel(Sender: TObject);
begin
  if not (Sender is TMenuItem) then exit;

  DoSelectFields(TEpiField(TMenu(Sender).Tag), dstValueLabel);
end;

procedure TRuntimeDesignFrame.SelectAllFieldsRange(Sender: TObject);
begin
  if not (Sender is TMenuItem) then exit;

  DoSelectFields(TEpiField(TMenu(Sender).Tag), dstRange);
end;

procedure TRuntimeDesignFrame.SelectAllFieldsDefaultValue(Sender: TObject);
begin
  if not (Sender is TMenuItem) then exit;

  DoSelectFields(TEpiField(TMenu(Sender).Tag), dstDefaultValue);
end;

procedure TRuntimeDesignFrame.SelectAllFieldsRepeat(Sender: TObject);
begin
  if not (Sender is TMenuItem) then exit;

  DoSelectFields(TEpiField(TMenu(Sender).Tag), dstRepeat);
end;

procedure TRuntimeDesignFrame.DoAlignControls(
  const AAlignMent: TDesignControlsAlignment; FixedDist: integer);
var
  Objs: TJvDesignObjectArray;
  L: Integer;
  AlignVal: Integer;
  i: Integer;
  ALeft: Integer;
  ATop: Integer;
  ABot: Integer;
  ARight: Integer;
  ATotRight: Integer;
  ACenterHorz: Integer;
  ACenterVert: Integer;
  ATotalCtrlHeight: Integer;
  ATotalCtrlWidth: Integer;
  AEvenDistHorz: Integer;
  AEvenDistVert: Integer;
  LastBot: Integer;
  LastRight: Integer;
  ABounds: TRect;
  EBounds: TRect;
  ATotLeft: Integer;
  ASideBounds: TRect;
  ESideBounds: TRect;
  P: TWinControl;
  Ctrl: TControl;
  J: Integer;

  procedure SortTop(var List: TJvDesignObjectArray);
  var
    n: Integer;
    newn: Integer;
    i: integer;
    Tmp: TObject;
  begin
    n := Length(List);
    repeat
      newn := 0;
      for i := 1 to n-1 do
        if (TControl(List[i-1]).Top > TControl(List[i]).Top) or
           (
            (TControl(List[i-1]).Top = TControl(List[i]).Top) and
            (TControl(List[i-1]).Left > TControl(List[i]).Left)
           )
        then
        begin
          Tmp := List[i];
          List[i] := List[i-1];
          List[i-1] := Tmp;
          newn := i;
        end;
      n := newn;
    until (n = 0);
  end;

  procedure SortLeft(var List: TJvDesignObjectArray);
  var
    n: Integer;
    newn: Integer;
    i: integer;
    Tmp: TObject;
  begin
    n := Length(List);
    repeat
      newn := 0;
      for i := 1 to n-1 do
        if (TControl(List[i-1]).Left > TControl(List[i]).Left) or
           (
            (TControl(List[i-1]).Left = TControl(List[i]).Left) and
            (TControl(List[i-1]).Top > TControl(List[i]).Top)
           )
        then
        begin
          Tmp := List[i];
          List[i] := List[i-1];
          List[i-1] := Tmp;
          newn := i;
        end;
      n := newn;
    until (n = 0);
  end;

begin
  Objs := DesignPanel.Surface.Selected;

  L := Length(Objs);
  if L = 0 then exit;

  J := 0;
  for i := 0 to L - 1 do
  begin
    if not Supports(Objs[i], IDesignEpiControl) then
      Continue
    else begin
      Objs[j] := Objs[i];
      Inc(j);
    end;
  end;
  if J <> L then
  begin
    SetLength(Objs, J);
    L := J;
  end;

  if AAlignMent in [dcaEvenHorz, dcaFixedHorz] then
    SortLeft(Objs);
  if AAlignMent in [dcaEvenVert, dcaFixedVert] then
    SortTop(Objs);

  Ctrl := TControl(Objs[0]);

  ASideBounds := Ctrl.BoundsRect;
  ESideBounds := (Ctrl as IDesignEpiControl).ExtendedBounds;
  ATotalCtrlHeight := 0;
  ATotalCtrlWidth  := 0;

  for i := 0 to L - 1 do
  with TControl(Objs[i]) do
  begin
    ABounds := BoundsRect;
    EBounds := (TControl(Objs[i]) as IDesignEpiControl).ExtendedBounds;

    UnionRect(ASideBounds, ASideBounds, ABounds);
    UnionRect(ESideBounds, ESideBounds, EBounds);

    ATotalCtrlHeight += EBounds.Bottom - EBounds.Top;
    ATotalCtrlWidth  += EBounds.Right - EBounds.Left;
  end;

  // Check that ATotalCtrlHeight & ATotalCtrlWidth does not
  // exceed parent boundries if in a section. This can make
  // controls disappear.
  P := Ctrl.Parent;
  if (P <> FDesignPanel) then
  begin
    if (AAlignMent = dcaFixedVert) and
       (
        (
         (Ctrl as IDesignEpiControl).ExtendedBounds.Top +
         ATotalCtrlHeight +
         ((L - 1) * FixedDist)
        )
        >
         P.ClientHeight
       )
    then
      FixedDist := Max(((P.ClientHeight - (Ctrl as IDesignEpiControl).ExtendedBounds.Top) - ATotalCtrlHeight) div (L - 1), 1);

    if (AAlignMent = dcaFixedHorz) and
       (
        (
         (Ctrl as IDesignEpiControl).ExtendedBounds.Left +
         ATotalCtrlWidth +
         ((L - 1) * FixedDist)
        )
        >
         P.ClientWidth
       )
    then
      FixedDist := Max(((P.ClientWidth - (Ctrl as IDesignEpiControl).ExtendedBounds.Left) - ATotalCtrlWidth) div (L - 1), 1);
  end;

  with ASideBounds do
  begin
    ACenterVert := Left + ((Right - Left) div 2);
    ACenterHorz := Top  + ((Bottom - Top) div 2);
  end;

  if L > 1 then
    with ESideBounds do
    begin
      AEvenDistHorz := Max(1,
        ((Right - Left) - ATotalCtrlWidth) div (L - 1));
      AEvenDistVert := Max(1,
        ((Bottom - Top) - ATotalCtrlHeight) div (L - 1));
    end;

  // Initial positions -> using controls left/top restrains first control
  // to same position.
  EBounds := (TControl(Objs[0]) as IDesignEpiControl).ExtendedBounds;
  Case AAlignMent of
    dcaEvenVert:   LastBot   := EBounds.Top - AEvenDistVert;
    dcaEvenHorz:   LastRight := EBounds.Left - AEvenDistHorz;
    dcaFixedVert:  LastBot   := EBounds.Top - FixedDist;
    dcaFixedHorz:  LastRight := EBounds.Left - FixedDist;
  end;

  DisableAutoSizing;
  for i := 0 to L - 1 do
  with TControl(Objs[i]) do
  begin
    ABounds := BoundsRect;
    EBounds := (TControl(Objs[i]) as IDesignEpiControl).ExtendedBounds;
    case AAlignMent of
      dcaLeftMost:   OffsetRect(ABounds, ASideBounds.Left - ABounds.Left, 0);
      dcaRightMost:  OffsetRect(ABounds, ASideBounds.Right - ABounds.Right, 0);
      dcaTopMost:    OffsetRect(ABounds, 0, ASideBounds.Top - ABounds.Top);
      dcaBottomMost: OffsetRect(ABounds, 0, ASideBounds.Bottom - ABounds.Bottom);
      dcaCenterVert: OffsetRect(ABounds, ACenterVert - (ABounds.Left + (Width div 2)), 0);
      dcaCenterHorz: OffsetRect(ABounds, 0, ACenterHorz - (ABounds.Top + (Height div 2)));
      dcaEvenVert:   OffsetRect(EBounds, 0, LastBot  + AEvenDistVert - EBounds.Top);
      dcaEvenHorz:   OffsetRect(EBounds, LastRight + AEvenDistHorz - EBounds.Left, 0);
      dcaFixedVert:  OffsetRect(EBounds, 0, LastBot  + FixedDist - EBounds.Top);
      dcaFixedHorz:  OffsetRect(EBounds, LastRight + FixedDist - EBounds.Left, 0);
    end;

    if AAlignMent in [dcaLeftMost..dcaCenterHorz] then
      BoundsRect := ABounds
    else
      (TControl(Objs[i]) as IDesignEpiControl).ExtendedBounds := EBounds;

    LastBot   := EBounds.Bottom;
    LastRight := EBounds.Right;
  end;
  EnableAutoSizing;
end;

procedure TRuntimeDesignFrame.AlignControls(
  const AAlignMent: TDesignControlsAlignment; const FixedDist: integer);
begin
  DoAlignControls(AAlignMent, FixedDist);
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
  NewTimeFieldAction.ShortCut          := D_NewTimeField;
  NewTimeFieldFastAction.ShortCut      := D_NewTimeField_Fast;
  NewHeadingAction.ShortCut            := D_NewHeading;
  NewHeadingFastAction.ShortCut        := D_NewHeading_Fast;
//  NewSectionAction.ShortCut            := D_NewSection;

  EditControlAction.ShortCut           := D_EditControl;
  DeleteControlAction.ShortCut         := D_DeleteControl;
  DeleteControlFastAction.ShortCut     := D_DeleteControl_Fast;
  DeleteAllAction.ShortCut             := D_DeleteAllControl;
  ImportAction.ShortCut                := D_ImportData;
  CutControlAction.ShortCut            := D_CutControl;
  CopyControlAction.ShortCut           := D_CopyControl;
  PasteControlAction.ShortCut          := D_PasteControl;

  SelectFirstAction.ShortCut           := D_MoveTop;
  SelectPgUpAction.ShortCut            := D_MoveSideUp;
  SelectPriorAction.ShortCut           := D_MoveControlUp;
  SelectNextAction.ShortCut            := D_MoveControlDown;
  SelectPgDnAction.ShortCut            := D_MoveSideDown;
  SelectLastAction.ShortCut            := D_MoveBottom;
  SelectAllAction.ShortCut             := D_SelectAll;

  // Align
  ShowAlignFormAction.ShortCut         := D_AlignForm;
  AlignLeftAction.ShortCut             := D_AlignLeft;
  AlignRightAction.ShortCut            := D_AlignRight;
  AlignTopAction.ShortCut              := D_AlignTop;
  AlignBottomAction.ShortCut           := D_AlignBottom;

  UndoAction.ShortCut                  := D_Undo;
  RedoAction.ShortCut                  := D_Redo;
  PrintDataFormAction.ShortCut         := D_PrintDataForm;
  ExpandPageAction.ShortCut            := D_ExpandDataframe;
  ViewDatasetAction.ShortCut           := D_BrowseData;
end;

procedure TRuntimeDesignFrame.UpdateControls;
var
  i: Integer;
begin
  if not Assigned(FDatafile) then exit;

  for i := 0 to DataFile.ControlItems.Count - 1 do
    (ControlFromEpiControl(DataFile.ControlItem[i]) as IDesignEpiControl).UpdateControl;
end;

procedure TRuntimeDesignFrame.UpdateInterface;
begin
  PasteAsDateAction.ImageIndex := Ord(ManagerSettings.DefaultDateType);
  with DateToolButton do
  begin
    Tag := Ord(ManagerSettings.DefaultDateType);
    ImageIndex := Tag;
  end;
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
  ICtrl: IDesignEpiControl;

  function RelativeTop(Ctrl: TControl): integer;
  begin
    Result := FDesignPanel.ScreenToControl(Ctrl.ControlToScreen(Point(0,0))).Y;
  end;

begin
  Surface := FDesignPanel.Surface;
  if (Surface.Count = 0) or (not (Supports(Surface.Selection[0], IDesignEpiControl, ICtrl))) then
    EpiCtrl := (FDesignPanel as IDesignEpiControl).EpiControl
  else
    EpiCtrl := ICtrl.EpiControl;

  Idx := DataFile.ControlItems.IndexOf(EpiCtrl);

  case AAction of
    dsaHome:
      if DataFile.ControlItems.Count = 1 then
        Idx := 0
      else
        Idx := 1;
    dsaPgUp:
      begin
        CBot := FDesignScrollBox.VertScrollBar.Position + FDesignScrollBox.VertScrollBar.Page;
        for Idx := DataFile.ControlItems.Count - 1 downto 0 do
        begin
          Ctrl := ControlFromEpiControl(DataFile.ControlItem[Idx]);
          if (RelativeTop(Ctrl) + Ctrl.Height) < CBot then
            Break;
        end;
      end;
    dsaPrior:
      Idx := ifthen((Idx - 1) >= 0,                    // if
                    Idx - 1,                           //then
                    DataFile.ControlItems.Count - 1);  //else
    dsaNext:
      Idx := ifthen((Idx + 1) <= DataFile.ControlItems.Count - 1,  // if
                    Idx + 1,                                       // then
                    0);                                            // else       //Min(DataFile.ControlItems.Count - 1, Idx +1);
    dsaPgDn:
      begin
        CTop := FDesignScrollBox.VertScrollBar.Position;
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

  CTop := RelativeTop(Ctrl);
  CBot := CTop + Ctrl.Height;
  SPos := FDesignScrollBox.VertScrollBar.Position;
  SPage := FDesignScrollBox.VertScrollBar.Page;
  case AAction of
    dsaPrior:
      begin
        if (CBot < SPos) or
           (CTop > (SPos + SPage))
        then
          // Out of bounds completely => Center view.
        begin
          FDesignScrollBox.VertScrollBar.Position := CTop - (SPage div 2);
          Exit;
        end;

        if (CTop < (SPos + (SPage div 4))) then
        begin
          FDesignScrollBox.VertScrollBar.Position := CTop - (SPage div 4);
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
          FDesignScrollBox.VertScrollBar.Position := CTop - (SPage div 2);
          Exit;
        end;

        if (CBot > ((SPos + SPage) - (SPage div 4))) then
        begin
          FDesignScrollBox.VertScrollBar.Position := (CBot - SPage) + (SPage div 4);
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
  if (Length(ControlList) = 1) and (Supports(ControlList[0], IDesignEpiControl)) then
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

procedure TRuntimeDesignFrame.DeleteControls(ForceDelete: boolean);
var
  EpiDsgCtrl: IDesignEpiControl;
  EpiCtrl: TEpiCustomControlItem;
  i: Integer;
  S: String;
begin
  if FDesignPanel.Surface.Selector.IsSelected(FDesignPanel)
  then
    FDesignPanel.Surface.Selector.RemoveFromSelection(FDesignPanel);


  if (not ForceDelete) and
     (FDesignPanel.Surface.Count >= 1)
  then
  begin
    EpiCtrl := nil;

    if FDesignPanel.Surface.Count > 1 then
    begin
      for i := 0 to FDesignPanel.Surface.Count - 1 do
      begin
        if Supports(FDesignPanel.Surface.Selection[i], IDesignEpiControl, EpiDsgCtrl) and
           (not (EpiDsgCtrl.EpiControl is TEpiHeading))
        then
        begin
          EpiCtrl := EpiDsgCtrl.EpiControl;
          Break;
        end;
      end;

      if MessageDlg('Warning', 'Are you sure you want to delete?' + LineEnding +
                    'Number of selected controls: ' + IntToStr(FDesignPanel.Surface.Count),
                    mtWarning, mbYesNo, 0, mbNo) = mrNo
      then
        Exit;
    end else
    if Supports(FDesignPanel.Surface.Selection[0], IDesignEpiControl, EpiDsgCtrl) then
    begin
      EpiCtrl := EpiDsgCtrl.EpiControl;
      if EpiCtrl is TEpiSection then
        S := 'Section: ';
      if EpiCtrl is TEpiField then
        S := 'Field: ';
      if EpiCtrl is TEpiHeading then
        S := 'Heading: ';
      S += EpiCtrl.Name;

      if MessageDlg('Warning', 'Are you sure you want to delete?' + LineEnding +
                    S, mtWarning, mbYesNo, 0, mbNo) = mrNo
      then
        Exit;
    end else
    begin
      if MessageDlg('Warning', 'Are you sure you want to delete the extender?',
                    mtWarning, mbYesNo, 0, mbNo) = mrNo
      then
        Exit;
    end;


    if (DataFile.Size > 0) and
       ((EpiCtrl is TEpiField) or
         ((EpiCtrl is TEpiSection) and (TEpiSection(EpiCtrl).Fields.Count > 0))) and
       (MessageDlg('Warning', 'Field(s) contains data.' + LineEnding +
        'Are you sure you want to delete?', mtWarning, mbYesNo, 0, mbNo) = mrNo) then
      exit;
  end;

  FDesignPanel.Surface.DeleteComponents;
end;

procedure TRuntimeDesignFrame.SelecterBtnClick(Sender: TObject);
begin
  FAddClass := '';
  DoToogleBtn(Sender);
end;

procedure TRuntimeDesignFrame.SelectFirstActionExecute(Sender: TObject);
begin
  with FDesignScrollBox.VertScrollBar do
    Position := 0;

  SelectControl(dsaHome);
end;

procedure TRuntimeDesignFrame.SelectLastActionExecute(Sender: TObject);
begin
  with FDesignScrollBox.VertScrollBar do
    Position := Range;

  SelectControl(dsaEnd);
end;

procedure TRuntimeDesignFrame.SelectNextActionExecute(Sender: TObject);
begin
  SelectControl(dsaNext);
end;

procedure TRuntimeDesignFrame.SelectPgDnActionExecute(Sender: TObject);
begin
  with FDesignScrollBox.VertScrollBar do
    Position := Position + Page;

  SelectControl(dsaPgDn);
end;

procedure TRuntimeDesignFrame.SelectPgUpActionExecute(Sender: TObject);
begin
  with FDesignScrollBox.VertScrollBar do
    Position := Position - Page;

  SelectControl(dsaPgUp);
end;

procedure TRuntimeDesignFrame.SelectPriorActionExecute(Sender: TObject);
begin
  SelectControl(dsaPrior);
end;

procedure TRuntimeDesignFrame.ShowAlignFormActionExecute(Sender: TObject);
begin
  ShowAlignmentForm(Self);
end;

procedure TRuntimeDesignFrame.TestToolButtonClick(Sender: TObject);
begin
//  ShowAlignmentForm(Self);
end;

procedure TRuntimeDesignFrame.UndoActionExecute(Sender: TObject);
begin
  GlobalCommandList.Undo;
end;

procedure TRuntimeDesignFrame.UndoActionUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := GlobalCommandList.CanUndo;
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
            ftInteger:
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
          TEpiField(EpiCtrl).ShowValueLabel := ManagerSettings.ShowValuelabelText;
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
  ShowPropertiesForm(False);
end;

procedure TRuntimeDesignFrame.ExpandPageActionExecute(Sender: TObject);
var
  Controller: TDesignController;
  Surface: TJvDesignSurface;
  TopLeft: TPoint;
  R: Integer;
  P: TScrollBarInc;
begin
  Controller := TDesignController(FDesignPanel.Surface.Controller);
  Surface    := FDesignPanel.Surface;

  FDesignScrollBox.BeginUpdateBounds;
  if not Assigned(FExtender) then
    try
      TopLeft := Point(FDesignScrollBox.ClientWidth div 2,0);

      RegisterClass(TDesignStaticText);
      FCreatingControl := false;
      Controller.SetDragRect(Rect(TopLeft.X, TopLeft.Y, 0, 0));
      Surface.AddClass := 'TDesignStaticText';
      Surface.AddComponent;

      FExtender := TWinControl(Surface.Selection[0]);
    finally
      Controller.ClearDragRect;
      UnRegisterClass(TDesignStaticText);
    end
  else
    TopLeft := FExtender.BoundsRect.TopLeft;

  if FDesignScrollBox.VertScrollBar.IsScrollBarVisible then
    R := FDesignScrollBox.VertScrollBar.Range
  else
    R := FDesignScrollBox.ClientHeight;
  P := FDesignScrollBox.VertScrollBar.Page;
  TopLeft.Y := R + P;
  FExtender.SetBounds(TopLeft.X, TopLeft.Y, 0, 0);

  FDesignScrollBox.EndUpdateBounds;
end;

procedure TRuntimeDesignFrame.ExportToolButtonClick(Sender: TObject);
begin
  DoPrintDataForm;
end;

procedure TRuntimeDesignFrame.DeleteControlActionExecute(Sender: TObject);
begin
  DeleteControls(false);
end;

procedure TRuntimeDesignFrame.DeleteControlFastActionExecute(Sender: TObject);
begin
  DeleteControls(true);
end;

procedure TRuntimeDesignFrame.DesignControlPopUpMenuPopup(Sender: TObject);
var
  P: TPoint;
  Ctrl: TControl;
  SubMenu: TMenuItem;
  MI: TMenuItem;
  EpiCtrl: TEpiCustomControlItem;
begin
  FPopUpPoint := TPopupMenu(Sender).PopupPoint;

  P := FDesignPanel.ScreenToClient(FPopUpPoint);
  with FDesignPanel.Surface do
  begin
    Ctrl := FindControl(P.X, P.Y);

    if (csNoDesignSelectable in Ctrl.ControlStyle)
    then
      if (Assigned(Ctrl.Parent)) and
         (Supports(Ctrl.Parent, IDesignEpiControl))
      then
        Ctrl := Ctrl.Parent
      else
        Exit;

    if not Selector.IsSelected(Ctrl) then
    begin
      Select(Ctrl);
      SelectionChange;
      UpdateDesigner;
    end;

    // Delete previous "Select..." menu -> if it didn't exists then
    // .free is called on Nil which is OK (causes no A/V).
    // We cannot put this in OnClose for the pop-up because on Windows,
    // the close event is fired before the OnClick of the menu-item.
    // Hence -> the menuitem is deleted and no event is fired!
    DesignControlPopUpMenu.Items.Find('Select...').Free;

    // Create submenu for selected component.
    SubMenu := TMenuItem.Create(DesignControlPopUpMenu);
    SubMenu.Caption := 'Select...';
    SubMenu.Enabled := false;

    if (Selector.Count = 1) and
       (Ctrl is TDesignField)
    then
    begin;
      SubMenu.Enabled := true;
      EpiCtrl := (Ctrl as IDesignEpiControl).EpiControl;

      // Create Field select menu.
      if EpiCtrl is TEpiField then
      with TEpiField(EpiCtrl) do
      begin
        MI := TMenuItem.Create(SubMenu);
        MI.Caption := 'All ' + EpiTypeNames[FieldType] + ' fields';
        MI.OnClick := @SelectAllFieldsType;
        MI.Tag := PtrInt(EpiCtrl);
        SubMenu.Add(MI);

        MI := TMenuItem.Create(SubMenu);
        MI.Caption := 'All ' + EpiTypeNames[FieldType] + ' fields in section ' + Section.Caption.Text;
        MI.OnClick := @SelectAllFieldsTypeInSection;
        MI.Tag := PtrInt(EpiCtrl);
        SubMenu.Add(MI);

        SubMenu.AddSeparator;

        if FieldType in IntFieldTypes + FloatFieldTypes + StringFieldTypes then
        begin
          MI := TMenuItem.Create(SubMenu);
          MI.Caption := '- w. length: ' + IntToStr(Length);
          MI.OnClick := @SelectAllFieldsLength;
          MI.Tag := PtrInt(EpiCtrl);
          SubMenu.Add(MI);
        end;

        if Assigned(ValueLabelSet) then
        begin
          MI := TMenuItem.Create(SubMenu);
          MI.Caption := '- w. Valuelabelset: ' + ValueLabelSet.Name;
          MI.OnClick := @SelectAllFieldsValueLabel;
          MI.Tag := PtrInt(EpiCtrl);
          SubMenu.Add(MI);
        end;

        if Assigned(Ranges) then
        begin
          MI := TMenuItem.Create(SubMenu);
          MI.Caption := '- w. Range: ' + Ranges[0].AsString[true] + ' - ' + Ranges[0].AsString[false];
          MI.OnClick := @SelectAllFieldsRange;
          MI.Tag := PtrInt(EpiCtrl);
          SubMenu.Add(MI);
        end;

        if DefaultValueAsString <> '' then
        begin
          MI := TMenuItem.Create(SubMenu);
          MI.Caption := '- w. Default value: ' + DefaultValueAsString;
          MI.OnClick := @SelectAllFieldsDefaultValue;
          MI.Tag := PtrInt(EpiCtrl);
          SubMenu.Add(MI);
        end;

        if RepeatValue then
        begin
          MI := TMenuItem.Create(SubMenu);
          MI.Caption := '- w. Repeat Value';
          MI.OnClick := @SelectAllFieldsRepeat;
          MI.Tag := PtrInt(EpiCtrl);
          SubMenu.Add(MI);
        end;
      end;
      if SubMenu.Count = 2 then
        SubMenu.Items[1].Free;
    end;
    DesignControlPopUpMenu.Items.Add(SubMenu);
  end;
end;

procedure TRuntimeDesignFrame.DesignerActionListUpdate(AAction: TBasicAction;
  var Handled: Boolean);
begin
  if Screen.ActiveCustomForm <> MainForm then
    DesignerActionList.State := asSuspended
  else
    DesignerActionList.State := asNormal;
end;

procedure TRuntimeDesignFrame.DeleteAllActionExecute(Sender: TObject);
begin
  {$IFNDEF EPI_DEBUG}
  if MessageDlg('Warning', 'Are you sure you want to clear dataform?',
    mtWarning, mbYesNo, 0, mbNo) = mrNo then exit;
  {$ENDIF}

  FDesignPanel.Surface.Clear;
end;

procedure TRuntimeDesignFrame.CopyControlActionExecute(Sender: TObject);
begin
  GlobalCopyListClear;
  FDesignPanel.Surface.Selector.RemoveFromSelection(FExtender);
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

procedure TRuntimeDesignFrame.ClearDataActionExecute(Sender: TObject);
var
  Objs: TJvDesignObjectArray;
  ICtrl: IDesignEpiControl;
  EpiCtrl: TEpiCustomControlItem;
  i: Integer;
  j: Integer;

  procedure ResetData(F: TEpiField);
  begin
    F.ResetData;
  end;

begin
  if DataFile.Size = 0 then exit;

  if
    MessageDlg('WARNING',
      'You are about to clear data from all selected fields!' + LineEnding +
      'Do you wish to proceed further?',
      mtWarning,
      mbYesNo,
      0,
      mbNo
    ) = mrNo
  then
    Exit;

  Objs := DesignPanel.Surface.Selected;

  for i := Low(Objs) to high(Objs) do
  begin
    if not Supports(Objs[i], IDesignEpiControl, ICtrl) then
      Continue;

    EpiCtrl := ICtrl.EpiControl;
    if EpiCtrl is TEpiHeading then Continue;

    if EpiCtrl is TEpiField then
     ResetData(TEpiField(EpiCtrl));

    if EpiCtrl is TEpiSection then
    with TEpiSection(EpiCtrl) do
      for j := 0 to Fields.Count - 1 do
        ResetData(Field[j]);
  end;
end;

procedure TRuntimeDesignFrame.ClearDataActionUpdate(Sender: TObject);
var
  Objs: TJvDesignObjectArray;
  i: Integer;
  ActionEnabled: Boolean;
begin
  Objs := DesignPanel.Surface.Selected;

  ActionEnabled := false;

  if DataFile.Size > 0 then
  begin
    for i := Low(Objs) to High(Objs) do
    begin
      if (Objs[i] is TDesignField) then
      begin
        ActionEnabled := true;
        Break;
      end;

      if (Objs[i] is TDesignSection) and
         (TEpiSection(TDesignSection(Objs[i]).EpiControl).Fields.Count > 0)
      then
      begin
        ActionEnabled := true;
        Break;
      end;
    end;
  end;

  ClearDataAction.Enabled := ActionEnabled;
end;

procedure TRuntimeDesignFrame.AlignLeftActionExecute(Sender: TObject);
begin
  AlignControls(dcaLeftMost);
end;

procedure TRuntimeDesignFrame.AlignRightActionExecute(Sender: TObject);
begin
  AlignControls(dcaRightMost);
end;

procedure TRuntimeDesignFrame.AlignTopActionExecute(Sender: TObject);
begin
  AlignControls(dcaTopMost);
end;

procedure TRuntimeDesignFrame.AlignBottomActionExecute(Sender: TObject);
begin
  AlignControls(dcaBottomMost);
end;

procedure TRuntimeDesignFrame.ClearSelectionActionExecute(Sender: TObject);
begin
  FDesignPanel.Surface.ClearSelection;
  FDesignPanel.Surface.SelectionChange;
end;

procedure TRuntimeDesignFrame.CutControlActionExecute(Sender: TObject);
begin
  GlobalCopyListClear;
  FDesignPanel.Surface.Selector.RemoveFromSelection(FExtender);
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
  z: Integer;
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
      Surface.Select(Selected);

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
  if AlignmentFormIsVisible then
    ShowAlignFormAction.Execute;
end;

constructor TRuntimeDesignFrame.Create(TheOwner: TComponent);
var
  ScrollBox: TJvDesignScrollBox;
begin
  inherited Create(TheOwner);
  FDesignScrollBox := TJvDesignScrollBox.Create(self);
  FDesignScrollBox.Align := alClient;
  FDesignScrollBox.OnMouseWheel := @DesignScrollBoxMouseWheel;
  FDesignScrollBox.Parent := Self;

  FDesignPanel := TDesignPanel.Create(Self);
  FDesignPanel.OnGetAddClass := @GetAddClass;
  FDesignPanel.OnSelectionChange := @SelectionChange;
  FDesignPanel.OnPaint := @PaintDesignPanel;
  FDesignPanel.Align := alClient;
  FDesignPanel.Color := clWhite;
  FDesignPanel.Parent := FDesignScrollBox;
  FDesignPanel.Surface.ControllerClass := TDesignController;
  FDesignPanel.Surface.MessengerClass := TDesignMessenger;

  FActiveButton := SelectorToolButton;

  FHintWindow := THintWindow.Create(Self);
  FHintWindow.AutoHide := true;
  FHintWindow.HideInterval := 5 * 1000;

  FPropertiesForm := TPropertiesForm.Create(Self);
  FPropertiesForm.Name := 'TPropertiesForm1';
  FPropertiesForm.OnShowHintMsg := @ShowHintMsg;
  FPropertiesForm.UpdateSelection(nil);

  {$IFNDEF EPI_DEBUG}
  TestToolButton.Visible := false;
  Panel1.Visible := false;
  Splitter1.Visible := false;
  {$ENDIF}

  UpdateFrame;

  FPopUpPoint := Point(-1, -1);
  FSettingDataFile := false;
end;

procedure TRuntimeDesignFrame.UpdateFrame;
begin
  UpdateShortcuts;
  UpdateControls;
  UpdateInterface;
end;

class procedure TRuntimeDesignFrame.RestoreDefaultPos(F: TRuntimeDesignFrame);
var
  Aform: TForm;
begin
  if Assigned(F) then
    TPropertiesForm.RestoreDefaultPos(F.FPropertiesForm)
  else
    TPropertiesForm.RestoreDefaultPos(nil);

  TImportStructureForm.RestoreDefaultPos;
  AlignmentFormRestoreDefaultPos;
  DataSetViewerFormRestoreDefaultPos;
end;

procedure TRuntimeDesignFrame.ShowPropertiesForm(NewControl: boolean);
begin
  if not Assigned(FPropertiesForm) then exit;

  DockMaster.MakeDockable(FPropertiesForm, true, true);

//  FPropertiesForm.Show;
//  FPropertiesForm.SetFocus;

  if (not DockMaster.IsFloating(FPropertiesForm)) or
     NewControl
  then
    FPropertiesForm.SetFocusOnNew;
end;

function TRuntimeDesignFrame.IsShortCut(var Message: TLMKey): boolean;
begin
  result :=
    // Only execute our actionlist if mainform is active!
    (Screen.ActiveCustomForm = MainForm) and
    (DesignerActionList.IsShortCut(Message));

  // Else ready for implementing a larger Short-cut editor.
end;

function TRuntimeDesignFrame.ValidateControls: boolean;
begin
  Result := true;

  if Assigned(FPropertiesForm) then
     result := FPropertiesForm.ValidateControls;
end;

end.

