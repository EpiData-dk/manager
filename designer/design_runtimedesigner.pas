unit design_runtimedesigner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, ExtCtrls, StdCtrls,
  JvDesignSurface, epidatafiles, LMessages, ActnList, Menus,
  manager_messages, epidatafilestypes, design_properties_form, types,
  epicustombase;

type

  { TRuntimeDesignFrame }

  TRuntimeDesignFrame = class(TFrame)
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
    ValueLabelLabel: TLabel;
    ValueLabelPanel: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure ClearSelectionActionExecute(Sender: TObject);
    procedure CopyControlActionExecute(Sender: TObject);
    procedure CutControlActionExecute(Sender: TObject);
    procedure DeleteAllActionExecute(Sender: TObject);
    procedure DeleteControlActionExecute(Sender: TObject);
    procedure DesignControlPopUpMenuClose(Sender: TObject);
    procedure DesignControlPopUpMenuPopup(Sender: TObject);
    procedure EditControlActionExecute(Sender: TObject);
    procedure FieldBtnClick(Sender: TObject);
    function FieldNamePrefix: string;
    procedure HeadingBtnClick(Sender: TObject);
    procedure JvDesignScrollBox1MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure PasteAsDateActionExecute(Sender: TObject);
    procedure PasteAsFloatActionExecute(Sender: TObject);
    procedure PasteAsIntActionExecute(Sender: TObject);
    procedure PasteAsStringActionExecute(Sender: TObject);
    procedure PasteControlActionExecute(Sender: TObject);
    procedure PasteAsHeadingActionExecute(Sender: TObject);
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
  manager_globals, managerprocs,
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

  Controller := TDesignController(FDesignPanel.Surface.Controller);
  Surface    := FDesignPanel.Surface;

  FLastSelectedFieldType := Ft;

  try
    ReadClipBoard(Cbl);
    for i := 0 to Cbl.Count -1 do
      begin
        if Trim(Cbl[i]) = '' then continue;

        FCreatingControl := true;
        Controller.SetDragRect(Rect(P.X, P.Y, 0, 0));
        Surface.AddClass := 'TDesignField';
        Surface.AddComponent;

        F := TDesignField(Surface.Selection[0]);
        TEpiField(F.EpiControl).Question.Text := Cbl[i];

        Inc(P.Y, ManagerSettings.SpaceBtwFieldField + F.Height);
      end;
  finally
    Cbl.Free;
    Controller.ClearDragRect;
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
  // TODO : Implement
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
begin
  FDesignPanel.Surface.PasteComponents;
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

  Controller := TDesignController(FDesignPanel.Surface.Controller);
  Surface    := FDesignPanel.Surface;

  try
    ReadClipBoard(Cbl);
    for i := 0 to Cbl.Count -1 do
      begin
        if Trim(Cbl[i]) = '' then continue;

        FCreatingControl := true;
        Controller.SetDragRect(Rect(P.X, P.Y, 0, 0));
        Surface.AddClass := 'TDesignHeading';
        Surface.AddComponent;

        L := TDesignHeading(Surface.Selection[0]);
        TEpiHeading(L.EpiControl).Caption.Text := Cbl[i];

        Inc(P.Y, ManagerSettings.SpaceBtwLabelLabel + L.Height);
      end;
  finally
    Cbl.Free;
    Controller.ClearDragRect;
  end;
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
        EpiCtrl := TEpiSection((Ctrl.Parent as IDesignEpiControl).EpiControl).NewHeading;
      if Ctrl is TDesignSection then
        EpiCtrl := DataFile.NewSection;

      ApplyCommonCtrlSetting(Ctrl, EpiCtrl);
      FCreatingControl := false;
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

procedure TRuntimeDesignFrame.DesignControlPopUpMenuClose(Sender: TObject);
begin
  FPopUpPoint := Point(-1,-1);
end;

procedure TRuntimeDesignFrame.DesignControlPopUpMenuPopup(Sender: TObject);
var
  P: TPoint;
begin
  FPopUpPoint := TPopupMenu(Sender).PopupPoint;

  P := FDesignPanel.ScreenToClient(FPopUpPoint);
  FDesignPanel.Surface.Select(FDesignPanel.Surface.FindControl(P.X, P.Y));
  FDesignPanel.Surface.UpdateDesigner;
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
        begin
          Surface.Select(FDesignPanel);

          with Section[i] do
            Controller.SetDragRect(Bounds(Left, Top, Width, Height));

          Surface.AddClass := 'TDesignSection';
          Surface.AddComponent;

          Selected := FDesignPanel.Surface.SelectedContainer;
        end
      else
        Selected := FDesignPanel;

      ApplyCommonCtrlSetting(Selected, S);

      for j := 0 to S.Fields.Count - 1 do
        begin
          F := S.Field[j];
          P := DesignClientToParent(Point(F.Left, F.Top), Selected, FDesignPanel);
          Controller.SetDragRect(Rect(P.X, P.Y, 0, 0));
          Surface.AddClass := 'TDesignField';
          Surface.AddComponent;
          ApplyCommonCtrlSetting(Surface.Selection[0], F);
        end;

      for j := 0 to S.Headings.Count - 1 do
        begin
          H := S.Heading[j];
          P := DesignClientToParent(Point(H.Left, H.Top), Selected, FDesignPanel);
          Controller.SetDragRect(Rect(P.X, P.Y, 0, 0));
          Surface.AddClass := 'TDesignHeading';
          Surface.AddComponent;
          ApplyCommonCtrlSetting(Surface.Selection[0], H);
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

