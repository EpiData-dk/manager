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
    RangeLabel: TLabel;
    RangePanel: TPanel;
    RecordsLabel: TLabel;
    RecordsPanel: TPanel;
    RecordStaticLabel: TLabel;
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
    procedure EditControlActionExecute(Sender: TObject);
    procedure FieldBtnClick(Sender: TObject);
    function FieldNamePrefix: string;
    procedure HeadingBtnClick(Sender: TObject);
    procedure JvDesignScrollBox1MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure PasteControlActionExecute(Sender: TObject);
    procedure SectionBtnClick(Sender: TObject);
    procedure SelecterBtnClick(Sender: TObject);
    procedure TestToolButtonClick(Sender: TObject);
  private
    FDatafile: TEpiDataFile;
    FActiveButton: TToolButton;
    FDesignPanel: TJvDesignPanel;
    FAddClass: string;
    FLastSelectedFieldType: TEpiFieldType;
    FPropertiesForm: TPropertiesForm;
    FSettingDataFile: boolean;
    FCreatingControl: boolean;
    procedure GetAddClass(Sender: TObject; var ioClass: string);
    procedure SelectionChange(Sender: TObject);
    procedure DoToogleBtn(Sender: TObject);
    function DesignPanelAsJvObjectArray: TJvDesignObjectArray;
    procedure ApplyCommonCtrlSetting(Ctrl: TControl;
      EpiCtrl: TEpiCustomControlItem);
  protected
    function GetDataFile: TEpiDataFile;
    procedure SetDataFile(AValue: TEpiDataFile);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure   UpdateFrame;
    procedure   RestoreDefaultPos;
    function    IsShortCut(var Message: TLMKey): boolean;
    property DataFile: TEpiDataFile read GetDataFile write SetDataFile;
  end;

implementation

{$R *.lfm}

uses
  JvDesignImp, design_types, design_designpanel,
  Graphics, design_designcontroller, design_designmessenger,
  main, epistringutils, JvDesignUtils, settings2_var,
  manager_globals,
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

  if EpiCtrl is TEpiSection then
    TEpiSection(EpiCtrl).Fields.OnGetPrefix := @FieldNamePrefix;

  //Ctrl.PopupMenu := DesignControlPopUpMenu;
end;


procedure TRuntimeDesignFrame.GetAddClass(Sender: TObject; var ioClass: string);
begin
  if FActiveButton = SelectorToolButton then exit;

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

procedure TRuntimeDesignFrame.PasteControlActionExecute(Sender: TObject);
begin
  FDesignPanel.Surface.PasteComponents;
end;

procedure TRuntimeDesignFrame.SectionBtnClick(Sender: TObject);
begin
  FAddClass := 'TDesignSection';
  DoToogleBtn(Sender);
end;

procedure TRuntimeDesignFrame.SelecterBtnClick(Sender: TObject);
begin
  FAddClass := '';
  DoToogleBtn(Sender);
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
  // TODO : Update PropertiesForm.
  Label4.Caption := 'Selection Count: ' + IntToStr(FDesignPanel.Surface.Count);


  if (FCreatingControl) and
     (not FSettingDataFile)
  then
    begin
      Ctrl := FDesignPanel.Surface.Selection[0];

      if Ctrl is TDesignField then
        EpiCtrl := TEpiSection((Ctrl.Parent as IDesignEpiControl).EpiControl).NewField(FLastSelectedFieldType);
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

procedure TRuntimeDesignFrame.DeleteAllActionExecute(Sender: TObject);
begin
  FDesignPanel.Surface.Clear;
end;

procedure TRuntimeDesignFrame.CopyControlActionExecute(Sender: TObject);
begin
  GlobalCopyList.Clear;
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
  GlobalCopyList.Clear;
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

  Controller.SetDragRect(Rect(0,0,0,0));
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
  if ActionList1.IsShortCut(Message) then
    exit;

  // Else ready for implementing a larger Short-cut editor.
end;

end.

