unit design_runtimedesigner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, ExtCtrls, StdCtrls,
  JvDesignSurface, design_customdesigner, epidatafiles, LMessages,
  manager_messages, epidatafilestypes;

type

  { TRuntimeDesignFrame }

  TRuntimeDesignFrame = class(TCustomDesignFrame)
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
    OtherToolButton: TToolButton;
    Panel1: TPanel;
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
    ToolButton1: TToolButton;
    ValueLabelLabel: TLabel;
    ValueLabelPanel: TPanel;
    procedure DeleteAllToolButtonClick(Sender: TObject);
    procedure DeleteToolButtonClick(Sender: TObject);
    procedure DesignPanelGetAddClass(Sender: TObject; var ioClass: string);
    procedure FieldBtnClick(Sender: TObject);
    procedure HeadingBtnClick(Sender: TObject);
    procedure SectionBtnClick(Sender: TObject);
    procedure SelecterBtnClick(Sender: TObject);
  private
    FDatafile: TEpiDataFile;
    FActiveButton: TToolButton;
    FDesignPanel: TJvDesignPanel;
    FAddClass: string;
    FLastSelectedFieldType: TEpiFieldType;
    procedure LMDesignerAdd(var Msg: TLMessage); message LM_DESIGNER_ADD;
    procedure DoToogleBtn(Sender: TObject);
  protected
    function GetDataFile: TEpiDataFile; override;
    procedure SetDataFile(AValue: TEpiDataFile); override;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure   UpdateFrame; override;
    procedure   RestoreDefaultPos; override;
  end;

implementation

{$R *.lfm}

uses
  JvDesignImp, epicustombase, design_types, design_designpanel,
  Graphics;

type

  { TDesignController }

  TDesignController = class(TJvDesignController)
  private
    FFrame: TRuntimeDesignFrame;
  protected
    function MouseDown(Button: TMouseButton; X, Y: Integer;
       TheMessage: TLMMouse): Boolean; override;
    function MouseMove(X, Y: Integer; TheMessage: TLMMouse): Boolean; override;
    function MouseUp(Button: TMouseButton; X, Y: Integer; TheMessage: TLMMouse
       ): Boolean; override;
  public
    constructor Create(ASurface: TJvDesignSurface); override;
  end;


{ TDesignController }

function TDesignController.MouseDown(Button: TMouseButton; X, Y: Integer;
  TheMessage: TLMMouse): Boolean;
begin
  Result := inherited MouseDown(Button, X, Y, TheMessage);
  if Assigned(FFrame) then
    FFrame.Label1.Caption := 'Mouse (1): X = ' + IntToStr(X) + ' | Y = ' + IntToStr(Y);

  if Assigned(FFrame) then
    FFrame.Label2.Caption := 'Mouse (2): X = ' + IntToStr(TheMessage.XPos) + ' | Y = ' + IntToStr(TheMessage.YPos);
end;

function TDesignController.MouseMove(X, Y: Integer; TheMessage: TLMMouse
  ): Boolean;
begin
  Result := inherited MouseMove(X, Y, TheMessage);

  if Assigned(FFrame) then
    FFrame.Label1.Caption := 'Mouse (1): X = ' + IntToStr(X) + ' | Y = ' + IntToStr(Y);

//  if Assigned(FFrame) then
//    FFrame.Label2.Caption := 'Mouse (2): X = ' + IntToStr(TheMessage.XPos) + ' | Y = ' + IntToStr(TheMessage.YPos);
end;

function TDesignController.MouseUp(Button: TMouseButton; X, Y: Integer;
  TheMessage: TLMMouse): Boolean;
begin
  Result := inherited MouseUp(Button, X, Y, TheMessage);
  if Assigned(FFrame) then
    FFrame.Label1.Caption := 'Mouse (1): X = ' + IntToStr(X) + ' | Y = ' + IntToStr(Y);

  if Assigned(FFrame) then
    FFrame.Label2.Caption := 'Mouse (2): X = ' + IntToStr(TheMessage.XPos) + ' | Y = ' + IntToStr(TheMessage.YPos);
end;

constructor TDesignController.Create(ASurface: TJvDesignSurface);
var
  P: TControl;
begin
  inherited Create(ASurface);
  P := ASurface.Container;

  while (P <> nil) and (not (P is TRuntimeDesignFrame)) do
    P := P.Parent;

  if P <> nil then
    FFrame := TRuntimeDesignFrame(P);;
end;

{ TRuntimeDesignFrame }

procedure TRuntimeDesignFrame.LMDesignerAdd(var Msg: TLMessage);
var
  EpiCtrlClass: TEpiCustomControlItemClass;
  Ctrl: TControl;
  EpiCtrl: TEpiCustomControlItem;
  Section: TEpiSection;
begin
  Ctrl := TControl(Msg.WParam);
  EpiCtrlClass := TEpiCustomControlItemClass(Msg.LParam);

  Section := TEpiSection((Ctrl.Parent as IDesignEpiControl).EpiControl);

  if EpiCtrlClass = TEpiHeading then
    EpiCtrl := Section.NewHeading;
  if EpiCtrlClass = TEpiField then
    EpiCtrl := Section.NewField(FLastSelectedFieldType);
  if EpiCtrlClass = TEpiSection then
    EpiCtrl := FDatafile.NewSection;


  (Ctrl as IDesignEpiControl).EpiControl := EpiCtrl;
end;

procedure TRuntimeDesignFrame.DoToogleBtn(Sender: TObject);
begin
  if not (Sender is TToolButton) then exit;
  FActiveButton.Down := false;

  if FActiveButton = OtherToolButton then
    OtherToolButton.ImageIndex := 7;

  TToolButton(Sender).Down := true;
  FActiveButton := TToolButton(Sender);
end;

procedure TRuntimeDesignFrame.DesignPanelGetAddClass(Sender: TObject;
  var ioClass: string);
begin
  if FActiveButton = SelectorToolButton then exit;

  if FAddClass = 'TDesignField' then
    FLastSelectedFieldType := TEpiFieldType(FActiveButton.Tag);

  ioClass := FAddClass;
  SelectorToolButton.Click;
end;

procedure TRuntimeDesignFrame.FieldBtnClick(Sender: TObject);
begin
  FAddClass := 'TDesignField';
  DoToogleBtn(Sender);
end;

procedure TRuntimeDesignFrame.HeadingBtnClick(Sender: TObject);
begin
  FAddClass := 'TDesignHeading';
  DoToogleBtn(Sender);
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

procedure TRuntimeDesignFrame.DeleteToolButtonClick(Sender: TObject);
begin
  FDesignPanel.Surface.DeleteComponents;
end;

procedure TRuntimeDesignFrame.DeleteAllToolButtonClick(Sender: TObject);
begin
  FDesignPanel.Surface.Clear;
end;

function TRuntimeDesignFrame.GetDataFile: TEpiDataFile;
begin
  result := FDatafile;
end;

procedure TRuntimeDesignFrame.SetDataFile(AValue: TEpiDataFile);
begin
  FDatafile := AValue;
  (FDesignPanel as IDesignEpiControl).EpiControl := FDatafile.MainSection;

  FDesignPanel.Active := true;
  TJvDesignSelector(FDesignPanel.Surface.Selector).HandleWidth := 4;
end;

constructor TRuntimeDesignFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FDesignPanel := TDesignPanel.Create(Self);
  FDesignPanel.OnGetAddClass := @DesignPanelGetAddClass;
  FDesignPanel.Align := alClient;
  FDesignPanel.Color := clWhite;
  FDesignPanel.Parent := Self;

  FActiveButton := SelectorToolButton;
  FDesignPanel.Surface.ControllerClass := TDesignController;
end;

procedure TRuntimeDesignFrame.UpdateFrame;
begin
  //
end;

procedure TRuntimeDesignFrame.RestoreDefaultPos;
begin
  //
end;

end.

