unit design_runtimedesigner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, ExtCtrls, StdCtrls,
  JvDesignSurface, design_customdesigner, epidatafiles;

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
    DesignPanel: TJvDesignPanel;
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
    procedure ToogleBtn(Sender: TObject);
  private
    FDatafile: TEpiDataFile;
    FActiveButton: TToolButton;
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
  JvDesignImp, LMessages;

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

procedure TRuntimeDesignFrame.ToogleBtn(Sender: TObject);
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

  case FActiveButton.Tag of
    1,3,12,4:
      ioClass := 'TDesignField';
    5:
      ioClass := 'TDesignHeading';
    6:
      ioClass := 'TDesignSection';
  end;

  SelectorToolButton.Click;
end;

procedure TRuntimeDesignFrame.DeleteToolButtonClick(Sender: TObject);
begin
  DesignPanel.Surface.DeleteComponents;
end;

procedure TRuntimeDesignFrame.DeleteAllToolButtonClick(Sender: TObject);
begin
  DesignPanel.Surface.Clear;
end;

function TRuntimeDesignFrame.GetDataFile: TEpiDataFile;
begin
  result := FDatafile;
end;

procedure TRuntimeDesignFrame.SetDataFile(AValue: TEpiDataFile);
begin
  FDatafile := AValue;

  DesignPanel.Active := true;
  TJvDesignSelector(DesignPanel.Surface.Selector).HandleWidth := 4;
end;

constructor TRuntimeDesignFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FActiveButton := SelectorToolButton;
  DesignPanel.Surface.ControllerClass := TDesignController;
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

