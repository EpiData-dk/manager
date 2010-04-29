unit design_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, FileUtil, LResources, Forms, ComCtrls, Controls,
  ActnList, ExtCtrls, StdCtrls, epidatafiles, epidatafilestypes, design_field,
  design_heading, design_section, epicustombase, design_custombase;

type

  { TDesignFrame }

  TDesignFrame = class(TFrame)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    NewSectionAction: TAction;
    NewHeadingAction: TAction;
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
    Panel1: TPanel;
    SelectorToolButton: TToolButton;
    ToolButton10: TToolButton;
    FloatToolButton: TToolButton;
    StringToolButton: TToolButton;
    DateToolButton: TToolButton;
    OtherToolButton: TToolButton;
    ToolButton2: TToolButton;
    ToolButton6: TToolButton;
    HeadingToolButton: TToolButton;
    SectionToolButton: TToolButton;
    ToolButton9: TToolButton;
    procedure   ToggleToolBtn(Sender: TObject);
  private
    { common private }
    FDataFile: TEpiDataFile;
    FDesignerBox: TScrollBox;
    FActiveButton: TToolButton;
    procedure DockSiteGetSiteInfo(Sender: TObject; DockClient: TControl;
      var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
    function    ShowForm(EpiControl: TEpiCustomControlItem;
      Pos: TPoint): TModalResult;
    function    NewDesignControl(AClass: TControlClass;
      AParent: TWinControl; Pos: TPoint;
      EpiControl: TEpiCustomControlItem): TControl;
    procedure DesignControlStartDock(Sender: TObject; var DragObject: TDragDockObject);

    { Docksite controls - methods }
    procedure DockSiteMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DockSiteMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DockSiteDockDrop(Sender: TObject; Source: TDragDockObject; X,
      Y: Integer);
    procedure DockSiteUnDock(Sender: TObject; Client: TControl;
      NewTarget: TWinControl; var Allow: Boolean);
  private
    { Position handling }
    function    FindNewPosition(ParentControl: TWinControl;
      AClass: TControlClass): TPoint;
  private
    { Field Handling }
    function    NewField(FieldType: TEpiFieldType): TEpiField;
  private
    { Heading Handling }
    function    NewHeading: TEpiHeading;
  private
    { Section handling }
    FActiveSection: TEpiSection;
    FMouseDownPos: TPoint;
    function    NewSection: TEpiSection;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; ADataFile: TEpiDataFile);
    property    DataFile: TEpiDataFile read FDataFile;
  end; 

implementation

{$R *.lfm}

uses
  Graphics;

{ TDesignFrame }

procedure TDesignFrame.ToggleToolBtn(Sender: TObject);
begin
  if not (Sender is TToolButton) then exit;
  FActiveButton.Down := false;
  TToolButton(Sender).Down := true;
  FActiveButton := TToolButton(Sender);
end;

procedure TDesignFrame.DockSiteGetSiteInfo(Sender: TObject; DockClient: TControl;
  var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
var
  Sdr: TWinControl absolute Sender;
  S: String;
  Tl, Br: TPOint;

  function PointInRect(APoint: TPoint; ARect: TRect): boolean;
  begin
    result :=
      (ARect.Left < APoint.X) and (APoint.X < ARect.Right) and
      (ARect.Top < APoint.Y) and (APoint.Y < ARect.Bottom);
  end;

begin

  Label1.Caption := 'Sender: ' + Sdr.Name;
  Label2.Caption := 'DockClient: ' + DockClient.Name;

  With InfluenceRect do
    Label3.Caption := Format('Infl: (%d,%d)-(%d,%d)', [Left, Top, right, Bottom]);
  With MousePos do
    Label4.Caption := Format('Mouse: (%d,%d)', [X, Y]);

  if not PointInRect(MousePos, InfluenceRect) then
    CanDock := false;

  WriteStr(s, CanDock);
  Label5.Caption := s;
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
  L: TDesignHeading;
begin
  Result := AClass.Create(AParent);
  (Result as IDesignEpiControl).EpiControl := EpiControl;
  if Result is TDesignSection then
  with TDesignSection(Result) do
  begin
    Name := 'DesignSection' + IntToStr(AParent.ControlCount);
    EpiControl.Left := FMouseDownPos.X;
    EpiControl.Top := FMouseDownPos.Y;
    Width := Abs(Pos.X - FMouseDownPos.X);
    Height := Abs(Pos.Y - FMouseDownPos.Y);
    DockSite := true;
    DragKind := dkDock;
    DragMode := dmAutomatic;
    OnMouseDown := @DockSiteMouseDown;
    OnMouseUp := @DockSiteMouseUp;
    OnStartDock := @DesignControlStartDock;
    OnDockDrop := @DockSiteDockDrop;
    OnUnDock   := @DockSiteUnDock;
    OnGetSiteInfo := @DockSiteGetSiteInfo;
  end else begin
    EpiControl.Left := Pos.X;
    EpiControl.Top  := Pos.Y;
    if Result is TDesignHeading then
    with TDesignHeading(Result) do
    begin
      Name := 'DesignHeading' + IntToStr(AParent.ControlCount);
      DragKind := dkDock;
      DragMode := dmAutomatic;
      OnGetSiteInfo := @DockSiteGetSiteInfo;
    end else
    with TDesignField(Result) do
    begin
      Name := 'DesignField' + IntToStr(AParent.ControlCount);
      DragKind := dkDock;
      DragMode := dmAutomatic;
      OnGetSiteInfo := @DockSiteGetSiteInfo;
    end;
  end;
  Result.Parent   := AParent;
end;

procedure TDesignFrame.DesignControlStartDock(Sender: TObject;
  var DragObject: TDragDockObject);
begin
  DragObject := TDragDockObject.Create(TControl(Sender));
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
begin
  // If this is not Section tool, then no need to record anything.
  if FActiveButton.Tag <> 6 then exit;

  FMouseDownPos := Point(X, Y);
end;

procedure TDesignFrame.DockSiteMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  SenderWin: TWinControl absolute Sender;
  ScreenPt: TPoint;
  ParentPt: TPoint;
  EpiControl: TEpiCustomControlItem;
const
  TagToFieldType: array[1..4] of TEpiFieldType = (
    ftInteger, ftFloat, ftString, ftDMYDate
  );
begin
  ParentPt := Point(X, Y);
  ScreenPt := SenderWin.ClientToScreen(Point(X, Y));
  case FActiveButton.Tag of
    // Selector
    0: Exit;

    // Integer, Float, String, Date
    1, 2, 3, 4:
      begin
        EpiControl := NewField(TagToFieldType[FActiveButton.Tag]);
        if ShowForm(EpiControl, ScreenPt) = mrOK then
          NewDesignControl(TDesignField, SenderWin, ParentPt, EpiControl);
      end;
    // Heading
    5:
      begin
        EpiControl := NewHeading;
        if ShowForm(EpiControl, ScreenPt) = mrOK then
          NewDesignControl(TDesignHeading, SenderWin, ParentPt, EpiControl);
      end;
    // Section
    6:
      begin
        // Sections can only be created on the designer.
        if not (Sender = FDesignerBox) then exit;
        EpiControl := NewSection;
        if ShowForm(EpiControl, ScreenPt) = mrOK then
          NewDesignControl(TDesignSection, SenderWin, ParentPt, EpiControl);
      end;
  end;
  ToggleToolBtn(SelectorToolButton);
end;

procedure TDesignFrame.DockSiteDockDrop(Sender: TObject;
  Source: TDragDockObject; X, Y: Integer);
var
  WinSender: TWinControl absolute Sender;
begin
  (Source.Control as IDesignEpiControl).EpiControl.Left := X - Source.DockOffset.X;
  (Source.Control as IDesignEpiControl).EpiControl.Top  := Y - Source.DockOffset.Y;
end;

procedure TDesignFrame.DockSiteUnDock(Sender: TObject; Client: TControl;
  NewTarget: TWinControl; var Allow: Boolean);
begin
  Allow := false;
  // NewTarget = false: trying to release the client outside of program window.
  if not Assigned(NewTarget) then exit;
  // This effectively prevents dragging section onto section.
  if (NewTarget.ClassType = Client.ClassType) then exit;
  if (NewTarget = FDesignerBox) or
    (NewTarget is TDesignSection) then Allow := true;
end;

constructor TDesignFrame.Create(TheOwner: TComponent; ADataFile: TEpiDataFile);
begin
  inherited Create(TheOwner);
  FDataFile := ADataFile;
  FActiveButton := SelectorToolButton;
  FActiveSection := ADataFile.MainSection;
  DragManager.DragThreshold := 5;
  DragManager.DragImmediate := False;
  // Designer box creation and setup.
  // - (This is subject to change if we find a better component than
  //    the form or a scrollbox).
  FDesignerBox             := TScrollBox.Create(Self);
  FDesignerBox.Name        := 'DesingerBox';
  FDesignerBox.Parent      := Self;
  FDesignerBox.Align       := alClient;
  FDesignerBox.DockSite    := true;
  FDesignerBox.Color       := clWhite;
  FDesignerBox.AutoScroll  := true;
  FDesignerBox.OnMouseUp   := @DockSiteMouseUp;
  FDesignerBox.OnMouseDown := @DockSiteMouseDown;
  FDesignerBox.OnDockDrop  := @DockSiteDockDrop;
  FDesignerBox.OnUnDock    := @DockSiteUnDock;
  FDesignerBox.OnGetSiteInfo := @DockSiteGetSiteInfo;
end;

end.

