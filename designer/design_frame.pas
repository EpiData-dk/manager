unit design_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, FileUtil, LResources, Forms, ComCtrls, Controls,
  ActnList, ExtCtrls, StdCtrls, Menus, epidatafiles, epidatafilestypes,
  design_field, design_heading, design_section, epicustombase,
  design_custombase;

type

  { TDesignFrame }

  TDesignFrame = class(TFrame)
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
    EpiControlPopUpMenu: TPopupMenu;
    SelectorToolButton: TToolButton;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    FloatToolButton: TToolButton;
    StringToolButton: TToolButton;
    DateToolButton: TToolButton;
    OtherToolButton: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton6: TToolButton;
    HeadingToolButton: TToolButton;
    SectionToolButton: TToolButton;
    ToolButton9: TToolButton;
    procedure   Button1Click(Sender: TObject);
    procedure   DeleteControlActionExecute(Sender: TObject);
    procedure   EditControlActionExecute(Sender: TObject);
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
    function    ShowForm(EpiControl: TEpiCustomControlItem;
      Pos: TPoint): TModalResult;
    procedure   ShowEpiControlPopup(Sender: TControl; Pos: TPoint);

  private
    { Docksite controls - methods }
    procedure   DockSiteMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure   DockSiteMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure   DockSiteDockOver(Sender: TObject; Source: TDragDockObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure   DockSiteDockDrop(Sender: TObject; Source: TDragDockObject; X,
      Y: Integer);
    procedure   DockSiteUnDock(Sender: TObject; Client: TControl;
      NewTarget: TWinControl; var Allow: Boolean);

  private
    { Design controls - methods }
    FActiveControl: TControl;
    function    NewDesignControl(AClass: TControlClass;
      AParent: TWinControl; Pos: TPoint;
      EpiControl: TEpiCustomControlItem): TControl;
    function    NewSectionControl(StartPos, EndPos: TPoint;
      EpiControl: TEpiCustomControlItem): TControl;
    procedure   DesignControlMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure   DesignControlMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure   EnterControl(Sender: TObject);
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
    function    NewSection: TEpiSection;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; ADataFile: TEpiDataFile);
    property    DataFile: TEpiDataFile read FDataFile;
  end; 

implementation

{$R *.lfm}

uses
  Graphics, Clipbrd, epidocument, epiadmin, math;

type

  { TScrollBoxEx }

  TScrollBoxEx = class(TScrollBox, IDesignEpiControl)
  private
    FEpiControl: TEpiCustomControlItem;
    function GetEpiControl: TEpiCustomControlItem;
    procedure SetEpiControl(const AValue: TEpiCustomControlItem);
  public
    property EpiControl: TEpiCustomControlItem read GetEpiControl write SetEpiControl;
  end;

  TControlEx = class(TControl);

{ TScrollBoxEx }

function TScrollBoxEx.GetEpiControl: TEpiCustomControlItem;
begin
  result := FEpiControl;
end;

procedure TScrollBoxEx.SetEpiControl(const AValue: TEpiCustomControlItem);
begin
  if FEpiControl = AValue then exit;
  FEpiControl := AValue;
end;


{ TDesignFrame }

procedure TDesignFrame.ToggleToolBtn(Sender: TObject);
begin
  if not (Sender is TToolButton) then exit;
  FActiveButton.Down := false;
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

procedure TDesignFrame.Button1Click(Sender: TObject);
begin
  Clipboard.AsText := DataFile.SaveToXml('', 0);
end;

procedure TDesignFrame.DeleteControlActionExecute(Sender: TObject);
var
  EpiCtrl: TEpiCustomControlItem;
  LocalCtrl: TControl;
begin
  EpiCtrl := (FActiveControl as IDesignEpiControl).EpiControl;

  // TODO : Find next control in line.
  LocalCtrl := FActiveControl;
  EnterControl(FDesignerBox);

  // TODO : Show warning when containing data.
  TEpiCustomList(EpiCtrl.Owner).RemoveItem(EpiCtrl);
  EpiCtrl.Free;
  LocalCtrl.Free;
end;

procedure TDesignFrame.EditControlActionExecute(Sender: TObject);
var
  EpiCtrl: TEpiCustomControlItem;
  Pt: TPoint;
begin
  EpiCtrl := (FActiveControl as IDesignEpiControl).EpiControl;
  if (FRightMouseUp.X = -1) then
    Pt := FActiveControl.Parent.ClientToScreen(Point(EpiCtrl.Left, EpiCtrl.Top))
  else
    Pt := FRightMouseUp;
  ShowForm(EpiCtrl, Pt);
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
  Ctrl: TControlEx;
begin
  Result := AClass.Create(AParent);
  Ctrl := TControlEx(Result);

  (Ctrl as IDesignEpiControl).EpiControl := EpiControl;
  with Ctrl do
  begin
    OnMouseDown := @DesignControlMouseDown;
    OnMouseUp   := @DesignControlMouseUp;
  end;

  with EpiControl do
  begin
    BeginUpdate;
    Left := Pos.X;
    Top  := Pos.Y;
    EndUpdate;
  end;

  Result.Parent  := AParent;
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
    OnMouseUp   := @DockSiteMouseUp;
    OnDockDrop  := @DockSiteDockDrop;
    OnUnDock    := @DockSiteUnDock;
    OnDockOver  := @DockSiteDockOver;
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

  // Forces designer box last in docksite list, because retrieving docksite
  // is not implemented fully in the LCL.
  FDesignerBox.DockSite := false;
  FDesignerBox.DockSite := true;
end;

procedure TDesignFrame.ShowEpiControlPopup(Sender: TControl; Pos: TPoint);
begin
  EpiControlPopUpMenu.PopUp(Pos.X, Pos.Y);
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
var
  WinSender: TWinControl absolute Sender;
begin
  FActiveSection := TEpiSection((Sender as IDesignEpiControl).EpiControl);
  case Button of
    mbLeft: FLeftMouseDown := WinSender.ClientToScreen(Point(X, Y));
    mbRight: FRightMouseDown := WinSender.ClientToScreen(Point(X, Y));
  end;

  EnterControl(Sender);
end;

procedure TDesignFrame.DockSiteMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  WinSender: TWinControl absolute Sender;
  ParentPt: TPoint;
  EpiControl: TEpiCustomControlItem;
const
  TagToFieldType: array[1..4] of TEpiFieldType = (
    ftInteger, ftFloat, ftString, ftDMYDate
  );
begin
  case Button of
    mbLeft: FLeftMouseUp := WinSender.ClientToScreen(Point(X, Y));
    mbRight: FRightMouseUp := WinSender.ClientToScreen(Point(X, Y));
  end;

  // Right button activates popup-menu
  if Button = mbRight then
  begin
    ShowEpiControlPopup(WinSender, FRightMouseUp);
    Exit;
  end;

  ParentPt := WinSender.ScreenToClient(FLeftMouseUp);

  case FActiveButton.Tag of
    // Selector
    0: Exit;

    // Integer, Float, String, Date
    1, 2, 3, 4:
      begin
        EpiControl := NewField(TagToFieldType[FActiveButton.Tag]);
        if ShowForm(EpiControl, FLeftMouseUp) = mrOK then
          NewDesignControl(TDesignField, WinSender, ParentPt, EpiControl)
        else begin
          FActiveSection.Fields.RemoveItem(EpiControl);
          EpiControl.Free;
        end;
      end;
    // Heading
    5:
      begin
        EpiControl := NewHeading;
        if ShowForm(EpiControl, FLeftMouseUp) = mrOK then
          NewDesignControl(TDesignHeading, WinSender, ParentPt, EpiControl)
        else begin
          FActiveSection.Headings.RemoveItem(EpiControl);
          EpiControl.Free;
        end;
      end;
    // Section
    6:
      begin
        // Sections can only be created on the designer.
        if not (Sender = FDesignerBox) then exit;
        EpiControl := NewSection;
        if ShowForm(EpiControl, FLeftMouseDown) = mrOK then
          NewSectionControl(WinSender.ScreenToClient(FLeftMouseDown),
            WinSender.ScreenToClient(FLeftMouseUp), EpiControl)
        else begin
          FDataFile.Sections.RemoveItem(EpiControl);
          EpiControl.Free;
        end;
      end;
  end;
  ToggleToolBtn(SelectorToolButton);
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
end;

procedure TDesignFrame.DockSiteDockDrop(Sender: TObject;
  Source: TDragDockObject; X, Y: Integer);
var
  NSection, OSection: TEpiSection;
  EpiControl: TEpiCustomControlItem;
begin
  // Sender = the site being dragged onto.
  // Source.control = the control being dragged.

  EpiControl := (Source.Control as IDesignEpiControl).EpiControl;
  EpiControl.Left := X - Source.DockOffset.X;
  EpiControl.Top  := Y - Source.DockOffset.Y;

  // Sanity checks:
  // - sections do not need to be relocated in the Core structure.
  if EpiControl is TEpiSection then exit;

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
var
  LocalSection: TEpiSection;
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
begin
  FActiveControl := TControl(Sender);
  DeleteControlAction.Enabled := (Sender <> FDesignerBox);
end;

constructor TDesignFrame.Create(TheOwner: TComponent; ADataFile: TEpiDataFile);
var
  LocalAdm: TEpiAdmin;
  Grp: TEpiGroup;
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
  FDesignerBox             := TScrollBoxEx.Create(Self);
  FDesignerBox.Name        := 'DesignerBox';
  FDesignerBox.Parent      := Self;
  FDesignerBox.Align       := alClient;
  FDesignerBox.DockSite    := true;
  FDesignerBox.Color       := clWhite;
  FDesignerBox.AutoScroll  := true;
  FDesignerBox.OnMouseUp   := @DockSiteMouseUp;
  FDesignerBox.OnMouseDown := @DockSiteMouseDown;
  FDesignerBox.OnDockDrop  := @DockSiteDockDrop;
  FDesignerBox.OnUnDock    := @DockSiteUnDock;
  FDesignerBox.OnDockOver  := @DockSiteDockOver;
  (FDesignerBox as IDesignEpiControl).EpiControl := ADataFile.MainSection;

  EnterControl(FDesignerBox);

  // DEBUGGING!!!!
  LocalAdm := TEpiDocument(FDataFile.RootOwner).Admin;
  Grp := LocalAdm.NewGroup;
  Grp.Name.Text := 'Group 1';
  Grp.Rights := [earCreate, earRead, earUpdate, earDelete, earVerify,
    earStructure, earTranslate, earUsers, earPassword];
  WriteLn('Groupcount: ', LocalAdm.Groups.Count);
  Grp := LocalAdm.NewGroup;
  Grp.Name.Text := 'Group 2';
  Grp.Rights := [earCreate, earRead, earUpdate, earDelete, earVerify,
    earStructure, earTranslate, earUsers, earPassword];
  WriteLn('Groupcount: ', LocalAdm.Groups.Count);
  Grp := LocalAdm.NewGroup;
  Grp.Name.Text := 'Group 3';
  Grp.Rights := [earCreate, earRead, earUpdate, earDelete, earVerify,
    earStructure, earTranslate, earUsers, earPassword];
  WriteLn('Groupcount: ', LocalAdm.Groups.Count);
end;

end.

