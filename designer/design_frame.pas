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
    Button1: TButton;
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
    procedure   Button1Click(Sender: TObject);
    procedure   ToggleToolBtn(Sender: TObject);
  private
    { common private }
    FDataFile: TEpiDataFile;
    FDesignerBox: TScrollBox;
    FActiveButton: TToolButton;
    function    ShowForm(EpiControl: TEpiCustomControlItem;
      Pos: TPoint): TModalResult;
    function    NewDesignControl(AClass: TControlClass;
      AParent: TWinControl; Pos: TPoint;
      EpiControl: TEpiCustomControlItem): TControl;

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
  Graphics, Clipbrd, epidocument, epiadmin;

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

procedure TDesignFrame.Button1Click(Sender: TObject);
begin
  Clipboard.AsText := DataFile.SaveToXml('', 0);
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
  EpiControl.BeginUpdate;
  (Result as IDesignEpiControl).EpiControl := EpiControl;
  if Result is TDesignSection then
  with TDesignSection(Result) do
  begin
    EpiControl.Left := FMouseDownPos.X;
    EpiControl.Top := FMouseDownPos.Y;
    TEpiSection(EpiControl).Width := Abs(Pos.X - FMouseDownPos.X);
    TEpiSection(EpiControl).Height := Abs(Pos.Y - FMouseDownPos.Y);
    OnMouseDown := @DockSiteMouseDown;
    OnMouseUp   := @DockSiteMouseUp;
    OnDockDrop  := @DockSiteDockDrop;
    OnUnDock    := @DockSiteUnDock;
    OnDockOver  := @DockSiteDockOver;

    // Forces designer box last in docksite list, because retrieving docksite
    // is not implemented fully.
    FDesignerBox.DockSite := false;
    FDesignerBox.DockSite := true;
  end else begin
    EpiControl.Left := Pos.X;
    EpiControl.Top  := Pos.Y;
  end;
  Result.Name     := EpiControl.Id;
  Result.Parent   := AParent;
  EpiControl.EndUpdate;
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
  FActiveSection := TEpiSection((Sender as IDesignEpiControl).EpiControl);
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
          NewDesignControl(TDesignField, SenderWin, ParentPt, EpiControl)
        else begin
          FActiveSection.Fields.RemoveItem(EpiControl);
          EpiControl.Free;
        end;
      end;
    // Heading
    5:
      begin
        EpiControl := NewHeading;
        if ShowForm(EpiControl, ScreenPt) = mrOK then
          NewDesignControl(TDesignHeading, SenderWin, ParentPt, EpiControl)
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
        ScreenPt := ClientToScreen(FMouseDownPos);
        if ShowForm(EpiControl, ScreenPt) = mrOK then
          NewDesignControl(TDesignSection, SenderWin, ParentPt, EpiControl)
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
  // - since bugs exists in the LCL this is needed.
//  if not Supports(Sender, IDesignEpiControl) then exit;
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



  // DEBUGGING!!!!
  LocalAdm := TEpiDocument(FDataFile.RootOwner).Admin;
  Grp := LocalAdm.NewGroup;
  Grp.Name.Text := 'Group 1';
  Grp.Rights := [earCreate, earRead, earUpdate, earDelete, earVerify,
    earStructure, earTranslate, earUsers, earPassword];
  Grp := LocalAdm.NewGroup;
  Grp.Name.Text := 'Group 2';
  Grp.Rights := [earCreate, earRead, earUpdate, earDelete, earVerify,
    earStructure, earTranslate, earUsers, earPassword];
  Grp := LocalAdm.NewGroup;
  Grp.Name.Text := 'Group 3';
  Grp.Rights := [earCreate, earRead, earUpdate, earDelete, earVerify,
    earStructure, earTranslate, earUsers, earPassword];
end;

end.

