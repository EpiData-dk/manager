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
    procedure DesignBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DesignBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
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

  private
    { Position handling }
    function    FindNewPosition(ParentControl: TWinControl;
      AClass: TControlClass): TPoint;
  private
    { Field Handling }
    function    NewField(FieldType: TEpiFieldType): TEpiField;
    function    NewDesignField(AParent: TWinControl; Pos: TPoint;
      Field: TEpiField): TDesignField;
  private
    { Heading Handling }
    function    NewHeading: TEpiHeading;
    function    NewDesignHeading(AParent: TWinControl; Pos: TPoint;
      Heading: TEpiHeading): TDesignHeading;
  private
    { Section handling }
    FActiveSection: TEpiSection;
    FMouseDownPos: TPoint;
    function    NewSection: TEpiSection;
    function    NewDesignSection(AParent: TWinControl; Pos: TPoint;
      Section: TEpiSection): TDesignSection;
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
begin
  Result := AClass.Create(AParent);
  (Result as IDesignEpiControl).EpiControl := EpiControl;
  if Result is TDesignSection then
  with TDesignSection(Result) do
  begin
    EpiControl.Left := FMouseDownPos.X;
    EpiControl.Top := FMouseDownPos.Y;
    Width := Abs(Pos.X - FMouseDownPos.X);
    Height := Abs(Pos.Y - FMouseDownPos.Y);
    OnMouseDown := @DesignBoxMouseDown;
    OnMouseUp := @DesignBoxMouseUp;
  end else begin
    EpiControl.Left := Pos.X;
    EpiControl.Top  := Pos.Y;
  end;
  Result.Parent   := AParent;
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

function TDesignFrame.NewDesignField(AParent: TWinControl; Pos: TPoint;
  Field: TEpiField): TDesignField;
begin
  result := TDesignField.Create(AParent);
  Result.EpiControl := Field;
  Field.Left := Pos.X;
  Field.Top := Pos.Y;
  result.Parent := AParent;
end;

function TDesignFrame.NewHeading: TEpiHeading;
begin
  result := FActiveSection.NewHeading;
end;

function TDesignFrame.NewDesignHeading(AParent: TWinControl; Pos: TPoint;
  Heading: TEpiHeading): TDesignHeading;
begin
  result := TDesignHeading.Create(AParent);
  Result.EpiControl := Heading;
  result.Parent := AParent;
end;

function TDesignFrame.NewSection: TEpiSection;
begin
  result := DataFile.NewSection;
end;

function TDesignFrame.NewDesignSection(AParent: TWinControl; Pos: TPoint;
  Section: TEpiSection): TDesignSection;
begin
  result := TDesignSection.Create(AParent);
  Result.EpiControl := Section;
  result.Parent := AParent;
end;

procedure TDesignFrame.DesignBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // If this is not Section tool, then no need to record anything.
  if FActiveButton.Tag <> 6 then exit;

  FMouseDownPos := Point(X, Y);
end;

procedure TDesignFrame.DesignBoxMouseUp(Sender: TObject; Button: TMouseButton;
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
end;

constructor TDesignFrame.Create(TheOwner: TComponent; ADataFile: TEpiDataFile);
begin
  inherited Create(TheOwner);
  FDataFile := ADataFile;
  FActiveButton := SelectorToolButton;
  FActiveSection := ADataFile.MainSection;

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
  FDesignerBox.OnMouseUp   := @DesignBoxMouseUp;
  FDesignerBox.OnMouseDown := @DesignBoxMouseDown;
end;

end.

