unit FieldEdit;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UEpiDataFile, StdCtrls, Controls,
  UDataFileTypes;

type

  { TFieldEdit }

  TFieldEdit = class(TEdit)
  private
    FField: TEpiField;
    // Optional field name label.
    FFieldNameLabel: TLabel;
    FVariableLabel: TLabel;
    FVariableLabelOffset: TPoint;
    procedure OnFieldChange(Sender: TObject; EventType: TEpiFieldChangeEventType; OldValue: EpiVariant);
    procedure UpdateFieldNameLabel;
  protected
    procedure SetParent(NewParent: TWinControl); override;
    procedure SetVisible(Value: Boolean); override;
    procedure CalculateDockSizes;
  public
    constructor Create(AField: TEpiField; AOwner: TComponent);
    destructor Destroy; override;
    procedure DoStartDock(var DragObject: TDragObject); override;
    procedure DoEndDock(Target: TObject; X, Y: Integer); override;
    property Field: TEpiField read FField;
    property VariableLabel: TLabel read FVariableLabel;
    property FieldNameLabel: TLabel read FFieldNameLabel;
  published
    property OnStartDock;
    property OnEndDock;
  end;

  { TFieldDockObject }

  TFieldDockObject = class(TDragDockObject)
  private
     OldFieldRec: TRect;
     FieldRec: TRect;
  protected
     procedure AdjustDockRect(ARect: TRect); override;
     procedure InitDock(APosition: TPoint); override;
     procedure ShowDockImage; override;
     procedure MoveDockImage; override;
     procedure HideDockImage; override;
  public
    constructor Create(AControl: TControl); override;
    destructor Destroy; override;
  end;

  { TFieldLabel }

  TFieldLabel = class(TLabel)
  private
    FField: TEpiField;
    procedure OnFieldChange(Sender: TObject; EventType: TEpiFieldChangeEventType; OldValue: EpiVariant);
  public
    constructor Create(AField: TEpiField; AOwner: TComponent);
    destructor Destroy; override;
    property Field: TEpiField read FField write FField;
  published
    property OnStartDock;
    property OnEndDock;
  end;

implementation

uses
  InterfaceBase, LCLType, Math, LCLProc, main, settings, ExtCtrls;

{ TFieldEdit }

procedure TFieldEdit.OnFieldChange(Sender: TObject;
  EventType: TEpiFieldChangeEventType; OldValue: EpiVariant);
var
  OldWidth: LongInt;
  Dx: Integer;
begin
  with TEpiField(Sender) do
  begin
    case EventType of
      fceName:
        begin
          FFieldNameLabel.Caption := FieldName;
          Text := FieldName;
        end;
      fceLength:
        begin
          if Assigned(Parent) then
            Width := TPanel(Parent).Canvas.TextWidth('W') * FieldLength
          else
            Width := 8 * FieldLength;
        end;
      fceDecimals: ;
      fceFX: Left := FieldX;
      fceFY: Top  := FieldY;
      fceFColTxt:;
      fceFColHl:;
      fceFColBg:;
      fceVarLabel:
        begin
          OldWidth := Self.VariableLabel.Width;
          Self.FVariableLabel.Caption := VariableLabel;
          if (Self.FVariableLabel.Width + Self.FVariableLabel.Left) > Left then
          begin
            Dx := Self.FVariableLabel.Width - OldWidth;
            LabelX := LabelX - Max(0, Dx);
          end;
        end;
      fceVX: Self.FVariableLabel.Left := LabelX;
      fceVY: Self.FVariableLabel.Top  := LabelY;
      fceVColTxt:;
      fceVColBg:;
    end;
  end;
  if EventType in [fceName, fceVarLabel, fceVX, fceVY] then
    UpdateFieldNameLabel;
end;

procedure TFieldEdit.UpdateFieldNameLabel;
begin
  FieldNameLabel.Caption  := Text;
  FieldNameLabel.Left     := VariableLabel.Left - (FieldNameLabel.Width + 5);
  FieldNameLabel.Top      := VariableLabel.Top;
end;

procedure TFieldEdit.SetParent(NewParent: TWinControl);
begin
  inherited SetParent(NewParent);
  FVariableLabel.Parent := NewParent;
  if (BuilderSettings.ShowFieldNamesInLabel) then
    FFieldNameLabel.Parent := NewParent;

  UpdateFieldNameLabel;
end;

procedure TFieldEdit.SetVisible(Value: Boolean);
begin
  inherited SetVisible(Value);
  UpdateFieldNameLabel;
//  FVariableLabel.Visible := Value;
//  FFieldNameLabel.Visible := Value;
end;

procedure TFieldEdit.CalculateDockSizes;
begin
  inherited CalculateDockSizes;
end;

constructor TFieldEdit.Create(AField: TEpiField; AOwner: TComponent);
begin
  inherited Create(AOwner);
  FField := AField;
  FField.OnChange := @OnFieldChange;
  FVariableLabel := TLabel.Create(Self);
  FFieldNameLabel := TLabel.Create(Self);

  Text                    := Field.FieldName;
  Left                    := Field.FieldX;
  Top                     := Field.FieldY;

  VariableLabel.Caption   := Field.VariableLabel;
  VariableLabel.Left      := Field.LabelX;
  VariableLabel.Top       := Field.LabelY;

  UpdateFieldNameLabel;
end;

destructor TFieldEdit.Destroy;
begin
  inherited Destroy;
  FField := nil;
  // Do not destroy - it's handled byt the visual destruction of the frame.
  FVariableLabel := nil;
  FFieldNameLabel := nil;
end;

procedure TFieldEdit.DoStartDock(var DragObject: TDragObject);
begin
  inherited DoStartDock(DragObject);
  FVariableLabelOffset := Point(Left - FVariableLabel.Left, Top - FVariableLabel.Top);
end;

procedure TFieldEdit.DoEndDock(Target: TObject; X, Y: Integer);
begin
  inherited DoEndDock(Target, X, Y);
  Field.FieldX := Left;
  Field.FieldY := Top;

  Field.LabelX := Left - FVariableLabelOffset.X;
  Field.LabelY := Top - FVariableLabelOffset.Y;

  UpdateFieldNameLabel;
end;

{ TFieldLabel }

procedure TFieldLabel.OnFieldChange(Sender: TObject;
  EventType: TEpiFieldChangeEventType; OldValue: EpiVariant);
begin
  with TEpiField(Sender) do
  begin
    case EventType of
      fceName: ;
      fceLength: ;
      fceDecimals: ;
      fceFX: Left := FieldX;
      fceFY: Top  := FieldY;
      fceFColTxt:;
      fceFColHl:;
      fceFColBg:;
      fceVarLabel: Caption := VariableLabel;
      fceVX: Left := LabelX;
      fceVY: Top  := LabelY;
      fceVColTxt:;
      fceVColBg:;
    end;
  end
end;

constructor TFieldLabel.Create(AField: TEpiField; AOwner: TComponent);
begin
  inherited Create(AOwner);
  FField := AField;
  FField.OnChange := @OnFieldChange;
end;

destructor TFieldLabel.Destroy;
begin
  inherited Destroy;
  FField := nil;
end;

{ TFieldDockObject }

procedure TFieldDockObject.AdjustDockRect(ARect: TRect);
begin
  inherited AdjustDockRect(ARect);
  with ARect do
    MainForm.Label1.Caption := Format(
     'AdjustDockRect - Top: %d, Left: %d, Bottom: %d, Right: %d',
     [Top, Left, Bottom, Right]);

{  with DockOffset do
    OffsetRect(FieldRec, -X, -Y);      }
end;

procedure TFieldDockObject.InitDock(APosition: TPoint);
var
  TmpPoint: TPoint;
begin
  inherited InitDock(APosition);

  // Determine hotspot scale for adjusting the undocked rectangle.
  // Since the undocked extent of the control doesn't change, we fix the hotspot offset.
  // Usage: OffsetRect(DockRect, FDockOffset);

  MainForm.Label2.Caption := Format(
    'InitDock. Position - X: %d , Y: %d',
    [APosition.X, APosition.Y]);

  // mouse click offset from control TopLeft in screen coordinates
  with DockRect do
  begin
    TopLeft := Control.ClientToScreen(Point(0, 0));
    TmpPoint := TFieldEdit(Control).VariableLabel.ClientToScreen(Point(0,0));

    Left := Min(Left, TmpPoint.X);
    Top := Min(Top, TmpPoint.Y);

    BottomRight := Control.ClientToScreen(Point(Control.Width, Control.Height));
    TmpPoint := TFieldEdit(Control).VariableLabel.ClientToScreen(
      Point(TFieldEdit(Control).VariableLabel.Width,
            TFieldEdit(Control).VariableLabel.Height));

    Bottom := Max(Bottom, TmpPoint.Y);
    Right  := Max(Right, TmpPoint.X);
  end;
//  OldFieldRec := Rect(MaxInt, 0, MaxInt, 0);
end;

procedure TFieldDockObject.ShowDockImage;
begin
  Inherited ShowDockImage;
  With FieldRec do
    MainForm.Label3.Caption := Format(
      'ShowDockImage: FieldRect - Top: %d, Left: %d, Bottom: %d, Right: %d',
      [Top, Left, Bottom, Right]);

{  WidgetSet.DrawDefaultDockImage(OldFieldRec, FieldRec, disShow);
  OldFieldRec := FieldRec;    }
end;

procedure TFieldDockObject.MoveDockImage;
begin
  inherited MoveDockImage;
  With FieldRec do
    MainForm.Label4.Caption := Format(
      'MoveDockImage: FieldRect - Top: %d, Left: %d, Bottom: %d, Right: %d',
      [Top, Left, Bottom, Right]);

{  //Draw the form outlines when the position has changed
  if not CompareMem(@FieldRec, @OldFieldRec, SizeOf(TRect)) then
  begin
    WidgetSet.DrawDefaultDockImage(OldFieldRec, FieldRec, disMove);
    OldFieldRec := FieldRec;
  end;   }
end;

procedure TFieldDockObject.HideDockImage;
begin
  inherited HideDockImage;
  With FieldRec do
    MainForm.Label5.Caption := Format(
      'HideDockImage: FieldRect - Top: %d, Left: %d, Bottom: %d, Right: %d',
      [Top, Left, Bottom, Right]);
{
  WidgetSet.DrawDefaultDockImage(OldFieldRec, FieldRec, disHide);
  OldFieldRec := Rect(MaxInt, 0, MaxInt, 0);  }
end;

constructor TFieldDockObject.Create(AControl: TControl);
begin
  inherited Create(AControl);
end;

destructor TFieldDockObject.Destroy;
begin
  inherited Destroy;
end;

end.

