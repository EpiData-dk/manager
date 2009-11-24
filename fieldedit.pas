unit FieldEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UEpiDataFile, StdCtrls, Controls;

type

  { TFieldEdit }

  TFieldEdit = class(TEdit)
  private
    FField: TEpiField;
    // Optional field name label.
    FFieldNameLabel: TLabel;
    FFieldNameOffset: TPoint;
    FVariableLabel: TLabel;
    FVariableLabelOffset: TPoint;
  protected
    procedure SetParent(NewParent: TWinControl); override;
    procedure CalculateDockSizes;
  public
    constructor Create(AField: TEpiField; AOwner: TComponent);
    destructor Destroy; override;
    procedure DoStartDock(var DragObject: TDragObject); override;
    procedure DoEndDock(Target: TObject; X, Y: Integer); override;
    property Field: TEpiField read FField write FField;
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
  InterfaceBase, LCLType, Math, LCLProc, main, settings;

{ TFieldEdit }

procedure TFieldEdit.SetParent(NewParent: TWinControl);
begin
  inherited SetParent(NewParent);
  FVariableLabel.Parent := NewParent;
  if (BuilderSettings.ShowFieldNamesInLabel) then
    FFieldNameLabel.Parent := NewParent;
end;

procedure TFieldEdit.CalculateDockSizes;
begin
  inherited CalculateDockSizes;
end;

constructor TFieldEdit.Create(AField: TEpiField; AOwner: TComponent);
begin
  inherited Create(AOwner);
  FField := AField;
  FVariableLabel := TLabel.Create(Self);
  FFieldNameLabel := TLabel.Create(Self);
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
  FFieldNameOffset := Point(Left - FFieldNameLabel.Left, Top - FFieldNameLabel.Top);
end;

procedure TFieldEdit.DoEndDock(Target: TObject; X, Y: Integer);
begin
  inherited DoEndDock(Target, X, Y);

  FVariableLabel.Left := Left - FVariableLabelOffset.X;
  FVariableLabel.Top := Top - FVariableLabelOffset.Y;

  FFieldNameLabel.Left := Left - FFieldNameOffset.X;
  FFieldNameLabel.Top := Top - FFieldNameOffset.Y;
end;

{ TFieldLabel }

constructor TFieldLabel.Create(AField: TEpiField; AOwner: TComponent);
begin
  inherited Create(AOwner);
  FField := AField;
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

