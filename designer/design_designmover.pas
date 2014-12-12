unit design_designmover;

{$mode objfpc}{$H+}
{$DEFINE DESIGNER_GUIDES}

interface

uses
  Classes, SysUtils, types, JvDesignImp, JvDesignSurface, LCLType,
  controls, design_runtimedesigner;

type

  { TDesignMover }

  TDesignMover = class(TJvDesignMover)
  {$IFDEF DARWIN}
  private
    FOldPaint: TNotifyEvent;
    FDesignPanel: TJvDesignPanel;
    procedure ApplyDarwinHack;
    procedure RemoveDarwinHack;
    procedure MoverPaint(Sender: TObject);
  {$ENDIF}
  private
    FFrame: TRuntimeDesignFrame;
    FOuterControls: Array[TAnchorKind] of TControl;
    FOuterRect: TRect;
    procedure CalcOuterDragRect;
    procedure CalcOuterPaintRect;
    procedure PaintOuterRect;
    procedure CalcOuterDragControls;
  private
    YCtrl: TControl;
    XCtrl: TControl;
    function DragRectsInParent: boolean;
    procedure AdjustDragRects;
  protected
    procedure CalcDragRects; override;
    procedure CalcPaintRects; override;
    procedure ApplyDragRects; override;
    procedure PaintDragRects; override;
  public
    constructor Create(AOwner: TJvDesignSurface); override;
    destructor Destroy; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer
       ); override;
  end;

implementation

uses
  design_commander, manager_globals, LCLIntf, Graphics, forms,
  math, design_designcontroller, JvDesignUtils, settings2_var;

type

  { TMoveCommand }

  TMoveCommand = class(TCustomCommand)
  private
    FSurface: TJvDesignSurface;
    FBoundsRect: TRect;
    FControl: TControl;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);
       override;
  public
    constructor Create(Const Ctrl: TControl; Const Surface: TJvDesignSurface);
    procedure ReDo; override;
    procedure Undo; override;
  end;

  TMoveCommandList = class(TCustomCommandList);

{ TMoveCommand }

procedure TMoveCommand.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  // TODO : Handle a destroyed control.
  if (AComponent = FControl) and (Operation = opRemove) then
    Free;
end;

constructor TMoveCommand.Create(const Ctrl: TControl;
  const Surface: TJvDesignSurface);
begin
  FBoundsRect := Ctrl.BoundsRect;
  FControl := Ctrl;
  FSurface := Surface;
  Ctrl.FreeNotification(Self);
end;

procedure TMoveCommand.ReDo;
begin
  Undo;
end;

procedure TMoveCommand.Undo;
var
  R: TRect;
begin
  R := FControl.BoundsRect;
  FControl.BoundsRect := FBoundsRect;
  FBoundsRect := R;

  FSurface.UpdateDesigner;
end;

{ TDesignMover }

{$IFDEF DARWIN}
procedure TDesignMover.ApplyDarwinHack;
var
  WinCtrl: TWinControl;
begin
  WinCtrl := Surface.Container;
  while Assigned(WinCtrl) do
    if WinCtrl is TJvDesignPanel then
      break
    else
      WinCtrl := WinCtrl.Parent;

  if Assigned(WinCtrl) then
  begin
    FDesignPanel := TJvDesignPanel(WinCtrl);
    FOldPaint := FDesignPanel.OnPaint;
    FDesignPanel.OnPaint := @MoverPaint;
  end;
end;

procedure TDesignMover.RemoveDarwinHack;
begin
  FDesignPanel.OnPaint := FOldPaint;
end;

procedure TDesignMover.MoverPaint(Sender: TObject);
var
  ARect: TRect;
  L: Integer;
  i: Integer;
begin
  if Assigned(FOldPaint) then FOldPaint(Sender);

  L := Length(FDragRects);
  if L = 0 then exit;

  with FDesignPanel.Canvas do
  begin
    Pen.Style := psDash;
    Pen.Color := clBlack;
    Brush.Style := bsClear;

    for i := 0 to L - 1 do
    begin
      ARect.TopLeft := FDesignPanel.ScreenToClient(FDragRects[i].TopLeft);
      ARect.BottomRight := FDesignPanel.ScreenToClient(FDragRects[i].BottomRight);
      Rectangle(ARect);
    end;
  end;
end;
{$ENDIF}

procedure TDesignMover.CalcOuterDragRect;
var
  i: Integer;
  P: TPoint;
begin
  FOuterRect := Rect(
    FOuterControls[akLeft].Left,
    FOuterControls[akTop].Top,
    FOuterControls[akRight].BoundsRect.Right,
    FOuterControls[akBottom].BoundsRect.Bottom
    );
  P := GetMouseDelta;
  OffsetRect(FOuterRect, P.X, P.Y);
end;

procedure TDesignMover.CalcOuterPaintRect;
var
  ScreenPoint: TPoint;
begin
  {$IFDEF DESIGNER_GUIDES}
  with Surface.Selection[0] do
    ScreenPoint := Parent.ClientToScreen(Point(0, 0));
  OffsetRect(FOuterRect, ScreenPoint.X, ScreenPoint.Y);
  {$ENDIF}
end;

procedure TDesignMover.PaintOuterRect;
begin
  {$IFDEF DESIGNER_GUIDES}
//  DesignPaintRubberbandRect(Surface.Container, FOuterRect, psSolid);
  {$ENDIF}
end;

procedure TDesignMover.CalcOuterDragControls;
var
  i: Integer;
begin
  FOuterControls[akTop]    := Surface.Selection[0];
  FOuterControls[akLeft]   := Surface.Selection[0];
  FOuterControls[akRight]  := Surface.Selection[0];
  FOuterControls[akBottom] := Surface.Selection[0];

  for i := 1 to Surface.Count - 1 do
  begin
    if FOuterControls[akTop].Top > Surface.Selection[i].Top then
      FOuterControls[akTop] := Surface.Selection[i];

    if FOuterControls[akLeft].Left > Surface.Selection[i].Left then
      FOuterControls[akLeft] := Surface.Selection[i];

    if FOuterControls[akRight].BoundsRect.Right < Surface.Selection[i].BoundsRect.Right then
      FOuterControls[akRight] := Surface.Selection[i];

    if FOuterControls[akBottom].BoundsRect.Bottom < Surface.Selection[i].BoundsRect.Bottom then
      FOuterControls[akBottom] := Surface.Selection[i];
  end;
end;

function TDesignMover.DragRectsInParent: boolean;
var
  C: TWinControl;
  P: TWinControl;
  i: Integer;
  Pt: TPoint;
begin
  C := Surface.Container;
  if Surface.Selector.Count = 0 then
    Exit;

  P := Surface.Selection[0].Parent;

  Result := true;
  for i := Low(FDragRects) to High(FDragRects) do
  begin
    Pt := P.ScreenToClient(FDragRects[i].TopLeft);
    if (Pt.X < 0) or (Pt.Y < 0)
    then
      Exit(false);

    Pt := P.ScreenToClient(FDragRects[i].BottomRight);
    if (P <> C) and
       ((Pt.X > P.Width) or (Pt.Y > P.Height))
    then
      Exit(false);
  end;
end;

procedure TDesignMover.AdjustDragRects;
var
  Y: Integer;
  X: Integer;
  I: Integer;
  WinCtrl: TWinControl;
  Ctrl: TControl;
  SnapDist: Integer;
  CtrlBound: TRect;
  BestTopDiff: Integer;
  BestLeftDiff: Integer;
  BestRightDiff: Integer;
  BestBtmDiff: Integer;
  TopDiff: Integer;
  LeftDiff: Integer;
  RightDiff: Integer;
  BtmDiff: Integer;
  j: Integer;
  BestTopCtrl: TControl;
  BestBtmCtrl: TControl;
  BestLeftCtrl: TControl;
  BestRightCtrl: TControl;
begin
  if not ManagerSettings.SnapFields then exit;

  WinCtrl := Surface.Selection[0].Parent;
  SnapDist := ManagerSettings.SnappingThresHold;

  BestTopDiff   := MaxInt;
  BestLeftDiff  := MaxInt;
  BestRightDiff := MaxInt;
  BestBtmDiff   := MaxInt;
  XCtrl         := nil;
  YCtrl         := nil;

  for i := 0 to WinCtrl.ControlCount - 1 do
    begin
      Ctrl := WinCtrl.Controls[i];

         // Exclude selected controls
      if Surface.Selector.IsSelected(Ctrl) or
         // Exclude controls with special properties.
         ([csNoDesignSelectable, csNoDesignVisible] * Ctrl.ControlStyle <> [])
      then
        Continue;

      CtrlBound := Ctrl.BoundsRect;

      for j := Low(FDragRects) to High(FDragRects) do
        begin
          TopDiff   := -(FDragRects[j].Top - CtrlBound.Top);
          LeftDiff  := -(FDragRects[j].Left - CtrlBound.Left);
          RightDiff := -(FDragRects[j].Right - CtrlBound.Right);
          BtmDiff   := -(FDragRects[j].Bottom - CtrlBound.Bottom);

          if (Abs(TopDiff) <= SnapDist) and
             (Abs(TopDiff) < BestTopDiff)
          then
          begin
            BestTopDiff := TopDiff;
            BestTopCtrl := Ctrl;
          end;

          if (Abs(BtmDiff) <= SnapDist) and
             (Abs(BtmDiff) < BestBtmDiff)
          then
          begin
            BestBtmDiff := BtmDiff;
            BestBtmCtrl := Ctrl;
          end;

          if (Abs(LeftDiff) <= SnapDist) and
             (Abs(LeftDiff) < BestLeftDiff)
          then
          begin
            BestLeftDiff := LeftDiff;
            BestLeftCtrl := Ctrl;
          end;

          if (Abs(RightDiff) <= SnapDist) and
             (Abs(RightDiff) < BestRightDiff)
          then
          begin
            BestRightDiff := RightDiff;
            BestRightCtrl := Ctrl;
          end;
        end;
    end;  //   for i := 0 to WinCtrl.ControlCount - 1 do

  X := 0;
  Y := 0;

  if (BestTopDiff <> MaxInt) or
     (BestBtmDiff <> MaxInt)
  then
    if Abs(BestBtmDiff) < Abs(BestTopDiff) then
    begin
      Y := BestBtmDiff;
      YCtrl := BestBtmCtrl;
    end
    else
    begin
      Y := BestTopDiff;
      YCtrl := BestTopCtrl;
    end;

  if (BestLeftDiff <> MaxInt) or
     (BestRightDiff <> MaxInt)
  then
    if Abs(BestRightDiff) < Abs(BestLeftDiff) then
    begin
      X := BestRightDiff;
      XCtrl := BestRightCtrl;
    end
    else
    begin
      X := BestLeftDiff;
      XCtrl := BestLeftCtrlS;
    end;

  if (X <> 0) or (Y <> 0) then
    for I := Low(FDragRects) to High(FDragRects) do
      OffsetRect(FDragRects[i], X, Y);
end;

procedure TDesignMover.CalcDragRects;
begin
  inherited CalcDragRects;
  CalcOuterDragRect;
  AdjustDragRects;
end;

procedure TDesignMover.CalcPaintRects;
begin
  inherited CalcPaintRects;
  CalcOuterPaintRect;
end;

procedure TDesignMover.ApplyDragRects;
var
  L: TCustomCommandList;
  i: Integer;
  MC: TMoveCommand;
  T1: TDateTime;
  T2: TDateTime;
begin
  // Create command for Undo/Redo functionality
  if Surface.Count > 1 then
  begin
    L := TMoveCommandList.Create;
    L.FreeOnEmpty := true;
    GlobalCommandList.AddCommand(L);
  end else
    L := GlobalCommandList;

  for i := 0 to Surface.Count - 1 do
    begin
      MC := TMoveCommand.Create(Surface.Selection[i], Surface);
      L.AddCommand(MC);
    end;

  T1 := Now;
  inherited ApplyDragRects;
  T2 := Now;
//  if IsConsole then
//    WriteLn('ApplyDragRects: ', FormatDateTime('NN:SS:ZZZZ', T2-T1));
end;

procedure TDesignMover.PaintDragRects;
begin
  {$IFNDEF DARWIN}
  inherited PaintDragRects;
  {$ELSE}
  FDesignPanel.Invalidate;
  {$ENDIF}
  if Length(FDragRects) > 1 then
    PaintOuterRect;
end;

constructor TDesignMover.Create(AOwner: TJvDesignSurface);
var
  Kind: TAnchorKind;
begin
  inherited Create(AOwner);
  FFrame := TDesignController(Surface.Controller).Frame;

  CalcOuterDragControls;
  {$IFDEF DARWIN}
  ApplyDarwinHack;
  {$ENDIF}
end;

destructor TDesignMover.Destroy;
begin
  {$IFDEF DARWIN}
  RemoveDarwinHack;
  {$ENDIF}
  inherited Destroy;
end;

procedure TDesignMover.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);

  // Changing the cursor in linux f*cks with mouse capture, hence
  // if the mouse is released OUTSIDE the program drag/drop is
  // seriously misbehaving afterwards.
  {$IFNDEF LINUX}
  if DragRectsInParent
  then
    Screen.Cursor := crDrag
  else
    Screen.Cursor := crNoDrop;
  {$ENDIF}
end;

procedure TDesignMover.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  ValidPos: Boolean;
begin
  PaintDragRects;
  FMouseLast := Point(X, Y);
  ValidPos := DragRectsInParent;

  if ValidPos then
    ApplyDragRects;
end;

end.

