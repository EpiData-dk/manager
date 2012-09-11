unit design_designmover;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, JvDesignImp, JvDesignSurface, LCLType;

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
  protected
    procedure PaintDragRects; override;
  {$ENDIF}
  protected
    procedure ApplyDragRects; override;
  public
    constructor Create(AOwner: TJvDesignSurface); override;
    destructor Destroy; override;
  end;

implementation

uses
  design_commander, manager_globals, controls, LCLIntf, Graphics;

type

  { TMoveCommand }

  TMoveCommand = class(TCustomCommand)
  private
    FBoundsRect: TRect;
    FControl: TControl;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);
       override;
  public
    constructor Create(Const Ctrl: TControl);
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

constructor TMoveCommand.Create(const Ctrl: TControl);
begin
  FBoundsRect := Ctrl.BoundsRect;
  FControl := Ctrl;
  Ctrl.FreeNotification(Self);
end;

procedure TMoveCommand.ReDo;
var
  R: TRect;
begin
  R := FControl.BoundsRect;
  FControl.BoundsRect := FBoundsRect;
  FBoundsRect := R;
end;

procedure TMoveCommand.Undo;
var
  R: TRect;
begin
  R := FControl.BoundsRect;
  FControl.BoundsRect := FBoundsRect;
  FBoundsRect := R;
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
    Pen.Style := psDot;
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

procedure TDesignMover.PaintDragRects;
begin
  FDesignPanel.Invalidate;
end;
{$ENDIF}

procedure TDesignMover.ApplyDragRects;
var
  L: TCustomCommandList;
  i: Integer;
  MC: TMoveCommand;
begin
  if Surface.Count > 1 then
  begin
    L := TMoveCommandList.Create;
    L.FreeOnEmpty := true;
    GlobalCommandList.AddCommand(L);
  end else
    L := GlobalCommandList;

  for i := 0 to Surface.Count - 1 do
    begin
      MC := TMoveCommand.Create(Surface.Selection[i]);
      L.AddCommand(MC);
    end;

  inherited ApplyDragRects;
end;

constructor TDesignMover.Create(AOwner: TJvDesignSurface);
begin
  inherited Create(AOwner);
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

end.

