unit design_designmover;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, JvDesignImp, JvDesignSurface;

type

  { TDesignMover }

  TDesignMover = class(TJvDesignMover)
  protected
    procedure ApplyDragRects; override;
  public
    constructor Create(AOwner: TJvDesignSurface); override;
  end;

implementation

uses
  design_commander, manager_globals, controls;

type

  { TMoveCommand }

  TMoveCommand = class(TCustomCommand)
  private
    FBoundsRect: TRect;
    FControl: TControl;
  public
    constructor Create(Const Ctrl: TControl);
    procedure ReDo; override;
    procedure Undo; override;
  end;

  TMoveCommandList = class(TCustomCommandList);

{ TMoveCommand }

constructor TMoveCommand.Create(const Ctrl: TControl);
begin
  FBoundsRect := Ctrl.BoundsRect;
  FControl := Ctrl;
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

procedure TDesignMover.ApplyDragRects;
var
  L: TCustomCommandList;
  i: Integer;
  MC: TMoveCommand;
begin
  if Surface.Count > 1 then
  begin
    L := TMoveCommandList.Create;
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
end;

end.

