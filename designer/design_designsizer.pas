unit design_designsizer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, JvDesignImp;

type

  { TDesignSizer }

  TDesignSizer = class(TJvDesignSizer)
  protected
    procedure ApplyDragRect; override;
  end;

implementation

uses
   manager_globals, design_commander, Controls, JvDesignSurface;

type
  { TSizeCommand }

  TSizeCommand = class(TCustomCommand)
  private
    FSurface: TJvDesignSurface;
    FBoundsRect: TRect;
    FControl: TControl;
  public
    constructor Create(Const Ctrl: TControl; Surface: TJvDesignSurface);
    procedure ReDo; override;
    procedure Undo; override;
  end;

{ TMoveCommand }

constructor TSizeCommand.Create(const Ctrl: TControl; Surface: TJvDesignSurface
  );
begin
  FSurface := Surface;
  FBoundsRect := Ctrl.BoundsRect;
  FControl := Ctrl;
end;

procedure TSizeCommand.ReDo;
var
  R: TRect;
begin
  R := FControl.BoundsRect;
  FControl.BoundsRect := FBoundsRect;
  FBoundsRect := R;
  FSurface.UpdateDesigner;
end;

procedure TSizeCommand.Undo;
var
  R: TRect;
begin
  R := FControl.BoundsRect;
  FControl.BoundsRect := FBoundsRect;
  FBoundsRect := R;
  FSurface.UpdateDesigner;
end;

{ TDesignSizer }

procedure TDesignSizer.ApplyDragRect;
var
  Cmd: TSizeCommand;
  R: TRect;
  R1: TRect;
  R2: TRect;
begin
  Cmd := TSizeCommand.Create(Surface.Selection[0], Surface);
  inherited ApplyDragRect;

  R1 := Cmd.FBoundsRect;
  R2 := Cmd.FControl.BoundsRect;

  if not CompareMem(@R1, @R2, SizeOf(TRect)) then
    GlobalCommandList.AddCommand(Cmd)
  else
    Cmd.Free;
end;

end.

