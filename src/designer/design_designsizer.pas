unit design_designsizer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, JvDesignImp, JvDesignSurface, Controls;

type

  { TDesignSizer }

  TDesignSizer = class(TJvDesignSizer)
  {$IFDEF DARWIN}
  private
    FOldPaint: TNotifyEvent;
    FDesignPanel: TJvDesignPanel;
    procedure ApplyDarwinHack;
    procedure RemoveDarwinHack;
    procedure SizerPaint(Sender: TObject);
  protected
    function GetClient: TControl; override;
    procedure PaintDragRect; override;
  {$ENDIF}
  protected
    procedure ApplyDragRect; override;
  public
    constructor CreateSizer(AOwner: TJvDesignSurface;
       AHandle: TJvDesignHandleId); override;
    destructor Destroy; override;
  end;

implementation

uses
   manager_globals, design_commander, Graphics;

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

{$IFDEF DARWIN}
procedure TDesignSizer.ApplyDarwinHack;
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
    FDesignPanel.OnPaint := @SizerPaint;
  end;
end;

procedure TDesignSizer.RemoveDarwinHack;
begin
  FDesignPanel.OnPaint := FOldPaint;
end;

procedure TDesignSizer.SizerPaint(Sender: TObject);
begin
  if Assigned(FOldPaint) then FOldPaint(Sender);

  with FDesignPanel.Canvas do
  begin
    Pen.Style := psDash;
    Pen.Color := clBlack;
    Brush.Style := bsClear;

    Rectangle(FDragRect);
  end;
end;

function TDesignSizer.GetClient: TControl;
begin
  Result := FDesignPanel;
end;

procedure TDesignSizer.PaintDragRect;
begin
  FDesignPanel.Invalidate;
end;
{$ENDIF}

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

constructor TDesignSizer.CreateSizer(AOwner: TJvDesignSurface;
  AHandle: TJvDesignHandleId);
begin
  inherited CreateSizer(AOwner, AHandle);
  {$IFDEF DARWIN}
  ApplyDarwinHack;
  {$ENDIF}
end;

destructor TDesignSizer.Destroy;
begin
  {$IFDEF DARWIN}
  RemoveDarwinHack;
  {$ENDIF}
  inherited Destroy;
end;

end.

