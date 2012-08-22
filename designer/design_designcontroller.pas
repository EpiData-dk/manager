unit design_designcontroller;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JvDesignSurface, JvDesignImp, design_runtimedesigner,
  Controls, LMessages;

type

  { TDesignController }

  TDesignController = class(TJvDesignController)
  private
    FFrame: TRuntimeDesignFrame;
    // A little cheating to be able to set the drag rect manually... needed
    // for applying a dataframe!
    FDragRect: TRect;
  protected
    function KeyUp(AKeyCode: Cardinal): Boolean; override;
    function MouseDown(Button: TMouseButton; X, Y: Integer;
       TheMessage: TLMMouse): Boolean; override;
    function MouseMove(X, Y: Integer; TheMessage: TLMMouse): Boolean; override;
    function MouseUp(Button: TMouseButton; X, Y: Integer; TheMessage: TLMMouse
       ): Boolean; override;
    function GetDragRect: TRect; override;
  public
    constructor Create(ASurface: TJvDesignSurface); override;
    procedure SetDragRect(ARect: TRect);
  end;


implementation

uses
  LCLIntf, LCLType, design_control_section, manager_messages,
  main;

{ TDesignController }

function TDesignController.KeyUp(AKeyCode: Cardinal): Boolean;
var
  Shift: TShiftState;
begin
  // Catch all predefined Key-Up shortcuts and ignore them.
  // They should have own implementations by epidata, processed
  // with on KeyDown events.
  Shift := GetKeyShiftState;

  // Cut, Copy, Paste!
  if (ssCtrl in Shift) and
     (AKeyCode in [VK_C, VK_X, VK_V])
  then
    Result := true;

  if (([ssCtrl, ssShift] * Shift) = []) and
     (AKeyCode in [VK_ESCAPE, VK_DELETE])
  then
    Result := true;

  if result then
    Exit
  else
    Result := inherited KeyUp(AKeyCode);
end;

function TDesignController.MouseDown(Button: TMouseButton; X, Y: Integer;
  TheMessage: TLMMouse): Boolean;
begin
  if TheMessage.Msg = LM_LBUTTONDBLCLK then
  begin
    FFrame.Label8.Caption := 'DblClick';
  end;

  Result := inherited MouseDown(Button, X, Y, TheMessage);
  if Assigned(FFrame) then
    FFrame.Label1.Caption := 'Mouse (1): X = ' + IntToStr(X) + ' | Y = ' + IntToStr(Y);

  if Assigned(FFrame) then
    FFrame.Label2.Caption := 'Mouse (2): X = ' + IntToStr(TheMessage.XPos) + ' | Y = ' + IntToStr(TheMessage.YPos);

  if (Clicked <> nil) and (Clicked <> Surface.Container) then
  begin
    // TODO : Relay message than a control was clicked - what about SelectionChange?

    // Heads up on creating new control!
    if (Clicked is TDesignSection) and (DragMode = dmCreate) then
      SendMessage(FFrame.Handle, LM_DESIGNER_CONTROLLERNOTIFY, WPARAM(Clicked), 0);
  end;
end;

function TDesignController.MouseMove(X, Y: Integer; TheMessage: TLMMouse
  ): Boolean;
begin
  Result := inherited MouseMove(X, Y, TheMessage);

  if Assigned(FFrame) then
    FFrame.Label1.Caption := 'Mouse (1): X = ' + IntToStr(X) + ' | Y = ' + IntToStr(Y);

//  if Assigned(FFrame) then
//    FFrame.Label2.Caption := 'Mouse (2): X = ' + IntToStr(TheMessage.XPos) + ' | Y = ' + IntToStr(TheMessage.YPos);
end;

function TDesignController.MouseUp(Button: TMouseButton; X, Y: Integer;
  TheMessage: TLMMouse): Boolean;
begin
  Result := inherited MouseUp(Button, X, Y, TheMessage);

  if Assigned(FFrame) then
    FFrame.Label1.Caption := 'Mouse (1): X = ' + IntToStr(X) + ' | Y = ' + IntToStr(Y);

  if Assigned(FFrame) then
    FFrame.Label2.Caption := 'Mouse (2): X = ' + IntToStr(TheMessage.XPos) + ' | Y = ' + IntToStr(TheMessage.YPos);
end;

function TDesignController.GetDragRect: TRect;

  function RectEmpty(Const ARect: TRect): boolean;
  begin
    result :=
      (ARect.Left = 0) and
      (ARect.Top = 0) and
      (ARect.Right = 0) and
      (ARect.Bottom = 0);
  end;

begin
  if not RectEmpty(FDragRect) then
    result := FDragRect
  else
    Result := inherited GetDragRect;
end;

constructor TDesignController.Create(ASurface: TJvDesignSurface);
var
  P: TControl;
begin
  inherited Create(ASurface);
  FDragRect := Rect(0,0,0,0);

  P := ASurface.Container;
  while (P <> nil) and (not (P is TRuntimeDesignFrame)) do
    P := P.Parent;
  if (P <> nil) and (P is TRuntimeDesignFrame) then
    FFrame := TRuntimeDesignFrame(P);
end;

procedure TDesignController.SetDragRect(ARect: TRect);
begin
  FDragRect := ARect;
end;

end.

