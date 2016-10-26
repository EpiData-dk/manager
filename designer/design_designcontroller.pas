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
    // For some odd reason GTK2 sends a a btn-down message to multiple components, regardsless
    // of how we return the result in IsDesignMessage. In order to NOT create 2+ mousetools due to
    // the extra messages, we do a check in our own DesignController to ensure that only a single
    // tool is created on mouse down.
    FMouseToolIsSet: Boolean;
  protected
    function DoCreateMouseTool(ADragMode: TJvDesignDragMode
       ): TJvDesignCustomMouseTool; override;
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
    procedure ClearDragRect;
    property Clicked;
  public
    property Frame: TRuntimeDesignFrame read FFrame;
  end;


implementation

uses
  LCLIntf, LCLType, design_control_section, manager_messages,
  main, manager_globals, design_designmover, design_designsizer,
  design_designbander;

{ TDesignController }

function TDesignController.DoCreateMouseTool(ADragMode: TJvDesignDragMode
  ): TJvDesignCustomMouseTool;
begin
  case ADragMode of
//    dmNone: ;
    dmMove:
      result := TDesignMover.Create(Surface);
    dmResize:
      result := TDesignSizer.CreateSizer(Surface, HandleID);
    dmSelect,
    dmCreate:
      result := TDesignBander.Create(Surface);
  else
    Result := inherited DoCreateMouseTool(ADragMode);
  end;

  FMouseToolIsSet := Assigned(Result);

  if Assigned(REsult) then
    Writeln('MouseTool = ', Result.ClassName);
end;

function TDesignController.KeyUp(AKeyCode: Cardinal): Boolean;
var
  Shift: TShiftState;
begin
  // Catch all predefined Key-Up shortcuts and ignore them.
  // They should have own implementations by epidata, processed
  // on KeyDown events.
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
  if (not FMouseToolIsSet) then
    Result := inherited MouseDown(Button, X, Y, TheMessage);
end;

function TDesignController.MouseMove(X, Y: Integer; TheMessage: TLMMouse
  ): Boolean;
begin
  Result := inherited MouseMove(X, Y, TheMessage);
end;

function TDesignController.MouseUp(Button: TMouseButton; X, Y: Integer;
  TheMessage: TLMMouse): Boolean;
begin
  if DragMode = dmMove then
  begin
    MainForm.BeginUpdatingForm;
//    GlobalCommandList;
  end;

  Result := inherited MouseUp(Button, X, Y, TheMessage);
  if (FMouseToolIsSet) then
    FMouseToolIsSet := false;

  case DragMode of
    dmNone: ;
    dmMove:
      MainForm.EndUpdatingForm;
    dmResize: ;
    dmSelect: ;
    dmCreate:
      FFrame.ShowPropertiesForm(true);
  end;
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
  FMouseToolIsSet := false;

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

procedure TDesignController.ClearDragRect;
begin
  SetDragRect(Rect(0,0,0,0));
end;

end.

