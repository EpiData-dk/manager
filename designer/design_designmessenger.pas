unit design_designmessenger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JvDesignImp, JvDesignSurface, Controls,
  LMessages, design_runtimedesigner;

type

  { TDesignMessenger }

  TDesignMessenger = class(TJvDesignDesignerMessenger)
  private
    FFrame: TRuntimeDesignFrame;
  protected
    procedure SetContainer(AValue: TWinControl); override;
  public
    function IsDesignMessage(ASender: TControl; var AMessage: TLMessage
       ): Boolean; override;
  end;

implementation

uses
  LCLType;

{ TDesignMessenger }

procedure TDesignMessenger.SetContainer(AValue: TWinControl);
begin
  inherited SetContainer(AValue);

  while Assigned(AValue) do
    if AValue is TRuntimeDesignFrame then
      break
    else
      AValue := AValue.Parent;

  if Assigned(AValue) then
    FFrame := TRuntimeDesignFrame(AValue);
end;

function TDesignMessenger.IsDesignMessage(ASender: TControl;
  var AMessage: TLMessage): Boolean;

  function MousePoint: TPoint;
  begin
    Result := Container.ScreenToClient(Mouse.CursorPos);
  end;

var
  Ctrl: TControl;
  P: TPoint;
begin
  case AMessage.Msg of
    LM_MOUSEWHEEL:
      // Handle MouseWheel, since on (especially) windows this is not handle
      // correctly using normal message chain.
      FFrame.JvDesignScrollBox1.WindowProc(AMessage);
    CN_KEYDOWN,
    CN_SYSKEYDOWN:
      begin
        // Relay to Frame before going further -> this could be
        // a shortcut we wish to handle.
        if FFrame.IsShortCut(TLMKey(AMessage)) then
          result := true
        else
          Result := inherited IsDesignMessage(ASender, AMessage);
      end;
    LM_LBUTTONDOWN, LM_LBUTTONUP,
    LM_RBUTTONDOWN, LM_RBUTTONUP:
      begin
        if not FFrame.ValidateControls then
          // We do not proceed with sending MouseDown/Up message, since a control cannot be validated!
          // -> proceeding may allow additional controls to be created etc.
          Result := true
        else
          Result := inherited IsDesignMessage(ASender, AMessage);
      end
  else
    Result := inherited IsDesignMessage(ASender, AMessage);
  end;
  if Result then
    AMessage.Result := 1;
end;

end.

