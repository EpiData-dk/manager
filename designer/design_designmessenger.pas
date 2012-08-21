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
      Result := inherited IsDesignMessage(ASender, AMessage);
    LM_RBUTTONDOWN:
      begin
        // Activate pop-ups on right-click.
        P := MousePoint;
        Ctrl := TJvDesignPanel(Container).Surface.FindControl(P.X, P.Y);

        if Assigned(Ctrl.PopupMenu) then
        begin
          Ctrl.PopupMenu.PopUp;
          Result := true;
        end else
          Result := inherited IsDesignMessage(ASender, AMessage);
      end;
    LM_KEYFIRST..LM_KEYLAST:
      begin
        // Relay to Frame before going further -> this could be
        // a shortcut we wish to handle.
        if FFrame.IsShortCut(TLMKey(AMessage)) then
          Exit(true)
        else
          Result := inherited IsDesignMessage(ASender, AMessage);
      end
  else
    Result := inherited IsDesignMessage(ASender, AMessage);
  end;
end;

end.

