unit design_designmessenger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JvDesignImp, JvDesignSurface, Controls,
  LMessages;

type

  { TDesignMessenger }

  TDesignMessenger = class(TJvDesignDesignerMessenger)
    function IsDesignMessage(ASender: TControl; var AMessage: TLMessage
       ): Boolean; override;
  end;

implementation

uses
  LCLType;

{ TDesignMessenger }

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
        P := MousePoint;
        Ctrl := TJvDesignPanel(Container).Surface.FindControl(P.X, P.Y);

        if Assigned(Ctrl.PopupMenu) then
        begin
          Ctrl.PopupMenu.PopUp;
          Result := true;
        end else
          Result := inherited IsDesignMessage(ASender, AMessage);
      end;
  else
    Result := inherited IsDesignMessage(ASender, AMessage);
  end;
end;

end.

