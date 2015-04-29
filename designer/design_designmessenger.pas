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
    procedure ShowPropertiesForm(Data: PtrInt);
  protected
    procedure SetContainer(AValue: TWinControl); override;
  public
    function IsDesignMessage(ASender: TControl; var AMessage: TLMessage
       ): Boolean; override;
  end;

implementation

uses
  LCLType, Forms;

{ TDesignMessenger }

procedure TDesignMessenger.ShowPropertiesForm(Data: PtrInt);
begin
  TRuntimeDesignFrame(Data).ShowPropertiesForm(true);
end;

procedure TDesignMessenger.SetContainer(AValue: TWinControl);
begin
  inherited SetContainer(AValue);

  while Assigned(AValue) do
    if AValue is TRuntimeDesignFrame then
      break
    else
      AValue := AValue.Parent;

  if Assigned(AValue) then
    FFrame := TRuntimeDesignFrame(AValue)
  else
    FFrame := nil;
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
  S: String;
begin
  S := '';
  case AMessage.Msg of
    LM_MOUSEWHEEL:
      // Handle MouseWheel, since on (especially) windows this is not handle
      // correctly using normal message chain.
      // See also design_designpanel.pas!
      FFrame.DesignScrollBar.WindowProc(AMessage);
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
        else begin
          if (AMessage.msg = LM_LBUTTONDOWN) or
             (AMessage.msg = LM_RBUTTONDOWN)
          then
            GetParentForm(FFrame).BringToFront;
          Result := inherited IsDesignMessage(ASender, AMessage);
        end;

        case AMessage.Msg of
          LM_LBUTTONDOWN:  S := 'LM_LBUTTONDOWN';
          LM_LBUTTONUP:    S := 'LM_LBUTTONUP';
          LM_RBUTTONDOWN:  S := 'LM_RBUTTONDOWN';
          LM_RBUTTONUP:    S := 'LM_RBUTTONUP';
        end;
      end;
    LM_LBUTTONDBLCLK,
    LM_RBUTTONDBLCLK:
      begin
        case AMessage.Msg of
          LM_LBUTTONDBLCLK: S := 'LM_RBUTTONDBLCLK';
          LM_RBUTTONDBLCLK: S := 'LM_LBUTTONDBLCLK';
        end;
        {$IFNDEF LINUX}
        Application.QueueAsyncCall(@ShowPropertiesForm, PtrInt(FFrame));
        {$ENDIF}
      end;
  else
    Result := inherited IsDesignMessage(ASender, AMessage);
  end;

{  if (S <> '')
  then
    WriteLn('TDesignMessenger.IsDesignMessage: ' + S);     }

  if Result then
    AMessage.Result := 1;
end;

end.

