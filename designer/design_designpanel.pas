unit design_designpanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JvDesignSurface, design_types, epicustombase,
  epidatafiles, forms, Controls;

type
  { TDesignPanel }

  {
    The whole IFDEF Windows is because WM_MOUSEWHEEL does not work properly with our
    constuction of Panel within scroolbox! Hence, intercept mousewheel message and
    scroll correctly!
  }

  TDesignPanel = class(TJvDesignPanel, IDesignEpiControl)
  private
    FSection: TEpiSection;
    function  GetEpiControl: TEpiCustomControlItem;
    function GetExtendedBounds: TRect;
    procedure SetEpiControl(const AValue: TEpiCustomControlItem);
    procedure SetExtendedBounds(const AValue: TRect);
  {$IFDEF MSWINDOWS}
  protected
    procedure CreateWnd; override;
  {$ENDIF}
  public
    function DesignFrameClass: TCustomFrameClass;
    procedure UpdateControl;
    procedure FixupCopyControl;
    property EpiControl: TEpiCustomControlItem read GetEpiControl write SetEpiControl;
    property ExtendedBounds: TRect read GetExtendedBounds write SetExtendedBounds;
  end;


implementation

uses
  {$IFDEF MSWINDOWS}
  windows, win32proc, Win32Int, LMessages, LCLMessageGlue,
  {$ENDIF}
  design_properties_sectionframe, manager_globals;

{$IFDEF MSWINDOWS}
var
  PrevWndProc: WNDPROC;
{$ENDIF}

{ TDesignPanel }

function TDesignPanel.GetEpiControl: TEpiCustomControlItem;
begin
  result := FSection;
end;

function TDesignPanel.GetExtendedBounds: TRect;
begin
  with Result do
  begin
    // LEFT
    Left := Self.Left;

    // RIGHT
    Right := Self.Left + Self.Width;

    // TOP
    Top := Self.Top;

    // BOTTOM
    Bottom := Self.Top + Self.Height;
  end;
end;

procedure TDesignPanel.SetEpiControl(const AValue: TEpiCustomControlItem);
begin
  FSection := TEpiSection(AValue);
  FSection.AddCustomData(DesignControlCustomDataKey, Self);
end;

procedure TDesignPanel.SetExtendedBounds(const AValue: TRect);
begin
  BoundsRect := AValue;
end;

{$IFDEF MSWINDOWS}
function WndCallback(Ahwnd: HWND; uMsg: UINT; wParam: WParam; lParam: LParam):LRESULT; stdcall;
var
  W: TWinControl;
  P: TPoint;
  Msg: TLMMouseEvent;
begin
  if uMsg=WM_MOUSEWHEEL then
  begin
    Ahwnd := windows.GetParent(Ahwnd);
    W := GetWin32WindowInfo(Ahwnd)^.WinControl;

    P := Classes.Point(GET_X_LPARAM(LParam), GET_Y_LPARAM(LParam));
    Windows.ScreenToClient(Ahwnd, P);
    with Msg do
    begin
      Msg := LM_MOUSEWHEEL;
      X := P.X;
      Y := P.Y;
      Button := LOWORD(Integer(WParam));
      WheelDelta := SmallInt(HIWORD(Integer(WParam)));
      State := KeysToShiftState(Button);
      Result := 0;
      UserData := Pointer(GetWindowLong(Ahwnd, GWL_USERDATA));
    end;
    DeliverMessage(W, Msg);
    Exit;
  end;
  result := CallWindowProc(PrevWndProc, Ahwnd, uMsg, WParam, LParam);
end;

procedure TDesignPanel.CreateWnd;
begin
  inherited CreateWnd;
  PrevWndProc := Windows.WNDPROC(SetWindowLongPtr(Self.Handle, GWL_WNDPROC, PtrInt(@WndCallback)));
end;
{$ENDIF}

function TDesignPanel.DesignFrameClass: TCustomFrameClass;
begin
  result := TSectionPropertiesFrame;
end;

procedure TDesignPanel.UpdateControl;
begin
  // Do nothing
end;

procedure TDesignPanel.FixupCopyControl;
begin
  // Do nothing...
end;

end.

