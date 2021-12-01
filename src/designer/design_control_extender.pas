unit design_control_extender;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, Controls;

type

  { TDesignStaticText }

  TDesignStaticText = class(TCustomStaticText)
 public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: integer); override;
  end;

implementation

uses
  design_designpanel, design_runtimedesigner;

{ TDesignStaticText }

constructor TDesignStaticText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BorderStyle := sbsSunken;
  Transparent := true;
end;

destructor TDesignStaticText.Destroy;
var
  WinCtrl: TWinControl;
begin
  WinCtrl := Parent;
  while Assigned(WinCtrl) do
  begin
    if WinCtrl is TRuntimeDesignFrame then break;
    WinCtrl := WinCtrl.Parent;
  end;

  if Assigned(WinCtrl) then
    TRuntimeDesignFrame(WinCtrl).Extender := nil;

  inherited Destroy;
end;

procedure TDesignStaticText.SetBounds(ALeft, ATop, AWidth, AHeight: integer);
var
  W: Integer;
  H: Integer;
  WinCtrl: TWinControl;
begin
  WinCtrl := Parent;
  while Assigned(WinCtrl) do
  begin
    if WinCtrl is TDesignPanel then break;
    WinCtrl := WinCtrl.Parent;
  end;

  if not Assigned(WinCtrl) then exit;

  Caption := '';
  Caption := '=== Page Extender ===';

  TDesignPanel(WinCtrl).Canvas.GetTextSize(Caption, W, H);
  inherited SetBounds(ALeft, ATop, W, H+5);
  TDesignPanel(WinCtrl).Surface.UpdateDesigner;
end;

end.

