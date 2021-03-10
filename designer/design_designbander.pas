unit design_designbander;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JvDesignSurface, JvDesignImp, Controls;

type

  { TDesignBander }

  TDesignBander = class(TJvDesignBander)
  {$IFDEF DARWIN}
  private
    FOldPaint: TNotifyEvent;
    FDesignPanel: TJvDesignPanel;
    procedure ApplyDarwinHack;
    procedure RemoveDarwinHack;
    procedure BanderPaint(Sender: TObject);
  protected
    function GetClient: TControl; override;
    procedure PaintDragRect; override;
  public
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer
       ); override;
  {$ENDIF}
  public
    constructor Create(AOwner: TJvDesignSurface); override;
    destructor Destroy; override;
  end;

implementation

uses
  Graphics;

{ TDesignBander }

{$IFDEF DARWIN}
procedure TDesignBander.ApplyDarwinHack;
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
    FDesignPanel.OnPaint := @BanderPaint;
  end;
end;

procedure TDesignBander.RemoveDarwinHack;
begin
  FDesignPanel.OnPaint := FOldPaint;
end;

procedure TDesignBander.BanderPaint(Sender: TObject);
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

function TDesignBander.GetClient: TControl;
begin
  Result := FDesignPanel;
end;

procedure TDesignBander.PaintDragRect;
begin
  FDesignPanel.Invalidate;
end;

procedure TDesignBander.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  RemoveDarwinHack;
  FDesignPanel.Invalidate;
end;
{$ENDIF}

constructor TDesignBander.Create(AOwner: TJvDesignSurface);
begin
  inherited Create(AOwner);
  {$IFDEF DARWIN}
  ApplyDarwinHack;
  {$ENDIF}
end;

destructor TDesignBander.Destroy;
begin
  {$IFDEF DARWIN}
  RemoveDarwinHack;
  {$ENDIF}
  inherited Destroy;
end;

end.

