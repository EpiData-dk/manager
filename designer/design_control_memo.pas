unit design_control_memo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, epicustombase, epidatafiles, design_types,
  Forms, Controls, JvDesignSurface;

type
  { TDesignMemo }

  TDesignMemo = Class(TMemo, IDesignEpiControl)
  private
    FSection: TEpiSection;
  private
    function GetEpiControl: TEpiCustomControlItem;
    function GetExtendedBounds: TRect;
    procedure SetEpiControl(const AValue: TEpiCustomControlItem);
    procedure SetExtendedBounds(const AValue: TRect);
    procedure UpdateHint;
    procedure UpdateEpiControl;
    procedure ReadMemo(Stream: TStream);
    procedure WriteMemo(Stream: TStream);
    function  Surface: TJvDesignSurface;
  protected
    procedure SetParent(NewParent: TWinControl); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoFixupCopyControl;
  public
    constructor Create(AOwner: TComponent); Override;
    destructor Destroy; override;
    procedure UpdateControl;
    procedure SetBounds(aLeft, aTop, aWidth, aHeight: integer); override;
    procedure FixupCopyControl;
    function DesignFrameClass: TCustomFrameClass;
    property EpiControl: TEpiCustomControlItem read GetEpiControl write SetEpiControl;
    property ExtendedBounds: TRect read GetExtendedBounds write SetExtendedBounds;
  end;

implementation

uses
  managerprocs, settings2_var, LCLIntf, main, manager_messages,
  LCLType, design_properties_sectionframe,
  epistringutils, epidatafilestypes,
  manager_globals;

{ TDesignMemo }

function TDesignMemo.GetEpiControl: TEpiCustomControlItem;
begin
  result := FSection;
end;

function TDesignMemo.GetExtendedBounds: TRect;
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

procedure TDesignMemo.SetEpiControl(const AValue: TEpiCustomControlItem);
begin
  FSection := TEpiSection(AValue);
end;

procedure TDesignMemo.SetExtendedBounds(const AValue: TRect);
begin
  BoundsRect := AValue;
end;

procedure TDesignMemo.UpdateHint;
begin

end;

procedure TDesignMemo.UpdateEpiControl;
begin
  if not Assigned(FSection) then exit;

  with FSection do
  begin
    BeginUpdate;
    Left := Self.Left;
    Top := Self.Top;
    Width := Self.Width;
    Height := Self.Height;
    EndUpdate;
  end;
end;

procedure TDesignMemo.ReadMemo(Stream: TStream);
begin

end;

procedure TDesignMemo.WriteMemo(Stream: TStream);
begin

end;

function TDesignMemo.Surface: TJvDesignSurface;
var
  CurrControl: TControl;
begin
  CurrControl := Self;
  while Assigned(CurrControl) and
        (not (CurrControl is TJvDesignPanel))
  do
    CurrControl := CurrControl.Parent;

  if Assigned(CurrControl) then
    result := TJvDesignPanel(CurrControl).Surface;
end;

procedure TDesignMemo.SetParent(NewParent: TWinControl);
var
  Fixup: Boolean;
begin
  Fixup := false;
  if (Parent = nil) and
     (NewParent <> nil) and
     (FSection <> nil)
  then
    Fixup := true;

  inherited SetParent(NewParent);

  if Fixup then
    DoFixupCopyControl;
end;

procedure TDesignMemo.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
end;

procedure TDesignMemo.DoFixupCopyControl;
begin
  //
end;

constructor TDesignMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TDesignMemo.Destroy;
begin
  inherited Destroy;
end;

procedure TDesignMemo.UpdateControl;
begin
  Surface.UpdateDesigner;
end;

procedure TDesignMemo.SetBounds(aLeft, aTop, aWidth, aHeight: integer);
begin
  inherited SetBounds(aLeft, aTop, aWidth, aHeight);
  UpdateEpiControl;
end;

procedure TDesignMemo.FixupCopyControl;
begin
  DoFixupCopyControl;
end;

function TDesignMemo.DesignFrameClass: TCustomFrameClass;
begin
  result := TSectionPropertiesFrame;
end;

initialization
  RegisterClass(TDesignMemo);

end.

