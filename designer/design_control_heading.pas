unit design_control_heading;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, epicustombase, epidatafiles, design_types,
  Forms, Controls, JvDesignSurface;

type
  { TDesignHeading }

  TDesignHeading = Class(TLabel, IDesignEpiControl)
  private
    FHeading: TEpiHeading;
  private
    function GetEpiControl: TEpiCustomControlItem;
    function GetExtendedBounds: TRect;
    procedure OnHeadingChange(Sender: TObject; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
    procedure SetEpiControl(const AValue: TEpiCustomControlItem);
    procedure SetExtendedBounds(const AValue: TRect);
    procedure UpdateHint;
    procedure UpdateEpiControl;
    procedure ReadHeading(Stream: TStream);
    procedure WriteHeading(Stream: TStream);
    function Surface: TJvDesignSurface;
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
  LCLType, design_properties_headingframe,
  epistringutils, epidatafilestypes,
  manager_globals;

{ TDesignHeading }

procedure TDesignHeading.ReadHeading(Stream: TStream);
var
  CopyHeading: TEpiHeading;
begin
  Stream.Read(CopyHeading, SizeOf(Pointer));
  FHeading := TEpiHeading(CopyHeading.Clone(nil));
end;

procedure TDesignHeading.WriteHeading(Stream: TStream);
var
  CopyHeading: TEpiHeading;
begin
  CopyHeading := TEpiHeading(FHeading.Clone(nil));
  GlobalCopyList.Add(CopyHeading);
  Stream.Write(CopyHeading, Sizeof(Pointer));
end;

function TDesignHeading.Surface: TJvDesignSurface;
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

procedure TDesignHeading.SetParent(NewParent: TWinControl);
var
  Fixup: Boolean;
begin
  Fixup := false;
  if (Parent = nil) and
     (NewParent <> nil) and
     (FHeading <> nil)
  then
    Fixup := true;

  inherited SetParent(NewParent);

  if Fixup then
    DoFixupCopyControl;
end;

function TDesignHeading.GetEpiControl: TEpiCustomControlItem;
begin
  Result := FHeading;
end;

function TDesignHeading.GetExtendedBounds: TRect;
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

procedure TDesignHeading.OnHeadingChange(Sender: TObject;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  if EventGroup = eegCustomBase then
  begin
    case TEpiCustomChangeEventType(EventType) of
      ecceDestroy:
        begin
          FHeading.UnRegisterOnChangeHook(@OnHeadingChange);
          FHeading := nil;
        end;
      ecceName: ;
      ecceAddItem: ;
      ecceDelItem: ;
      ecceSetItem: ;
      ecceSetTop: ;
      ecceSetLeft: ;

      ecceUpdate,
      ecceText:
        begin
          Caption := TEpiHeading(Sender).Caption.Text;
          Surface.UpdateDesigner;
        end;
    end;
  end;

  if EventGroup = eegHeading then
  begin
    case TEpiHeadingChangeEvent(EventType) of
      ehceType: UpdateControl;
    end;
  end;
end;

procedure TDesignHeading.SetEpiControl(const AValue: TEpiCustomControlItem);
begin
  FHeading := TEpiHeading(AValue);
  FHeading.RegisterOnChangeHook(@OnHeadingChange);
  FHeading.AddCustomData(DesignControlCustomDataKey, Self);

  Caption := FHeading.Caption.Text;
  UpdateEpiControl;
  UpdateControl;
end;

procedure TDesignHeading.SetExtendedBounds(const AValue: TRect);
begin
  BoundsRect := AValue;
end;

procedure TDesignHeading.UpdateHint;
begin

end;

procedure TDesignHeading.UpdateEpiControl;
begin
  if not Assigned(FHeading) then exit;

  with FHeading do
  begin
    BeginUpdate;
    Left := Self.Left;
    Top := Self.Top;
    EndUpdate;
  end;
end;

procedure TDesignHeading.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('EpiHeading', @ReadHeading, @WriteHeading, Assigned(FHeading));
end;

procedure TDesignHeading.DoFixupCopyControl;
var
  Section: TEpiSection;
begin
  Section := TEpiSection((Parent as IDesignEpiControl).EpiControl);
  if not Section.Headings.ValidateRename(FHeading.Name, false) then
    FHeading.Name := Section.Headings.GetUniqueItemName(TEpiHeading);
  Section.Headings.AddItem(FHeading);

  SetEpiControl(FHeading);
end;

constructor TDesignHeading.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name := GetRandomComponentName;

  // Standard properties being set for the component.
  ParentFont := false;
  Align := alNone;
  ShowHint := true;
  ParentColor := true;

  UpdateControl;
end;

destructor TDesignHeading.Destroy;
begin
  if Assigned(FHeading) then
    begin
      FHeading.UnRegisterOnChangeHook(@OnHeadingChange);
      FHeading.Free;
    end;
  inherited Destroy;
end;

procedure TDesignHeading.UpdateControl;
begin
  if not Assigned(FHeading) then exit;

  case FHeading.HeadingType of
    htH1: Font.Assign(ManagerSettings.HeadingFont1);
    htH2: Font.Assign(ManagerSettings.HeadingFont2);
    htH3: Font.Assign(ManagerSettings.HeadingFont3);
    htH4: Font.Assign(ManagerSettings.HeadingFont4);
    htH5: Font.Assign(ManagerSettings.HeadingFont5);
  end;
  Surface.UpdateDesigner;
end;

procedure TDesignHeading.SetBounds(aLeft, aTop, aWidth, aHeight: integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  UpdateEpiControl;
end;

procedure TDesignHeading.FixupCopyControl;
begin
  DoFixupCopyControl;
end;

function TDesignHeading.DesignFrameClass: TCustomFrameClass;
begin
  result := THeadingPropertiesFrame;
end;

initialization
  RegisterClass(TDesignHeading);

end.

