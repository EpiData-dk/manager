unit design_control_heading;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, epicustombase, epidatafiles, design_types,
  Forms, Controls;

type
  { TDesignHeading }

  TDesignHeading = Class(TLabel, IDesignEpiControl)
  private
    FHeading: TEpiHeading;
  private
    function GetEpiControl: TEpiCustomControlItem;
    procedure OnHeadingChange(Sender: TObject; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
    procedure SetEpiControl(const AValue: TEpiCustomControlItem);
    procedure UpdateHint;
    procedure UpdateEpiControl;
    procedure ReadHeading(Stream: TStream);
    procedure WriteHeading(Stream: TStream);
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
  end;


implementation

uses
  managerprocs, settings2_var, LCLIntf, main, manager_messages,
  LCLType, design_properties_headingframe,
  JvDesignSurface, epistringutils,
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

procedure TDesignHeading.OnHeadingChange(Sender: TObject;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);

  function Surface: TJvDesignSurface;
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

procedure TDesignHeading.SetEpiControl(const AValue: TEpiCustomControlItem);
begin
  FHeading := TEpiHeading(AValue);
  FHeading.RegisterOnChangeHook(@OnHeadingChange);
  FHeading.AddCustomData(DesignControlCustomDataKey, Self);

  Caption := FHeading.Caption.Text;
  UpdateEpiControl;
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
  Font.Assign(ManagerSettings.HeadingFont);
  Align := alNone;
  ShowHint := true;
  ParentColor := true;
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
  Font.Assign(ManagerSettings.HeadingFont);
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

