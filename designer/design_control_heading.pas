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
  protected
    procedure SetParent(NewParent: TWinControl); override;
  public
    constructor Create(AOwner: TComponent); Override;
    destructor Destroy; override;
    procedure SetBounds(aLeft, aTop, aWidth, aHeight: integer); override;
    function DesignFrameClass: TCustomFrameClass;
    property EpiControl: TEpiCustomControlItem read GetEpiControl write SetEpiControl;
  end;


implementation

uses
  managerprocs, settings2_var, LCLIntf, main, manager_messages,
  LCLType, design_properties_headingframe,
  JvDesignSurface;

{ TDesignHeading }

function TDesignHeading.GetEpiControl: TEpiCustomControlItem;
begin
  Result := FHeading;
end;

procedure TDesignHeading.OnHeadingChange(Sender: TObject;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
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
        TJvDesignPanel(Owner).Surface.UpdateDesigner;
      end;
  end;
end;

procedure TDesignHeading.SetEpiControl(const AValue: TEpiCustomControlItem);
begin
  FHeading := TEpiHeading(AValue);
  FHeading.RegisterOnChangeHook(@OnHeadingChange);

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

procedure TDesignHeading.SetParent(NewParent: TWinControl);
begin
  inherited SetParent(NewParent);
  if csDestroying in ComponentState then exit;

  if not Assigned(EpiControl) then
    SendMessage(MainForm.Handle, LM_DESIGNER_ADD, WPARAM(Self), LPARAM(TEpiHeading));
end;

constructor TDesignHeading.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name := GetRandomComponentName;

  // Standard properties being set for the component.
  ParentFont := false;
  Font := ManagerSettings.HeadingFont;
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

procedure TDesignHeading.SetBounds(aLeft, aTop, aWidth, aHeight: integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  UpdateEpiControl;
end;

function TDesignHeading.DesignFrameClass: TCustomFrameClass;
begin
  result := THeadingPropertiesFrame;
end;

initialization
  RegisterClass(TDesignHeading);

end.

