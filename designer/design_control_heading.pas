unit design_control_heading;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, epicustombase, epidatafiles, design_types;

type
  { TDesignHeading }

  TDesignHeading = Class(TLabel, IDesignEpiControl)
  private
    FHeading: TEpiHeading;
    FUpdating: Boolean;
  private
    function GetEpiControl: TEpiCustomControlItem;
    procedure OnHeadingChange(Sender: TObject; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
    procedure OnCaptionChange(Sender: TObject; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
    procedure SetEpiControl(const AValue: TEpiCustomControlItem);
    procedure UpdateHint;
    procedure UpdateControl;
  protected
    procedure DoSetBounds(ALeft, ATop, AWidth, AHeight: integer); override;
  public
    constructor Create(AOwner: TComponent); Override;
    destructor  Destroy; override;
    property    EpiControl: TEpiCustomControlItem read GetEpiControl write SetEpiControl;
  end;


implementation

uses
  managerprocs, Controls, settings2_var, LCLIntf, main, manager_messages,
  LCLType;

{ TDesignHeading }

function TDesignHeading.GetEpiControl: TEpiCustomControlItem;
begin
  Result := FHeading;
end;

procedure TDesignHeading.OnHeadingChange(Sender: TObject;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  if FUpdating then exit;
end;

procedure TDesignHeading.OnCaptionChange(Sender: TObject;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin

end;

procedure TDesignHeading.SetEpiControl(const AValue: TEpiCustomControlItem);
begin
  FHeading := TEpiHeading(AValue);
  UpdateControl;
end;

procedure TDesignHeading.UpdateHint;
begin

end;

procedure TDesignHeading.UpdateControl;
begin
  if not Assigned(FHeading) then exit;
  if FUpdating then exit;;

  FUpdating := true;
  with FHeading do
  begin
    BeginUpdate;
    Left := Self.Left;
    Top := Self.Top;
    Caption.Text := Self.Caption;
    EndUpdate;
  end;
  FUpdating := false;;
end;

procedure TDesignHeading.DoSetBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  inherited DoSetBounds(ALeft, ATop, AWidth, AHeight);
  UpdateControl;
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
  FUpdating := false;;

  PostMessage(MainForm.Handle, LM_DESIGNER_ADD, WPARAM(Self), LPARAM(TEpiHeading));
end;

destructor TDesignHeading.Destroy;
begin
  inherited Destroy;
end;

initialization
  RegisterClass(TDesignHeading);

end.

