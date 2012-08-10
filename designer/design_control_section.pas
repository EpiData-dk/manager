unit design_control_section;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, epicustombase, epidatafiles, design_types;

type
  { TDesignSection }

  TDesignSection = Class(TGroupBox, IDesignEpiControl)
  private
    FSection: TEpiSection;
    FUpdating: boolean;
  private
    function GetEpiControl: TEpiCustomControlItem;
    procedure OnChange(Sender: TObject; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
    procedure OnTextChange(Sender: TObject; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
    procedure SetEpiControl(const AValue: TEpiCustomControlItem);
    procedure UpdateHint;
    procedure UpdateControl;
  protected
    procedure DoSetBounds(ALeft, ATop, AWidth, AHeight: integer); override;
  public
    constructor Create(AOwner: TComponent); Override;
    destructor Destroy; override;
    property EpiControl: TEpiCustomControlItem read GetEpiControl write SetEpiControl;
  end;

implementation

uses
  managerprocs, settings2_var, LCLIntf, LCLType, main, manager_messages;

{ TDesignSection }

function TDesignSection.GetEpiControl: TEpiCustomControlItem;
begin
  Result := FSection;
end;

procedure TDesignSection.OnChange(Sender: TObject; EventGroup: TEpiEventGroup;
  EventType: Word; Data: Pointer);
begin

end;

procedure TDesignSection.OnTextChange(Sender: TObject;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin

end;

procedure TDesignSection.SetEpiControl(const AValue: TEpiCustomControlItem);
begin
  FSection := TEpiSection(Avalue);
  UpdateControl;
end;

procedure TDesignSection.UpdateHint;
begin

end;

procedure TDesignSection.UpdateControl;
begin
  if not Assigned(FSection) then exit;
  if FUpdating then exit;

  FUpdating := true;
  with FSection do
  begin
    BeginUpdate;
    Top := Self.Top;
    Left := Self.Left;
    Width := Self.Width;
    Height := Self.Height;
    Caption.Text := Self.Caption;
    EndUpdate;
  end;
  FUpdating := False;
end;

procedure TDesignSection.DoSetBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  inherited DoSetBounds(ALeft, ATop, AWidth, AHeight);
  UpdateControl;
end;

constructor TDesignSection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name := GetRandomComponentName;

  ShowHint := true;
  ParentColor := true;
  Font := ManagerSettings.SectionFont;
  PostMessage(MainForm.Handle, LM_DESIGNER_ADD, WPARAM(Self), LPARAM(TEpiSection));
end;

destructor TDesignSection.Destroy;
begin
  inherited Destroy;
end;

initialization
  RegisterClass(TDesignSection);

end.

