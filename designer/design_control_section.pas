unit design_control_section;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, epicustombase, epidatafiles, design_types,
  Forms, Controls;

type
  { TDesignSection }

  TDesignSection = Class(TGroupBox, IDesignEpiControl)
  private
    FSection: TEpiSection;
  private
    procedure OnChange(Sender: TObject; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
    procedure UpdateHint;
    procedure UpdateEpiControl;
    function  GetEpiControl: TEpiCustomControlItem;
    procedure SetEpiControl(const AValue: TEpiCustomControlItem);
  protected
    procedure SetParent(NewParent: TWinControl); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
  public
    constructor Create(AOwner: TComponent); Override;
    destructor Destroy; override;
    procedure WriteState(Writer: TWriter); override;
    procedure ReadState(Reader: TReader); override;
    function DesignFrameClass: TCustomFrameClass;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: integer); override;
    property  EpiControl: TEpiCustomControlItem read GetEpiControl write SetEpiControl;
  end;

implementation

uses
  managerprocs, settings2_var, LCLIntf, LCLType, main, manager_messages,
  design_properties_sectionframe, epistringutils;

{ TDesignSection }

function TDesignSection.GetEpiControl: TEpiCustomControlItem;
begin
  Result := FSection;
end;

procedure TDesignSection.OnChange(Sender: TObject; EventGroup: TEpiEventGroup;
  EventType: Word; Data: Pointer);
begin
  case TEpiCustomChangeEventType(EventType) of
    ecceDestroy:
      begin
        FSection.UnRegisterOnChangeHook(@OnChange);
        FSection := nil;
      end;
    ecceName: ;
    ecceAddItem: ;
    ecceDelItem: ;
    ecceSetItem: ;
    ecceSetTop: ;
    ecceSetLeft: ;
    ecceUpdate,
    ecceText:
      Caption := TEpiSection(Sender).Caption.Text;
  end;
end;

procedure TDesignSection.SetEpiControl(const AValue: TEpiCustomControlItem);
begin
  FSection := TEpiSection(Avalue);
  FSection.RegisterOnChangeHook(@OnChange);
  UpdateEpiControl;
end;

procedure TDesignSection.UpdateHint;
begin

end;

procedure TDesignSection.UpdateEpiControl;
begin
  if not Assigned(FSection) then exit;

  with FSection do
  begin
    BeginUpdate;
    Top := Self.Top;
    Left := Self.Left;
    Width := Self.Width;
    Height := Self.Height;
    EndUpdate;
  end;
end;

procedure TDesignSection.SetParent(NewParent: TWinControl);
begin
  inherited SetParent(NewParent);
  if csDestroying in ComponentState then exit;

  if not Assigned(FSection) then
    SendMessage(MainForm.Handle, LM_DESIGNER_ADD, WPARAM(Self), LPARAM(TEpiSection));
end;

procedure TDesignSection.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I : Integer;
  Control : TControl;
begin
  for I := 0 to ControlCount-1 do
    Proc(Controls[i]);
end;

constructor TDesignSection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name := GetRandomComponentName;

  ShowHint := true;
  ParentColor := true;
  Font := ManagerSettings.SectionFont;
  Caption := '';
end;

destructor TDesignSection.Destroy;
begin
  if Assigned(FSection) then
    begin
      FSection.UnRegisterOnChangeHook(@OnChange);
      FSection.Free;
    end;
  inherited Destroy;
end;

procedure TDesignSection.WriteState(Writer: TWriter);
begin
  inherited WriteState(Writer);
  Writer.WriteListEnd;
  Writer.WriteString(FSection.Name);
end;

procedure TDesignSection.ReadState(Reader: TReader);
var
  NewName: String;
begin
  inherited ReadState(Reader);
  NewName := Reader.ReadString;

  PostMessage(MainForm.Handle, LM_DESIGNER_COPY, WPARAM(Self), LPARAM(TString.Create(NewName)));
end;

function TDesignSection.DesignFrameClass: TCustomFrameClass;
begin
  result := TSectionPropertiesFrame;
end;

procedure TDesignSection.SetBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  UpdateEpiControl;
end;

initialization
  RegisterClass(TDesignSection);

end.

