unit design_control_memo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, epicustombase, epidatafiles, episettings,
  design_types, Forms, Controls, JvDesignSurface;

type
  { TDesignMemo }

  TDesignMemo = Class(TMemo, IDesignEpiControl)
  private
    FMemoField: TEpiMemoField;
    FNameLabel: TLabel;
    FQuestionLabel: TLabel;
    FProjectSettings: TEpiProjectSettings;
    procedure OnProjectSettingsChange(const Sender: TEpiCustomBase;
      const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
    procedure OnFieldChange(const Sender: TEpiCustomBase;
      const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
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
  managerprocs, settings2_var, LCLIntf,
  LCLType, design_properties_fieldframe, epidocument, Graphics,
  epistringutils, epidatafilestypes,
  manager_globals;

{ TDesignMemo }

procedure TDesignMemo.OnProjectSettingsChange(const Sender: TEpiCustomBase;
  const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
begin
  if (EventGroup = eegCustomBase)
  then
    case TEpiCustomChangeEventType(EventType) of
      ecceDestroy:
        begin
          FProjectSettings.UnRegisterOnChangeHook(@OnProjectSettingsChange);
          FProjectSettings := nil;
        end;
      ecceUpdate:
        UpdateControl;
      ecceName: ;
      ecceAddItem: ;
      ecceDelItem: ;
      ecceSetItem: ;
      ecceSetTop: ;
      ecceSetLeft: ;
      ecceText: ;
    end;

  if (EventGroup = eegProjectSettings)
  then
    case TEpiProjectSettingChangeEvent(EventType) of
      epceFieldName,
      epceFieldBorder:
        UpdateControl;
      epceBackupInterval: ;
      epceBackupShutdown: ;
      epceAutoIncStart: ;
    end;
end;

procedure TDesignMemo.OnFieldChange(const Sender: TEpiCustomBase;
  const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
begin
  if EventGroup = eegFields then
    case TEpiFieldsChangeEventType(EventType) of
      efceSetDecimal,
      efceSetLength: ;

      efceSetLeft,
      efceSetTop: ;

      efceEntryMode,
      efceShowValueLabel:
        UpdateControl;

      efceValueLabelSet: ;
    end;

  if EventGroup = eegCustomBase then
    case TEpiCustomChangeEventType(EventType) of
      ecceDestroy:
        begin
          if Initiator <> FMemoField then exit;
          FMemoField.UnRegisterOnChangeHook(@OnFieldChange);
          FMemoField := nil;
        end;
      ecceUpdate: ;
      ecceName:
        UpdateControl;
      ecceAddItem: ;
      ecceDelItem: ;
      ecceSetItem: ;
      ecceSetTop: ;
      ecceSetLeft: ;
      ecceText:
        UpdateControl;
      ecceReferenceDestroyed: ;
    end;
end;

function TDesignMemo.GetEpiControl: TEpiCustomControlItem;
begin
  result := FMemoField;
end;

function TDesignMemo.GetExtendedBounds: TRect;
begin
  with Result do
  begin
    // LEFT
    Left := Self.Left;
    if FQuestionLabel.Caption <> '' then
      Left := FQuestionLabel.Left;
    if FNameLabel.Caption <> '' then
      Left := FNameLabel.Left;

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
  FMemoField := TEpiMemoField(AValue);

  FMemoField.RegisterOnChangeHook(@OnFieldChange);
  FProjectSettings := TEpiDocument(FMemoField.RootOwner).ProjectSettings;
  FProjectSettings.RegisterOnChangeHook(@OnProjectSettingsChange);

  FMemoField.AddCustomData(DesignControlCustomDataKey, Self);

//  UpdateValueLabelConnection(nil, FField.ValueLabelSet);

  UpdateEpiControl;
  UpdateControl;
end;

procedure TDesignMemo.SetExtendedBounds(const AValue: TRect);
var
  lRect: TRect;
begin
  lRect := AValue;

  if FQuestionLabel.Caption <> '' then
    lRect.Left := (Left - FQuestionLabel.Left) + AValue.Left;
  if FNameLabel.Caption <> '' then
    lRect.Left := (Left - FNameLabel.Left) + AValue.Left;

  BoundsRect := lRect;
end;

procedure TDesignMemo.UpdateHint;
begin

end;

procedure TDesignMemo.UpdateEpiControl;
begin
  if not Assigned(FMemoField) then exit;

  with FMemoField do
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
     (FMemoField <> nil)
  then
    Fixup := true;

  inherited SetParent(NewParent);
  if [csDestroying{, csLoading}] * ComponentState <> [] then exit;

  FQuestionLabel.Parent := NewParent;
  FNameLabel.Parent := NewParent;

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

  Name := GetRandomComponentName;

  FQuestionLabel := TLabel.Create(Self);
  FQuestionLabel.Anchors := [];
  FQuestionLabel.AnchorToNeighbour(akRight, 5, Self);
  FQuestionLabel.AnchorParallel(akTop, 0, Self);
  FQuestionLabel.ParentFont := false;
  FQuestionLabel.ControlStyle := FQuestionLabel.ControlStyle + [csNoDesignSelectable];
  FQuestionLabel.SetSubComponent(true);

  FNameLabel := TLabel.Create(Self);
  FNameLabel.Anchors := [];
  FNameLabel.AnchorToNeighbour(akRight, 5, FQuestionLabel);
  FNameLabel.AnchorParallel(akTop, 0, FQuestionLabel);
  FNameLabel.ParentFont := false;
  FNameLabel.ControlStyle := FNameLabel.ControlStyle + [csNoDesignSelectable];
  FNameLabel.SetSubComponent(true);

  AutoSize := false;
  ReadOnly := true;
  Align := alNone;
  ShowHint := true;
  ParentColor := false;
  ParentFont := false;
  Text := '';
end;

destructor TDesignMemo.Destroy;
begin
  if Assigned(FMemoField) then
    begin
      FProjectSettings.UnRegisterOnChangeHook(@OnProjectSettingsChange);
      FMemoField.Free;
    end;

  FNameLabel.Anchors       := [];
  FQuestionLabel.Anchors   := [];

  inherited Destroy;
end;

procedure TDesignMemo.UpdateControl;
begin
  FNameLabel.Font.Assign(ManagerSettings.FieldFont);
  FQuestionLabel.Font.Assign(ManagerSettings.FieldFont);
  Font.Assign(ManagerSettings.FieldFont);

  SetBounds(Left, Top, Width, Height);

  // Change caption, since Visible does not work when csDesigning.
  if FProjectSettings.ShowFieldNames then
    FNameLabel.Caption := FMemoField.Name
  else
    FNameLabel.Caption := '';

  FQuestionLabel.Caption := FMemoField.Question.Text;

  if FMemoField.EntryMode = emMustEnter then
    Color := clRed
  else
    Color := clDefault;
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
  result := TFieldPropertiesFrame;
end;

initialization
  RegisterClass(TDesignMemo);

end.

