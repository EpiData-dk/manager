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
  manager_globals, math;

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

    // TOP
    Top := Self.Top;
    if (FQuestionLabel.Caption <> '') then
      Top := FQuestionLabel.Top;
    if (FNameLabel.Caption <> '') then
      Top := FNameLabel.Top;

    // RIGHT
    Right := Math.Max(Left + Self.Width,
                      Left + (FQuestionLabel.Left + FQuestionLabel.Width));

    // BOTTOM
    Bottom := Top + Self.Height;
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
    lRect.Top := (Top - FQuestionLabel.Left) + AValue.Top;
  if FNameLabel.Caption <> '' then
    lRect.Top := (Top - FNameLabel.Top) + AValue.Top;

  if (Left + (FQuestionLabel.Left + FQuestionLabel.Width) > Width) then
    lRect.Right := Left + Width;

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
var
  CopyField: TEpiField;
  RefMap: TEpiReferenceMap;
begin
  Stream.Read(CopyField, SizeOf(Pointer));

  RefMap := TEpiReferenceMap.Create;
  FMemoField := TEpiMemoField(CopyField.Clone(nil, RefMap));
  RefMap.FixupReferences;
  RefMap.Free;
end;

procedure TDesignMemo.WriteMemo(Stream: TStream);
var
  CopyField: TEpiField;
  RefMap: TEpiReferenceMap;
begin
  RefMap := TEpiReferenceMap.Create;
  CopyField := TEpiField(FMemoField.Clone(nil, RefMap));
  RefMap.FixupReferences;
  RefMap.Free;

  // Wipe data content on copy!
  CopyField.ResetData;

  GlobalCopyList.Add(CopyField);
  Stream.Write(CopyField, Sizeof(Pointer));
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

  FNameLabel.Parent := NewParent;
  FQuestionLabel.Parent := NewParent;

  if Fixup then
    DoFixupCopyControl;
end;

procedure TDesignMemo.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('EpiField', @ReadMemo, @WriteMemo, Assigned(FMemoField));
end;

procedure TDesignMemo.DoFixupCopyControl;
var
  Section: TEpiSection;
begin
  Section := TEpiSection((Parent as IDesignEpiControl).EpiControl);

  if not Section.Fields.ValidateRename(FMemoField.Name, false) then
    FMemoField.Name := Section.Fields.GetUniqueItemName(TEpiField);

  Section.Fields.AddItem(FMemoField);

  SetEpiControl(FMemoField);
end;

constructor TDesignMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Name := GetRandomComponentName;

  FNameLabel := TLabel.Create(Self);
  FNameLabel.Anchors := [];
  FNameLabel.AnchorParallel(akLeft, 0, Self);
  FNameLabel.AnchorToNeighbour(akBottom, 5, Self);
  FNameLabel.ParentFont := false;
  FNameLabel.ControlStyle := FNameLabel.ControlStyle + [csNoDesignSelectable];
  FNameLabel.SetSubComponent(true);

  FQuestionLabel := TLabel.Create(Self);
  FQuestionLabel.Anchors := [];
  FQuestionLabel.AnchorToNeighbour(akLeft, 5, FNameLabel);
  FQuestionLabel.AnchorParallel(akBottom, 0, FNameLabel);
  FQuestionLabel.ParentFont := false;
  FQuestionLabel.ControlStyle := FQuestionLabel.ControlStyle + [csNoDesignSelectable];
  FQuestionLabel.SetSubComponent(true);

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

  FQuestionLabel.Anchors   := [];
  FNameLabel.Anchors       := [];

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

