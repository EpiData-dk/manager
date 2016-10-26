unit design_control_field;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, epicustombase, epidatafiles, epidatafilestypes,
  Controls, StdCtrls, design_types, Forms, episettings, epivaluelabels;

type
  { TDesignField }
  TDesignField = class(TEdit, IDesignEpiControl)
  private
    FField: TEpiField;
    FOnShowHint: TDesignFrameShowHintEvent;
    FWidth: Integer;
    FHeight: Integer;
    FInitialized: boolean;
    FProjectSettings: TEpiProjectSettings;
    FNameLabel: TLabel;
    FQuestionLabel: TLabel;
    FValueLabelLabel: TLabel;
    function  GetEpiControl: TEpiCustomControlItem;
    function  GetExtendedBounds: TRect;
    procedure OnFieldChange(Const Sender, Initiator: TEpiCustomBase;
      EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
    procedure OnValueLabelSetChange(Const Sender, Initiator: TEpiCustomBase;
      EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
    procedure OnProjectSettingsChange(Const Sender, Initiator: TEpiCustomBase;
      EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
    procedure SetEpiControl(const AValue: TEpiCustomControlItem);
    procedure SetExtendedBounds(Const AValue: TRect);
    procedure UpdateHint;
    procedure UpdateEpiControl;
    procedure UpdateValueLabelConnection(Const OldVLSet, NewVLSet: TEpiValueLabelSet);
    procedure ReadField(Stream: TStream);
    procedure WriteField(Stream: TStream);
    procedure DoShowHint(Const Msg: String);
    procedure HintDeactivate(Sender: TObject);
  protected
    procedure SetParent(NewParent: TWinControl); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoFixupCopyControl;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   UpdateControl;
    function    DesignFrameClass: TCustomFrameClass;
    procedure   SetBounds(ALeft, ATop, AWidth, AHeight: integer); override;
    procedure   FixupCopyControl;
    property    EpiControl: TEpiCustomControlItem read GetEpiControl write SetEpiControl;
    property    ExtendedBounds: TRect read GetExtendedBounds write SetExtendedBounds;
    property    NameLabel: TLabel read FNameLabel;
    property    QuestionLabel: TLabel read FQuestionLabel;
    property    OnShowHint: TDesignFrameShowHintEvent read FOnShowHint write FOnShowHint;
  end;


implementation

uses
  managerprocs, Graphics, main, LCLIntf, LCLType, manager_messages,
  design_properties_fieldframe, JvDesignSurface, epidocument,
  epistringutils, manager_globals, settings2_var, math,
  CustomTimer;


function Surface(Self: TControl): TJvDesignSurface;
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


{ TDesignField }

procedure TDesignField.ReadField(Stream: TStream);
var
  CopyField: TEpiField;
  RefMap: TEpiReferenceMap;
begin
  Stream.Read(CopyField, SizeOf(Pointer));

  RefMap := TEpiReferenceMap.Create;
  FField := TEpiField(CopyField.Clone(nil, RefMap));
  RefMap.FixupReferences;
  RefMap.Free;
end;

procedure TDesignField.WriteField(Stream: TStream);
var
  CopyField: TEpiField;
  RefMap: TEpiReferenceMap;
begin
  RefMap := TEpiReferenceMap.Create;
  CopyField := TEpiField(FField.Clone(nil, RefMap));
  RefMap.FixupReferences;
  RefMap.Free;

  // Wipe data content on copy!
  CopyField.ResetData;

  GlobalCopyList.Add(CopyField);
  Stream.Write(CopyField, Sizeof(Pointer));
end;

procedure TDesignField.DoShowHint(const Msg: String);
var
  H: THintWindow;
  R: types.TRect;
  P: TPoint;
  T: TCustomTimer;
begin
  T := TCustomTimer.Create(self);
  T.Enabled := false;
  T.Interval := 3500;
  T.OnTimer := @HintDeactivate;

  H := THintWindow.Create(T);
  H.HideInterval := 3000;
  H.AutoHide := true;

  R := H.CalcHintRect(0, Msg, nil);
  P := Self.ClientToScreen(Point(Self.Width + 2, 0));
  OffsetRect(R, P.X, P.Y);
  H.ActivateHint(R, Msg);

  T.Enabled := true;
end;

procedure TDesignField.HintDeactivate(Sender: TObject);
begin
  Sender.Free;
end;

function TDesignField.GetEpiControl: TEpiCustomControlItem;
begin
  Result := FField;
end;

function TDesignField.GetExtendedBounds: TRect;
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
    if FValueLabelLabel.Caption <> '' then
      Right := FValueLabelLabel.Left + FValueLabelLabel.Width;

    // TOP
    Top := Self.Top;

    // BOTTOM
    Bottom := Self.Top + Self.Height;
  end;
end;

procedure TDesignField.OnFieldChange(const Sender, Initiator: TEpiCustomBase;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
var
  S: String;
begin
  if EventGroup = eegFields then
    case TEpiFieldsChangeEventType(EventType) of
      efceSetDecimal,
      efceSetLength:
        begin
          SetBounds(Left, Top, 0, 0);
          Surface(Self).UpdateDesigner;
        end;

      efceSetLeft,
      efceSetTop: ;

      efceEntryMode,
      efceShowValueLabel:
        UpdateControl;

      efceValueLabelSet:
        begin
          UpdateValueLabelConnection(TEpiValueLabelSet(Data), FField.ValueLabelSet);
          UpdateControl;
        end;
    end;

  if EventGroup = eegCustomBase then
    case TEpiCustomChangeEventType(EventType) of
      ecceDestroy:
        begin
          if Initiator <> FField then exit;
          UpdateValueLabelConnection(FField.ValueLabelSet, nil);
          FField.UnRegisterOnChangeHook(@OnFieldChange);
          FField := nil;
        end;
      ecceUpdate: ;
      ecceName:
        UpdateControl;
      ecceAddItem: ;
      ecceDelItem:
        begin
          if (Initiator = FField.Relates) and
             // The FField.Relates is destroyed each and every time the content is
             // applied in Field Properties Frame!
             (not (ebsDestroying in Initiator.State)) and
             // If the component itself is being destroyed, sending a hint
             // may cause an A/V due to an Async call for the hint form.
             (not (csDestroying in ComponentState))
          then
          begin
            S := 'A relate jump was removed because a childform was deleted!';
            DoShowHint(S);
          end;
        end;
      ecceSetItem: ;
      ecceSetTop: ;
      ecceSetLeft: ;
      ecceText:
        UpdateControl;
      ecceReferenceDestroyed:
        begin
          // If the component itself is being destroyed, sending a hint
          // may cause an A/V due to an Async call for the hint form.
          if (csDestroying in ComponentState) then exit;

          S := '';

          if Initiator is TEpiJump then
            S := 'A Jump-to field was deleted: ' + TEpiField(Data).Name;

          if Initiator is TEpiComparison then
            S := 'Compare to field was deleted: ' + TEpiField(Data).Name;

          if Initiator is TEpiCalculation then
            S := 'A calculation field was deleted: ' + TEpiField(Data).Name;

          if S <> '' then
            DoShowHint(S);
        end;
    end;
end;

procedure TDesignField.OnValueLabelSetChange(const Sender,
  Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
var
  MaxLen: LongInt;
begin
  if (csDestroying in ComponentState) then exit;
  if (Initiator <> FField.ValueLabelSet) then exit;

  if (EventGroup = eegValueLabelSet) then
  case TEpiValueLabelSetChangeEvent(EventType) of
    evlsMaxValueLength:
      begin
        // Show a hint if valuelabel does not fit anymore and change valuelabel to NIL.
        MaxLen := FField.ValueLabelSet.MaxValueLength;

        if (MaxLen <= FField.Length) then Exit;

        DoShowHint('Warning: Valuelabel set is no longer compatible with length of field!');

        UpdateValueLabelConnection(FField.ValueLabelSet, nil);
        FField.ValueLabelSet := nil;
      end;
  end;

  UpdateControl;
end;

procedure TDesignField.OnProjectSettingsChange(const Sender,
  Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
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

procedure TDesignField.SetEpiControl(const AValue: TEpiCustomControlItem);
begin
  FField := TEpiField(AValue);
  FField.RegisterOnChangeHook(@OnFieldChange);
  FProjectSettings := TEpiDocument(FField.RootOwner).ProjectSettings;
  FProjectSettings.RegisterOnChangeHook(@OnProjectSettingsChange);

  FField.AddCustomData(DesignControlCustomDataKey, Self);

  UpdateValueLabelConnection(nil, FField.ValueLabelSet);

  UpdateEpiControl;
  UpdateControl;
end;

procedure TDesignField.SetExtendedBounds(const AValue: TRect);
var
  lRect: TRect;
begin
  // Copy inital TOP, LEFT
  lRect := AValue;

  // LEFT
  if FQuestionLabel.Caption <> '' then
    lRect.Left := (Left - FQuestionLabel.Left) + AValue.Left;
  if FNameLabel.Caption <> '' then
    lRect.Left := (Left - FNameLabel.Left) + AValue.Left;

  // RIGHT, BOT.
  // - not needed -> is controlled by overridden SetBounds..
  BoundsRect := lRect;
end;

procedure TDesignField.UpdateHint;
begin

end;

procedure TDesignField.UpdateEpiControl;
begin
  if not Assigned(FField) then exit;

  with FField do
  begin
    BeginUpdate;
    Left := Self.Left;
    Top := Self.Top;
    EndUpdate;
  end;
end;

procedure TDesignField.UpdateValueLabelConnection(const OldVLSet,
  NewVLSet: TEpiValueLabelSet);
begin
  if Assigned(OldVLSet) then
    OldVLSet.UnRegisterOnChangeHook(@OnValueLabelSetChange);

  if Assigned(NewVLSet) then
    FField.ValueLabelSet.RegisterOnChangeHook(@OnValueLabelSetChange);
end;

procedure TDesignField.UpdateControl;
begin
  FNameLabel.Font.Assign(ManagerSettings.FieldFont);
  FQuestionLabel.Font.Assign(ManagerSettings.FieldFont);
  Font.Assign(ManagerSettings.FieldFont);

  SetBounds(Left, Top, 0, 0);

  // Change caption, since Visible does not work when csDesigning.
  if FProjectSettings.ShowFieldNames then
    FNameLabel.Caption := FField.Name
  else
    FNameLabel.Caption := '';

  FQuestionLabel.Caption := FField.Question.Text;
  if Assigned(FField.ValueLabelSet) and
     (FField.ShowValueLabel)
  then
    FValueLabelLabel.Caption := FField.ValueLabelSet.Name
  else
    FValueLabelLabel.Caption := '';

  if not FProjectSettings.ShowFieldBorders then
    BorderStyle := bsNone
  else
    BorderStyle := bsSingle;

  if FField.EntryMode = emMustEnter then
    Color := clRed
  else
    Color := clDefault;
end;

procedure TDesignField.SetParent(NewParent: TWinControl);
var
  Fixup: Boolean;
begin
  Fixup := false;
  if (Parent = nil) and
     (NewParent <> nil) and
     (FField <> nil)
  then
    Fixup := true;

  inherited SetParent(NewParent);
  if [csDestroying{, csLoading}] * ComponentState <> [] then exit;

  FQuestionLabel.Parent := NewParent;
  FNameLabel.Parent := NewParent;
  FValueLabelLabel.Parent := NewParent;

  if Fixup then
    DoFixupCopyControl;
end;

procedure TDesignField.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('EpiField', @ReadField, @WriteField, Assigned(FField));
end;

procedure TDesignField.DoFixupCopyControl;
var
  Section: TEpiSection;
begin
  Section := TEpiSection((Parent as IDesignEpiControl).EpiControl);
  if not Section.Fields.ValidateRename(FField.Name, false) then
    FField.Name := Section.Fields.GetUniqueItemName(TEpiField);

  if Section.DataFile <> FField.DataFile then
    { This is a cross-datafile copy, hence all field refering object should
      be removed   }
  begin
    FField.ValueLabelWriteField := nil;

    if Assigned(FField.Jumps) then
    begin
      FField.Jumps.Free;
      FField.Jumps := nil;
    end;

    if Assigned(FField.Calculation) then
    begin
      FField.Calculation.Free;
      FField.Calculation := nil;
    end;

    if Assigned(FField.Comparison) then
    begin
      FField.Comparison.Free;
      FField.Comparison := nil;
    end;

    if Assigned(FField.Relates) then
    begin
      FField.Relates.Free;
      FField.Relates := nil;
    end;

    DoShowHint('Field was pasted to a different dataform' + LineEnding +
               'Jumps, Calculations, Comparisons and Relates have been reset!');
  end;
  Section.Fields.AddItem(FField);

  SetEpiControl(FField);
end;

constructor TDesignField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name := GetRandomComponentName;
  FQuestionLabel := TLabel.Create(Self);
  FQuestionLabel.Anchors := [];
  FQuestionLabel.AnchorToNeighbour(akRight, 5, Self);
  FQuestionLabel.AnchorParallel(akBottom, 0, Self);
  FQuestionLabel.ParentFont := false;
  FQuestionLabel.ControlStyle := FQuestionLabel.ControlStyle + [csNoDesignSelectable];
  FQuestionLabel.SetSubComponent(true);
  FNameLabel := TLabel.Create(Self);
  FNameLabel.Anchors := [];
  FNameLabel.AnchorToNeighbour(akRight, 5, FQuestionLabel);
  FNameLabel.AnchorParallel(akBottom, 0, FQuestionLabel);
  FNameLabel.ParentFont := false;
  FNameLabel.ControlStyle := FNameLabel.ControlStyle + [csNoDesignSelectable];
  FNameLabel.SetSubComponent(true);
  FValueLabelLabel := TLabel.Create(Self);
  FValueLabelLabel.Anchors := [];
  FValueLabelLabel.AnchorToNeighbour(akLeft, 10, Self);
  FValueLabelLabel.AnchorParallel(akBottom, 0, Self);
  FValueLabelLabel.Font.Color := clLime;
  FValueLabelLabel.ControlStyle := FValueLabelLabel.ControlStyle + [csNoDesignSelectable];
  FValueLabelLabel.SetSubComponent(true);

  // Standard properties being set for the component.
  AutoSize := false;
  ReadOnly := true;
  Align := alNone;
  ShowHint := true;
  ParentColor := false;
  ParentFont := false;
  Text := '';
end;

destructor TDesignField.Destroy;
begin
  if Assigned(FField) then
    begin
      UpdateValueLabelConnection(FField.ValueLabelSet, nil);
      FProjectSettings.UnRegisterOnChangeHook(@OnProjectSettingsChange);
      FField.UnRegisterOnChangeHook(@OnFieldChange);
      FField.Free;
    end;
  FNameLabel.Anchors       := [];
  FQuestionLabel.Anchors   := [];
  FValueLabelLabel.Anchors := [];
  inherited Destroy;
end;

function TDesignField.DesignFrameClass: TCustomFrameClass;
begin
  result := TFieldPropertiesFrame;
end;

procedure TDesignField.SetBounds(ALeft, ATop, AWidth, AHeight: integer);
var
  S: Char;
  SideBuf: Integer;
  Cv: TCanvas;
begin
  if (Parent = nil) or
     (FField = nil) or
     (GetParentForm(Self) = nil)
  then
  begin
    inherited SetBounds(ALeft, ATop, AWidth, AHeight);
    Exit;
  end;

  // Calculate Width
  Cv := GetParentForm(Self).Canvas;
  case FField.FieldType of
    ftString,
    ftUpperString: S := 'W';
  else
    S := '4';
  end;
  case BorderStyle of
    bsNone:   SideBuf := {$IFDEF DARWIN}    6 {$ELSE} 0 {$ENDIF};
    bsSingle: SideBuf := {$IFDEF MSWINDOWS} 7 {$ELSE} 6 {$ENDIF};
  end;
  //         Side buffer (pixel from controls left side to first character.
  AWidth   := (SideBuf * 2) + Cv.GetTextWidth(S) * Min(FField.Length, 50);

  // Inherited to set the correct bounds
  inherited SetBounds(ALeft, ATop, AWidth, GetControlClassDefaultSize.cy);

  // Update the EpiControl with values.
  UpdateEpiControl;

  // Update DesignerSurface selection.
  // TC (2015-01-26): No! This forces a cascade of AutoSizing call each time controls are
  // moved! If many controls are selected and moved, this has a LARGE impact on visual
  // performance!
  //Surface.UpdateDesigner;
end;

procedure TDesignField.FixupCopyControl;
begin
  DoFixupCopyControl;
end;

initialization
  RegisterClasses([TDesignField]);

end.

