unit design_control_field;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epicustombase, epidatafiles, epidatafilestypes,
  Controls, StdCtrls, design_types, Forms, episettings, epivaluelabels;

type
  { TDesignField }
  TDesignField = class(TEdit, IDesignEpiControl)
  private
    FField: TEpiField;
    FWidth: Integer;
    FHeight: Integer;
    FInitialized: boolean;
    FProjectSettings: TEpiProjectSettings;
    FNameLabel: TLabel;
    FQuestionLabel: TLabel;
    FValueLabelLabel: TLabel;
    function  GetEpiControl: TEpiCustomControlItem;
    procedure OnFieldChange(Sender: TObject; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
    procedure OnValueLabelSetChange(Sender: TObject; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
    procedure OnProjectSettingsChange(Sender: TObject; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
    procedure SetEpiControl(const AValue: TEpiCustomControlItem);
    procedure UpdateHint;
    procedure UpdateEpiControl;
    procedure UpdateValueLabelConnection(Const OldVLSet, NewVLSet: TEpiValueLabelSet);
    procedure ReadField(Stream: TStream);
    procedure WriteField(Stream: TStream);
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
    property    NameLabel: TLabel read FNameLabel;
    property    QuestionLabel: TLabel read FQuestionLabel;
  end;


implementation

uses
  managerprocs, Graphics, main, LCLIntf, LCLType, manager_messages,
  design_properties_fieldframe, JvDesignSurface, epidocument,
  epistringutils, manager_globals, settings2_var;

{ TDesignField }

procedure TDesignField.ReadField(Stream: TStream);
var
  CopyField: TEpiField;
begin
  Stream.Read(CopyField, SizeOf(Pointer));
  FField := TEpiField(CopyField.Clone(nil));
  // Manually add ValueLabelSet, this is not done in cloning.
  FField.ValueLabelSet := CopyField.ValueLabelSet;
end;

procedure TDesignField.WriteField(Stream: TStream);
var
  CopyField: TEpiField;
begin
  CopyField := TEpiField(FField.Clone(nil));
  // Manually add ValueLabelSet, this is not done in cloning.
  CopyField.ValueLabelSet := FField.ValueLabelSet;
  GlobalCopyList.Add(CopyField);
  Stream.Write(CopyField, Sizeof(Pointer));
end;

function TDesignField.GetEpiControl: TEpiCustomControlItem;
begin
  Result := FField;
end;

procedure TDesignField.OnFieldChange(Sender: TObject;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  if EventGroup = eegFields then
    case TEpiFieldsChangeEventType(EventType) of
      efceSetDecimal,
      efceSetLength:
        SetBounds(Left, Top, 0, 0);
      efceSetLeft,
      efceSetTop: ;
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
          FProjectSettings.UnRegisterOnChangeHook(@OnProjectSettingsChange);
          FProjectSettings := nil;
          FField.UnRegisterOnChangeHook(@OnFieldChange);
          FField := nil;
        end;
      ecceUpdate: ;
      ecceName:
        FNameLabel.Caption :=  TEpiField(Sender).Name;
      ecceAddItem: ;
      ecceDelItem: ;
      ecceSetItem: ;
      ecceSetTop: ;
      ecceSetLeft: ;
      ecceText:
        FQuestionLabel.Caption := TEpiField(Sender).Question.Text;
    end;
end;

procedure TDesignField.OnValueLabelSetChange(Sender: TObject;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  if (EventGroup = eegCustomBase) and (TEpiCustomChangeEventType(EventType) = ecceDestroy) then exit;

  UpdateControl;
end;

procedure TDesignField.OnProjectSettingsChange(Sender: TObject;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin
  if (EventGroup = eegCustomBase)
  then
    case TEpiCustomChangeEventType(EventType) of
      ecceDestroy:
        exit;
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

  UpdateValueLabelConnection(nil, FField.ValueLabelSet);

  UpdateEpiControl;
  UpdateControl;
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
  Text := '';
end;

destructor TDesignField.Destroy;
begin
  if Assigned(FField) then
    begin
      UpdateValueLabelConnection(FField.ValueLabelSet, nil);
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
  AWidth   := (SideBuf * 2) + Cv.GetTextWidth(S) * FField.Length;

  // Inherited to set the correct bounds
  inherited SetBounds(ALeft, ATop, AWidth, GetControlClassDefaultSize.cy);

  // Update the EpiControl with values.
  UpdateEpiControl;

  // Update DesignerSurface selection.
  Surface.UpdateDesigner;
end;

procedure TDesignField.FixupCopyControl;
begin
  DoFixupCopyControl;
end;

initialization
  RegisterClasses([TDesignField]);

end.

