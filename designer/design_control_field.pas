unit design_control_field;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epicustombase, epidatafiles, epidatafilestypes,
  Controls, StdCtrls, design_types, Forms, episettings;

type
  { TDesignField }
  TDesignField = class(TEdit, IDesignEpiControl)
  private
    FField: TEpiField;
    FWidth: Integer;
    FHeight: Integer;
    FInitialized: boolean;
    FUpdating: boolean;
    FProjectSettings: TEpiProjectSettings;
    FNameLabel: TLabel;
    FQuestionLabel: TLabel;
    FValueLabelLabel: TLabel;
    function  GetEpiControl: TEpiCustomControlItem;
    procedure OnFieldChange(Sender: TObject; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
    procedure OnProjectSettingsChange(Sender: TObject; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
    procedure SetEpiControl(const AValue: TEpiCustomControlItem);
    procedure UpdateHint;
    procedure UpdateEpiControl;
    procedure UpdateControl;
  protected
    procedure SetParent(NewParent: TWinControl); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function    DesignFrameClass: TCustomFrameClass;
    procedure   SetBounds(ALeft, ATop, AWidth, AHeight: integer); override;
    property    EpiControl: TEpiCustomControlItem read GetEpiControl write SetEpiControl;
    property    NameLabel: TLabel read FNameLabel;
    property    QuestionLabel: TLabel read FQuestionLabel;
  end;


implementation

uses
  managerprocs, Graphics, main, LCLIntf, LCLType, manager_messages,
  design_properties_fieldframe, JvDesignSurface, epidocument;

{ TDesignField }

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
      efceSetTop:
        ;
      efceShowValueLabel: ;
    end;

  if EventGroup= eegCustomBase then
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

  UpdateEpiControl;
  UpdateControl;
end;

procedure TDesignField.UpdateHint;
begin

end;

procedure TDesignField.UpdateEpiControl;
begin
  if not Assigned(FField) then exit;
  if FUpdating then exit;;

  FUpdating := true;
  with FField do
  begin
    BeginUpdate;
    Left := Self.Left;
    Top := Self.Top;
    EndUpdate;
  end;
  FUpdating := false;;
end;

procedure TDesignField.UpdateControl;
begin
  SetBounds(Left, Top, 0, 0);

  // Change caption, since Visible does not work when csDesigning.
  if FProjectSettings.ShowFieldNames then
    FNameLabel.Caption := FField.Name
  else
    FNameLabel.Caption := '';
  FQuestionLabel.Caption := FField.Question.Text;
  if Assigned(FField.ValueLabelSet) then
    FValueLabelLabel.Caption := FField.ValueLabelSet.Name;

  if not FProjectSettings.ShowFieldBorders then
    BorderStyle := bsNone
  else
    BorderStyle := bsSingle;
end;

procedure TDesignField.SetParent(NewParent: TWinControl);
begin
  inherited SetParent(NewParent);
  if csDestroying in ComponentState then exit;

  FQuestionLabel.Parent := NewParent;
  FNameLabel.Parent := NewParent;
  FValueLabelLabel.Parent := NewParent;

  // Trick to utilize Parent when adding an EpiControl, but not postponing
  // adding epicontrol too late (using PostMessage)
  if not Assigned(EpiControl)
  then
    SendMessage(MainForm.Handle, LM_DESIGNER_ADD, WPARAM(Self), LPARAM(TEpiField));
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
  FNameLabel := TLabel.Create(Self);
  FNameLabel.Anchors := [];
  FNameLabel.AnchorToNeighbour(akRight, 5, FQuestionLabel);
  FNameLabel.AnchorParallel(akBottom, 0, FQuestionLabel);
  FNameLabel.ParentFont := false;
  FNameLabel.ControlStyle := FNameLabel.ControlStyle + [csNoDesignSelectable];
  FValueLabelLabel := TLabel.Create(Self);
  FValueLabelLabel.Anchors := [];
  FValueLabelLabel.AnchorToNeighbour(akLeft, 10, Self);
  FValueLabelLabel.AnchorParallel(akBottom, 0, Self);
  FValueLabelLabel.Font.Color := clLime;
  FValueLabelLabel.ControlStyle := FValueLabelLabel.ControlStyle + [csNoDesignSelectable];

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
      FField.UnRegisterOnChangeHook(@OnFieldChange);
      FField.Free;
    end;
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
  if (Parent = nil) or (FField = nil) then
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
  TJvDesignPanel(Owner).Surface.UpdateDesigner;
end;

initialization
  RegisterClasses([TDesignField, TLabel]);

end.

