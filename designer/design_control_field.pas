unit design_control_field;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epicustombase, epidatafiles, epidatafilestypes,
  Controls, StdCtrls, design_types;

type
  { TDesignField }
  TDesignField = class(TEdit, IDesignEpiControl)
  private
    FField: TEpiField;
    FWidth: Integer;
    FHeight: Integer;
    FInitialized: boolean;
    FUpdating: boolean;
  private
    FNameLabel: TLabel;
    FQuestionLabel: TLabel;
    FValueLabelLabel: TLabel;
    function  GetEpiControl: TEpiCustomControlItem;
    procedure OnFieldChange(Sender: TObject; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
    procedure OnQuestionChange(Sender: TObject; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
    procedure OnProjectSettingsChange(Sender: TObject; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
    procedure SetEpiControl(const AValue: TEpiCustomControlItem);
    procedure UpdateHint;
    procedure UpdateEpiControl;
    procedure UpdateControl;
  protected
    procedure SetParent(NewParent: TWinControl); override;
    procedure DoSetBounds(ALeft, ATop, AWidth, AHeight: integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property    EpiControl: TEpiCustomControlItem read GetEpiControl write SetEpiControl;
    property    NameLabel: TLabel read FNameLabel;
    property    QuestionLabel: TLabel read FQuestionLabel;
  end;


implementation

uses
  managerprocs, Graphics, main, LCLIntf, LCLType, manager_messages;

{ TDesignField }

function TDesignField.GetEpiControl: TEpiCustomControlItem;
begin
  Result := FField;
end;

procedure TDesignField.OnFieldChange(Sender: TObject;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin

end;

procedure TDesignField.OnQuestionChange(Sender: TObject;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin

end;

procedure TDesignField.OnProjectSettingsChange(Sender: TObject;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
begin

end;

procedure TDesignField.SetEpiControl(const AValue: TEpiCustomControlItem);
begin
  FField := TEpiField(AValue);
  FNameLabel.Caption := FField.Name;
  UpdateEpiControl;
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

end;

procedure TDesignField.SetParent(NewParent: TWinControl);
begin
  inherited SetParent(NewParent);

  FQuestionLabel.Parent := NewParent;
  FNameLabel.Parent := NewParent;
  FValueLabelLabel.Parent := NewParent;
end;

procedure TDesignField.DoSetBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  if FInitialized then
    inherited DoSetBounds(ALeft, ATop, FWidth, FHeight)
  else
    inherited DoSetBounds(ALeft, ATop, AWidth, AHeight);


  if not FInitialized then
  begin
    FWidth := AWidth;
    FHeight := AHeight;
  end;

  UpdateEpiControl;

  FInitialized := true;
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

  PostMessage(MainForm.Handle, LM_DESIGNER_ADD, WPARAM(Self), LPARAM(TEpiField));
end;

destructor TDesignField.Destroy;
begin
  inherited Destroy;
end;

initialization
  RegisterClasses([TDesignField, TLabel]);

end.

