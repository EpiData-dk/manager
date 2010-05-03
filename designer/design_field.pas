unit design_field;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, epidatafiles, epicustombase, design_custombase;

type
  { TDesignField }

  TDesignField = class(TEdit, IDesignEpiControl)
  private
    FField: TEpiField;
    FHeading: TEpiHeading;
    function GetEpiControl: TEpiCustomControlItem;
    procedure SetEpiControl(const AValue: TEpiCustomControlItem);
    procedure OnChange(Sender: TObject; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property    EpiControl: TEpiCustomControlItem read GetEpiControl write SetEpiControl;
  end;

  { TDesignFieldForm }

  TDesignFieldForm = class(TDesignCustomForm)
    BitBtn1: TBitBtn;
  private
    { private declarations }
    FField: TEpiField;
  protected
    procedure SetEpiControl(const AValue: TEpiCustomControlItem); override;
    function GetEpiControl: TEpiCustomControlItem; override;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end; 

implementation

{$R *.lfm}

{ TDesignField }

function TDesignField.GetEpiControl: TEpiCustomControlItem;
begin
  result := FField;
end;

procedure TDesignField.SetEpiControl(const AValue: TEpiCustomControlItem);
begin
  FField := TEpiField(AValue);
  FField.RegisterOnChangeHook(@OnChange);
end;

procedure TDesignField.OnChange(Sender: TObject; EventGroup: TEpiEventGroup;
  EventType: Word; Data: Pointer);
begin
  case EventGroup of
    eegCustomBase:
      case TEpiCustomChangeEventType(EventType) of
        ecceSetLeft: Left := FField.Left;
        ecceSetTop:  Top  := FField.Top;
//        ecceName:    n := FField.Caption.Text;
        ecceUpdate:
          begin
            Left := FField.Left;
            Top  := FField.Top;
//            Caption := FField.Caption.Text;
          end;
      end;
  end;
end;

constructor TDesignField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Standard properties being set for the component.
  DragKind := dkDock;
  DragMode := dmAutomatic;
  AutoSelect := false;
  AutoSize := false;
  ReadOnly := true;
  TabStop := false;
  Color:= clMenuBar;
  Align := alNone;
end;

destructor TDesignField.Destroy;
begin
  inherited Destroy;
end;

{ TDesignFieldForm }

procedure TDesignFieldForm.SetEpiControl(const AValue: TEpiCustomControlItem);
begin
  FField := TEpiField(AValue);
end;

function TDesignFieldForm.GetEpiControl: TEpiCustomControlItem;
begin
  Result := FField;
end;

constructor TDesignFieldForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TDesignFieldForm.Destroy;
begin
  inherited Destroy;
end;

end.

