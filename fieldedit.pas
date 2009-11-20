unit FieldEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UEpiDataFile, StdCtrls, Controls;

type

  { TFieldEdit }

  TFieldEdit = class(TEdit)
  private
    FField: TEpiField;
    FVarLabel: TLabel;
  protected

  public
    constructor Create(AField: TEpiField; AOwner: TComponent);
    destructor Destroy; override;
    property Field: TEpiField read FField write FField;
    property VariableLabel: TLabel read FVarLabel write FVarLabel;
  published
    property OnStartDock;
    property OnEndDock;
  end;

  { TFieldDockObject }

  TFieldDockObject = class(TDragDockObject)
  private
     OldFieldRec: TRect;
     FieldRec: TRect;
     procedure ShowDockImage; override;
     procedure MoveDockImage; override;
     procedure HideDockImage; override;
  public
    constructor Create(AControl: TControl); override;
    destructor Destroy; override;
  end;



  { TFieldLabel }

  TFieldLabel = class(TLabel)
  private
    FField: TEpiField;
  public
    constructor Create(AField: TEpiField; AOwner: TComponent);
    destructor Destroy; override;
    property Field: TEpiField read FField write FField;
  published
    property OnStartDock;
    property OnEndDock;
  end;

implementation

uses
  InterfaceBase, LCLType, Math;

{ TFieldEdit }

constructor TFieldEdit.Create(AField: TEpiField; AOwner: TComponent);
begin
  inherited Create(AOwner);
  FField := AField;
  FVarLabel := TLabel.Create(Self);
end;

destructor TFieldEdit.Destroy;
begin
  inherited Destroy;
  FField := nil;
  // Do not destroy - it's handled byt the visual destruction of the frame.
  FVarLabel := nil;
end;

{ TFieldLabel }

constructor TFieldLabel.Create(AField: TEpiField; AOwner: TComponent);
begin
  inherited Create(AOwner);
  FField := AField;
end;

destructor TFieldLabel.Destroy;
begin
  inherited Destroy;
  FField := nil;
end;

{ TFieldDockObject }

procedure TFieldDockObject.ShowDockImage;
begin
  Inherited ShowDockImage;
  WidgetSet.DrawDefaultDockImage(OldFieldRec, FieldRec, disShow);
  OldFieldRec := FieldRec;
end;

procedure TFieldDockObject.MoveDockImage;
begin
  inherited MoveDockImage;

  With TFieldEdit(Control) do
  begin
    FieldRec.Left := Min(Left, VariableLabel.Left);
    FieldRec.Top := Min(Top, VariableLabel.Top);
    FieldRec.Right := Max(
      Left + Width,
      VariableLabel.Left + VariableLabel.Width);
    FieldRec.Bottom := Max(
      Top + Height,
      VariableLabel.Top + VariableLabel.Height);
  end;

  //Draw the form outlines when the position has changed
  if not CompareMem(@FieldRec, @OldFieldRec, SizeOf(TRect)) then
  begin
    WidgetSet.DrawDefaultDockImage(OldFieldRec, FieldRec, disMove);
    OldFieldRec := FieldRec;
  end;
end;

procedure TFieldDockObject.HideDockImage;
begin
  inherited HideDockImage;

  WidgetSet.DrawDefaultDockImage(OldFieldRec, FieldRec, disHide);
  OldFieldRec := Rect(MaxInt, 0, MaxInt, 0);
end;

constructor TFieldDockObject.Create(AControl: TControl);
begin
  inherited Create(AControl);
end;

destructor TFieldDockObject.Destroy;
begin
  inherited Destroy;
end;

end.

