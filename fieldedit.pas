unit FieldEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UEpiDataFile, StdCtrls;

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
  FVarLabel.Free;
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

end.

