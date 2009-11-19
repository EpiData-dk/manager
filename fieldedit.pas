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
    FOldWidth: Integer;
    FOldHeight: Integer;
  protected

  public
    constructor Create(AField: TEpiField; AOwner: TComponent);
    destructor Destroy; override;
    property Field: TEpiField read FField write FField;
    property OldWidth: Integer read FOldWidth write FOldWidth;
    property OldHeight: Integer read FOldHeight write FOldHeight;
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
end;

destructor TFieldEdit.Destroy;
begin
  inherited Destroy;
  FField := nil;
end;

end.

