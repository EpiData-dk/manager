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

  protected

  public
    constructor Create(AField: TEpiField; AOwner: TComponent);
    destructor Destroy; override;
    property Field: TEpiField read FField write FField;
  published

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
end;

end.

