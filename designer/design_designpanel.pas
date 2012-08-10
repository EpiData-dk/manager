unit design_designpanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JvDesignSurface, design_types, epicustombase,
  epidatafiles;

type
  { TDesignPanel }

  TDesignPanel = class(TJvDesignPanel, IDesignEpiControl)
  private
    FSection: TEpiSection;
    function  GetEpiControl: TEpiCustomControlItem;
    procedure SetEpiControl(const AValue: TEpiCustomControlItem);
  public
    property EpiControl: TEpiCustomControlItem read GetEpiControl write SetEpiControl;
  end;


implementation

{ TDesignPanel }

function TDesignPanel.GetEpiControl: TEpiCustomControlItem;
begin
  result := FSection;
end;

procedure TDesignPanel.SetEpiControl(const AValue: TEpiCustomControlItem);
begin
  FSection := TEpiSection(AValue);
end;

end.

