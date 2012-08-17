unit design_designpanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JvDesignSurface, design_types, epicustombase,
  epidatafiles, forms;

type
  { TDesignPanel }

  TDesignPanel = class(TJvDesignPanel, IDesignEpiControl)
  private
    FSection: TEpiSection;
    function  GetEpiControl: TEpiCustomControlItem;
    procedure SetEpiControl(const AValue: TEpiCustomControlItem);
  public
    function DesignFrameClass: TCustomFrameClass;
    property EpiControl: TEpiCustomControlItem read GetEpiControl write SetEpiControl;
  end;


implementation

uses
  design_properties_sectionframe;

{ TDesignPanel }

function TDesignPanel.GetEpiControl: TEpiCustomControlItem;
begin
  result := FSection;
end;

procedure TDesignPanel.SetEpiControl(const AValue: TEpiCustomControlItem);
begin
  FSection := TEpiSection(AValue);
end;

function TDesignPanel.DesignFrameClass: TCustomFrameClass;
begin
  result := TSectionPropertiesFrame;
end;

end.

