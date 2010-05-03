unit design_custombase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, forms, epicustombase, controls;

type

  { IDesignEpiControl }

  IDesignEpiControl = interface ['{D816F23A-0CC6-418A-8A6F-B1D28FC42E52}']
    function GetEpiControl: TEpiCustomControlItem;
    procedure SetEpiControl(const AValue: TEpiCustomControlItem);
    property EpiControl: TEpiCustomControlItem read GetEpiControl write SetEpiControl;
  end;

  { TDesignCustomForm }

  TDesignCustomForm = class(TForm)
  protected
    function GetEpiControl: TEpiCustomControlItem; virtual; abstract;
    procedure SetEpiControl(const AValue: TEpiCustomControlItem); virtual; abstract;
  public
    property EpiControl: TEpiCustomControlItem read GetEpiControl write SetEpiControl;
  end;

  function EpiTextToControlText(Const Str: string): string;
  function ControlTextToEpiText(Const Str: string): string;


implementation

function EpiTextToControlText(const Str: string): string;
begin
  result := StringReplace(Str, '&', '&&', [rfReplaceAll]);
end;

function ControlTextToEpiText(const Str: string): string;
begin
  result := StringReplace(Str, '&&', '&', [rfReplaceAll]);
end;

end.

