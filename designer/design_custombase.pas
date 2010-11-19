unit design_custombase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, forms, epicustombase, controls, AVL_Tree;


type
  { TDesignCustomForm }

  TDesignCustomForm = class(TForm)
  protected
    function GetEpiControl: TEpiCustomControlItem; virtual; abstract;
    procedure SetEpiControl(const AValue: TEpiCustomControlItem); virtual; abstract;
  public
    property EpiControl: TEpiCustomControlItem read GetEpiControl write SetEpiControl;
  end;

implementation


end.

