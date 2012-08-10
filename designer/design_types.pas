unit design_types;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, epicustombase, AVL_Tree, Controls;

type
  { IDesignEpiControl }

  IDesignEpiControl = interface ['IDesignEpiControl'] //['{D816F23A-0CC6-418A-8A6F-B1D28FC42E52}']
    function  GetEpiControl: TEpiCustomControlItem;
    procedure SetEpiControl(const AValue: TEpiCustomControlItem);
    property  EpiControl: TEpiCustomControlItem read GetEpiControl write SetEpiControl;
  end;

  TDesignFrameShowHintEvent = procedure(Sender: TObject; Ctrl: TControl; const Msg: string) of object;

implementation

end.

