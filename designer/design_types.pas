unit design_types;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, epicustombase, AVL_Tree, Controls, Forms;

type
  { IDesignEpiControl }

  IDesignEpiControl = interface ['IDesignEpiControl'] //['{D816F23A-0CC6-418A-8A6F-B1D28FC42E52}']
    function DesignFrameClass: TCustomFrameClass;
    function GetEpiControl: TEpiCustomControlItem;
    procedure SetEpiControl(const AValue: TEpiCustomControlItem);
    procedure FixupCopyControl;
    property  EpiControl: TEpiCustomControlItem read GetEpiControl write SetEpiControl;
  end;

  TDesignFrameShowHintEvent = procedure(Sender: TObject; Ctrl: TControl; const Msg: string) of object;

  TEpiCustomControlItemArray = array of TEpiCustomControlItem;

  IDesignPropertiesFrame = interface ['IDesignPropertiesFrame']
    procedure SetEpiControls(EpiControls: TEpiCustomControlItemArray);
    function  ApplyChanges: boolean;
  end;

implementation

end.

