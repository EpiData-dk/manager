unit design_types;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epicustombase, AVL_Tree, Controls;

type
  { IDesignEpiControl }

  IDesignEpiControl = interface ['{D816F23A-0CC6-418A-8A6F-B1D28FC42E52}']
    function  GetEpiControl: TEpiCustomControlItem;
    function  GetXTreeNode: TAVLTreeNode;
    function  GetYTreeNode: TAVLTreeNode;
    procedure SetEpiControl(const AValue: TEpiCustomControlItem);
    procedure SetXTreeNode(const AValue: TAVLTreeNode);
    procedure SetYTreeNode(const AValue: TAVLTreeNode);
    property  EpiControl: TEpiCustomControlItem read GetEpiControl write SetEpiControl;
    property  XTreeNode: TAVLTreeNode read GetXTreeNode write SetXTreeNode;
    property  YTreeNode: TAVLTreeNode read GetYTreeNode write SetYTreeNode;
  end;

  { IPositionHandler }

  IPositionHandler = interface ['{EE58F27F-C0EB-43E1-BCF2-8525F632F527}']
    function GetXTree: TAVLTree;
    function GetYTree: TAVLTree;
    property XTree: TAVLTree read GetXTree;
    property YTree: TAVLTree read GetYTree;
  end;

  TDesignerControlType = (dctField, dctSection, dctHeading);

  TDesignFrameShowHintEvent = procedure(Sender: TObject; Ctrl: TControl; const Msg: string) of object;


implementation

end.

