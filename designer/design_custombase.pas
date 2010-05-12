unit design_custombase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, forms, epicustombase, controls, AVL_Tree;

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
  function XTreeSort(Item1, Item2: Pointer): Integer;
  function YTreeSort(Item1, Item2: Pointer): Integer;
  function WriteTree(Tree: TAVLTree): string;

  procedure AddToPositionHandler(PositionHandler: IPositionHandler;
    Ctrl: TControl);
  procedure RemoveFromPositionHandler(PositionHandler: IPositionHandler;
    Ctrl: TControl);


type
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

function XTreeSort(Item1, Item2: Pointer): Integer;
var
  Ctrl1: TControl absolute Item1;
  Ctrl2: TControl absolute Item2;
  EpiCtrl1: TEpiCustomControlItem;
  EpiCtrl2: TEpiCustomControlItem;
begin
  if Item1 = Item2 then exit(0);

  EpiCtrl1 := (Ctrl1 as IDesignEpiControl).EpiControl;
  EpiCtrl2 := (Ctrl2 as IDesignEpiControl).EpiControl;

  if EpiCtrl1.Left > EpiCtrl2.Left then
    result := 1
  else if EpiCtrl1.Left < EpiCtrl2.Left then
    result := -1
  else
    if EpiCtrl1.Top > EpiCtrl2.Top then
      result := 1
    else if EpiCtrl1.Top < EpiCtrl2.Top then
      result := -1
    else
      result := 0;
end;

function YTreeSort(Item1, Item2: Pointer): Integer;
var
  Ctrl1: TControl absolute Item1;
  Ctrl2: TControl absolute Item2;
  EpiCtrl1: TEpiCustomControlItem;
  EpiCtrl2: TEpiCustomControlItem;
begin
  if Item1 = Item2 then exit(0);

  EpiCtrl1 := (Ctrl1 as IDesignEpiControl).EpiControl;
  EpiCtrl2 := (Ctrl2 as IDesignEpiControl).EpiControl;

  if EpiCtrl1.Top > EpiCtrl2.Top then
    result := 1
  else if EpiCtrl1.Top < EpiCtrl2.Top then
    result := -1
  else
    if EpiCtrl1.Left > EpiCtrl2.Left then
      result := 1
    else if EpiCtrl1.Left < EpiCtrl2.Left then
      result := -1
    else
      result := 0;
end;

function WriteTree(Tree: TAVLTree): string;

  function WriteTreeNode(ANode: TAVLTreeNode; const Prefix: string): string;
  begin
    if ANode=nil then exit('');
    Result := WriteTreeNode(ANode.Right, Prefix + '  ');
    Result += Prefix +
      Format(
      'Node: %s (%d, %d)',
      [TControl(aNode.Data).Name,
       TControl(aNode.Data).Left,
       TControl(aNode.Data).Top]
      ) + LineEnding;
    Result += WriteTreeNode(ANode.Left,  Prefix + '  ');
  end;

  begin
  result := WriteTreeNode(Tree.Root, '');
end;

procedure AddToPositionHandler(PositionHandler: IPositionHandler; Ctrl: TControl
  );
begin
  (Ctrl as IDesignEpiControl).XTreeNode := PositionHandler.XTree.Add(Ctrl);
  (Ctrl as IDesignEpiControl).YTreeNode := PositionHandler.YTree.Add(Ctrl);
end;

procedure RemoveFromPositionHandler(PositionHandler: IPositionHandler;
  Ctrl: TControl);
begin
  PositionHandler.XTree.Remove(Ctrl);
  PositionHandler.YTree.Remove(Ctrl);
  (Ctrl as IDesignEpiControl).XTreeNode := nil;
  (Ctrl as IDesignEpiControl).YTreeNode := nil;
end;

function EpiTextToControlText(const Str: string): string;
begin
  result := StringReplace(Str, '&', '&&', [rfReplaceAll]);
end;

function ControlTextToEpiText(const Str: string): string;
begin
  result := StringReplace(Str, '&&', '&', [rfReplaceAll]);
end;

end.

