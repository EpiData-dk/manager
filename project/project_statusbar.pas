unit project_statusbar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epiv_custom_statusbar;

type

  { TManagerStatusBar }

  TManagerStatusBar = class(TEpiVCustomStatusBar)
  public
    constructor Create(TheOwner: TComponent); override;
    procedure LoadSettings;
  end;

implementation

uses
  settings2_var, epiv_statusbar_item_recordcount, epiv_statusbar_item_cycleno,
  epiv_statusbar_item_currentuser,{ epiv_statusbar_item_savetime, }
  epiv_statusbar_item_selectionnames;

{ TManagerStatusBar }

constructor TManagerStatusBar.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

procedure TManagerStatusBar.LoadSettings;
var
  i: Integer;
  L: TList;
  Item: TEpiVCustomStatusBarItem;
begin
//  ManagerSettings.;

  L := EpiV_GetCustomStatusBarItems;
  if Assigned(L) then
    for i := 0 to L.Count - 1 do
      AddItem(TEpiVCustomStatusBarItemClass(L[i]).Create(Self));

{  for i := 0 to 3 do
    begin
      Item := TEpiVCustomStatusBarItem.Create(self);
      if (I mod 2) = 1 then
        Item.Resizable := true;
      AddItem(Item);
    end;

  if Assigned(L) then
    for i := 0 to L.Count - 1 do
      AddItem(TEpiVCustomStatusBarItemClass(L[i]).Create(Self));  }
end;

end.

