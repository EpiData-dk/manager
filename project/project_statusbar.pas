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
  settings2_var;

{ TManagerStatusBar }

constructor TManagerStatusBar.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

procedure TManagerStatusBar.LoadSettings;
var
  L: TStrings;
  S: String;
  Idx: Integer;
begin
  Clear;

  L := TStringList.Create;
  L.StrictDelimiter := true;
  L.CommaText := ManagerSettings.StatusBarItemNames;

  for S in L do
  begin
    Idx := EpiV_GetCustomStatusBarItems.IndexOf(S);
    if (Idx < 0) then continue;

    AddItem(TEpiVCustomStatusBarItemClass(EpiV_GetCustomStatusBarItems.Objects[Idx]).Create(Self));
  end;
  Resize;
end;

end.

