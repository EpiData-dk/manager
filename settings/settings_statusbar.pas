unit settings_statusbar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, Buttons,
  settings2_interface, settings2_var;

type

  { TSettingsStatusbar }

  TSettingsStatusbar = class(TFrame, ISettingsFrame)
    Panel1: TPanel;
    ScrollBox1: TScrollBox;
    CheckBox1: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
  private
    FManagerSettings: PManagerSettings;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure SetSettings(Data: PManagerSettings);
    function ApplySettings: boolean;
  end;

implementation

{$R *.lfm}

uses
  epiv_custom_statusbar, epiv_statusbar_item_recordcount, epiv_statusbar_item_cycleno,
  epiv_statusbar_item_currentuser, epiv_statusbar_item_savetime,
  epiv_statusbar_item_selectionnames;

{ TSettingsStatusbar }

constructor TSettingsStatusbar.Create(TheOwner: TComponent);
var
  L: TStringList;
  CSBClass: TEpiVCustomStatusBarItemClass;
  i: Integer;
begin
  inherited Create(TheOwner);

  ScrollBox1.BorderStyle := bsNone;

  for i := 0 to EpiV_GetCustomStatusBarItems.Count - 1 do
  begin

  end;
end;

function TSettingsStatusbar.ApplySettings: boolean;
var
  S: String;
  i: Integer;
begin

  result:= true;
end;

procedure TSettingsStatusbar.SetSettings(Data: PManagerSettings);
begin
  FManagerSettings := Data;

end;

end.

