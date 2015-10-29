unit settings_statusbar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, CheckBoxThemed, Forms, Controls, ExtCtrls,
  StdCtrls, Buttons, settings2_interface, settings2_var;

type

  { TSettingsStatusbar }

  TSettingsStatusbar = class(TFrame, ISettingsFrame)
    ScrollBox1: TScrollBox;
    Label1: TLabel;
    Label2: TLabel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Bevel5: TBevel;
    Bevel6: TBevel;
    Bevel8: TBevel;
    Bevel9: TBevel;
    Splitter1: TSplitter;
    MainCB: TCheckBoxThemed;
    procedure IncludeCBChange(Sender: TObject);
  private
    FControlList: TList;
    FManagerSettings: PManagerSettings;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetSettings(Data: PManagerSettings);
    function ApplySettings: boolean;
  end;

implementation

{$R *.lfm}

uses
  epiv_custom_statusbar, epiv_statusbar_item_recordcount, epiv_statusbar_item_cycleno,
  epiv_statusbar_item_currentuser, epiv_statusbar_item_savetime,
  epiv_statusbar_item_selectionnames;

type
  TControlsRec = record
    ExItem: TEpiVCustomStatusBarItem;
    CB:     TCheckBoxThemed;
  end;
  PControlsRec = ^TControlsRec;

{ TSettingsStatusbar }

procedure TSettingsStatusbar.IncludeCBChange(Sender: TObject);
begin
  with TCheckBoxThemed(Sender) do
  begin
    if (not Checked) then
      Checked := true;
  end;
end;

constructor TSettingsStatusbar.Create(TheOwner: TComponent);
var
  CSBClass: TEpiVCustomStatusBarItemClass;
  i: Integer;
  CB: TCheckBoxThemed;
  ExItem: TEpiVCustomStatusBarItem;
  PrevPanel: TCustomPanel;
  ItemCaption: TLabel;
  Rec: PControlsRec;
begin
  inherited Create(TheOwner);

//  ScrollBox1.BorderStyle := bsNone;
  FControlList := TList.Create;

  PrevPanel := nil;

  for i := 0 to EpiV_GetCustomStatusBarItems.Count - 1 do
  begin
    CSBClass := TEpiVCustomStatusBarItemClass(EpiV_GetCustomStatusBarItems.Objects[i]);

    ExItem := CSBClass.Create(nil);
    ExItem.Panel.AnchorToNeighbour(akLeft, 5, Bevel2);
    ExItem.Panel.AnchorToNeighbour(akRight, 5, Bevel3);
    if (i = 0) then
      ExItem.Panel.AnchorToNeighbour(akTop, 5, Bevel5)
    else
      ExItem.Panel.AnchorToNeighbour(akTop, 5, PrevPanel);
    ExItem.Panel.Height := 26;
    ExItem.Panel.Parent := ScrollBox1;
    ExItem.Update(sucExample);
    PrevPanel := ExItem.Panel;

    CB := TCheckBoxThemed.Create(Self);
    CB.Caption := '';
    CB.AnchorParallel(akLeft, 0, MainCB);
    CB.AnchorVerticalCenterTo(PrevPanel);
    CB.AutoSize := true;
    CB.Parent := ScrollBox1;

    ItemCaption := TLabel.Create(Self);
    ItemCaption.AnchorToNeighbour(akLeft, 5, Bevel1);
    ItemCaption.AnchorToNeighbour(akRight, 5, Bevel2);
    ItemCaption.AnchorVerticalCenterTo(PrevPanel);
    ItemCaption.AutoSize := false;
    ItemCaption.Parent := ScrollBox1;
    ItemCaption.Caption := CSBClass.Caption;

    Rec := New(PControlsRec);
    Rec^.ExItem := ExItem;
    Rec^.CB     := CB;
    FControlList.Add(Rec);
  end;
end;

destructor TSettingsStatusbar.Destroy;
var
  i: Integer;
begin
  for i := 0 to FControlList.Count - 1 do
    PControlsRec(FControlList[i])^.ExItem.Free;

  inherited Destroy;
end;

function TSettingsStatusbar.ApplySettings: boolean;
var
  S: String;
  i: Integer;
  ExItem: TEpiVCustomStatusBarItem;
  CB: TCheckBoxThemed;
begin
  result:= true;
  S := '';

  for i := 0 to FControlList.Count - 1 do
  begin
    ExItem := PControlsRec(FControlList[i])^.ExItem;
    CB     := PControlsRec(FControlList[i])^.CB;

    if CB.Checked then
      S := S + ExItem.Name + ',';
  end;
  Delete(S, Length(S), 1);

  FManagerSettings^.StatusBarItemNames := S;
end;

procedure TSettingsStatusbar.SetSettings(Data: PManagerSettings);
var
  L: TStringList;
  ExItem: TEpiVCustomStatusBarItem;
  CB: TCheckBoxThemed;
  i: Integer;
begin
  FManagerSettings := Data;

  L := TStringList.Create;
  L.StrictDelimiter := true;
  L.CommaText := FManagerSettings^.StatusBarItemNames;

  for i := 0 to FControlList.Count - 1 do
  begin
    ExItem := PControlsRec(FControlList[i])^.ExItem;
    CB     := PControlsRec(FControlList[i])^.CB;

    CB.Checked := (L.IndexOf(ExItem.Name) >= 0);
  end;
end;

end.

