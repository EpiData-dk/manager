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
    Img1: TImage;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Bevel5: TBevel;
    Bevel6: TBevel;
    Bevel8: TBevel;
    Bevel9: TBevel;
    Img2: TImage;
    Splitter1: TSplitter;
    procedure IncludeCBChange(Sender: TObject);
  private
    FControlList: TStringList;
    FManagerSettings: PManagerSettings;
    procedure MoveUpClick(Sender: TObject);
    procedure MoveDownClick(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetSettings(Data: PManagerSettings);
    function ApplySettings: boolean;
  end;

implementation

{$R *.lfm}

uses
  epiv_datamodule,
  epiv_custom_statusbar, epiv_statusbar_item_recordcount, epiv_statusbar_item_cycleno,
  epiv_statusbar_item_currentuser, epiv_statusbar_item_savetime,
  epiv_statusbar_item_selectionnames, epiv_statusbar_item_progressbar,
  epiv_statusbar_item_saveicon;

type
  TControlsRec = record
    ExItem: TEpiVCustomStatusBarItem;
    CB:     TCheckBoxThemed;
    UpBtn:  TSpeedButton;
    DnBtn:  TSpeedButton;
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

procedure TSettingsStatusbar.MoveUpClick(Sender: TObject);
var
  Idx, TmpTag: PtrInt;
  MyRec, UpRec, DownRec: PControlsRec;
  Ctrl: TControl;
begin
  Idx := TSpeedButton(Sender).Tag;

  // Cannot move top element further up
  if Idx = 0 then exit;

  MyRec := PControlsRec(FControlList.Objects[Idx]);
  UpRec := PControlsRec(FControlList.Objects[Idx-1]);

  if (Idx < (FControlList.Count-1)) then
    DownRec := PControlsRec(FControlList.Objects[Idx+1])
  else
    DownRec := nil;


  TmpTag := MyRec^.UpBtn.Tag;
  MyRec^.UpBtn.Tag := UpRec^.UpBtn.Tag;
  MyRec^.DnBtn.Tag := UpRec^.DnBtn.Tag;
  UpRec^.UpBtn.Tag := TmpTag;
  UpRec^.DnBtn.Tag := TmpTag;

  Ctrl := UpRec^.ExItem.Panel.AnchorSideTop.Control;

  BeginAutoSizing;
  UpRec^.ExItem.Panel.AnchorSideTop.Control := nil;
  MyRec^.ExItem.Panel.AnchorSideTop.Control := Ctrl;
  UpRec^.ExItem.Panel.AnchorSideTop.Control := MyRec^.ExItem.Panel;

  if Assigned(DownRec) then
    DownRec^.ExItem.Panel.AnchorSideTop.Control := UpRec^.ExItem.Panel;

  EndAutoSizing;
  FControlList.Exchange(Idx - 1, Idx);
end;

procedure TSettingsStatusbar.MoveDownClick(Sender: TObject);
var
  Idx, TmpTag: PtrInt;
  MyRec, DownRec, DownDownRec: PControlsRec;
begin
  Idx := TSpeedButton(Sender).Tag;

  // Cannot move top element further up
  if Idx = (FControlList.Count - 1) then exit;

  MyRec := PControlsRec(FControlList.Objects[Idx]);
  DownRec := PControlsRec(FControlList.Objects[Idx+1]);

  if (Idx < (FControlList.Count - 2)) then
    DownDownRec := PControlsRec(FControlList.Objects[Idx+2])
  else
    DownDownRec := nil;

  TmpTag := MyRec^.UpBtn.Tag;
  MyRec^.UpBtn.Tag := DownRec^.UpBtn.Tag;
  MyRec^.DnBtn.Tag := DownRec^.DnBtn.Tag;
  DownRec^.UpBtn.Tag := TmpTag;
  DownRec^.DnBtn.Tag := TmpTag;

  BeginAutoSizing;
  DownRec^.ExItem.Panel.AnchorSideTop.Control := MyRec^.ExItem.Panel.AnchorSideTop.Control;
  MyRec^.ExItem.Panel.AnchorSideTop.Control := DownRec^.ExItem.Panel;
  if Assigned(DownDownRec) then
    DownDownRec^.ExItem.Panel.AnchorSideTop.Control := MyRec^.ExItem.Panel;

  EndAutoSizing;
  FControlList.Exchange(Idx, Idx + 1);
end;
constructor TSettingsStatusbar.Create(TheOwner: TComponent);
var
  CSBClass: TEpiVCustomStatusBarItemClass;
  i: Integer;
  CB: TCheckBoxThemed;
  ExItem: TEpiVCustomStatusBarItem;
  ItemCaption: TLabel;
  Rec: PControlsRec;
  MoveUpBtn, MoveDownBtn: TSpeedButton;
begin
  inherited Create(TheOwner);

//  ScrollBox1.BorderStyle := bsNone;
  FControlList := TStringList.Create;

  for i := 0 to EpiV_GetCustomStatusBarItems.Count - 1 do
  begin
    CSBClass := TEpiVCustomStatusBarItemClass(EpiV_GetCustomStatusBarItems.Objects[i]);

    ExItem := CSBClass.Create(nil);
    ExItem.Panel.AnchorToNeighbour(akLeft, 5, Bevel2);
    ExItem.Panel.AnchorToNeighbour(akRight, 5, Bevel3);
    ExItem.Panel.AnchorToNeighbour(akTop, 5, Bevel5);
    ExItem.Panel.Height := 26;
    ExItem.Panel.Parent := ScrollBox1;
    ExItem.Update(sucExample);

    CB := TCheckBoxThemed.Create(Self);
    CB.Caption := '';
    CB.AnchorToCompanion(akRight, 5, Bevel1);
    CB.AnchorVerticalCenterTo(ExItem.Panel);
    CB.AutoSize := true;
    CB.Parent := ScrollBox1;

    ItemCaption := TLabel.Create(Self);
    ItemCaption.AnchorToNeighbour(akLeft, 5, Bevel1);
    ItemCaption.AnchorToNeighbour(akRight, 5, Bevel2);
    ItemCaption.AnchorVerticalCenterTo(ExItem.Panel);
    ItemCaption.AutoSize := false;
    ItemCaption.Parent := ScrollBox1;
    ItemCaption.Caption := CSBClass.Caption;

    MoveUpBtn := TSpeedButton.Create(Self);
    MoveUpBtn.ShowCaption := false;
    MoveUpBtn.AnchorVerticalCenterTo(ExItem.Panel);
    MoveUpBtn.AnchorParallel(akLeft, 0, Img1);
    MoveUpBtn.AnchorParallel(akRight, 0, Img1);
    MoveUpBtn.Height := 24;
    MoveUpBtn.Parent := ScrollBox1;
    MoveUpBtn.OnClick := @MoveUpClick;
    MoveUpBtn.Tag := i;
    DM.Icons16.GetBitmap(35, MoveUpBtn.Glyph);

    MoveDownBtn := TSpeedButton.Create(Self);
    MoveDownBtn.ShowCaption := false;
    MoveDownBtn.AnchorVerticalCenterTo(ExItem.Panel);
    MoveDownBtn.AnchorParallel(akLeft, 0, Img2);
    MoveDownBtn.AnchorParallel(akRight, 0, Img2);
    MoveDownBtn.Height := 24;
    MoveDownBtn.Parent := ScrollBox1;
    MoveDownBtn.OnClick := @MoveDownClick;
    MoveDownBtn.Tag := i;
    DM.Icons16.GetBitmap(36, MoveDownBtn.Glyph);

    Rec := New(PControlsRec);
    Rec^.ExItem := ExItem;
    Rec^.CB     := CB;
    Rec^.UpBtn  := MoveUpBtn;
    Rec^.DnBtn  := MoveDownBtn;
    FControlList.AddObject(CSBClass.Name, TObject(Rec));
  end;

  DM.Icons16.GetBitmap(35, Img1.Picture.Bitmap);
  DM.Icons16.GetBitmap(36, Img2.Picture.Bitmap);
end;

destructor TSettingsStatusbar.Destroy;
var
  i: Integer;
begin
  for i := 0 to FControlList.Count - 1 do
    PControlsRec(FControlList.Objects[i])^.ExItem.Free;

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
    ExItem := PControlsRec(FControlList.Objects[i])^.ExItem;
    CB     := PControlsRec(FControlList.Objects[i])^.CB;

    if CB.Checked then
      S := S + ExItem.Name + ',';

  end;
  Delete(S, Length(S), 1);

  FManagerSettings^.StatusBarItemNames := S;
end;

procedure TSettingsStatusbar.SetSettings(Data: PManagerSettings);
var
  L: TStringList;
  CB: TCheckBoxThemed;
  i, Idx: Integer;
  PrevCtrl: TControl;
  Rec, ERec: PControlsRec;
begin
  FManagerSettings := Data;

  L := TStringList.Create;
  L.StrictDelimiter := true;
  L.CommaText := FManagerSettings^.StatusBarItemNames;

  I := 0;
  PrevCtrl := Bevel5;

  while (I < L.Count) do
  begin
    if (I >= FControlList.Count) then break;

    Idx := FControlList.IndexOf(L[I]);
    if (Idx < 0) then exit;

    Rec := PControlsRec(FControlList.Objects[Idx]);
    ERec := PControlsRec(FControlList.Objects[I]);

    Rec^.ExItem.Panel.AnchorSideTop.Control := PrevCtrl;
    Rec^.CB.Checked := true;
    Rec^.UpBtn.Tag  := I;
    Rec^.DnBtn.Tag  := I;

    ERec^.UpBtn.Tag  := Idx;
    ERec^.DnBtn.Tag  := Idx;

    PrevCtrl := Rec^.ExItem.Panel;

    FControlList.Exchange(I, Idx);
    Inc(I);
  end;

  while (I < FControlList.Count) do
  begin
    Rec := PControlsRec(FControlList.Objects[I]);
    Rec^.ExItem.Panel.AnchorSideTop.Control := PrevCtrl;
    Rec^.CB.Checked := False;
    Rec^.UpBtn.Tag  := I;
    Rec^.DnBtn.Tag  := I;

    PrevCtrl := Rec^.ExItem.Panel;
    Inc(I);
  end;
end;

end.

