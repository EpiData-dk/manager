unit checkversionform;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Classes, SysUtils, Forms, StdCtrls, CheckBoxThemed, Buttons;

type

  { TCheckVersionForm }

  TCheckVersionForm = class(TForm)
  private
    CurrentVersionCaption: TLabel;
    CurrentVersionInfo: TLabel;

    PublicVersionCaption: TLabel;
    PublicVersionInfo: TLabel;

    TestVersionCaption: TLabel;
    TestVersionInfo: TLabel;

    CheckVersionOnlineChkBox: TCheckBoxThemed;
    CloseBtn: TBitBtn;
    procedure CloseBtnClick(Sender: TObject);
    procedure DownloadLinkClick(Sender: TObject);
    procedure DownloadLinkMouseEnter(Sender: TObject);
    procedure DownloadLinkMouseLeave(Sender: TObject);
    procedure ShowForm(Sender: TObject);
  private
    procedure CreateControls;
    procedure UpdateVersions;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

implementation

uses
  Controls, Graphics, LCLIntf, ExtCtrls, epiversionutils, Dialogs,
  settings2_var;


const
  DefaultURL = 'http://epidata.dk';
  PublicDownloadURL = 'http://epidata.dk/download.php';
  TestDownloadURL = 'http://epidata.dk/testing.php';

{ TCheckVersionForm }

procedure TCheckVersionForm.CloseBtnClick(Sender: TObject);
begin
  ManagerSettings.CheckForUpdates := CheckVersionOnlineChkBox.Checked;
end;

procedure TCheckVersionForm.DownloadLinkClick(Sender: TObject);
var
  URL: String;
begin
  URL := DefaultURL;

  if Sender = PublicVersionInfo then
    URL := PublicDownloadURL;

  if Sender = TestVersionInfo then
    URL := TestDownloadURL;

  OpenURL(URL);
end;

procedure TCheckVersionForm.DownloadLinkMouseEnter(Sender: TObject);
begin
  Screen.Cursor := crHandPoint;
end;

procedure TCheckVersionForm.DownloadLinkMouseLeave(Sender: TObject);
begin
  Screen.Cursor := crDefault;
end;

procedure TCheckVersionForm.ShowForm(Sender: TObject);
begin
  UpdateVersions;
  CloseBtn.SetFocus;
end;

procedure TCheckVersionForm.CreateControls;
var
  Image: TImage;
begin
  Image := TImage.Create(Self);
  with Image do
  begin
    Width := 64;
    Height := 64;
    AnchorParallel(akTop, 10, Self);
    AnchorParallel(akLeft, 10, Self);
    Picture.Icon := Application.Icon;
    Parent := Self;
  end;

  CurrentVersionCaption := TLabel.Create(Self);
  with CurrentVersionCaption do
  begin
    Caption := 'Current Virsion:';
    AutoSize := true;
    Anchors := [];
    AnchorParallel(akTop,   0, Image);
    AnchorToNeighbour(akLeft, 15, Image);
    Parent := Self
  end;

  CurrentVersionInfo := TLabel.Create(self);
  with CurrentVersionInfo do
  begin
    Caption := '0.0.0.0';
    AutoSize := true;
    Anchors := [];
    AnchorToNeighbour(akLeft, 20, CurrentVersionCaption);
    AnchorParallel(akBottom, 0, CurrentVersionCaption);
    Parent := Self
  end;

  PublicVersionCaption := TLabel.Create(Self);
  with PublicVersionCaption do
  begin
    Caption := 'Public Version:';
    AutoSize := true;
    Anchors := [];
    AnchorToNeighbour(akTop, 10, CurrentVersionCaption);
    AnchorParallel(akLeft, 0, CurrentVersionCaption);
    Parent := Self
  end;

  PublicVersionInfo := TLabel.Create(self);
  with PublicVersionInfo do
  begin
    Font.Color := clBlue;
    Font.Underline := true;
    Caption := '0.0.1.0';
    Hint := 'Go to download page: ' + PublicDownloadURL;
    ShowHint := true;
    AutoSize := true;
    Anchors := [];
    AnchorParallel(akLeft, 0, CurrentVersionInfo);
    AnchorParallel(akBottom, 0, PublicVersionCaption);
    OnClick := @DownloadLinkClick;
    OnMouseEnter := @DownloadLinkMouseEnter;
    OnMouseLeave := @DownloadLinkMouseLeave;
    Parent := Self
  end;

  TestVersionCaption := TLabel.Create(Self);
  with TestVersionCaption do
  begin
    Caption := 'Test Version:';
    AutoSize := true;
    Anchors := [];
    AnchorToNeighbour(akTop, 10, PublicVersionCaption);
    AnchorParallel(akLeft, 0, CurrentVersionCaption);
    Parent := Self
  end;

  TestVersionInfo := TLabel.Create(self);
  with TestVersionInfo do
  begin
    Font.Color := clBlue;
    Font.Underline := true;
    Caption := '0.1.0.0';
    Hint := 'Go to download page: ' + TestDownloadURL;
    ShowHint := true;
    AutoSize := true;
    Anchors := [];
    AnchorParallel(akLeft, 0, CurrentVersionInfo);
    AnchorParallel(akBottom, 0, TestVersionCaption);
    OnClick := @DownloadLinkClick;
    OnMouseEnter := @DownloadLinkMouseEnter;
    OnMouseLeave := @DownloadLinkMouseLeave;
    Parent := Self
  end;

  CheckVersionOnlineChkBox := TCheckBoxThemed.Create(Self);
  with CheckVersionOnlineChkBox do
  begin
    Caption := 'Automatically check for' + LineEnding + 'new version online';
    Hint    := 'Use preferences to set the number of days between checks';
    ShowHint := true;
    AutoSize := true;
    AnchorToNeighbour(akTop, 10, TestVersionCaption);
    AnchorParallel(akLeft, 0, Image);
    Checked := ManagerSettings.CheckForUpdates;
    Parent := Self
  end;

  CloseBtn := TBitBtn.Create(Self);
  with CloseBtn do
  begin
    Kind := bkClose;
    AutoSize := true;
    Anchors := [];
    AnchorParallel(akBottom, 10, Self);
    AnchorParallel(akRight, 10, Self);
    OnClick := @CloseBtnClick;
    Parent := Self;
  end;
end;

procedure TCheckVersionForm.UpdateVersions;
var
  ManagerVersion: TEpiVersionInfo;
  Stable: TEpiVersionInfo;
  Test: TEpiVersionInfo;
  Response: string;
begin
  ManagerVersion := GetEpiVersion(HINSTANCE);
  if not CheckVersionOnline('epidatamanager', Stable, Test, Response) then
  begin
    ShowMessage(
      'ERROR: Could not find version information.' + LineEnding +
      'Response: ' + Response);
    exit;
  end;

  with ManagerVersion do
    CurrentVersionInfo.Caption := Format('%d.%d.%d.%d', [VersionNo, MajorRev, MinorRev, BuildNo]);

  With Stable do
    PublicVersionInfo.Caption := Format('%d.%d.%d.%d', [VersionNo, MajorRev, MinorRev, BuildNo]);

  With Test do
    TestVersionInfo.Caption := Format('%d.%d.%d.%d', [VersionNo, MajorRev, MinorRev, BuildNo]);
end;


constructor TCheckVersionForm.Create(TheOwner: TComponent);
begin
  inherited CreateNew(TheOwner);

  CreateControls;
  Caption := 'Check Version';

  OnShow := @ShowForm;
end;

end.

