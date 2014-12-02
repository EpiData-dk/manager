unit checkversionform;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Classes, SysUtils, Forms, StdCtrls, CheckBoxThemed, Buttons;

type

  { TCheckVersionForm }

  TCheckVersionForm = class(TForm)
    procedure DownloadLinkMouseLeave(Sender: TObject);
  private
    CurrentVersionCaption: TLabel;
    CurrentVersionInfo: TLabel;

    PublicVersionCaption: TLabel;
    PublicVersionInfo: TLabel;

    TestVersionCaption: TLabel;
    TestVersionInfo: TLabel;

    CheckVersionOnlineChkBox: TCheckBoxThemed;
    CloseBtn: TBitBtn;
    procedure DownloadLinkClick(Sender: TObject);
    procedure DownloadLinkMouseEnter(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
  end;

implementation

uses
  Controls, Graphics, LCLIntf;


const
  DefaultURL = 'http://epidata.dk';
  PublicDownloadURL = 'http://epidata.dk/download.php';
  TestDownloadURL = 'http://epidata.dk/testing.php';

{ TCheckVersionForm }

procedure TCheckVersionForm.DownloadLinkMouseEnter(Sender: TObject);
begin
  Screen.Cursor := crHandPoint;
end;

procedure TCheckVersionForm.DownloadLinkMouseLeave(Sender: TObject);
begin
  Screen.Cursor := crDefault;
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

constructor TCheckVersionForm.Create(TheOwner: TComponent);
begin
  inherited CreateNew(TheOwner);

  CurrentVersionCaption := TLabel.Create(Self);
  with CurrentVersionCaption do
  begin
    Caption := 'Current Virsion:';
    AutoSize := true;
    Anchors := [];
    AnchorParallel(akTop, 10, Self);
    AnchorParallel(akLeft, 10, Self);
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
    AnchorParallel(akLeft, 10, Self);
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
    AnchorParallel(akLeft, 10, Self);
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
    AnchorParallel(akLeft, 10, Self);
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
    Parent := Self;
  end;

  DefaultControl := CloseBtn;
end;

end.

