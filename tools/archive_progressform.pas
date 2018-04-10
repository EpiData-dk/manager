unit archive_progressform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, epitools_archieve, Buttons;

type

  { TArchiveProgressForm }

  TArchiveProgressForm = class(TForm)
  private
    FCancel: boolean;
    FLastUpdate: QWord;
    FOverallProgress: TProgressBar;
    FFileProgress: TProgressBar;
    FCancelBtn: TBitBtn;
    function GetMaxFileCount: Integer;
    procedure SetMaxFileCount(AValue: Integer);
    procedure CancelClick(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Progress(Sender: TObject; FileNo, FileProgress: Integer; Const Filename: String; out Cancel: boolean);
    property MaxFileCount: Integer read GetMaxFileCount write SetMaxFileCount;
  end;

implementation

uses
  StdCtrls;

{ TArchiveProgressForm }

function TArchiveProgressForm.GetMaxFileCount: Integer;
begin
  result := FOverallProgress.Max;
end;

procedure TArchiveProgressForm.SetMaxFileCount(AValue: Integer);
begin
  FOverallProgress.Max := AValue;
end;

procedure TArchiveProgressForm.CancelClick(Sender: TObject);
begin
  FCancel := true;
end;

constructor TArchiveProgressForm.Create(TheOwner: TComponent);
var
  ALabel: TLabel;
begin
  inherited CreateNew(TheOwner);

  Position := poOwnerFormCenter;
  BorderStyle := bsDialog;

  Caption := 'Progress';

  ALabel := TLabel.Create(Self);
  ALabel.Parent := self;
  ALabel.Anchors := [];
  ALabel.AnchorHorizontalCenterTo(Self);
  ALabel.AnchorParallel(akTop, 10, Self);
  ALabel.Caption := 'Overall:';

  FOverallProgress := TProgressBar.Create(Self);
  FOverallProgress.Parent := self;
  FOverallProgress.Anchors := [];
  FOverallProgress.AnchorHorizontalCenterTo(Self);
  FOverallProgress.AnchorToNeighbour(akTop, 10, ALabel);
  FOverallProgress.AnchorParallel(akLeft, 10, Self);
  FOverallProgress.AnchorParallel(akRight, 10, Self);
  FOverallProgress.Smooth := true;
  FOverallProgress.BarShowText := true;
  FOverallProgress.Min := 0;
  FOverallProgress.Max := 1;

  ALabel := TLabel.Create(Self);
  ALabel.Parent := self;
  ALabel.Anchors := [];
  ALabel.AnchorHorizontalCenterTo(Self);
  ALabel.AnchorToNeighbour(akTop, 10, FOverallProgress);
  ALabel.Caption := 'Current File:';

  FFileProgress := TProgressBar.Create(Self);
  FFileProgress.Parent := self;
  FFileProgress.Anchors := [];
  FFileProgress.AnchorHorizontalCenterTo(Self);
  FFileProgress.AnchorToNeighbour(akTop, 10, ALabel);
  FFileProgress.AnchorParallel(akLeft, 10, Self);
  FFileProgress.AnchorParallel(akRight, 10, Self);
  FFileProgress.Smooth := true;
  FFileProgress.BarShowText := true;
  FFileProgress.Min := 0;
  FFileProgress.Max := 100;

  FCancelBtn := TBitBtn.Create(Self);
  FCancelBtn.Parent := self;
  FCancelBtn.Kind := bkCancel;
  FCancelBtn.Anchors := [];
  FCancelBtn.AnchorHorizontalCenterTo(Self);
  FCancelBtn.AnchorToNeighbour(akTop, 10, FFileProgress);
  FCancelBtn.AutoSize := true;
  FCancelBtn.OnClick := @CancelClick;

  FLastUpdate := 0;
  FCancel := false;
end;

procedure TArchiveProgressForm.Progress(Sender: TObject; FileNo,
  FileProgress: Integer; const Filename: String; out Cancel: boolean);
begin
  FOverallProgress.Position := FileNo;
  FFileProgress.Position := FileProgress;

  if (GetTickCount64 - FLastUpdate) > 50 then
    begin
      Application.ProcessMessages;
      FLastUpdate := GetTickCount64;
    end;

  Cancel := FCancel;
end;

end.

