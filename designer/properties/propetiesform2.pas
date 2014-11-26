unit propetiesform2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, JvDesignSurface, epirelations;

type

  { TPropertiesForm2 }

  TPropertiesForm2 = class(TForm)
  private
    procedure ApplyClick(Sender: TObject);
    procedure CancelClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormDeactivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ResetClick(Sender: TObject);
  private
    FFrame: TCustomFrame;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure UpdateSelection(Objects: TJvDesignObjectArray;
      Const Relation: TEpiMasterRelation);
  end;

var
  PropertiesForm2: TPropertiesForm2;

implementation

uses
  ExtCtrls, Buttons, Controls, design_types,
  epidatafiles,

  fieldproperties_frame2;

{ TPropertiesForm2 }

procedure TPropertiesForm2.ApplyClick(Sender: TObject);
begin

end;

procedure TPropertiesForm2.CancelClick(Sender: TObject);
begin

end;

procedure TPropertiesForm2.FormCloseQuery(Sender: TObject; var CanClose: boolean
  );
begin

end;

procedure TPropertiesForm2.FormDeactivate(Sender: TObject);
begin

end;

procedure TPropertiesForm2.FormShow(Sender: TObject);
begin

end;

procedure TPropertiesForm2.ResetClick(Sender: TObject);
begin

end;

constructor TPropertiesForm2.Create(TheOwner: TComponent);
var
  P: TPanel;
  CloseBtn: TBitBtn;
  Btn: TBitBtn;
  ApplyBtn: TBitBtn;
begin
  inherited CreateNew(TheOwner);

  FFrame := nil;

  BeginFormUpdate;

//  Color := clSkyBlue;
  Top := 50;
  Left := 50;
  Width := 400;
  Height := 600;

  P := TPanel.Create(Self);
  P.Align := alBottom;
  P.Height := 48;
  P.Parent := Self;

  CloseBtn := TBitBtn.Create(Self);
  CloseBtn.Kind := bkClose;
  CloseBtn.Anchors := [];
  CloseBtn.AnchorVerticalCenterTo(P);
  CloseBtn.AnchorParallel(akRight, 10, P);
  CloseBtn.AutoSize := true;
  CloseBtn.Parent := P;

  Btn := TBitBtn.Create(Self);
  Btn.Kind := bkCancel;
  Btn.Anchors := [];
  Btn.AnchorVerticalCenterTo(P);
  Btn.AnchorToNeighbour(akRight, 10, CloseBtn);
  Btn.AutoSize := true;
  Btn.Parent := P;
  Btn.OnClick := @CancelClick;

  ApplyBtn := TBitBtn.Create(Self);
  ApplyBtn.Kind := bkRetry;
  ApplyBtn.Anchors := [];
  ApplyBtn.Caption := 'Apply';
  ApplyBtn.AnchorVerticalCenterTo(P);
  ApplyBtn.AnchorParallel(akLeft, 10, P);
  ApplyBtn.AutoSize := true;
  ApplyBtn.Parent := P;
  ApplyBtn.OnClick := @ApplyClick;

  Btn := TBitBtn.Create(Self);
  Btn.Kind := bkCustom;
  Btn.Anchors := [];
  Btn.Caption := 'Reset';
  Btn.AnchorVerticalCenterTo(P);
  Btn.AnchorToNeighbour(akLeft, 10, ApplyBtn);
  Btn.AutoSize := true;
  Btn.Parent := P;
  Btn.OnClick := @ResetClick;

  OnShow := @FormShow;
  OnDeactivate := @FormDeactivate;
  OnCloseQuery := @FormCloseQuery;

  DefaultControl := ApplyBtn;
  KeyPreview := true;
  EndFormUpdate;
end;

procedure TPropertiesForm2.UpdateSelection(Objects: TJvDesignObjectArray;
  const Relation: TEpiMasterRelation);
var
  FrameClass: TCustomFrameClass;
  EpiCtrlItemArray: TEpiCustomControlItemArray;
begin
  if Length(Objects) = 0 then exit;
  if not Supports(Objects[0], IDesignEpiControl) then
    Exit;

  BeginFormUpdate;

  try
    SetLength(EpiCtrlItemArray, 1);
    EpiCtrlItemArray[0] := (Objects[0] as IDesignEpiControl).EpiControl;

    if not Assigned(EpiCtrlItemArray[0]) then exit;

    FrameClass := nil;

    if EpiCtrlItemArray[0].InheritsFrom(TEpiField) then
      FrameClass := TFieldPropertiesFrame2;

    FFrame.Free;

    if not Assigned(FrameClass) then exit;

    FFrame := FrameClass.Create(Self);
    FFrame.Align := alClient;
    TFieldPropertiesFrame2(FFrame).Field := TEpiField(EpiCtrlItemArray[0]);
    TFieldPropertiesFrame2(FFrame).Initialize;
    FFrame.Parent := Self;
  finally
    EndFormUpdate;
  end;
end;

end.

