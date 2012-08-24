unit design_properties_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, JvDesignSurface;

type

  { TPropertiesForm }

  TPropertiesForm = class(TForm)
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
  private
    FFrame: TCustomFrame;
    procedure ShowEmptyPage;
    procedure FormDeactivate(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure UpdateSelection(Objects: TJvDesignObjectArray);
  end;

implementation

uses
  Buttons, ExtCtrls, Controls, epicustombase, design_types;

{ TPropertiesForm }

procedure TPropertiesForm.FormDeactivate(Sender: TObject);
begin
  if Assigned(FFrame) then
    if not (FFrame as IDesignPropertiesFrame).ApplyChanges then
      Self.SetFocus;
end;

procedure TPropertiesForm.FormCloseQuery(Sender: TObject; var CanClose: boolean
  );
begin
  if Assigned(FFrame) then
    CanClose := (FFrame as IDesignPropertiesFrame).ApplyChanges;
end;

procedure TPropertiesForm.ShowEmptyPage;
begin
  // TODO: Show an empty page.
end;

constructor TPropertiesForm.Create(TheOwner: TComponent);
var
  P: TPanel;
  Btn: TBitBtn;
  CloseBtn: TBitBtn;
begin
  inherited CreateNew(TheOwner);

  BeginFormUpdate;

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

  Btn := TBitBtn.Create(Self);
  Btn.Kind := bkRetry;
  Btn.Anchors := [];
  Btn.Caption := 'Apply';
  Btn.AnchorVerticalCenterTo(P);
  Btn.AnchorParallel(akLeft, 10, P);
  Btn.AutoSize := true;
  Btn.Parent := P;

  OnDeactivate := @FormDeactivate;
  OnCloseQuery := @FormCloseQuery;

  EndFormUpdate;
end;

procedure TPropertiesForm.UpdateSelection(Objects: TJvDesignObjectArray);
var
  AClassType: TClass;
  i: Integer;
  EpiCtrlItemArray: TEpiCustomControlItemArray;

  procedure NewFrame(FrameClass: TCustomFrameClass);
  begin
    FFrame := FrameClass.Create(Self);
    FFrame.Align := alClient;
    FFrame.Parent := Self;
  end;

begin
{  if Assigned(FFrame) then
  begin
    if not (FFrame as IDesignPropertiesFrame).ApplyChanges then
      Exit;
  end;}


  if Length(Objects) = 0 then
  begin
    ShowEmptyPage;
    Exit;
  end;

  AClassType := Objects[0].ClassType;
  for i := Low(Objects)+1 to High(Objects) do
  begin
    if Objects[i].ClassType <> AClassType then
    begin
      ShowEmptyPage;
      Exit;
    end;
  end;

  if Assigned(FFrame) and (FFrame.ClassType <> (Objects[0] as IDesignEpiControl).DesignFrameClass) then
  begin
    FFrame.Free;
    NewFrame((Objects[0] as IDesignEpiControl).DesignFrameClass);
  end else
  if (not Assigned(FFrame)) then
  begin
    NewFrame((Objects[0] as IDesignEpiControl).DesignFrameClass);
  end;

  SetLength(EpiCtrlItemArray, Length(Objects));
  for i := Low(Objects) to High(Objects) do
    EpiCtrlItemArray[i] := (Objects[i] as IDesignEpiControl).EpiControl;

  if Assigned(FFrame) then
    (FFrame as IDesignPropertiesFrame).SetEpiControls(EpiCtrlItemArray);
end;

end.

