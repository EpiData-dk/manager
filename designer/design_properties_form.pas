unit design_properties_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, JvDesignSurface, design_types, epicustombase;

type

  { TPropertiesForm }

  TPropertiesForm = class(TForm)
  private
    EpiCtrlItemArray: TEpiCustomControlItemArray;
    FFrame: TCustomFrame;
    FOnShowHintMsg: TDesignFrameShowHintEvent;
    procedure UpdateCaption(const S: string);
    procedure EpiCtrlChangeHook(Sender: TObject; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
    procedure ShowEmptyPage;
    procedure FormDeactivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
    procedure ApplyClick(Sender: TObject);
    procedure CancelClick(Sender: TObject);
    procedure ResetClick(Sender: TObject);
    procedure RegisterHooks;
    procedure UnregisterHooks;
    function EpiCtrlIsEmpty: boolean;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateSelection(Objects: TJvDesignObjectArray);
    procedure RestoreDefaultPos;
    procedure SetFocusOnNew;
    function  ValidateControls: boolean;
    property  OnShowHintMsg: TDesignFrameShowHintEvent read FOnShowHintMsg write FOnShowHintMsg;
  end;

implementation

uses
  Buttons, ExtCtrls, Controls,
  design_properties_baseframe, Graphics, design_properties_emptyframe,
  settings2, settings2_var, main;

{ TPropertiesForm }

procedure TPropertiesForm.FormDeactivate(Sender: TObject);
begin
  if not ValidateControls then
    Self.SetFocus;
end;

procedure TPropertiesForm.FormCloseQuery(Sender: TObject; var CanClose: boolean
  );
begin
//  if Assigned(FFrame) then
  CanClose := ValidateControls;// (FFrame as IDesignPropertiesFrame).ApplyChanges;

  if CanClose and
     ManagerSettings.SaveWindowPositions
  then
    SaveFormPosition(Self, 'ControlsForm');
end;

procedure TPropertiesForm.FormShow(Sender: TObject);
begin
  if ManagerSettings.SaveWindowPositions then
    LoadFormPosition(Self, 'ControlsForm');
end;

procedure TPropertiesForm.ApplyClick(Sender: TObject);
begin
  if ValidateControls then
    MainForm.SetFocus;
end;

procedure TPropertiesForm.CancelClick(Sender: TObject);
begin
  if assigned(FFrame) then
    (FFrame as IDesignPropertiesFrame).ResetControls;
  Close;
end;

procedure TPropertiesForm.ResetClick(Sender: TObject);
begin
  if assigned(FFrame) then
    (FFrame as IDesignPropertiesFrame).ResetControls;
end;

procedure TPropertiesForm.RegisterHooks;
var
  i: Integer;
begin
  for i := 0 to Length(EpiCtrlItemArray) - 1 do
    if Assigned(EpiCtrlItemArray[i]) then
       EpiCtrlItemArray[i].RegisterOnChangeHook(@EpiCtrlChangeHook, true);
end;

procedure TPropertiesForm.UnregisterHooks;
var
  i: Integer;
begin
  for i := 0 to Length(EpiCtrlItemArray) - 1 do
    if Assigned(EpiCtrlItemArray[i]) then
       EpiCtrlItemArray[i].UnRegisterOnChangeHook(@EpiCtrlChangeHook);
end;

function TPropertiesForm.EpiCtrlIsEmpty: boolean;
var
  i: Integer;
begin
  result := false;

  for i := 0 to Length(EpiCtrlItemArray) - 1 do
    if Assigned(EpiCtrlItemArray[i]) then
      exit;

  result := true;
end;

procedure TPropertiesForm.UpdateCaption(const S: string);
begin
  Caption := S;
end;

procedure TPropertiesForm.EpiCtrlChangeHook(Sender: TObject;
  EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
var
  i: Integer;
begin
  if EventGroup <> eegCustomBase then exit;
  if TEpiCustomChangeEventType(EventType) <> ecceDestroy then exit;

  for i := 0 to Length(EpiCtrlItemArray) - 1 do
    if Sender = EpiCtrlItemArray[i] then
    begin
      EpiCtrlItemArray[i] := nil;
      break;
    end;
end;

procedure TPropertiesForm.ShowEmptyPage;
begin
  if Assigned(FFrame) then
    FFrame.Free;

  FFrame := TEmptyPropertiesFrame.Create(Self);
  TDesignPropertiesFrame(FFrame).OnShowHintMsg := OnShowHintMsg;
  TDesignPropertiesFrame(FFrame).OnUpdateCaption := @UpdateCaption;
  FFrame.Align := alClient;
  FFrame.Parent := Self;
end;

constructor TPropertiesForm.Create(TheOwner: TComponent);
var
  P: TPanel;
  Btn: TBitBtn;
  CloseBtn: TBitBtn;
  ApplyBtn: TBitBtn;
begin
  inherited CreateNew(TheOwner);

  BeginFormUpdate;

  Color := clSkyBlue;

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

destructor TPropertiesForm.Destroy;
begin
  UnregisterHooks;
  inherited Destroy;
end;

procedure TPropertiesForm.UpdateSelection(Objects: TJvDesignObjectArray);
var
  AClassType: TClass;
  i: Integer;

  procedure NewFrame(FrameClass: TCustomFrameClass);
  begin
    FFrame := FrameClass.Create(Self);
    FFrame.Align := alClient;
    FFrame.Parent := Self;
    TDesignPropertiesFrame(FFrame).OnShowHintMsg := OnShowHintMsg;
    TDesignPropertiesFrame(FFrame).OnUpdateCaption := @UpdateCaption;
  end;

begin
  if not ValidateControls then exit;

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

  UnregisterHooks;

  SetLength(EpiCtrlItemArray, Length(Objects));
  for i := Low(Objects) to High(Objects) do
    EpiCtrlItemArray[i] := (Objects[i] as IDesignEpiControl).EpiControl;

  RegisterHooks;

  if Assigned(FFrame) then
    (FFrame as IDesignPropertiesFrame).SetEpiControls(EpiCtrlItemArray);
end;

procedure TPropertiesForm.RestoreDefaultPos;
begin
  BeginFormUpdate;
  SetBounds(300, 20, 500, 500);
  EndFormUpdate;
  SaveFormPosition(Self, 'ControlsForm');
end;

procedure TPropertiesForm.SetFocusOnNew;
begin
  (FFrame as IDesignPropertiesFrame).FocusOnNewControl;
end;

function TPropertiesForm.ValidateControls: boolean;
begin
  result := true;

  if Assigned(FFrame) and
     (not EpiCtrlIsEmpty)
  then
    Result := (FFrame as IDesignPropertiesFrame).ApplyChanges;
end;

end.

