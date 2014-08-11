unit design_properties_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, JvDesignSurface, design_types, epicustombase,
  Controls, epirelations;

type

  { TPropertiesForm }

  TPropertiesForm = class(TForm)
  private
    EpiCtrlItemArray: TEpiCustomControlItemArray;
    FFrame: TCustomFrame;
    FOnShowHintMsg: TDesignFrameShowHintEvent;
    procedure UpdateCaption(const S: string);
    procedure EpiCtrlChangeHook(Const Sender, Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
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
    procedure DoShowHint(Sender: TObject; Ctrl: TControl; const Msg: string);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateSelection(Objects: TJvDesignObjectArray;
      Const Relation: TEpiMasterRelation);
    procedure ReloadControls;
    procedure SetFocusOnNew;
    function  ValidateControls: boolean;
    property  OnShowHintMsg: TDesignFrameShowHintEvent read FOnShowHintMsg write FOnShowHintMsg;
  public
    class procedure RestoreDefaultPos(F: TPropertiesForm);
  end;

var
  PropertiesForm: TPropertiesForm;

implementation

uses
  Buttons, ExtCtrls, design_properties_baseframe, Graphics,
  design_properties_emptyframe, settings2, settings2_var, main,
  field_valuelabelseditor_form, project_types, epidatafiles;

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

procedure TPropertiesForm.DoShowHint(Sender: TObject; Ctrl: TControl;
  const Msg: string);
begin
  if Assigned(OnShowHintMsg) and
     Showing
  then
    FOnShowHintMsg(Sender, Ctrl, Msg);
end;

procedure TPropertiesForm.UpdateCaption(const S: string);
begin
  Caption := S;
end;

procedure TPropertiesForm.EpiCtrlChangeHook(const Sender,
  Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
var
  i: Integer;
begin
  if EventGroup <> eegCustomBase then exit;
  if TEpiCustomChangeEventType(EventType) <> ecceDestroy then exit;

  for i := 0 to Length(EpiCtrlItemArray) - 1 do
    if Initiator = EpiCtrlItemArray[i] then
    begin
      Initiator.UnRegisterOnChangeHook(@EpiCtrlChangeHook);
      EpiCtrlItemArray[i] := nil;
      break;
    end;
end;

procedure TPropertiesForm.ShowEmptyPage;
begin
  if Assigned(FFrame) then
    FFrame.Free;

  FFrame := TEmptyPropertiesFrame.Create(Self);
  TDesignPropertiesFrame(FFrame).OnShowHintMsg := @DoShowHint;
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

  FFrame := nil;

  BeginFormUpdate;

  Color := clSkyBlue;
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

destructor TPropertiesForm.Destroy;
begin
  UnregisterHooks;

  FFrame.Free;
  inherited Destroy;
end;

procedure TPropertiesForm.UpdateSelection(Objects: TJvDesignObjectArray;
  const Relation: TEpiMasterRelation);
var
  AClassType: TClass;
  i: Integer;
  Item: TEpiCustomItem;

  procedure NewFrame(FrameClass: TCustomFrameClass);
  begin
    FFrame := FrameClass.Create(Self);
    FFrame.Align := alClient;
    FFrame.Parent := Self;
    TDesignPropertiesFrame(FFrame).OnShowHintMsg := @DoShowHint;
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

  if not Supports(Objects[0], IDesignEpiControl) then
  begin
    ShowEmptyPage;
    Exit;
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

  if Assigned(FFrame)
  then
  with (FFrame as IDesignPropertiesFrame) do
  begin
    SetDataFile(Relation.Datafile);
    SetRelation(Relation);
    SetEpiControls(EpiCtrlItemArray);
  end;
end;

procedure TPropertiesForm.ReloadControls;
begin
  (FFrame as IDesignPropertiesFrame).ResetControls;
end;

class procedure TPropertiesForm.RestoreDefaultPos(F: TPropertiesForm);
var
  AForm: TForm;
begin
  if Assigned(F) then
    AForm := F
  else
    AForm := TForm.Create(nil);

  TFieldValueLabelEditor.RestoreDefaultPos;

  with AForm  do
  begin
    LockRealizeBounds;
    SetBounds(300, 200, 500, 500);
    UnlockRealizeBounds;
  end;
  SaveFormPosition(AForm, 'ControlsForm');
  if AForm <> F then AForm.Free;
end;

procedure TPropertiesForm.SetFocusOnNew;
begin
  if Showing then
    (FFrame as IDesignPropertiesFrame).FocusOnNewControl;
end;

function TPropertiesForm.ValidateControls: boolean;
begin
  result := true;

  if Assigned(FFrame) and
     Showing and
     (not EpiCtrlIsEmpty)
  then
    Result := (FFrame as IDesignPropertiesFrame).ApplyChanges;
end;

end.

