unit design_controls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, Buttons, ActnList, AVL_Tree, epicustombase, epidatafiles,
  epidocument, epivaluelabels, LCLType, design_propertiesbase_frame, design_types,
  LMessages;

type

  { TDesignControlsForm }

  TDesignControlsForm = class(TForm)
    CancelAction: TAction;
    CloseAction: TAction;
    ApplyAction: TAction;
    ActionList1: TActionList;
    CancelBtn: TBitBtn;
    OkBtn: TBitBtn;
    ApplyBtn: TBitBtn;
    OkApplyPanel: TPanel;
    procedure ApplyActionExecute(Sender: TObject);
    procedure CancelActionExecute(Sender: TObject);
    procedure CloseActionExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormDeactivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    FActiveFrame: TDesignPropertiesFrame;
    FEpiDocument: TEpiDocument;
    FOnShowHintMsg: TDesignFrameShowHintEvent;
    constructor Create(TheOwner: TComponent); override;
    function    GetEpiControl: TEpiCustomControlItem;
    procedure   SetEpiControl(const AValue: TEpiCustomControlItem);
    procedure   ShowHintMsg(Sender: TObject; Ctrl: TControl; const Msg: string);
    procedure   UpdateControlContent;
    procedure   UpdateCaption(Const S: String);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; Const EpiDocument: TEpiCustomBase);
    destructor  Destroy; override;
    function    ValidateControl: boolean;
    procedure   RestoreDefaultPos;
    procedure   Show;
    property    EpiControl: TEpiCustomControlItem read GetEpiControl write SetEpiControl;
    property    ActiveFrame: TDesignPropertiesFrame read FActiveFrame;
    property    OnShowHintMsg: TDesignFrameShowHintEvent read FOnShowHintMsg write FOnShowHintMsg;
  end;


function XTreeSort(Item1, Item2: Pointer): Integer;
function YTreeSort(Item1, Item2: Pointer): Integer;
function WriteTree(Tree: TAVLTree): string;

procedure AddToPositionHandler(PositionHandler: IPositionHandler;
  Ctrl: TControl);
procedure RemoveFromPositionHandler(PositionHandler: IPositionHandler;
  Ctrl: TControl);

function EpiTextToControlText(Const Str: string): string;
function ControlTextToEpiText(Const Str: string): string;

function DesignControlTypeFromControl(Ctrl: TControl): TDesignerControlType;

var
  DesignControlsForm: TDesignControlsForm;

implementation

{$R *.lfm}

uses
  epidatafilestypes, math, types, valuelabelseditor_form, epiadmin,
  LCLProc, settings2_var, settings2, epimiscutils, main, episettings,
  epiranges, strutils, epiconvertutils,
  // EpiControls property frames.
  design_fieldproperties_frame, design_headingproperties_frame,
  design_sectionproperties_frame;

var
  DesignPropertyFrames: array[0..2] of TDesignPropertiesFrame;

function XTreeSort(Item1, Item2: Pointer): Integer;
var
  Ctrl1: TControl absolute Item1;
  Ctrl2: TControl absolute Item2;
  EpiCtrl1: TEpiCustomControlItem;
  EpiCtrl2: TEpiCustomControlItem;
begin
  if Item1 = Item2 then exit(0);

  EpiCtrl1 := (Ctrl1 as IDesignEpiControl).EpiControl;
  EpiCtrl2 := (Ctrl2 as IDesignEpiControl).EpiControl;

  if EpiCtrl1.Left > EpiCtrl2.Left then
    result := 1
  else if EpiCtrl1.Left < EpiCtrl2.Left then
    result := -1
  else
    if EpiCtrl1.Top > EpiCtrl2.Top then
      result := 1
    else if EpiCtrl1.Top < EpiCtrl2.Top then
      result := -1
    else
      result := 0;
end;

function YTreeSort(Item1, Item2: Pointer): Integer;
var
  Ctrl1: TControl absolute Item1;
  Ctrl2: TControl absolute Item2;
  EpiCtrl1: TEpiCustomControlItem;
  EpiCtrl2: TEpiCustomControlItem;
begin
  if Item1 = Item2 then exit(0);

  EpiCtrl1 := (Ctrl1 as IDesignEpiControl).EpiControl;
  EpiCtrl2 := (Ctrl2 as IDesignEpiControl).EpiControl;

  if EpiCtrl1.Top > EpiCtrl2.Top then
    result := 1
  else if EpiCtrl1.Top < EpiCtrl2.Top then
    result := -1
  else
    if EpiCtrl1.Left > EpiCtrl2.Left then
      result := 1
    else if EpiCtrl1.Left < EpiCtrl2.Left then
      result := -1
    else
      result := 0;
end;

function WriteTree(Tree: TAVLTree): string;

  function WriteTreeNode(ANode: TAVLTreeNode; const Prefix: string): string;
  begin
    if ANode=nil then exit('');
    Result := WriteTreeNode(ANode.Right, Prefix + '  ');
    Result += Prefix +
      Format(
      'Node: %s (%d, %d)',
      [TControl(aNode.Data).Name,
       TControl(aNode.Data).Left,
       TControl(aNode.Data).Top]
      ) + LineEnding;
    Result += WriteTreeNode(ANode.Left,  Prefix + '  ');
  end;

  begin
  result := WriteTreeNode(Tree.Root, '');
end;

procedure AddToPositionHandler(PositionHandler: IPositionHandler; Ctrl: TControl
  );
begin
  (Ctrl as IDesignEpiControl).XTreeNode := PositionHandler.XTree.Add(Ctrl);
  (Ctrl as IDesignEpiControl).YTreeNode := PositionHandler.YTree.Add(Ctrl);
end;

procedure RemoveFromPositionHandler(PositionHandler: IPositionHandler;
  Ctrl: TControl);
begin
  PositionHandler.XTree.Remove(Ctrl);
  PositionHandler.YTree.Remove(Ctrl);
  (Ctrl as IDesignEpiControl).XTreeNode := nil;
  (Ctrl as IDesignEpiControl).YTreeNode := nil;
end;

function EpiTextToControlText(const Str: string): string;
begin
  result := StringReplace(Str, '&', '&&', [rfReplaceAll]);
end;

function ControlTextToEpiText(const Str: string): string;
begin
  result := StringReplace(Str, '&&', '&', [rfReplaceAll]);
end;

{ TDesignControlsForm }

procedure TDesignControlsForm.FormShow(Sender: TObject);
begin
  if ManagerSettings.SaveWindowPositions then
    LoadFormPosition(Self, 'ControlsForm');

  if Assigned(FActiveFrame) and
     Assigned(FActiveFrame.EpiControl) then
    FActiveFrame.UpdateFormContent;
end;

procedure TDesignControlsForm.ApplyActionExecute(Sender: TObject);
begin
  if FActiveFrame.ValidateControl then
    MainForm.SetFocus;
end;

procedure TDesignControlsForm.CancelActionExecute(Sender: TObject);
begin
  // Restores active frame to a valid state, loosing changes - but that's the point!
  FActiveFrame.UpdateFormContent;
  Close;
end;

procedure TDesignControlsForm.CloseActionExecute(Sender: TObject);
begin
  if FActiveFrame.ValidateControl then Close;
end;

procedure TDesignControlsForm.FormActivate(Sender: TObject);
begin
  FActiveFrame.UpdateFormContent;
end;

procedure TDesignControlsForm.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  if ManagerSettings.SaveWindowPositions then
    SaveFormPosition(Self, 'ControlsForm');
end;

procedure TDesignControlsForm.FormDeactivate(Sender: TObject);
begin
  if not Showing then exit;

  if not (FActiveFrame.ValidateControl) then
    Self.SetFocus;
end;

procedure TDesignControlsForm.FormDestroy(Sender: TObject);
var
  B: Boolean;
begin
  B:=true;
  FormCloseQuery(nil, B);
end;

procedure TDesignControlsForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key in [VK_0..VK_9]) and ([ssAlt] = Shift) then
    FActiveFrame.ShiftToTabSheet(Key - VK_0);
end;

procedure TDesignControlsForm.SetEpiControl(const AValue: TEpiCustomControlItem);
begin
  if AValue = nil then exit;
  try
    BeginFormUpdate;

    if Assigned(FActiveFrame) then
    begin
      if FActiveFrame.EpiControl = AValue then
      begin
        FActiveFrame.UpdateFormContent;
        exit;
      end;

      if Assigned(FActiveFrame.EpiControl) and
         (not FActiveFrame.ValidateControl) then exit;
      FActiveFrame.Parent := nil;
    end;

    if AValue is TEpiField then
      FActiveFrame := DesignPropertyFrames[0];
    if AValue is TEpiSection then
      FActiveFrame := DesignPropertyFrames[1];
    if AValue is TEpiHeading then
      FActiveFrame := DesignPropertyFrames[2];
    FActiveFrame.Align := alClient;
    FActiveFrame.Parent := self;

    Constraints.Assign(FActiveFrame.Constraints);
    Constraints.MinHeight := Constraints.MinHeight + OkApplyPanel.Height;

    FActiveFrame.EpiControl := AValue;
  finally
    EndFormUpdate;
  end;
end;

procedure TDesignControlsForm.ShowHintMsg(Sender: TObject; Ctrl: TControl;
  const Msg: string);
begin
  if Assigned(FOnShowHintMsg) then
    OnShowHintMsg(Sender, Ctrl, Msg);
end;

procedure TDesignControlsForm.UpdateControlContent;
begin
  BeginFormUpdate;
  FActiveFrame.UpdateFormContent;
  EndFormUpdate;
end;

procedure TDesignControlsForm.UpdateCaption(const S: String);
begin
  Caption := S;
end;

function TDesignControlsForm.ValidateControl: boolean;
begin
  if (not Showing) then exit(true);
  if (not Assigned(FActiveFrame.EpiControl)) then exit(true);

  result := FActiveFrame.ValidateControl;
end;

constructor TDesignControlsForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;


function TDesignControlsForm.GetEpiControl: TEpiCustomControlItem;
begin
  result := FActiveFrame.EpiControl;
end;

constructor TDesignControlsForm.Create(TheOwner: TComponent;
  const EpiDocument: TEpiCustomBase);
var
  i: Integer;
begin
  Create(TheOwner);
  FEpiDocument := TEpiDocument(EpiDocument);

  DesignPropertyFrames[0] := TFieldPropertiesFrame.Create(Self, FEpiDocument.ValueLabelSets, FEpiDocument.DataFiles[0]);
  DesignPropertyFrames[0].OnShowHintMsg := @ShowHintMsg;
  DesignPropertyFrames[0].OnUpdateCaption := @UpdateCaption;
  DesignPropertyFrames[1] := TSectionPropertiesFrame.Create(Self, FEpiDocument.Admin);
  DesignPropertyFrames[1].OnShowHintMsg := @ShowHintMsg;
  DesignPropertyFrames[1].OnUpdateCaption := @UpdateCaption;
  DesignPropertyFrames[2] := THeadingPropertiesFrame.Create(Self);
  DesignPropertyFrames[2].OnShowHintMsg := @ShowHintMsg;
  DesignPropertyFrames[2].OnUpdateCaption := @UpdateCaption;
end;

destructor TDesignControlsForm.Destroy;
begin
  FActiveFrame.Free;
  inherited Destroy;
end;

procedure TDesignControlsForm.RestoreDefaultPos;
begin
  BeginFormUpdate;
  Width := 500;
  Height := 500;
  Top := 20;
  Left := 300;
  EndFormUpdate;
  SaveFormPosition(Self, 'ControlsForm');
end;

procedure TDesignControlsForm.Show;
begin
  if not Assigned(FActiveFrame) then exit;

  Visible := true;
  FActiveFrame.ForceShow;
  BringToFront;
end;

end.

