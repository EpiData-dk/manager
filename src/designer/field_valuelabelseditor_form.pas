unit field_valuelabelseditor_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Buttons, laz.VirtualTrees, epivaluelabels,
  epidatafilestypes, LCLType, valuelabelgrid_frame;

type

  { TFieldValueLabelEditor }

  TFieldValueLabelEditor = class(TForm)
    CancelBtn: TBitBtn;
    OkBtn: TBitBtn;
    Panel2: TPanel;
    Label1: TLabel;
    Panel1: TPanel;
    Panel3: TPanel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure ValueLabelNameEditEditingDone(Sender: TObject);
  private
    ValueLabelNameEdit: TEdit;
    FGridFrame: TValueLabelGridFrame;
    FHintWindow: THintWindow;
    FValueLabelSets: TEpiValueLabelSets;
    function GetValueLabelSet: TEpiValueLabelSet;
    procedure SetValueLabelSet(AValue: TEpiValueLabelSet);
    procedure ShowHintMsg(Sender: TObject; Ctrl: TControl; Const Msg: String);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; ValueLabelSets: TEpiValueLabelSets);
    class procedure RestoreDefaultPos;
    property  ValueLabelSets: TEpiValueLabelSets read FValueLabelSets;
    property  ValueLabelSet: TEpiValueLabelSet read GetValueLabelSet write SetValueLabelSet;
  end;

implementation

{$R *.lfm}

uses
  LCLIntf, LMessages, epidocument, settings2_var, settings2, admin_authenticator, epiadmin;

const
  rsFormCaption = 'Variable Valuelabel Editor';

{ TFieldValueLabelEditor }

procedure TFieldValueLabelEditor.OkBtnClick(Sender: TObject);
var
  Node: PVirtualNode;
  VL: TEpiCustomValueLabel;
begin
  ModalResult := mrNone;
  if not Assigned(FValueLabelSets) then exit;

  if TRim(ValueLabelNameEdit.Text) = '' then
  begin
    ShowHintMsg(Self, ValueLabelNameEdit, 'ValueLabel name must not be empty.');
    ValueLabelNameEdit.SetFocus;
    Exit;
  end;

  if (ValueLabelSet.Name <> ValueLabelNameEdit.Text) and
     (not FValueLabelSets.ValidateRename(nil, ValueLabelNameEdit.Text)) then
  begin
    ShowHintMsg(Self, ValueLabelNameEdit, 'A ValueLabel set with same name already exists.');
    ValueLabelNameEdit.SetFocus;
    Exit;
  end;

  if not FGridFrame.ValidateGridEntries then exit;
  FGridFrame.ValueLabelSet.Name := ValueLabelNameEdit.Text;

  ModalResult := mrOk;
end;

procedure TFieldValueLabelEditor.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  CanClose := true;
  if ManagerSettings.SaveWindowPositions then
    SaveFormPosition(Self, 'FieldValueLabelEditor');
end;

procedure TFieldValueLabelEditor.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) and (Shift = []) then
  begin
    // Do not catch key if grid is in editing state!
    if FGridFrame.VLG.IsEditing then exit;

    Key := VK_UNKNOWN;
    ModalResult := mrCancel
  end;
end;

procedure TFieldValueLabelEditor.FormShow(Sender: TObject);
begin
  ValueLabelNameEdit.Text := FGridFrame.ValueLabelSet.Name;
  ValueLabelNameEdit.Enabled := (ValueLabelSet.LabelScope = vlsInternal) and
                                (Authenticator.IsAuthorized([earDefineProject]));

  Caption := rsFormCaption + BoolToStr(ValueLabelSet.LabelScope = vlsInternal,
    '',
    ' (View Only)'
  );

  FGridFrame.VLG.SetFocus;

  if FGridFrame.ValueLabelSet.Count > 0 then // assume existing VL set.
    CancelBtn.Enabled := false
  else
    FGridFrame.NewLineBtn.Click;

  if ManagerSettings.SaveWindowPositions then
    LoadFormPosition(Self, 'FieldValueLabelEditor');
end;

procedure TFieldValueLabelEditor.ValueLabelNameEditEditingDone(Sender: TObject);
const
  InEditingDone: boolean = false;
begin
  if InEditingDone then exit;
  InEditingDone := true;

  if ValueLabelNameEdit.Modified and
     (ValueLabelSet.Name <> ValueLabelNameEdit.Text) and
     (not ValueLabelSets.ValidateRename(ValueLabelSet, ValueLabelNameEdit.Text))
  then
  begin
    ShowHintMsg(nil, ValueLabelNameEdit, 'A ValueLabel set with same name already exists.');
    ValueLabelNameEdit.SetFocus;
  end else
    FGridFrame.VLG.SetFocus;

  InEditingDone := false;
end;

procedure TFieldValueLabelEditor.ShowHintMsg(Sender: TObject; Ctrl: TControl;
  const Msg: String);
var
  R: TRect;
  P: TPoint;
begin
  if not Assigned(FHintWindow) then
    FHintWindow := THintWindow.Create(Self);

  if (Ctrl = nil) or (Msg = '') then
  begin
    FHintWindow.Hide;
    Exit;
  end;

  R := FHintWindow.CalcHintRect(0, Msg, nil);
  P := Ctrl.ClientToScreen(Point(Ctrl.Width + 5, 0));
  OffsetRect(R, P.X, P.Y);
  FHintWindow.ActivateHint(R, Msg);
end;

procedure TFieldValueLabelEditor.SetValueLabelSet(AValue: TEpiValueLabelSet);
begin
  if FGridFrame.ValueLabelSet = AValue then Exit;
  FGridFrame.ValueLabelSet := AValue;

{  if AValue.LabelScope = vlsExternal then
  begin

  end;}
end;

function TFieldValueLabelEditor.GetValueLabelSet: TEpiValueLabelSet;
begin
  result := FGridFrame.ValueLabelSet;
end;

constructor TFieldValueLabelEditor.Create(TheOwner: TComponent;
  ValueLabelSets: TEpiValueLabelSets);
begin
  inherited Create(TheOwner);
  FValueLabelSets := ValueLabelSets;

  ValueLabelNameEdit := TEdit.Create(Self);
  with ValueLabelNameEdit do
  begin
    AnchorToNeighbour(akLeft, 10, Label1);
    AnchorParallel(akRight, 10, Panel3);
    AnchorVerticalCenterTo(Panel3);
    Parent := Panel3;
    OnEditingDone := @ValueLabelNameEditEditingDone;
  end;

  FGridFrame := TValueLabelGridFrame.Create(Self);
  with FGridFrame do
  begin
    // Setup:
    OnShowHintMsg := @ShowHintMsg;
    Align := alClient;
    Parent := Panel2;
  end;
end;

class procedure TFieldValueLabelEditor.RestoreDefaultPos;
var
  Aform: TForm;
begin
  Aform := TForm.Create(nil);
  Aform.Width := 600;
  Aform.Height := 480;
  Aform.top := (Screen.Monitors[0].Height - Aform.Height) div 2;
  Aform.Left := (Screen.Monitors[0].Width - Aform.Width) div 2;
  SaveFormPosition(Aform, 'FieldValueLabelEditor');
  AForm.free;
end;

end.

