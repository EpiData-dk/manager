unit field_valuelabelseditor_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Buttons, VirtualTrees, epivaluelabels,
  epidatafilestypes, LCLType, valuelabelgrid_frame;

type

  { TFieldValueLabelEditor }

  TFieldValueLabelEditor = class(TForm)
    Button1: TButton;
    CancelBtn: TBitBtn;
    OkBtn: TBitBtn;
    Panel2: TPanel;
    ValueLabelNameEdit: TEdit;
    Label1: TLabel;
    Panel1: TPanel;
    Panel3: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ValueLabelNameEditEditingDone(Sender: TObject);
  private
    FGridFrame: TValueLabelGridFrame;
    FHintWindow: THintWindow;
    FResultValueLabelSet: TEpiValueLabelSet;
    FValueLabelSets: TEpiValueLabelSets;
    FFieldType: TEpiFieldType;
    procedure SetValueLabelSets(AValue: TEpiValueLabelSets);
    procedure ShowHintMsg(Sender: TObject; Ctrl: TControl; Const Msg: String);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; AValueLabelSet: TEpiValueLabelSets; FieldType: TEpiFieldType);
    property  ValueLabelSets: TEpiValueLabelSets read FValueLabelSets;
    property  ResultValueLabelSet: TEpiValueLabelSet read FResultValueLabelSet;
  end;

implementation

{$R *.lfm}

uses
  LCLIntf, LMessages, valuelabelseditor_form, epidocument;

  { TFieldValueLabelEditor }

procedure TFieldValueLabelEditor.FormCreate(Sender: TObject);
begin
  FGridFrame := TValueLabelGridFrame.Create(Self);
  with FGridFrame do
  begin
    // Setup:
    OnShowHintMsg := @ShowHintMsg;
    Align := alClient;
    Parent := Panel2;
    ValueLabelSet := FValueLabelSets.NewValueLabelSet(FFieldType);
  end;
end;

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

  if not FValueLabelSets.ValidateRename(nil, ValueLabelNameEdit.Text) then
  begin
    ShowHintMsg(Self, ValueLabelNameEdit, 'A ValueLabel set with same name already exists.');
    ValueLabelNameEdit.SetFocus;
    Exit;
  end;

  if not FGridFrame.ValidateGridEntries then exit;

  FResultValueLabelSet := FGridFrame.ValueLabelSet;
  FResultValueLabelSet.Name := ValueLabelNameEdit.Text;

  ModalResult := mrOk;
end;

procedure TFieldValueLabelEditor.Button1Click(Sender: TObject);
begin
  GetValueLabelsEditor(TEpiDocument(FValueLabelSets.RootOwner)).Show;
  BringToFront;
end;

procedure TFieldValueLabelEditor.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if ModalResult <> mrOk then
    FGridFrame.ValueLabelSet.Free;
end;

procedure TFieldValueLabelEditor.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) and (Shift = []) then
  begin
    // Do not catch key if grid is in editing state!
    if FGridFrame.VLG.IsEditing then exit;

    Key := VK_UNKNOWN;
    ModalResult := mrCancel;
  end;
end;

procedure TFieldValueLabelEditor.FormShow(Sender: TObject);
begin
  FGridFrame.NewLineBtn.Click;
  ValueLabelNameEdit.SetFocus;
end;

procedure TFieldValueLabelEditor.ValueLabelNameEditEditingDone(Sender: TObject);
const
  InEditingDone: boolean = false;
begin
  if InEditingDone then exit;
  InEditingDone := true;

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

procedure TFieldValueLabelEditor.SetValueLabelSets(AValue: TEpiValueLabelSets);
begin
  if FValueLabelSets = AValue then Exit;
  FValueLabelSets := AValue;
end;

constructor TFieldValueLabelEditor.Create(TheOwner: TComponent;
  AValueLabelSet: TEpiValueLabelSets; FieldType: TEpiFieldType);
begin
  inherited Create(TheOwner);
  FValueLabelSets := AValueLabelSet;
  FFieldType := FieldType;
end;

end.

