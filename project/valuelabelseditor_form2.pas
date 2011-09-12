unit valuelabelseditor_form2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Buttons, VirtualTrees, epivaluelabels,
  epidatafilestypes, LCLType, valuelabelgrid_frame;

type

  { TValuelabelEditor2 }

  TValuelabelEditor2 = class(TForm)
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
    procedure  ShowHintMsg(Ctrl: TControl; Msg: String);
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

  { TValuelabelEditor2 }

procedure TValuelabelEditor2.FormCreate(Sender: TObject);
begin
  FGridFrame := TValueLabelGridFrame.Create(Self);
  with FGridFrame do
  begin
    // Setup:
    Align := alClient;
    Parent := Panel2;
    ValueLabelSet := FValueLabelSets.NewValueLabelSet(FFieldType);
  end;
end;

procedure TValuelabelEditor2.OkBtnClick(Sender: TObject);
var
  Node: PVirtualNode;
  VL: TEpiCustomValueLabel;
begin
  ModalResult := mrNone;
  if not Assigned(FValueLabelSets) then exit;

  if not FValueLabelSets.ValidateRename(nil, ValueLabelNameEdit.Text) then
  begin
    ShowHintMsg(ValueLabelNameEdit, 'A ValueLabel set with same name already exists.');
    ValueLabelNameEdit.SetFocus;
    Exit;
  end;

  if not FGridFrame.ValidateGridEntries then exit;

  FResultValueLabelSet := FGridFrame.ValueLabelSet;
  FResultValueLabelSet.Name := ValueLabelNameEdit.Text;

  ModalResult := mrOk;
end;

procedure TValuelabelEditor2.Button1Click(Sender: TObject);
begin
  GetValueLabelsEditor(TEpiDocument(FValueLabelSets.RootOwner)).Show;
  BringToFront;
end;

procedure TValuelabelEditor2.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if ModalResult <> mrOk then
    FGridFrame.ValueLabelSet.Free;
end;

procedure TValuelabelEditor2.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) and (Shift = []) then
  begin
    Key := VK_UNKNOWN;
    ModalResult := mrCancel;
  end;
end;

procedure TValuelabelEditor2.FormShow(Sender: TObject);
begin
  FGridFrame.NewLineBtn.Click;
  ValueLabelNameEdit.SetFocus;
end;

procedure TValuelabelEditor2.ValueLabelNameEditEditingDone(Sender: TObject);
const
  InEditingDone: boolean = false;
begin
  if InEditingDone then exit;
  InEditingDone := true;

  FGridFrame.VLG.SetFocus;

  InEditingDone := false;
end;

procedure TValuelabelEditor2.ShowHintMsg(Ctrl: TControl; Msg: String);
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
  P := Ctrl.ControlToScreen(Ctrl.BoundsRect.TopLeft);
  OffsetRect(R, P.X, P.Y);
  FHintWindow.ActivateHint(R, Msg);
end;

procedure TValuelabelEditor2.SetValueLabelSets(AValue: TEpiValueLabelSets);
begin
  if FValueLabelSets = AValue then Exit;
  FValueLabelSets := AValue;
end;

constructor TValuelabelEditor2.Create(TheOwner: TComponent;
  AValueLabelSet: TEpiValueLabelSets; FieldType: TEpiFieldType);
begin
  inherited Create(TheOwner);
  FValueLabelSets := AValueLabelSet;
  FFieldType := FieldType;
end;

end.

