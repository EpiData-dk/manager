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
    constructor Create(TheOwner: TComponent; FieldType: TEpiFieldType);
    property  ValueLabelSets: TEpiValueLabelSets read FValueLabelSets write SetValueLabelSets;
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
  end;
end;

procedure TValuelabelEditor2.OkBtnClick(Sender: TObject);
var
  Node: PVirtualNode;
  VL: TEpiCustomValueLabel;
begin
  ModalResult := mrNone;
{  if not Assigned(FValueLabelSets) then exit;

  if not FValueLabelSets.ValidateRename(nil, ValueLabelNameEdit.Text) then
  begin
    ValueLabelNameEdit.SetFocus;
    Exit;
  end;


  Node := VLSt.GetFirstChild(nil);
  While Assigned(Node) do
  begin
    with PVLRecord(VLST.GetNodeData(Node))^ do
    begin
      // Validate node data?
    end;
    Node := VLSt.GetNextSibling(Node);
  end;

  FResultValueLabelSet := FValueLabelSets.NewValueLabelSet(FFieldType);
  FResultValueLabelSet.Name := ValueLabelNameEdit.Text;

  Node := VLSt.GetFirstChild(nil);
  While Assigned(Node) do
  begin
    VL := FResultValueLabelSet.NewValueLabel;
    with PVLRecord(VLST.GetNodeData(Node))^ do
    begin
      case FFieldType of
        ftInteger:  TEpiIntValueLabel(VL).Value := StrToInt(Value);
        ftFloat:    TEpiFloatValueLabel(VL).Value := StrToFloat(Value);
        ftString:   TEpiStringValueLabel(VL).Value := Value;
      end;
      VL.TheLabel.Text := VLabel;
      VL.IsMissingValue := Missing;
    end;
    Node := VLSt.GetNextSibling(Node);
  end;

  ModalResult := mrOk;   }
end;

procedure TValuelabelEditor2.Button1Click(Sender: TObject);
begin
  GetValueLabelsEditor(TEpiDocument(FValueLabelSets.RootOwner)).Show;
  BringToFront;
end;

procedure TValuelabelEditor2.FormShow(Sender: TObject);
begin
//  DoAddNewLine;
end;

procedure TValuelabelEditor2.ValueLabelNameEditEditingDone(Sender: TObject);
const
  InEditingDone: boolean = false;
begin
  if InEditingDone then exit;
  InEditingDone := true;

  FGridFrame.SetFocus;

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
  FieldType: TEpiFieldType);
begin
  inherited Create(TheOwner);
  FFieldType := FieldType;
end;

end.

