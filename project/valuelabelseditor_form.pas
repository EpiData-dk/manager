unit valuelabelseditor_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, ExtDlgs, Grids, ActnList, epivaluelabels, epidocument;

type

  { TValueLabelEditor }

  TValueLabelEditor = class(TForm)
    DeleteValueLabelSets: TAction;
    NewValueLabelSet: TAction;
    ActionList1: TActionList;
    Panel1: TPanel;
    Splitter1: TSplitter;
    ValueLabelsGrid: TStringGrid;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ValueLabelSetTreeView: TTreeView;
    procedure DeleteValueLabelSetsExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure NewValueLabelSetExecute(Sender: TObject);
    procedure ValueLabelsGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure ValueLabelSetTreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure ValueLabelSetTreeViewChanging(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
    procedure ValueLabelsGridValidateEntry(sender: TObject; aCol,
      aRow: Integer; const OldValue: string; var NewValue: String);
  private
    { private declarations }
    FLanguageSelector: TComboBox;
    FValueLabelSets: TEpiValueLabelSets;
    FCurrentVLSet: TEpiValueLabelSet;
    constructor Create(TheOwner: TComponent); override;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; EpiDoc: TEpiDocument);
  end; 


implementation

{$R *.lfm}

uses
  project_frame, LCLIntf, epidatafilestypes;

{ TValueLabelEditor }

procedure TValueLabelEditor.NewValueLabelSetExecute(Sender: TObject);
begin
  //
end;

procedure TValueLabelEditor.ValueLabelsGridDrawCell(Sender: TObject; aCol,
  aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  Grid: TStringGrid absolute sender;
  Style: LongInt;
begin
  if not ((aRow = 0) and (aCol = 3)) then
    Grid.DefaultDrawCell(aCol, aRow, aRect, aState)
  else begin
    FLanguageSelector.Parent := Grid;
    FLanguageSelector.AutoSize := false;
    FLanguageSelector.Style := csDropDownList;
//    FLanguageSelector.BorderStyle := bsNone;
    {$IFDEF MSWINDOWS}
    Style := LCLIntf.GetWindowLong(FLanguageSelector.Handle, LCLIntf.GWL_EXSTYLE);
    Style := Style + LCLIntf.WS_EX_STATICEDGE;
    LCLIntf.SetWindowLong(FLanguageSelector.Handle, GWL_EXSTYLE, Style);
    {$ENDIF}
    FLanguageSelector.Left := aRect.Left;
    FLanguageSelector.Top := aRect.Top;
    FLanguageSelector.Width := aRect.Right - aRect.Left;
    FLanguageSelector.Height := aRect.Bottom - aRect.Top;
  end;
end;

procedure TValueLabelEditor.ValueLabelSetTreeViewChange(Sender: TObject;
  Node: TTreeNode);
var
  TreeView: TTreeView absolute Sender;
  i: Integer;
begin
  // Happens after the change is done.
  if csDestroying in TreeView.ComponentState then exit;

  FCurrentVLSet := TEpiValueLabelSet(Node.Data);
  with ValueLabelsGrid do
  begin
    BeginUpdate;
    RowCount := FCurrentVLSet.Count + 1;
    for i := 0 to FCurrentVLSet.Count - 1 do
    begin
      Cells[0, i + 1] := IntToStr(FCurrentVLSet[i].Order);
      Cells[1, i + 1] := FCurrentVLSet[i].ValueAsString;
      Cells[2, i + 1] := FCurrentVLSet[i].TheLabel.Text;
      Cells[3, i + 1] := FCurrentVLSet[i].TheLabel.TextLang['fr'];
    end;
    EndUpdate;
  end;
end;

procedure TValueLabelEditor.ValueLabelSetTreeViewChanging(Sender: TObject;
  Node: TTreeNode; var AllowChange: Boolean);
var
  TreeView: TTreeView absolute Sender;
begin
  // Happens before change is done.
  if csDestroying in TreeView.ComponentState then exit;
end;

procedure TValueLabelEditor.ValueLabelsGridValidateEntry(sender: TObject; aCol,
  aRow: Integer; const OldValue: string; var NewValue: String);
begin
  // Value change
  if aCol = 1 then
  begin
    // NewValue must not exists!
    if FCurrentVLSet.ValueLabel[NewValue] <> NewValue then
      NewValue := OldValue;

    case FCurrentVLSet.LabelType of
      ftInteger: TEpiIntValueLabel(FCurrentVLSet[aRow - 1]).Value := StrToInt(NewValue);
      ftFloat:   TEpiFloatValueLabel(FCurrentVLSet[aRow - 1]).Value := StrToFloat(NewValue);
      ftString:  TEpiStringValueLabel(FCurrentVLSet[aRow - 1]).Value := NewValue;
    end;
    Exit;
  end;

  // Label change
  if aCol = 2 then
  begin
    FCurrentVLSet[aRow - 1].TheLabel.Text := NewValue;
    Exit;
  end;

  // Label (tranlation) change
  if aCol = 3 then
  begin
    // TODO : Language
    FCurrentVLSet[aRow - 1].TheLabel.TextLang['fr'] := NewValue;
    Exit;
  end;

end;

constructor TValueLabelEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FLanguageSelector := TComboBox.Create(Self);
end;

constructor TValueLabelEditor.Create(TheOwner: TComponent; EpiDoc: TEpiDocument
  );
var
  i: Integer;
begin
  Create(TheOwner);
  FValueLabelSets := EpiDoc.DataFiles.ValueLabelSets;

  for i := 0 to FValueLabelSets.Count - 1 do
    ValueLabelSetTreeView.Items.AddObject(nil, FValueLabelSets[i].Name, FValueLabelSets[i]);

  ValueLabelsGrid.Cells[0,0] := 'Order';
  ValueLabelsGrid.Cells[1,0] := 'Value';
  ValueLabelsGrid.Cells[2,0] := 'Label';

  // TODO : Extract languages correctly.
  FLanguageSelector.Items.Add('Label (fr)');
  FLanguageSelector.Items.Add('Label (da)');
  FLanguageSelector.Items.Add('Label (de)');
end;

procedure TValueLabelEditor.DeleteValueLabelSetsExecute(Sender: TObject);
begin
  //
end;

procedure TValueLabelEditor.FormShow(Sender: TObject);
begin
  ToolBar1.Images := TProjectFrame(Owner).ActionList1.Images;
  ActionList1.Images := ToolBar1.Images;
  NewValueLabelSet.ImageIndex := 3;
  DeleteValueLabelSets.ImageIndex := 4;

  ValueLabelSetTreeView.Selected := ValueLabelSetTreeView.Items[0];
end;

end.

