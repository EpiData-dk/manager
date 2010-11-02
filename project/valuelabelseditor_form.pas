unit valuelabelseditor_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, ExtDlgs, Grids, ActnList, Menus, epivaluelabels,
  epidocument, epidatafilestypes, epicustombase, LMessages;

type

  { TValueLabelEditor }

  TValueLabelEditor = class(TForm)
    DeleteValueLabelSets: TAction;
    NewIntValueLabelSetMenuItem: TMenuItem;
    NewStringValueLabelSetMenuItem: TMenuItem;
    NewFloatValueLabelSetMenuItem: TMenuItem;
    NewValueLabelSet: TAction;
    ActionList1: TActionList;
    Panel1: TPanel;
    NewValueLabelSetTypeDropDown: TPopupMenu;
    Splitter1: TSplitter;
    ValueLabelsGrid: TStringGrid;
    ToolBar1: TToolBar;
    NewValueLabelSetToolBtl: TToolButton;
    DeleteValueLabelSetsToolBtn: TToolButton;
    ValueLabelSetTreeView: TTreeView;
    procedure DeleteValueLabelSetsExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure NewFloatValueLabelSetMenuItemClick(Sender: TObject);
    procedure NewIntValueLabelSetMenuItemClick(Sender: TObject);
    procedure NewStringValueLabelSetMenuItemClick(Sender: TObject);
    procedure NewValueLabelSetExecute(Sender: TObject);
    procedure ValueLabelSetTreeViewEdited(Sender: TObject; Node: TTreeNode;
      var S: string);
    procedure ValueLabelsGridColRowMoved(Sender: TObject; IsColumn: Boolean;
      sIndex, tIndex: Integer);
    procedure ValueLabelSetTreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure ValueLabelSetTreeViewChanging(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
    procedure ValueLabelsGridHeaderClick(Sender: TObject; IsColumn: Boolean;
      Index: Integer);
    procedure ValueLabelsGridValidateEntry(sender: TObject; aCol,
      aRow: Integer; const OldValue: string; var NewValue: String);
  private
    { private declarations }
    FLanguageSelector: TComboBox;
    FValueLabelSets: TEpiValueLabelSets;
    FCurrentVLSet: TEpiValueLabelSet;
    constructor Create(TheOwner: TComponent); override;
    function    DoNewValueLabelSet(Ft: TEpiFieldType): TEpiValueLabelSet;
    procedure   UpdateGridCells;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; EpiDoc: TEpiDocument);
  end; 


implementation

{$R *.lfm}

uses
  project_frame, LCLIntf, LCLType;

{ TValueLabelEditor }

procedure TValueLabelEditor.NewValueLabelSetExecute(Sender: TObject);
begin
  DoNewValueLabelSet(ftInteger); // TODO : Default new valuelabelset???
end;

procedure TValueLabelEditor.ValueLabelSetTreeViewEdited(Sender: TObject;
  Node: TTreeNode; var S: string);
var
  VLSet: TEpiValueLabelSet;
begin
  VLSet := TEpiValueLabelSet(Node.Data);
  VLSet.Name := S;
  Node.Text := S;
  ValueLabelSetTreeView.CustomSort(nil);
//  ValueLabelSetTreeView.Selected := node;
end;

procedure TValueLabelEditor.ValueLabelsGridColRowMoved(Sender: TObject;
  IsColumn: Boolean; sIndex, tIndex: Integer);
var
  Tmp: TEpiCustomValueLabel;
  TmpOrder: LongInt;
begin
  Dec(sIndex); Dec(tIndex);

  Tmp := FCurrentVLSet[sIndex];
  TmpOrder := FCurrentVLSet[tIndex].Order;
  FCurrentVLSet.Items[sIndex] := FCurrentVLSet[tIndex];
  FCurrentVLSet[sIndex].Order := Tmp.Order;
  Tmp.Order := TmpOrder;
  FCurrentVLSet.Items[tIndex] := Tmp;
  UpdateGridCells;
end;

procedure TValueLabelEditor.ValueLabelSetTreeViewChange(Sender: TObject;
  Node: TTreeNode);
var
  TreeView: TTreeView absolute Sender;
  i: Integer;
begin
  // Happens after the change is done.
  if csDestroying in TreeView.ComponentState then exit;

  if not Assigned(Node) then
  begin
    ValueLabelsGrid.RowCount := 1;
    exit;
  end;

  FCurrentVLSet := TEpiValueLabelSet(Node.Data);
  UpdateGridCells;
end;

procedure TValueLabelEditor.ValueLabelSetTreeViewChanging(Sender: TObject;
  Node: TTreeNode; var AllowChange: Boolean);
var
  TreeView: TTreeView absolute Sender;
begin
  // Happens before change is done.
  if csDestroying in TreeView.ComponentState then exit;
end;

procedure TValueLabelEditor.ValueLabelsGridHeaderClick(Sender: TObject;
  IsColumn: Boolean; Index: Integer);
begin
  if not IsColumn then exit;
  if not (Index = 3) then exit;

  ShowMessage('Labels');
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

function TValueLabelEditor.DoNewValueLabelSet(Ft: TEpiFieldType
  ): TEpiValueLabelSet;
begin
  result := FValueLabelSets.NewValueLabelSet(Ft);
  result.Name := '(untitled)';
  ValueLabelSetTreeView.Selected := ValueLabelSetTreeView.Items.AddObject(nil, result.Name, Result);
  ValueLabelSetTreeView.CustomSort(nil);
end;

procedure TValueLabelEditor.UpdateGridCells;
var
  i: Integer;
begin
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
      Cells[4, i + 1] := BoolToStr(FCurrentVLSet[i].IsMissingValue, '1', '0');
    end;
    AutoSizeColumns;
    EndUpdate;
  end;
end;

constructor TValueLabelEditor.Create(TheOwner: TComponent; EpiDoc: TEpiDocument
  );
var
  i: Integer;
begin
  Create(TheOwner);
  FValueLabelSets := EpiDoc.DataFiles.ValueLabelSets;

  ValueLabelSetTreeView.SortType := stText;
  for i := 0 to FValueLabelSets.Count - 1 do
    ValueLabelSetTreeView.Items.AddObject(nil, FValueLabelSets[i].Name, FValueLabelSets[i]);
  ValueLabelSetTreeView.CustomSort(nil);

  ValueLabelsGrid.Cells[0,0] := 'Order';

  // TODO : Extract languages correctly.
  FLanguageSelector.Items.Add('Label (fr)');
  FLanguageSelector.Items.Add('Label (da)');
  FLanguageSelector.Items.Add('Label (de)');
end;

procedure TValueLabelEditor.DeleteValueLabelSetsExecute(Sender: TObject);
var
  Node: TTreeNode;
  VLSet: TEpiValueLabelSet;
  NNode: TTreeNode;
begin
  Node := ValueLabelSetTreeView.Selected;
  VLSet := TEpiValueLabelSet(Node.Data);
  FValueLabelSets.RemoveItem(VLSet);
  VLSet.Free;
  NNode := Node.GetNext;
  if not Assigned(NNode) then
    NNode := Node.GetPrev;
  ValueLabelSetTreeView.Items.Delete(Node);
  ValueLabelSetTreeView.Selected := NNode;
end;

procedure TValueLabelEditor.FormShow(Sender: TObject);
begin
  ToolBar1.Images := TProjectFrame(Owner).ActionList1.Images;
  ActionList1.Images := ToolBar1.Images;
  NewValueLabelSet.ImageIndex := 3;
  DeleteValueLabelSets.ImageIndex := 4;

  if ValueLabelSetTreeView.Items.Count > 0 then
    ValueLabelSetTreeView.Selected := ValueLabelSetTreeView.Items[0];
end;

procedure TValueLabelEditor.NewFloatValueLabelSetMenuItemClick(Sender: TObject);
begin
  DoNewValueLabelSet(ftFloat);
end;

procedure TValueLabelEditor.NewIntValueLabelSetMenuItemClick(Sender: TObject);
begin
  DoNewValueLabelSet(ftInteger);
end;

procedure TValueLabelEditor.NewStringValueLabelSetMenuItemClick(Sender: TObject
  );
begin
  DoNewValueLabelSet(ftString);
end;

end.

