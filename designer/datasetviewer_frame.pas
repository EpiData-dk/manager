unit datasetviewer_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Grids, ExtCtrls, StdCtrls,
  ActnList, epidatafiles;

type

  { TDataSetViewFrame }

  TDataSetViewFrame = class(TFrame)
    SortByIndexAction: TAction;
    ShowValuesOrLabelsAction: TAction;
    ActionList1: TActionList;
    Button1: TButton;
    Button2: TButton;
    ListGrid: TStringGrid;
    Panel1: TPanel;
    procedure ListGridHeaderClick(Sender: TObject; IsColumn: Boolean;
      Index: Integer);
    procedure ListGridPrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure ShowValuesOrLabelsActionExecute(Sender: TObject);
    procedure SortByIndexActionExecute(Sender: TObject);
    procedure SortByIndexActionUpdate(Sender: TObject);
  private
    { private declarations }
    FDataFile: TEpiDataFile;
    FShowValueLabels: boolean;
    FSortCol: integer;
    FRecords: TBoundArray;
    procedure  UpdateGrid;
    procedure  GridColumnSort(Sender: TObject; ACol, ARow, BCol,
      BRow: Integer; var Result: integer);
    procedure  GridIndexSort(Sender: TObject; ACol, ARow, BCol,
      BRow: Integer; var Result: integer);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; Const DataFile: TEpiDataFile);
    procedure   ShowRecords(const Records: TBoundArray);
  end;

implementation

{$R *.lfm}

uses
  Graphics, math;

{ TDataSetViewFrame }

procedure TDataSetViewFrame.ListGridHeaderClick(Sender: TObject;
  IsColumn: Boolean; Index: Integer);
begin
  if not IsColumn then exit;
  if FSortCol = Index then exit;

  FSortCol := Index;
  ListGrid.OnCompareCells := @GridColumnSort;
  ListGrid.SortColRow(true, FSortCol);
end;

procedure TDataSetViewFrame.ListGridPrepareCanvas(sender: TObject; aCol,
  aRow: Integer; aState: TGridDrawState);
begin
  if (aRow = 0) and (aCol = FSortCol) then
    ListGrid.Canvas.Brush.Color := clSkyBlue;
end;

procedure TDataSetViewFrame.ShowValuesOrLabelsActionExecute(Sender: TObject);
begin
  FShowValueLabels := not FShowValueLabels;
  UpdateGrid;
end;

procedure TDataSetViewFrame.SortByIndexActionExecute(Sender: TObject);
begin
  if FSortCol = -1 then exit;

  FSortCol := -1;
  ListGrid.OnCompareCells := @GridIndexSort;
  ListGrid.SortColRow(true, 0);
end;

procedure TDataSetViewFrame.SortByIndexActionUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := FDataFile.KeyFields.Count > 0;
end;

procedure TDataSetViewFrame.UpdateGrid;
var
  i: Integer;
  j: Integer;
begin
  ListGrid.BeginUpdate;

  ListGrid.ColCount := FDataFile.Fields.Count + 1;
  if Length(FRecords) > 0 then
    ListGrid.RowCount := Length(FRecords) + 1
  else
    ListGrid.RowCount := FDataFile.Size + 1;

  for i := 0 to FDataFile.Fields.Count - 1 do
  with FDataFile.Fields[i] do
    ListGrid.Cells[i+1, 0] := Name;

  if Length(FRecords) > 0 then
  begin
    for i := 0 to Length(FRecords) - 1 do
    begin
      ListGrid.Cells[0, i + 1] := IntToStr(FRecords[i] + 1);

      for j := 0 to FDataFile.Fields.Count - 1 do
      with FDataFile.Fields[j] do
        if (FShowValueLabels) and (Assigned(ValueLabelSet)) and (not IsMissing[FRecords[i]]) then
          ListGrid.Cells[j + 1, i + 1] := ValueLabelSet.ValueLabelString[AsValue[FRecords[i]]]
        else
          ListGrid.Cells[j + 1, i + 1] := AsString[FRecords[i]];
    end;
  end else begin
    for i := 0 to FDataFile.Size - 1 do
    begin
      ListGrid.Cells[0, i + 1] := IntToStr(i + 1);

      for j := 0 to FDataFile.Fields.Count - 1 do
      with FDataFile.Fields[j] do
        if (FShowValueLabels) and (Assigned(ValueLabelSet)) and (not IsMissing[i]) then
          ListGrid.Cells[j + 1, i + 1] := ValueLabelSet.ValueLabelString[AsValue[i]]
        else
          ListGrid.Cells[j + 1, i + 1] := AsString[i];
    end;
  end;
  ListGrid.SortColRow(true, Max(FSortCol,0));
  ListGrid.AutoSizeColumns;
  ListGrid.EndUpdate();
end;

procedure TDataSetViewFrame.GridColumnSort(Sender: TObject; ACol, ARow, BCol,
  BRow: Integer; var Result: integer);
begin
  if ACol <> BCol then exit;

  ARow := StrToInt(ListGrid.Cells[0, ARow]) - 1;
  BRow := StrToInt(ListGrid.Cells[0, BRow]) - 1;

  if ACol = 0 then
    result := ARow - BRow
  else
    result := FDataFile.Field[ACol - 1].Compare(ARow, BRow);
end;

procedure TDataSetViewFrame.GridIndexSort(Sender: TObject; ACol, ARow, BCol,
  BRow: Integer; var Result: integer);
var
  i: Integer;
begin
  result := 0;
  if not FDataFile.KeyFields.Count = 0 then exit;

  ARow := StrToInt(ListGrid.Cells[0, ARow]) - 1;
  BRow := StrToInt(ListGrid.Cells[0, BRow]) - 1;

  for i := 0 to FDataFile.KeyFields.Count - 1 do
  begin
    result := FDataFile.KeyFields[i].Compare(ARow, BRow);
    if result <> 0 then break;
  end;
end;

constructor TDataSetViewFrame.Create(TheOwner: TComponent;
  const DataFile: TEpiDataFile);
var
  i: Integer;
  j: Integer;
begin
  inherited Create(TheOwner);
  FDataFile := DataFile;
  FShowValueLabels := false;
  FSortCol := 0;

  ListGrid.Align := alClient;
  ListGrid.Cells[0,0] := 'Record No:';
  ListGrid.OnCompareCells := @GridIndexSort;
  UpdateGrid;
  ListGrid.Parent := Self;
end;

procedure TDataSetViewFrame.ShowRecords(const Records: TBoundArray);
begin
  FRecords := Records;
  UpdateGrid;
end;

end.

