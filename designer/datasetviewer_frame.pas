unit datasetviewer_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Grids, ExtCtrls, StdCtrls,
  epidatafiles;

type

  { TDataSetViewFrame }

  TDataSetViewFrame = class(TFrame)
    Button1: TButton;
    ListGrid: TStringGrid;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure ListGridCompareCells(Sender: TObject; ACol, ARow, BCol,
      BRow: Integer; var Result: integer);
    procedure ListGridHeaderClick(Sender: TObject; IsColumn: Boolean;
      Index: Integer);
    procedure ListGridPrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
  private
    { private declarations }
    FDataFile: TEpiDataFile;
    FShowValueLabels: boolean;
    FSortCol: integer;
    procedure  UpdateGrid;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; Const DataFile: TEpiDataFile);
  end;

implementation

{$R *.lfm}

uses
  Graphics;

{ TDataSetViewFrame }

procedure TDataSetViewFrame.ListGridHeaderClick(Sender: TObject;
  IsColumn: Boolean; Index: Integer);
begin
  if not IsColumn then exit;

  FSortCol := Index;
  ListGrid.SortColRow(true, FSortCol);
end;

procedure TDataSetViewFrame.ListGridPrepareCanvas(sender: TObject; aCol,
  aRow: Integer; aState: TGridDrawState);
begin
  if (aRow = 0) and (aCol = FSortCol) then
    ListGrid.Canvas.Brush.Color := clSkyBlue;
end;

procedure TDataSetViewFrame.UpdateGrid;
var
  i: Integer;
  j: Integer;
begin
  ListGrid.BeginUpdate;

  for i := 0 to FDataFile.Fields.Count - 1 do
  with FDataFile.Fields[i] do
    ListGrid.Cells[i+1, 0] := Name;

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
  ListGrid.SortColRow(true, FSortCol);
  ListGrid.AutoSizeColumns;
  ListGrid.EndUpdate();
end;

procedure TDataSetViewFrame.ListGridCompareCells(Sender: TObject; ACol, ARow,
  BCol, BRow: Integer; var Result: integer);
begin
  if ACol <> BCol then exit;

  ARow := StrToInt(ListGrid.Cells[0, ARow]) - 1;
  BRow := StrToInt(ListGrid.Cells[0, BRow]) - 1;

  if ACol = 0 then
    result := ARow - BRow
  else
    result := FDataFile.Field[ACol - 1].Compare(ARow, BRow);
end;

procedure TDataSetViewFrame.Button1Click(Sender: TObject);
begin
  FShowValueLabels := not FShowValueLabels;
  UpdateGrid;
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
  ListGrid.ColCount := FDataFile.Fields.Count + 1;
  ListGrid.RowCount := FDataFile.Size + 1;

  ListGrid.Cells[0,0] := 'Record No:';
  UpdateGrid;
  ListGrid.Parent := Self;
end;

end.

