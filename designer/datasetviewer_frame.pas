unit datasetviewer_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Grids, ExtCtrls, StdCtrls,
  ActnList, epidatafiles;

type

  { TDataSetViewFrame }

  TDataSetViewFrame = class(TFrame)
    ShowIndexOrAllFieldsAction: TAction;
    Button3: TButton;
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
    procedure ShowIndexOrAllFieldsActionExecute(Sender: TObject);
    procedure ShowIndexOrAllFieldsActionUpdate(Sender: TObject);
    procedure ShowValuesOrLabelsActionExecute(Sender: TObject);
    procedure SortByIndexActionExecute(Sender: TObject);
    procedure SortByIndexActionUpdate(Sender: TObject);
  private
    { private declarations }
    FDataFile: TEpiDataFile;
    FDisplayFields: TEpiFields;
    FKeyFields: TEpiFields;
    FShowValueLabels: boolean;
    FShowAllFields: boolean;
    FSortCol: integer;
    FRecords: TBoundArray;
    function  GetKeyFields: TEpiFields;
    procedure SetDisplayFields(AValue: TEpiFields);
    procedure SetKeyFields(AValue: TEpiFields);
    procedure UpdateGrid;
    procedure GridColumnSort(Sender: TObject; ACol, ARow, BCol,
      BRow: Integer; var Result: integer);
    procedure  GridIndexSort(Sender: TObject; ACol, ARow, BCol,
      BRow: Integer; var Result: integer);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; Const DataFile: TEpiDataFile);
    procedure   ShowRecords(const Records: TBoundArray);
    property    KeyFields: TEpiFields read GetKeyFields write SetKeyFields;
    property    DisplayFields: TEpiFields read FDisplayFields write SetDisplayFields;
  end;


procedure ShowDataSetViewerForm(TheOwner: TComponent;
  const FormCaption: string;
  Const DataFile: TEpiDataFile;
  const Records: TBoundArray = nil;
  const KeyFields: TEpiFields = nil;
  const DisplayFields: TEpiFields = nil;
  const SortFieldNo: Integer = 0; // -1 = Index | 0 = Rec no| 1+ = Field...
  const ShowIndexFields: boolean = false
  );

implementation

{$R *.lfm}

uses
  Graphics, math, settings2, settings2_var, manager_globals;

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

procedure TDataSetViewFrame.ShowIndexOrAllFieldsActionExecute(Sender: TObject);
begin
  FShowAllFields := not FShowAllFields;
  UpdateGrid;
end;

procedure TDataSetViewFrame.ShowIndexOrAllFieldsActionUpdate(Sender: TObject);
begin
  ShowIndexOrAllFieldsAction.Enabled :=
    Assigned(FKeyFields) and (FKeyFields.Count > 0);
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
  TAction(Sender).Enabled := (Assigned(FKeyFields)) and (FKeyFields.Count > 0);
end;

procedure TDataSetViewFrame.UpdateGrid;
var
  i: Integer;
  j: Integer;
  Fields: TEpiFields;
  F: TEpiField;

  procedure AssignFields(ToFields, FromFields: TEpiFields);
  var
    i: integer;
  begin
    for i := 0 to FromFields.Count - 1 do
      ToFields.AddItem(FromFields[i]);
  end;

begin
  Fields := TEpiFields.Create(nil);

  if FShowAllFields then
    AssignFields(Fields, FDisplayFields)
  else begin
    AssignFields(Fields, FKeyFields);
    F := FDataFile.Fields.FieldByName[IndexIntegrityFieldName];
    if Assigned(F) then
      Fields.AddItem(F);
  end;

  ListGrid.BeginUpdate;

  ListGrid.ColCount := Fields.Count + 1;
  if Length(FRecords) > 0 then
    ListGrid.RowCount := Length(FRecords) + 1
  else
    ListGrid.RowCount := FDataFile.Size + 1;

  for i := 0 to Fields.Count - 1 do
  with Fields[i] do
    ListGrid.Cells[i+1, 0] := Name;

  if Length(FRecords) > 0 then
  begin
    for i := 0 to Length(FRecords) - 1 do
    begin
      ListGrid.Cells[0, i + 1] := IntToStr(FRecords[i] + 1);

      for j := 0 to Fields.Count - 1 do
      with Fields[j] do
        if (FShowValueLabels) and (Assigned(ValueLabelSet)) and (not IsMissing[FRecords[i]]) then
          ListGrid.Cells[j + 1, i + 1] := ValueLabelSet.ValueLabelString[AsValue[FRecords[i]]]
        else
          ListGrid.Cells[j + 1, i + 1] := AsString[FRecords[i]];
    end;
  end else begin
    for i := 0 to FDataFile.Size - 1 do
    begin
      ListGrid.Cells[0, i + 1] := IntToStr(i + 1);

      for j := 0 to Fields.Count - 1 do
      with Fields[j] do
        if (FShowValueLabels) and (Assigned(ValueLabelSet)) and (not IsMissing[i]) then
          ListGrid.Cells[j + 1, i + 1] := ValueLabelSet.ValueLabelString[AsValue[i]]
        else
          ListGrid.Cells[j + 1, i + 1] := AsString[i];
    end;
  end;
  if FSortCol > (ListGrid.ColCount - 1) then
    FSortCol := 0;
  ListGrid.SortColRow(true, Max(FSortCol,0));
  ListGrid.AutoSizeColumns;
  ListGrid.EndUpdate();
  Fields.Free;
end;

function TDataSetViewFrame.GetKeyFields: TEpiFields;
begin
  result := FKeyFields;
end;

procedure TDataSetViewFrame.SetDisplayFields(AValue: TEpiFields);
begin
  if Assigned(AValue) then
    FDisplayFields := AValue;
end;

procedure TDataSetViewFrame.SetKeyFields(AValue: TEpiFields);
begin
  if Assigned(AValue) then
    FKeyFields := AValue
  else
    FKeyFields := FDataFile.KeyFields;
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
  if not FKeyFields.Count = 0 then exit;

  ARow := StrToInt(ListGrid.Cells[0, ARow]) - 1;
  BRow := StrToInt(ListGrid.Cells[0, BRow]) - 1;

  for i := 0 to FKeyFields.Count - 1 do
  begin
    result := FKeyFields[i].Compare(ARow, BRow);
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
  FKeyFields := FDataFile.KeyFields;
  FDisplayFields := FDataFile.Fields;
  FShowValueLabels := false;
  FShowAllFields := true;
  FSortCol := 0;

  ListGrid.Align := alClient;
  ListGrid.Cells[0,0] := 'Record No:';
  ListGrid.OnCompareCells := @GridColumnSort;
  UpdateGrid;
  ListGrid.Parent := Self;
end;

procedure TDataSetViewFrame.ShowRecords(const Records: TBoundArray);
begin
  FRecords := Records;
  UpdateGrid;
end;

procedure ShowDataSetViewerForm(TheOwner: TComponent;
  const FormCaption: string; const DataFile: TEpiDataFile;
  const Records: TBoundArray; const KeyFields: TEpiFields;
  const DisplayFields: TEpiFields; const SortFieldNo: Integer;
  const ShowIndexFields: boolean);
const
  FormName = 'DataSetViewerForm';
var
  F: TForm;
  V: TDataSetViewFrame;
begin
  F := TForm.Create(TheOwner);
  F.Caption := FormCaption;

  V := TDataSetViewFrame.Create(F, DataFile);
  V.Align := alClient;
  V.Parent := F;
  if Assigned(KeyFields) then
    V.KeyFields := KeyFields;
  if Assigned(Records) then
    V.ShowRecords(Records);
  if Assigned(DisplayFields) then
    V.DisplayFields := DisplayFields;

  if SortFieldNo = -1 then
    V.SortByIndexAction.Execute
  else
    V.ListGridHeaderClick(nil, true, SortFieldNo);

  if ShowIndexFields then
    V.FShowAllFields := false;

  V.UpdateGrid;

  if ManagerSettings.SaveWindowPositions then
    LoadFormPosition(F, FormName);
  F.ShowModal;
  if ManagerSettings.SaveWindowPositions then
    SaveFormPosition(F, FormName);
  F.Free;
end;

end.

