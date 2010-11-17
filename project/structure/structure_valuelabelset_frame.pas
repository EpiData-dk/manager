unit structure_valuelabelset_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, Grids,
  epivaluelabels, structure_form;

type

  { TProject_Structure_ValueLabelSet_Frame }

  TProject_Structure_ValueLabelSet_Frame = class(TFrame, IProject_Structure_Frame)
    ValueLabelsGrid: TStringGrid;
    ValueLabelSetNameLabel: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    NumberOfMissingsLabel: TLabel;
    NumberOfValueLabelsLabel: TLabel;
    Panel1: TPanel;
  private
    FValueLabelSet: TEpiValueLabelSet;
    { private declarations }
    constructor Create(TheOwner: TComponent); override;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; Const AValue: TEpiValueLabelSet);
    procedure   Refresh;
    property    ValueLabelSet: TEpiValueLabelSet read FValueLabelSet;
  end; 

implementation

{$R *.lfm}

uses
  Math;

{ TProject_Structure_ValueLabelSet_Frame }

constructor TProject_Structure_ValueLabelSet_Frame.Create(TheOwner: TComponent
  );
begin
  inherited Create(TheOwner);
end;

constructor TProject_Structure_ValueLabelSet_Frame.Create(TheOwner: TComponent;
  const AValue: TEpiValueLabelSet);
var
  i: Integer;
begin
  Create(TheOwner);
  FValueLabelSet := AValue;

  ValueLabelSetNameLabel.Caption := ValueLabelSet.Name;
  NumberOfValueLabelsLabel.Caption := IntToStr(ValueLabelSet.Count);
  NumberOfMissingsLabel.Caption := IntToStr(ValueLabelSet.MissingCount);

  with ValueLabelsGrid do
  begin
    ColCount := 5;
    RowCount := ValueLabelSet.Count + 1;

    Cells[0, 0] := 'No';
    Cells[1, 0] := 'Order';
    Cells[2, 0] := 'Value';
    Cells[3, 0] := 'Label';
    Cells[4, 0] := 'Missing';

    for i := 0 to ValueLabelSet.Count - 1 do
    with ValueLabelSet[i] do
    begin
      Cells[0, i+1] := IntToStr(i+1);
      Cells[1, i+1] := IntToStr(Order);
      Cells[2, i+1] := ValueAsString;
      Cells[3, i+1] := TheLabel.Text;
      Cells[4, i+1] := BoolToStr(IsMissingValue, '*', '');
    end;
  end;
end;

procedure TProject_Structure_ValueLabelSet_Frame.Refresh;
var
  i: Integer;
  j: Integer;
begin
  with ValueLabelsGrid do
    for i := 0 to ColCount - 1 do
      for j := 0 to RowCount - 1 do
        ColWidths[i] := Max(ColWidths[i], Canvas.TextWidth(Cells[i, j]) + 8);
end;

end.

