unit structure_datafile_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Grids, ExtCtrls, StdCtrls,
  ComCtrls, epidatafiles, epidatafilestypes, types, structure_form;

type

  { TProject_Structure_Datafile_Frame }

  TProject_Structure_Datafile_Frame = class(TFrame, IProject_Structure_Frame)
    Label3: TLabel;
    NumberOfSectionsLabel: TLabel;
    NumberOfFieldsLabel: TLabel;
    NumberOfRecordsLabel: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    DataSetNameLabel: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    FieldGrid: TStringGrid;
    FieldsSheet: TTabSheet;
    SectionsSheet: TTabSheet;
    SectionsGrid: TStringGrid;
  private
    FDataFile: TEpiDataFile;
    { private declarations }
    constructor Create(TheOwner: TComponent); override;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; Const ADataFile: TEpiDataFile);
    procedure Refresh;
    property    DataFile: TEpiDataFile read FDataFile;
  end; 

implementation

{$R *.lfm}

uses
  math, epimiscutils, Graphics;

{ TProject_Structure_Datafile_Frame }

constructor TProject_Structure_Datafile_Frame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

constructor TProject_Structure_Datafile_Frame.Create(TheOwner: TComponent;
  const ADataFile: TEpiDataFile);
var
  i: Integer;
  Section: TEpiSection;
  S: String;
  j: Integer;
begin
  Create(TheOwner);
  FDataFile := ADataFile;

  DataSetNameLabel.Caption := DataFile.Name.Text;
  NumberOfRecordsLabel.Caption := IntToStr(DataFile.Size);
  NumberOfFieldsLabel.Caption := IntToStr(DataFile.Fields.Count);
  NumberOfSectionsLabel.Caption := IntToStr(DataFile.Sections.Count);


  with FieldGrid do
  begin
    Cells[0, 0] := 'No';
    Cells[1, 0] := 'Name';
    Cells[2, 0] := 'Question';
    Cells[3, 0] := 'Type';
    Cells[4, 0] := 'Length';
    Cells[5, 0] := 'Section';
    RowCount := DataFile.Fields.Count + 1;

    for i := 0 to DataFile.Fields.Count - 1 do
    with DataFile.Fields[i] do
    begin
      Cells[0, i+1] := IntToStr(i+1);
      Cells[1, i+1] := Name;
      Cells[2, i+1] := Question.Caption.Text;
      Cells[3, i+1] := EpiTypeNames[FieldType];//FieldType;
      Cells[4, i+1] := IntToStr(Length);
      Section := TEpiSection(Owner.Owner);
      if Section = DataFile.MainSection then
        Cells[5, i+1] := '(main)'
      else
        Cells[5, i+1] := Section.Name.Text;
    end;
  end;

  with SectionsGrid do
  begin
    Cells[0, 0] := 'No';
    Cells[1, 0] := 'Name';
    Cells[2, 0] := '# Fields';
    Cells[3, 0] := '# Headings';
    Cells[4, 0] := 'Groups Access';
    RowCount := DataFile.Sections.Count + 1;

    for i := 0 to DataFile.Sections.Count - 1 do
    with DataFile.Sections[i] do
    begin
      Cells[0, i+1] := IntToStr(i+1);
      if DataFile.Sections[i] = DataFile.MainSection then
        Cells[1, i+1] := '(main)'
      else
        Cells[1, i+1] := Name.Text;
      Cells[2, i+1] := IntToStr(Fields.Count);
      Cells[3, i+1] := IntToStr(Headings.Count);
      if Groups.Count > 0 then
        S := Groups[0].Name.Text;
      for j := 1 to Groups.Count -1 do
        S := S + ',' + Groups[j].Name.Text;
      Cells[4, i+1] := S;
    end;
end;
end;

procedure TProject_Structure_Datafile_Frame.Refresh;
var
  i: Integer;
  j: Integer;
begin
  with FieldGrid do
    for i := 0 to ColCount - 1 do
      for j := 0 to RowCount - 1 do
        ColWidths[i] := Max(ColWidths[i], Canvas.TextWidth(Cells[i, j]) + 8);

  with SectionsGrid do
    for i := 0 to ColCount - 1 do
      for j := 0 to RowCount - 1 do
        ColWidths[i] := Max(ColWidths[i], Canvas.TextWidth(Cells[i, j]) + 8);
end;

end.

