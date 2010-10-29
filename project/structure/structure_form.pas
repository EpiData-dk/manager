unit structure_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Grids, ExtCtrls, StdCtrls, epidocument;

type

  { IProject_Structure_Frame }

  IProject_Structure_Frame = interface  ['{F9722870-BDD4-4131-9247-3070F1F1DC48}']
    procedure Refresh;
  end;

  { TProject_Structure_Form }

  TProject_Structure_Form = class(TForm)
    DataFilesGrid: TStringGrid;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    CreatedLabel: TLabel;
    LastModifiedLabel: TLabel;
    NumberOfDataSetsLabel: TLabel;
    PageCtrl: TPageControl;
    Panel1: TPanel;
    ProjectOverviewTab: TTabSheet;
    procedure FormShow(Sender: TObject);
  private
    FDocument: TEpiDocument;
    { private declarations }
  protected
    constructor Create(TheOwner: TComponent); override;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; Const EpiDoc: TEpiDocument);
    property  Document: TEpiDocument read FDocument;
  end;

implementation

{$R *.lfm}

uses
  structure_datafile_frame, epidatafiles, epistringutils, math;

{ TProject_Structure_Form }

procedure TProject_Structure_Form.FormShow(Sender: TObject);
var
  i: Integer;
  j: Integer;
begin
  with DataFilesGrid do
    for i := 0 to ColCount - 1 do
      for j := 0 to RowCount - 1 do
        ColWidths[i] := Max(ColWidths[i], Canvas.TextWidth(Cells[i, j]) + 8);


  for i := 0 to PageCtrl.PageCount - 1 do
    if Supports(PageCtrl.Pages[i].Controls[0], IProject_Structure_Frame) then
     (PageCtrl.Pages[i].Controls[0] as IProject_Structure_Frame).Refresh;
end;

constructor TProject_Structure_Form.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

constructor TProject_Structure_Form.Create(TheOwner: TComponent;
  const EpiDoc: TEpiDocument);
var
  i: Integer;
  Tab: TTabSheet;
  DataFileStructureFrame: TProject_Structure_Datafile_Frame;
  Df: TEpiDataFile;
begin
  inherited Create(TheOwner);
  FDocument := EpiDoc;

  with Document do
  begin
    NumberOfDataSetsLabel.Caption := IntToStr(Document.DataFiles.Count);
    CreatedLabel.Caption          := DateTimeToStr(Document.Study.Created);
    LastModifiedLabel.Caption     := DateTimeToStr(Document.Study.ModifiedDate);

    with DataFilesGrid do
    begin
      Cells[0, 0] := 'No';
      Cells[1, 0] := 'Name';
      Cells[2, 0] := '# Records';
      Cells[3, 0] := '# Sections';
      Cells[4, 0] := '# Fields';
      Cells[5, 0] := '# Headings';

      DataFilesGrid.RowCount := Document.DataFiles.Count + 1;
      for i := 0 to Document.DataFiles.Count - 1 do
      begin
        Df := DataFiles[i];

        Cells[0, i+1] := IntToStr(i+1);
        Cells[1, i+1] := Df.Name.Text;
        Cells[2, i+1] := IntToStr(Df.Size);
        Cells[3, i+1] := IntToStr(Df.Sections.Count);
        Cells[4, i+1] := IntToStr(Df.Fields.Count);
        Cells[5, i+1] := IntToStr(Df.Headings.Count);

        Tab := TTabSheet.Create(PageCtrl);
        Tab.Name := 'DataFileTab' + IntToStr(i);
        Tab.Caption := EpiCutString(Df.Name.Text, 15);
        Tab.PageControl := PageCtrl;

        DataFileStructureFrame := TProject_Structure_Datafile_Frame.Create(Tab, Df);
        DataFileStructureFrame.Parent := Tab;
        DataFileStructureFrame.Align := alClient;
        DataFileStructureFrame.Name := Df.Id;
      end;
    end;
  end;
end;

end.

