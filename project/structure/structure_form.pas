unit structure_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, FileUtil, Forms, Controls, Graphics, Dialogs,
  ComCtrls, Grids, ExtCtrls, StdCtrls, ActnList, epidocument, epivaluelabels;

type

  { IProject_Structure_Frame }

  IProject_Structure_Frame = interface  ['{F9722870-BDD4-4131-9247-3070F1F1DC48}']
    procedure Refresh;
  end;

  { TProject_Structure_Form }

  TProject_Structure_Form = class(TForm)
    CloseFormAction: TAction;
    ActionList1: TActionList;
    DataFilesGrid: TStringGrid;
    Label5: TLabel;
    NumberOfValueLabelSetsLabelOverview: TLabel;
    ValueLabelsGrid: TStringGrid;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    CreatedLabel: TLabel;
    Label4: TLabel;
    LastModifiedLabel: TLabel;
    NumberOfDataSetsLabel: TLabel;
    NumberOfValueLabelSetsLabel: TLabel;
    ValueLabelsPageCtrl: TPageControl;
    PageCtrl: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    ProjectOverviewTab: TTabSheet;
    ValueLabelsSummarySheet: TTabSheet;
    ValueLabelsTabSheet: TTabSheet;
    procedure CloseFormActionExecute(Sender: TObject);
    procedure DataFilesGridDblClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
    procedure ValueLabelsGridDblClick(Sender: TObject);
  private
    FDocument: TEpiDocument;
    { private declarations }
  protected
    constructor Create(TheOwner: TComponent); override;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; Const EpiDoc: TEpiDocument);
    property  Document: TEpiDocument read FDocument;
    class procedure RestoreDefaultPos;
  end;

implementation

{$R *.lfm}

uses
  structure_datafile_frame, epidatafiles, epistringutils, math,
  structure_valuelabelset_frame, settings2_var, settings2;

{ TProject_Structure_Form }

procedure TProject_Structure_Form.FormShow(Sender: TObject);
var
  i: Integer;
  j: Integer;
begin
  if ManagerSettings.SaveWindowPositions then
    LoadFormPosition(Self, 'StructureForm');

  with DataFilesGrid do
    for i := 0 to ColCount - 1 do
      for j := 0 to RowCount - 1 do
        ColWidths[i] := Max(ColWidths[i], Canvas.TextWidth(Cells[i, j]) + 8);

  with ValueLabelsGrid do
    for i := 0 to ColCount - 1 do
      for j := 0 to RowCount - 1 do
        ColWidths[i] := Max(ColWidths[i], Canvas.TextWidth(Cells[i, j]) + 8);

  for i := 0 to PageCtrl.PageCount - 1 do
    if Supports(PageCtrl.Pages[i].Controls[0], IProject_Structure_Frame) then
     (PageCtrl.Pages[i].Controls[0] as IProject_Structure_Frame).Refresh;

  for i := 0 to ValueLabelsPageCtrl.PageCount - 1 do
    if Supports(ValueLabelsPageCtrl.Pages[i].Controls[0], IProject_Structure_Frame) then
     (ValueLabelsPageCtrl.Pages[i].Controls[0] as IProject_Structure_Frame).Refresh;

  PageCtrl.ActivePage := ProjectOverviewTab;
end;

procedure TProject_Structure_Form.ValueLabelsGridDblClick(Sender: TObject);
var
  Col, Row: Longint;
  P: TPoint;
begin
  P := ValueLabelsGrid.ScreenToClient(Mouse.CursorPos);
  ValueLabelsGrid.MouseToCell(P.X, P.Y, Col, Row);
  if (Row <= 0) or (Col <= 0) then exit;

  With ValueLabelsPageCtrl do
    ActivePage := Pages[Row];
end;

procedure TProject_Structure_Form.CloseFormActionExecute(Sender: TObject);
begin
  Close;
end;

procedure TProject_Structure_Form.DataFilesGridDblClick(Sender: TObject);
var
  Col, Row: Longint;
  P: TPoint;
begin
  P := DataFilesGrid.ScreenToClient(Mouse.CursorPos);
  DataFilesGrid.MouseToCell(P.X, P.Y, Col, Row);
  if (Row <= 0) or (Col <= 0) then exit;

  With PageCtrl do
    ActivePage := Pages[Row];
end;

procedure TProject_Structure_Form.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  CanClose := true;
  if ManagerSettings.SaveWindowPositions then
    SaveFormPosition(Self, 'StructureForm');
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
  Frame: TFrame;
  Df: TEpiDataFile;
  VLSet: TEpiValueLabelSet;
begin
  inherited Create(TheOwner);
  FDocument := EpiDoc;

  with Document do
  begin
    NumberOfDataSetsLabel.Caption := IntToStr(Document.DataFiles.Count);
    CreatedLabel.Caption          := DateTimeToStr(Document.Study.Created);
    LastModifiedLabel.Caption     := DateTimeToStr(Document.Study.ModifiedDate);
    NumberOfValueLabelSetsLabelOverview.Caption := IntToStr(Document.ValueLabelSets.Count);

    with DataFilesGrid do
    begin
      AllowOutboundEvents := false;;
      Cells[0, 0] := 'No';
      Cells[1, 0] := 'Name';
      Cells[2, 0] := '# Records';
      Cells[3, 0] := '# Sections';
      Cells[4, 0] := '# Fields';
      Cells[5, 0] := '# Headings';

      RowCount := Document.DataFiles.Count + 1;
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

        Frame := TProject_Structure_Datafile_Frame.Create(Tab, Df);
        Frame.Parent := Tab;
        Frame.Align := alClient;
        Frame.Name := Df.Id;
      end;
    end;

    NumberOfValueLabelSetsLabel.Caption := IntToStr(Document.ValueLabelSets.Count);
    with ValueLabelsGrid do
    begin
      AllowOutboundEvents := false;;
      ColCount := 4;
      RowCount := Document.ValueLabelSets.Count + 1;

      Cells[0, 0] := 'No';
      Cells[1, 0] := 'Name';
      Cells[2, 0] := '# Valuelabels';
      Cells[3, 0] := '# Missings';

      for i := 0 to Document.ValueLabelSets.Count - 1 do
      begin
        VLSet := Document.ValueLabelSets[i];

        Cells[0, i+1] := IntToStr(i+1);
        Cells[1, i+1] := VLSet.Name;
        Cells[2, i+1] := IntToStr(VLSet.Count);

        Tab := TTabSheet.Create(PageCtrl);
        Tab.Name := 'ValueLabelSetTab' + IntToStr(i);
        Tab.Caption := EpiCutString(VLSet.Name, 15);
        Tab.PageControl := ValueLabelsPageCtrl;

        Frame := TProject_Structure_ValueLabelSet_Frame.Create(Tab, VLSet);
        Frame.Parent := Tab;
        Frame.Align := alClient;
        Frame.Name := VLSet.Id;
        // Cheating (no real calculation), but it works.
        Cells[3, i+1] := TProject_Structure_ValueLabelSet_Frame(Frame).NumberOfMissingsLabel.Caption;
      end;
    end;
    ValueLabelsTabSheet.PageIndex := PageCtrl.PageCount-1;
  end;
end;

class procedure TProject_Structure_Form.RestoreDefaultPos;
var
  Aform: TForm;
begin
  Aform := TForm.Create(nil);
  Aform.Width := 800;
  Aform.Height := 600;
  Aform.top := (Screen.Monitors[0].Height - Aform.Height) div 2;
  Aform.Left := (Screen.Monitors[0].Width - Aform.Width) div 2;
  SaveFormPosition(Aform, 'StructureForm');
  AForm.free;
end;

end.

