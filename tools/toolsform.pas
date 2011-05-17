unit toolsform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, Buttons,
  epidocument, epidatafiles;

type

  { TToolsForm }

  TToolsForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    DataFilesListView: TListView;
    Panel1: TPanel;
    Panel2: TPanel;
    OptionsPanel: TPanel;
    DataSetInfoPanel: TPanel;
    Panel5: TPanel;
    FieldCountPanel: TPanel;
    SectionCountPanel: TPanel;
    RecordCountPanel: TPanel;
    DeletedCountPanel: TPanel;
    Splitter1: TSplitter;
    procedure DataFilesListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
  private
    FEpiDocument: TEpiDocument;
    FSelectedDatafiles: TList;
    procedure SetEpiDocument(const AValue: TEpiDocument);
    { private declarations }
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    property EpiDocument: TEpiDocument read FEpiDocument write SetEpiDocument;
    property SelectedDatafiles: TList read FSelectedDatafiles;
  end; 

implementation

{$R *.lfm}

{ TToolsForm }

procedure TToolsForm.DataFilesListViewSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  DF: TEpiDataFile;
begin
  if not Selected then Exit;

  DF := TEpiDataFile(Item.Data);
  FieldCountPanel.Caption := IntToStr(DF.Fields.Count);
  SectionCountPanel.Caption := IntToStr(DF.Sections.Count);
  RecordCountPanel.Caption := IntToStr(DF.Size);
  DeletedCountPanel.Caption := IntToStr(DF.Size);
end;

procedure TToolsForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  i: Integer;
begin
  CanClose := true;

  for i := 0 to DataFilesListView.Items.Count - 1 do
    with DataFilesListView.Items[i] do
      if Checked then
        FSelectedDatafiles.Add(Data);
end;

procedure TToolsForm.SetEpiDocument(const AValue: TEpiDocument);
var
  i: Integer;
  Item: TListItem;
begin
  if FEpiDocument = AValue then exit;
  FEpiDocument := AValue;

  for i := 0 to FEpiDocument.DataFiles.Count - 1 do
  begin
    Item := DataFilesListView.Items.Add;
    Item.Caption := FEpiDocument.DataFiles[i].Caption.Text;
    Item.Data    := FEpiDocument.DataFiles[i];
    Item.Checked := true;
  end;

  DataFilesListView.Selected := DataFilesListView.Items[0];
  DataFilesListViewSelectItem(nil, DataFilesListView.Items[0], true);
end;

constructor TToolsForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FSelectedDatafiles := TList.Create;
  OptionsPanel.Hide;
  Splitter1.Hide;
  Panel2.Align := alClient;
end;

end.

