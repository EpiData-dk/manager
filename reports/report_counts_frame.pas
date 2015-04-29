unit report_counts_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, epidocument,
  epicustombase, epiv_projecttreeview_frame, epiv_field_list_frame,
  epidatafiles, projectfilelist_frame, report_types, epiopenfile,
  report_base;

type

  { TCountByIdFrame }

  TCountByIdFrame = class(TFrame, IReportFrame)
    FieldListPanel: TPanel;
    FilePanel: TPanel;
    ProjectPanel: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
  private
    procedure FileListAddDoc(Sender: TObject; Document: TEpiDocument;
      const Filename: string; const RowNo: Integer);
    procedure FileListDocChange(Sender: TObject; Document: TEpiDocument;
      const Filename: string; const RowNo: Integer);
    procedure ProjectTreeChecked(Sender: TObject;
      const AObject: TEpiCustomBase; ObjectType: TEpiVTreeNodeObjectType;
      Checked: Boolean);
  private
    FFileList: TProjectFileListFrame;
    FProjectTree: TEpiVProjectTreeViewFrame;
    FFieldList: TEpiVFieldList;
    FDisplayFields: TEpiFields;
    procedure UpdateCommonFields;
    procedure PopulateFieldList(Const List: TList);
  public
    procedure AddDocumentFile(const DocumentFile: TEpiDocumentFile);
    procedure AddFiles(FileNames: TStrings);
    procedure ApplyReportOptions(Report: TReportBase);
    function CanPressOk: Boolean;
    constructor Create(TheOwner: TComponent); override;
    function GetCaption: string;
  end;

implementation

{$R *.lfm}

uses
  settings2_var, epimiscutils, epidatafilerelations, report_counts;

{ TCountByIdFrame }

procedure TCountByIdFrame.FileListAddDoc(Sender: TObject;
  Document: TEpiDocument; const Filename: string; const RowNo: Integer);
begin
  FProjectTree.AddDocument(Document);
  FProjectTree.CheckAll;
  UpdateCommonFields;
end;

{procedure TCountByIdFrame.BitBtn1Click(Sender: TObject);
var
  L: TList;
  FL: TEpiFields;
  F: TEpiField;
  i: Integer;
begin
  try
    L := FProjectTree.CheckList;

    FOptions.DataFiles := TEpiDataFiles.Create(nil);
    FOptions.DataFiles.UniqueNames := false;
    FOptions.DataFiles.Sorted := false;

    for i := 0 to L.Count - 1 do
      FOptions.DataFiles.AddItem(TEpiMasterRelation(L[i]).Datafile);

    FOptions.FieldNames := TStringList.Create;
    FL := FFieldList.CheckedList;
    for F in FL do
      FOptions.FieldNames.Add(F.Name);

  finally
    L.Free;
    FL.Free;
  end;
end;                }

procedure TCountByIdFrame.FileListDocChange(Sender: TObject;
  Document: TEpiDocument; const Filename: string; const RowNo: Integer);
var
  Added: Boolean;
begin
  Added := (FFileList.StructureGrid.Cells[FFileList.IncludeCol.Index + 1, RowNo] = FFileList.IncludeCol.ValueChecked);
  if Added then
    FProjectTree.AddDocument(Document)
  else
    FProjectTree.RemoveDocument(Document);
  FProjectTree.CheckAll;
  UpdateCommonFields;
end;

procedure TCountByIdFrame.ProjectTreeChecked(Sender: TObject;
  const AObject: TEpiCustomBase; ObjectType: TEpiVTreeNodeObjectType;
  Checked: Boolean);
begin
  UpdateCommonFields;
end;

procedure TCountByIdFrame.UpdateCommonFields;
var
  L: TList;
  DF: TEpiDataFile;
  F: TEpiField;
  CompareList: TList;
  FieldList: TList;
  MainDF: TEpiDataFile;
  CompareField: TEpiField;
  i: Integer;
begin
  L := FProjectTree.CheckList;
  FieldList := TList.Create;
  CompareList := TList.Create;

  try
    if L.Count = 0 then
    begin
      PopulateFieldList(nil);
      Exit;
    end;

    MainDF := TEpiMasterRelation(L[0]).Datafile;
    for F in MainDF.Fields do
      FieldList.Add(F);

    for i := 1 to L.Count -1 do
    begin
      DF := TEpiMasterRelation(L[i]).Datafile;

      for F in DF.Fields do
      begin
        CompareField := MainDF.Fields.FieldByName[F.Name];
        if not Assigned(CompareField) then continue;
        if (CompareField.FieldType <> f.FieldType) then continue;

        CompareList.Add(CompareField);
      end;
      FieldList.Assign(CompareList, laAnd);
      CompareList.Clear;
    end;

    PopulateFieldList(FieldList);
  finally
    L.Free;
    FieldList.Free;
    CompareList.Free;
  end;
end;

procedure TCountByIdFrame.PopulateFieldList(const List: TList);
var
  i: Integer;
begin
  FDisplayFields.Clear;

  if Assigned(List) then
    for i := 0 to List.Count - 1 do
      FDisplayFields.AddItem(TEpiField(List[i]));

  FFieldList.DisplayFields := FDisplayFields;
end;

procedure TCountByIdFrame.AddDocumentFile(const DocumentFile: TEpiDocumentFile);
begin
  FFileList.AddDocument(DocumentFile);
end;

procedure TCountByIdFrame.AddFiles(FileNames: TStrings);
begin
  FFileList.AddFiles(FileNames);
end;

procedure TCountByIdFrame.ApplyReportOptions(Report: TReportBase);
var
  L: TList;
  FL: TEpiFields;
  F: TEpiField;
  i: Integer;
  Options: TReportCountsOption;
begin
  try
    L := FProjectTree.CheckList;

    Options.DataFiles := TEpiDataFiles.Create(nil);
    Options.DataFiles.UniqueNames := false;
    Options.DataFiles.Sorted := false;

    for i := 0 to L.Count - 1 do
      Options.DataFiles.AddItem(TEpiMasterRelation(L[i]).Datafile);

    Options.FieldNames := TStringList.Create;
    FL := FFieldList.CheckedList;
    for F in FL do
      Options.FieldNames.Add(F.Name);

    Report.DocumentFiles := FFileList.SelectedDocfileList;
    TReportCounts(Report).Options := Options;
  finally
    L.Free;
    FL.Free;
  end;
end;

function TCountByIdFrame.CanPressOk: Boolean;
begin
  Result :=
    (FProjectTree.DocumentCount > 0) and
    (FFieldList.CheckedCount > 0);
end;

constructor TCountByIdFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FFileList := TProjectFileListFrame.Create(Self);
  with FFileList do
  begin
    Align  := alClient;
    Parent := FilePanel;

    OnAfterAddToGrid := @FileListAddDoc;
    OnDocumentIncludedChange := @FileListDocChange;
  end;

  FProjectTree := TEpiVProjectTreeViewFrame.Create(Self);
  with FProjectTree do
  begin
    Align              := alClient;
    Parent             := ProjectPanel;

    AllowSelectProject := False;
    CheckType          := pctIndividual;
    DisplayMode        := pdmSeperate;
    EditCaption        := False;
    EditStructure      := False;
    ShowCheckBoxes     := True;
    ShowHint           := True;
    ShowProject        := True;
    ShowRecordCount    := True;

    OnChecked          := @ProjectTreeChecked;
  end;

  FFieldList := TEpiVFieldList.Create(SElf);
  with FFieldList do
  begin
    Align              := alClient;
    Parent             := FieldListPanel;

    ShowCheckBoxes     := true;
    ShowMoveButtons    := true;
  end;

  FDisplayFields       := TEpiFields.Create(nil);
end;

function TCountByIdFrame.GetCaption: string;
begin
  result := 'Count By ID';
end;


end.

