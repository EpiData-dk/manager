unit count_by_id_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, CheckLst, projectfilelist_frame, epiv_projecttreeview_frame,
  epidocument, epicustombase, epiv_field_list_frame, epidatafiles, report_counts;

type

  { TCountByIdForm }

  TCountByIdForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BottomPanel: TPanel;
    Button1: TButton;
    FilePanel: TPanel;
    OpenDialog1: TOpenDialog;
    FieldListPanel: TPanel;
    ProjectPanel: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    procedure BitBtn1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure FileListAddDoc(Sender: TObject; Document: TEpiDocument;
      const Filename: string; const RowNo: Integer);
    procedure FileListDocChange(Sender: TObject; Document: TEpiDocument;
      const Filename: string; const RowNo: Integer);
    procedure ProjectTreeChecked(Sender: TObject;
      const AObject: TEpiCustomBase; ObjectType: TEpiVTreeNodeObjectType;
      Checked: Boolean);
  private
    FFileList: TProjectFileListFrame;
    FOptions: TReportCountsOption;
    FProjectTree: TEpiVProjectTreeViewFrame;
    FFieldList: TEpiVFieldList;
    FDisplayFields: TEpiFields;
    procedure UpdateCommonFields;
    procedure PopulateFieldList(Const List: TList);
  public
    constructor Create(TheOwner: TComponent); override;
    property    Options: TReportCountsOption read FOptions;
    property    FileListFrame: TProjectFileListFrame read FFileList;
  end;

implementation

{$R *.lfm}

uses
  settings2_var, epimiscutils, epirelations;

{ TCountByIdForm }

procedure TCountByIdForm.FileListAddDoc(Sender: TObject;
  Document: TEpiDocument; const Filename: string; const RowNo: Integer);
begin
  FProjectTree.AddDocument(Document);
  FProjectTree.CheckAll;
  UpdateCommonFields;
end;

procedure TCountByIdForm.Button1Click(Sender: TObject);
begin
  if not OpenDialog1.Execute then exit;

  FFileList.AddFiles(OpenDialog1.Files);
end;

procedure TCountByIdForm.BitBtn1Click(Sender: TObject);
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
end;

procedure TCountByIdForm.CheckBox1Change(Sender: TObject);
begin
  UpdateCommonFields;
end;

procedure TCountByIdForm.FileListDocChange(Sender: TObject;
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

procedure TCountByIdForm.ProjectTreeChecked(Sender: TObject;
  const AObject: TEpiCustomBase; ObjectType: TEpiVTreeNodeObjectType;
  Checked: Boolean);
begin
  UpdateCommonFields;
end;

procedure TCountByIdForm.UpdateCommonFields;
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

procedure TCountByIdForm.PopulateFieldList(const List: TList);
var
  i: Integer;
begin
  FDisplayFields.Clear;

  if Assigned(List) then
    for i := 0 to List.Count - 1 do
      FDisplayFields.AddItem(TEpiField(List[i]));

  FFieldList.DisplayFields := FDisplayFields;
end;

constructor TCountByIdForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FOptions.DataFiles := nil;
  FOptions.FieldNames := nil;

  OpenDialog1.InitialDir := ManagerSettings.WorkingDirUTF8;
  OpenDialog1.Filter     := GetEpiDialogFilter(dfImport + [dfCollection]);

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

end.

