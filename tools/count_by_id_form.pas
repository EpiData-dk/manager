unit count_by_id_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, CheckLst, projectfilelist_frame, epiv_projecttreeview_frame,
  epidocument, epicustombase;

type

  { TCountByIdForm }

  TCountByIdForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BottomPanel: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    IDCheckListBox: TCheckListBox;
    FilePanel: TPanel;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    ProjectPanel: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    procedure Button1Click(Sender: TObject);
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
    procedure UpdateCommonFields;
    procedure PopulateFieldList(Const List: TList);
  public
    constructor Create(TheOwner: TComponent); override;
  end;

implementation

{$R *.lfm}

uses
  settings2_var, epimiscutils, epidatafiles, epirelations;

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
    if L.Count = 0 then exit;

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
  F: TEpiField;
begin
  IDCheckListBox.Clear;

  IDCheckListBox.Items.BeginUpdate;

  for i := 0 to List.Count - 1 do
  begin
    F := TEpiField(List[i]);

    IDCheckListBox.AddItem(F.Name + ': ' + F.Question.Text, F);
  end;

  IDCheckListBox.Items.BeginUpdate;
end;

constructor TCountByIdForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  OpenDialog1.InitialDir := ManagerSettings.WorkingDirUTF8;
  OpenDialog1.Filter     := GetEpiDialogFilter(dfImport);

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
end;

end.

