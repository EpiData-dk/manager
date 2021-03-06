unit report_project_validation_frame2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls,
  Buttons, Dialogs, ComCtrls, projectfilelist_frame, epiv_projecttreeview_frame,
  epiv_field_list_frame, epiv_dataform_treeview, epidatafilerelations,
  epitools_projectvalidate, epidocument, epicustombase, epidatafiles,
  report_project_validation, report_types, report_base, epiopenfile;

type

  { TProjectValidationFrame2 }

  TProjectValidationFrame2 = class(TFrame, IReportFrame)
    CmpFAllNonKeyFBtn: TButton;
    CmpFExcludeTextFBtn: TButton;
    CmpFNoneBtn: TButton;
    ValidateTab: TTabSheet;
    FilePanel: TPanel;
    SortTab: TTabSheet;
    KFAutoIncBtn: TButton;
    KFIndexBtn: TButton;
    KFNoneBtn: TButton;
    OptionsChkGrp: TCheckGroup;
    OptionsTab: TTabSheet;
    PageControl: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    ProjectPanel: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    procedure BuildFieldLists(const ARelation: TEpiMasterRelation;
      const Depth: Cardinal; const Index: Cardinal; var aContinue: boolean;
      Data: Pointer = nil);
    procedure CmpFAllNonKeyFBtnClick(Sender: TObject);
    procedure CmpFExcludeTextFBtnClick(Sender: TObject);
    procedure CmpFNoneBtnClick(Sender: TObject);
    procedure FileListAddDoc(Sender: TObject; Document: TEpiDocument;
      const Filename: string; const RowNo: Integer);
    procedure FileListDocChange(Sender: TObject; Document: TEpiDocument;
      const Filename: string; const RowNo: Integer);
    procedure KFAutoIncBtnClick(Sender: TObject);
    procedure KFIndexBtnClick(Sender: TObject);
    procedure KFNoneBtnClick(Sender: TObject);
    procedure ProjectTreeSelected(Sender: TObject;
      const AObject: TEpiCustomBase; ObjectType: TEpiVTreeNodeObjectType);
    procedure ProjectTreeSelecting(Sender: TObject; const OldObject,
      NewObject: TEpiCustomBase; OldObjectType,
      NewObjectType: TEpiVTreeNodeObjectType; var Allowed: Boolean);
  private
    FFileList: TProjectFileListFrame;
    FProjectTree: TEpiVProjectTreeViewFrame;
    FSortTree: TEpiVFieldList;
    FValidateTree: TDataFormTreeViewFrame;
    FFieldListIndex: Integer;
    function  GetOptions: TEpiToolsProjectValidateOptions;
    procedure AddCustomDataWalk(const Relation: TEpiMasterRelation;
      const Depth: Cardinal; const Index: Cardinal; var aContinue: boolean;
      Data: Pointer = nil);
    procedure RemoveCustomDataWalk(const Relation: TEpiMasterRelation;
      const Depth: Cardinal; const Index: Cardinal; var aContinue: boolean;
      Data: Pointer = nil);
    procedure DocumentAdded(Const Doc: TEpiDocument);
    procedure DocumentRemoved(Const Doc: TEpiDocument);
  public
    { IReportFrame Inteface }
    procedure AddFiles(FileNames: TStrings);
    procedure ApplyReportOptions(Report: TReportBase);
    function  GetCaption: string;
    procedure AddDocumentFile(const DocumentFile: TEpiDocumentFile);
    function CanPressOk: Boolean;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property   FileList: TProjectFileListFrame read FFileList;
  end;

implementation

{$R *.lfm}

uses
  settings2_var, epidatafilestypes, epireport_report_projectvalidator,
  epimiscutils, epidatafilerelations_helper, epiadmin;

const
  SORT_FIELDS_KEY = 'SORT_FIELDS_KEY';
  COMPARE_FIELDS_KEY = 'COMPARE_FIELDS_KEY';

{ TProjectValidationFrame2 }

procedure TProjectValidationFrame2.BuildFieldLists(const ARelation: TEpiMasterRelation;
  const Depth: Cardinal; const Index: Cardinal; var aContinue: boolean;
  Data: Pointer);
var
  Fields: TEpiFields;
  F: TEpiField;
  Option: TEpiReportProjectValidateOption;
  List: TList;
  i: Integer;
begin
  Option := TEpiReportProjectValidateOption(Data^);
  with Option.FieldLists[FFieldListIndex] do
  begin
    Relation      := ARelation;
    SortFields    := TStringList.Create;
    CompareFields := TStringList.Create;

    Fields := TEpiFields(Relation.FindCustomData(SORT_FIELDS_KEY));
    for F in Fields do
      SortFields.Add(F.Name);

    List := TList(Relation.FindCustomData(COMPARE_FIELDS_KEY));
    for i := 0 to List.Count -1 do
      if (TObject(List[i]) is TEpiField) then
        CompareFields.Add(TEpiField(List[i]).Name);
  end;

  Inc(FFieldListIndex);
end;

procedure TProjectValidationFrame2.CmpFAllNonKeyFBtnClick(Sender: TObject);
var
  SFields: TEpiFields;
  F: TEpiField;
  NonKeyFields: TList;
begin
  if FProjectTree.SelectedObjectType <> otRelation then Exit;

  NonKeyFields := TList.Create;
  SFields := FSortTree.CheckedList;

  for F in TEpiMasterRelation(FProjectTree.SelectedObject).Datafile.Fields do
    if not SFields.ItemExistsByName(F.Name) then
      NonKeyFields.Add(F);

  FValidateTree.SelectedList := NonKeyFields;
  NonKeyFields.Free;
end;

procedure TProjectValidationFrame2.CmpFExcludeTextFBtnClick(Sender: TObject);
begin
  FValidateTree.SelectFieldTypes(StringFieldTypes, true);
end;

procedure TProjectValidationFrame2.CmpFNoneBtnClick(Sender: TObject);
begin
  FValidateTree.SelectNone;
end;

procedure TProjectValidationFrame2.FileListAddDoc(Sender: TObject; Document: TEpiDocument;
  const Filename: string; const RowNo: Integer);
begin
  DocumentAdded(Document);
end;

procedure TProjectValidationFrame2.FileListDocChange(Sender: TObject; Document: TEpiDocument;
  const Filename: string; const RowNo: Integer);
var
  Added: Boolean;
begin
  Added := (FFileList.StructureGrid.Cells[FFileList.IncludeCol.Index + 1, RowNo] = FFileList.IncludeCol.ValueChecked);

  if Added then
    DocumentAdded(Document)
  else
    DocumentRemoved(Document);
end;

procedure TProjectValidationFrame2.KFAutoIncBtnClick(Sender: TObject);
var
  Fs: TEpiFields;
  F: TEpiField;
begin
  if FSortTree.Locked then exit;
  if FProjectTree.SelectedObjectType <> otRelation then exit;

  Fs := TEpiFields.Create(nil);
  Fs.ItemOwner := false;
  for F in TEpiMasterRelation(FProjectTree.SelectedObject).Datafile.Fields do
    if F.FieldType = ftAutoInc then
      Fs.AddItem(F);

  FSortTree.CheckedList := Fs;
  Fs.Free;
end;

procedure TProjectValidationFrame2.KFIndexBtnClick(Sender: TObject);
var
  F: TEpiFields;
begin
  if FSortTree.Locked then exit;
  if FProjectTree.SelectedObjectType <> otRelation then exit;

  F := TEpiMasterRelation(FProjectTree.SelectedObject).Datafile.KeyFields;
  if Assigned(F) then
    FSortTree.CheckedList := F;
end;

procedure TProjectValidationFrame2.KFNoneBtnClick(Sender: TObject);
var
  F: TEpiFields;
begin
  if FSortTree.Locked then exit;

  F := TEpiFields.Create(nil);
  FSortTree.CheckedList := F;
  F.Free;
end;

procedure TProjectValidationFrame2.ProjectTreeSelected(Sender: TObject;
  const AObject: TEpiCustomBase; ObjectType: TEpiVTreeNodeObjectType);
var
  DoLock: Boolean;
begin
  if ObjectType <> otRelation then exit;

  FSortTree.DisplayFields := TEpiMasterRelation(AObject).Datafile.Fields;
  FSortTree.CheckedList   := TEpiFields(AObject.FindCustomData(SORT_FIELDS_KEY));
  FSortTree.Locked        := (TEpiMasterRelation(AObject).DetailRelations.Count > 0) or
                             (AObject is TEpiDetailRelation);

  FValidateTree.DataFile   := TEpiMasterRelation(AObject).Datafile;
  FValidateTree.SelectedList := TList(AObject.FindCustomData(COMPARE_FIELDS_KEY));
end;

procedure TProjectValidationFrame2.ProjectTreeSelecting(Sender: TObject; const OldObject,
  NewObject: TEpiCustomBase; OldObjectType,
  NewObjectType: TEpiVTreeNodeObjectType; var Allowed: Boolean);

  procedure AssignFields(Const AssignTo, AssignFrom: TEpiFields);
  var
    F: TEpiField;
  begin
    AssignTo.Clear;

    for F in AssignFrom do
      AssignTo.AddItem(F);

    AssignFrom.Free;
  end;

  procedure AssignList(Const AssignTo, AssignFrom: TList);
  var
    i: Integer;
  begin
    AssignTo.Clear;
    AssignTo.Assign(AssignFrom);
    AssignFrom.Free;
  end;

begin
  if NewObjectType <> otRelation then
  begin
    Allowed := false;
    Exit;
  end;

  if OldObjectType <> otRelation then
    Exit;

  AssignFields(TEpiFields(OldObject.FindCustomData(SORT_FIELDS_KEY)), FSortTree.CheckedList);
  AssignList(TList(OldObject.FindCustomData(COMPARE_FIELDS_KEY)), FValidateTree.SelectedList);
end;

function TProjectValidationFrame2.GetOptions: TEpiToolsProjectValidateOptions;
var
  i: Integer;
  Opt: TEpiToolsProjectValidateOption;
begin
  result := EpiProjectValidationOptionsAll;

  for i := 0 to OptionsChkGrp.Items.Count -1 do
  begin
    if OptionsChkGrp.Checked[i] then Continue;

    Opt := TEpiToolsProjectValidateOption(PtrInt(OptionsChkGrp.Items.Objects[i]));
    Exclude(Result, Opt);
  end;
end;

procedure TProjectValidationFrame2.AddCustomDataWalk(const Relation: TEpiMasterRelation;
  const Depth: Cardinal; const Index: Cardinal; var aContinue: boolean;
  Data: Pointer);
var
  Fields: TEpiFields;
  List: TList;
  F: TEpiField;
begin
  if not Assigned(Relation.FindCustomData(SORT_FIELDS_KEY)) then
  begin
    Fields := TEpiFields.Create(nil);
    Relation.AddCustomData(SORT_FIELDS_KEY, Fields);

    if (Relation is TEpiDetailRelation) or
       (Relation.DetailRelations.Count > 0)
    then
      Fields.Assign(Relation.Datafile.KeyFields);
  end;

  if not Assigned(Relation.FindCustomData(COMPARE_FIELDS_KEY)) then
  begin
    List := TList.Create;
    Relation.AddCustomData(COMPARE_FIELDS_KEY, List);
    For F in Relation.Datafile.Fields do
      List.Add(F);
  end;
end;

procedure TProjectValidationFrame2.RemoveCustomDataWalk(const Relation: TEpiMasterRelation;
  const Depth: Cardinal; const Index: Cardinal; var aContinue: boolean;
  Data: Pointer);
begin
  Relation.RemoveCustomData(SORT_FIELDS_KEY).Free;
  Relation.RemoveCustomData(COMPARE_FIELDS_KEY).Free;
end;

procedure TProjectValidationFrame2.DocumentAdded(const Doc: TEpiDocument);
begin
  Doc.Relations.OrderedWalk(@AddCustomDataWalk);
  FProjectTree.AddDocument(Doc);
end;

procedure TProjectValidationFrame2.DocumentRemoved(const Doc: TEpiDocument);
begin
  FProjectTree.RemoveDocument(Doc);
end;

procedure TProjectValidationFrame2.AddDocumentFile(const DocumentFile: TEpiDocumentFile);
begin
  FFileList.AddDocument(DocumentFile);
end;

function TProjectValidationFrame2.CanPressOk: Boolean;
begin
  result := FProjectTree.DocumentCount > 0;
end;

procedure TProjectValidationFrame2.AddFiles(FileNames: TStrings);
begin
  FFileList.AddFiles(FileNames);
end;

procedure TProjectValidationFrame2.ApplyReportOptions(Report: TReportBase);
var
  DocCount: Integer;
  i: Integer;
  FOptions: TReportProjectValidateOptions;
  FakeBool: Boolean;

begin
  // Before anything else, make sure changes in "List By" and "Validate Fields"
  // are correcte assigned to CustomData.
  ProjectTreeSelecting(Nil,
    FProjectTree.SelectedObject, nil,
    FProjectTree.SelectedObjectType, otRelation,
    FakeBool
  );

  DocCount := FProjectTree.DocumentCount;
  SetLength(FOptions, DocCount);

  for i := 0 to DocCount - 1 do
  with FOptions[i] do
  begin
    Document := FProjectTree.Documents[i];

    Options  := GetOptions;
    SetLength(FieldLists, Document.DataFiles.Count);

    FFieldListIndex := 0;
    Document.Relations.OrderedWalk(@BuildFieldLists, @FOptions[i]);
  end;

  with TReportProjectValidation(Report) do
  begin
    Options := FOptions;
    DocumentFiles := FFileList.SelectedDocfileList;
  end;
end;

function TProjectValidationFrame2.GetCaption: string;
begin
  result := 'Project Validation';
end;

constructor TProjectValidationFrame2.Create(TheOwner: TComponent);
var
  Option: TEpiToolsProjectValidateOption;
begin
  inherited Create(TheOwner);

  FFileList := TProjectFileListFrame.Create(Self);
  with FFileList do
  begin
    Align  := alClient;
    Parent := FilePanel;

    OnAfterAddToGrid := @FileListAddDoc;
    OnDocumentIncludedChange := @FileListDocChange;
    RequiredRights := [earReport];
  end;

  FProjectTree := TEpiVProjectTreeViewFrame.Create(Self);
  with FProjectTree do
  begin
    Align              := alClient;
    Parent             := ProjectPanel;

    AllowSelectProject := False;
    CheckType          := pctCascadeBottomUp;
    DisplayMode        := pdmSeperate;
    EditCaption        := False;
    EditStructure      := False;
    ShowCheckBoxes     := False;
    ShowHint           := True;
    ShowProject        := True;
    ShowRecordCount    := True;

    OnTreeNodeSelected  := @ProjectTreeSelected;
    OnTreeNodeSelecting := @ProjectTreeSelecting;
  end;

  FSortTree := TEpiVFieldList.Create(Self);
  with FSortTree do
  begin
    Align              := alClient;
    Parent             := SortTab;

    ShowCheckBoxes     := True;
    ShowMoveButtons    := True;
  end;

  FValidateTree := TDataFormTreeViewFrame.Create(Self);
  with FValidateTree do
  begin
    Align              := alClient;
    Parent             := ValidateTab;

    ShowHeadings       := false;
    ShowFieldTypes     := AllFieldTypes;
  end;

  With OptionsChkGrp.Items do
  begin
    BeginUpdate;
    Clear;

    for Option in EpiProjectValidationOptionsSelectable do
      AddObject(EpiToolProjectValidationOptionText[Option], TObject(PtrInt(Option)));

    EndUpdate;
  end;

  PageControl.ActivePage := SortTab;
end;

destructor TProjectValidationFrame2.Destroy;
var
  L: TStringList;
  i: Integer;
  Doc: TEpiDocument;
begin
  L :=  FFileList.DocList;

  for i := 0 to L.Count - 1 do
  begin
    Doc := TEpiDocument(L.Objects[i]);
    Doc.Relations.OrderedWalk(@RemoveCustomDataWalk);
  end;

  inherited Destroy;
end;

end.

