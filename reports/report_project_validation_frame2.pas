unit report_project_validation_frame2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, Buttons,
  Dialogs, ComCtrls, projectfilelist_frame, epiv_projecttreeview_frame,
  epiv_field_list_frame, epiv_dataform_treeview, epirelations,
  epidocument, epicustombase, epidatafiles;

type

  { TFrame1 }

  TFrame1 = class(TFrame)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BottomPanel: TPanel;
    Button1: TButton;
    CmpFAllNonKeyFBtn: TButton;
    CmpFExcludeTextFBtn: TButton;
    CmpFNoneBtn: TButton;
    ValidateTab: TTabSheet;
    FilePanel: TPanel;
    SortTab: TTabSheet;
    KFAutoIncBtn: TButton;
    KFIndexBtn: TButton;
    KFNoneBtn: TButton;
    OpenDialog1: TOpenDialog;
    OptionsChkGrp: TCheckGroup;
    OptionsTab: TTabSheet;
    PageControl: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    ProjectPanel: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    procedure BitBtn1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FileListAddDoc(Sender: TObject; Document: TEpiDocument;
      const Filename: string; const RowNo: Integer);
    procedure FileListDocChange(Sender: TObject; Document: TEpiDocument;
      const Filename: string; const RowNo: Integer);
    procedure ProjectTreeSelected(Sender: TObject;
      const AObject: TEpiCustomBase; ObjectType: TEpiVTreeNodeObjectType);
    procedure ProjectTreeSelecting(Sender: TObject; const OldObject,
      NewObject: TEpiCustomBase; OldObjectType,
      NewObjectType: TEpiVTreeNodeObjectType; var Allowed: Boolean);
  private
    FFileList: TProjectFileListFrame;
    FProjectTree: TEpiVProjectTreeViewFrame;
    FSortTree: TEpiVFieldList;
    FCompareTree: TDataFormTreeViewFrame;
    procedure AddCustomDataWalk(const Relation: TEpiMasterRelation;
      const Depth: Cardinal; const Index: Cardinal; var aContinue: boolean);
    procedure RemoveCustomDataWalk(const Relation: TEpiMasterRelation;
      const Depth: Cardinal; const Index: Cardinal; var aContinue: boolean);
    procedure DocumentAdded(Const Doc: TEpiDocument);
    procedure DocumentRemoved(Const Doc: TEpiDocument);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

uses
  settings2_var, epidatafilestypes, epitools_projectvalidate,
  epimiscutils;

const
  SORT_FIELDS_KEY = 'SORT_FIELDS_KEY';
  COMPARE_FIELDS_KEY = 'COMPARE_FIELDS_KEY';

{ TFrame1 }

procedure TFrame1.Button1Click(Sender: TObject);
begin
  if not OpenDialog1.Execute then exit;

  FFileList.AddFiles(OpenDialog1.Files);
end;

procedure TFrame1.BitBtn1Click(Sender: TObject);
begin
  //
end;

procedure TFrame1.FileListAddDoc(Sender: TObject; Document: TEpiDocument;
  const Filename: string; const RowNo: Integer);
begin
  DocumentAdded(Document);
end;

procedure TFrame1.FileListDocChange(Sender: TObject; Document: TEpiDocument;
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

procedure TFrame1.ProjectTreeSelected(Sender: TObject;
  const AObject: TEpiCustomBase; ObjectType: TEpiVTreeNodeObjectType);
begin
  if ObjectType <> otRelation then exit;

  FSortTree.DisplayFields := TEpiMasterRelation(AObject).Datafile.Fields;
  FSortTree.CheckedList   := TEpiFields(AObject.FindCustomData(SORT_FIELDS_KEY));
  FSortTree.Locked        := (TEpiMasterRelation(AObject).DetailRelations.Count > 0) or
                             (AObject is TEpiDetailRelation);

  FCompareTree.DataFile   := TEpiMasterRelation(AObject).Datafile;
  FCompareTree.SelectedList := TList(AObject.FindCustomData(COMPARE_FIELDS_KEY));
end;

procedure TFrame1.ProjectTreeSelecting(Sender: TObject; const OldObject,
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
  AssignList(TList(OldObject.FindCustomData(COMPARE_FIELDS_KEY)), FCompareTree.SelectedList);
end;

procedure TFrame1.AddCustomDataWalk(const Relation: TEpiMasterRelation;
  const Depth: Cardinal; const Index: Cardinal; var aContinue: boolean);
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

procedure TFrame1.RemoveCustomDataWalk(const Relation: TEpiMasterRelation;
  const Depth: Cardinal; const Index: Cardinal; var aContinue: boolean);
begin
  Relation.RemoveCustomData(SORT_FIELDS_KEY).Free;
  Relation.RemoveCustomData(COMPARE_FIELDS_KEY).Free;
end;

procedure TFrame1.DocumentAdded(const Doc: TEpiDocument);
begin
  Doc.Relations.OrderedWalk(@AddCustomDataWalk);
  FProjectTree.AddDocument(Doc);
end;

procedure TFrame1.DocumentRemoved(const Doc: TEpiDocument);
begin
  FProjectTree.RemoveDocument(Doc);
end;

constructor TFrame1.Create(TheOwner: TComponent);
var
  Option: TEpiToolsProjectValidateOption;
begin
  inherited Create(TheOwner);

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
    CheckType          := pctCascade;
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

  FCompareTree := TDataFormTreeViewFrame.Create(Self);
  with FCompareTree do
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
end;

destructor TFrame1.Destroy;
var
  L: TStringList;
  i: Integer;
  Doc: TEpiDocument;
  DF: TEpiDataFile;
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

