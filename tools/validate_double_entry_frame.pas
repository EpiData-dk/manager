unit validate_double_entry_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, FileUtil, Forms, Controls, ExtCtrls, ComCtrls,
  StdCtrls, report_types, epiv_projecttreeview_frame, epiv_dataform_treeview,
  projectfilelist_frame, epidocument, epicustombase, epirelations, contnrs,
  report_double_entry_validation, epitools_val_dbl_entry, epiopenfile,
  report_base;

type

  { TValidateDoubleEntryFrame }

  TValidateDoubleEntryFrame = class(TFrame, IReportFrame)
    CmpFAllNonKeyFBtn: TButton;
    CmpFExcludeTextFBtn: TButton;
    CmpFNoneBtn: TButton;
    CompareTab: TTabSheet;
    FilePanel: TPanel;
    KeyTab: TTabSheet;
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
    procedure CmpFAllNonKeyFBtnClick(Sender: TObject);
    procedure CmpFExcludeTextFBtnClick(Sender: TObject);
    procedure CmpFNoneBtnClick(Sender: TObject);
    procedure FileListAddDoc(Sender: TObject; Document: TEpiDocument;
      const Filename: string; const RowNo: Integer);
    procedure FileListDocChange(Sender: TObject; Document: TEpiDocument;
      const Filename: string; const RowNo: Integer);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure KFAutoIncBtnClick(Sender: TObject);
    procedure KFIndexBtnClick(Sender: TObject);
    procedure KFNoneBtnClick(Sender: TObject);
    procedure ProjectGetText(Sender: TObject; const AObject: TEpiCustomBase;
      ObjectType: TEpiVTreeNodeObjectType; const StaticText: boolean;
      var NodeText: string);
    procedure ProjectTreeSelected(Sender: TObject;
      const AObject: TEpiCustomBase; ObjectType: TEpiVTreeNodeObjectType);
    procedure ProjectTreeSelecting(Sender: TObject; const OldObject,
      NewObject: TEpiCustomBase; OldObjectType,
      NewObjectType: TEpiVTreeNodeObjectType; var Allowed: Boolean);
  private
    FFileList: TProjectFileListFrame;
    FProjectTree: TEpiVProjectTreeViewFrame;
    FKeyTree: TDataFormTreeViewFrame;
    FCompareTree: TDataFormTreeViewFrame;
    FProjectCount: Integer;
    FKeyTreeList: TObjectList;
    FCompareTreeList: TObjectList;
    FListCounter: Integer;
    FValidationOptions: TReportDoubleEntryValidationOptions;
    procedure DataFileTreeToCustomData(Const AObject: TEpiCustomBase);
    procedure CustomDataToDataFileTree(Const AObject: TEpiCustomBase);
    procedure BumpProjectCount(Const Value: Integer);
    procedure AddCustomDataWalk(const Relation: TEpiMasterRelation;
      const Depth: Cardinal; const Index: Cardinal; var aContinue: boolean;
      Data: Pointer = nil);
    procedure RemoveCustomDataWalk(const Relation: TEpiMasterRelation;
      const Depth: Cardinal; const Index: Cardinal; var aContinue: boolean;
      Data: Pointer = nil);
  private
    { Options Handling }
    function  GetDEVOptions: TEpiToolsDblEntryValidateOptions;
    procedure SetDEVOptions(Options: TEpiToolsDblEntryValidateOptions);
  public
    constructor Create(TheOwner: TComponent); override;
    property ValidationOptions: TReportDoubleEntryValidationOptions read FValidationOptions;
    property FileListFrame: TProjectFileListFrame read FFileList;
  public
    { IReportFrame }
    procedure AddDocumentFile(const DocumentFile: TEpiDocumentFile);
    procedure AddFiles(FileNames: TStrings);
    procedure ApplyReportOptions(Report: TReportBase);
    function GetCaption: string;
    function CanPressOk: Boolean;
  end;

implementation

{$R *.lfm}

uses
  settings2_var, epidatafiles, settings2,
  epimiscutils, epidatafilestypes;

const
  KEYTREE_CUSTOMDATA = 'KEYTREE_CUSTOMDATA';
  COMPARETREE_CUSTOMDATA = 'COMPARETREE_CUSTOMDATA';
  DEV_OPTIONS_CUSTOMDATA = 'DEV_OPTIONS_CUSTOMDATA';

type
  PEpiToolsDblEntryValidateOptions = ^TEpiToolsDblEntryValidateOptions;

{ TValidateDoubleEntryFrame }

procedure TValidateDoubleEntryFrame.CmpFAllNonKeyFBtnClick(Sender: TObject);
var
  List: TList;
  DF: TEpiDataFile;
  F: TEpiField;
  NewList: TList;
begin
  if FListCounter <> 2 then exit;
  FCompareTree.SelectNone;

  List := FKeyTree.SelectedList;
  DF := FCompareTree.DataFile;

  NewList := TList.Create;
  for F in DF.Fields do
    if List.IndexOf(F) < 0 then
      NewList.Add(F);

  FCompareTree.SelectedList := NewList;
  List.Free;
  NewList.Free;
end;

procedure TValidateDoubleEntryFrame.CmpFExcludeTextFBtnClick(Sender: TObject);
begin
  FCompareTree.SelectFieldTypes(StringFieldTypes, true);
end;

procedure TValidateDoubleEntryFrame.CmpFNoneBtnClick(Sender: TObject);
begin
  FCompareTree.SelectNone;
end;
{
procedure TValidateDoubleEntryFrame.BitBtn1Click(Sender: TObject);
var
  L: TList;
  MR: TEpiMasterRelation;
  SelectFiles: TStringList;
  MainDataFiles: TEpiDataFiles;
  DuplDataFiles: TEpiDataFiles;
  FieldList: TList;
  i: Integer;
  j: Integer;
begin
  // Before anything else, make sure the lastes changed to the DataFileTree's
  // are applied to the custom data.
  DataFileTreeToCustomData(FProjectTree.SelectedObject);

  try
    L := FProjectTree.CheckList;
    if L.Count = 0 then exit;

    SetLength(FValidationOptions, L.Count);

    SelectFiles := FFileList.SelectedList;
    MainDataFiles := TEpiDocument(SelectFiles.Objects[0]).DataFiles;
    DuplDataFiles := TEpiDocument(SelectFiles.Objects[1]).DataFiles;

    MR := TEpiMasterRelation(L.Items[0]);
//    SwapDatafiles := false;

//    if MainDataFiles.IndexOf(MR.Datafile) < 0 then
//      SwapDatafiles := true;

    for i := 0 to L.Count - 1 do
      begin
        MR := TEpiMasterRelation(L.Items[i]);

        with FValidationOptions[i] do
        begin
          MainDF := MR.Datafile;
          DuplDF := TEpiDataFile(DuplDataFiles.GetItemByName(MR.Datafile.Name));

          Keyfields := TEpiFields.Create(nil);
          FieldList := TList(MR.FindCustomData(KEYTREE_CUSTOMDATA));
          for j := 0 to FieldList.Count - 1 do
            if TEpiCustomItem(FieldList[j]).InheritsFrom(TEpiField) then   // Sort out sections!
              KeyFields.AddItem(TEpiCustomItem(FieldList[j]));

          Comparefields := TEpiFields.Create(nil);
          FieldList := TList(MR.FindCustomData(COMPARETREE_CUSTOMDATA));
          for j := 0 to FieldList.Count - 1 do
            if TEpiCustomItem(FieldList[j]).InheritsFrom(TEpiField) then   // Sort out sections!
              Comparefields.AddItem(TEpiCustomItem(FieldList[j]));
        end;
      end;
  finally
    L.Free;
    SelectFiles.Free;
  end;
end;         }

procedure TValidateDoubleEntryFrame.FileListAddDoc(Sender: TObject;
  Document: TEpiDocument; const Filename: string; const RowNo: Integer);
begin
  BumpProjectCount(1);
  FProjectTree.AddDocument(Document);
  FProjectTree.CheckAll;
end;

procedure TValidateDoubleEntryFrame.FileListDocChange(Sender: TObject;
  Document: TEpiDocument; const Filename: string; const RowNo: Integer);
begin
  if FFileList.StructureGrid.Cells[FFileList.IncludeCol.Index + 1, RowNo] = FFileList.IncludeCol.ValueChecked
  then
    begin
      BumpProjectCount(1);
      FProjectTree.AddDocument(Document);
      FProjectTree.CheckAll;
    end
  else
    begin
      FProjectTree.RemoveDocument(Document);
      BumpProjectCount(-1);
      FProjectTree.CheckAll;
    end;
end;

procedure TValidateDoubleEntryFrame.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
{  if ManagerSettings.SaveWindowPositions and
     (CloseAction in [caHide, caFree])
  then
  begin
    SaveFormPosition(Self, 'DoubleEntryForm');
    SaveSplitterPosition(Splitter1, 'DoubleEntryForm_Splitter1');
    SaveSplitterPosition(Splitter2, 'DoubleEntryForm_Splitter2');
  end;     }
end;

procedure TValidateDoubleEntryFrame.FormShow(Sender: TObject);
begin
 { if ManagerSettings.SaveWindowPositions then
  begin
    LoadFormPosition(Self, 'DoubleEntryForm');
    LoadSplitterPosition(Splitter1, 'DoubleEntryForm_Splitter1');
    LoadSplitterPosition(Splitter2, 'DoubleEntryForm_Splitter2');
  end;   }
end;

procedure TValidateDoubleEntryFrame.KFAutoIncBtnClick(Sender: TObject);
begin
  FKeyTree.SelectNone;
  FKeyTree.SelectFieldTypes([ftAutoInc], false);
end;

procedure TValidateDoubleEntryFrame.KFIndexBtnClick(Sender: TObject);
begin
  FKeyTree.SelectKey;
end;

procedure TValidateDoubleEntryFrame.KFNoneBtnClick(Sender: TObject);
begin
  FKeyTree.SelectNone;
end;

procedure TValidateDoubleEntryFrame.ProjectGetText(Sender: TObject;
  const AObject: TEpiCustomBase; ObjectType: TEpiVTreeNodeObjectType;
  const StaticText: boolean; var NodeText: string);
begin
  if StaticText then Exit;

  if (ObjectType = otFake) then
    if (FProjectCount < 2) then
      NodeText := 'Too few projects. Select two to compare!'
    else
    if (FProjectCount > 2) then
      NodeText := 'Too many projects. Select two to compare!'
    else
      NodeText := 'The project does not have the same structure!'
end;

procedure TValidateDoubleEntryFrame.ProjectTreeSelected(Sender: TObject;
  const AObject: TEpiCustomBase; ObjectType: TEpiVTreeNodeObjectType);
begin
  case ObjectType of
    otEmpty:
      Exit;
    otFake:
      begin
        FKeyTree.DataFile := nil;
        FCompareTree.DataFile := nil;
        SetDEVOptions([]);
      end;
    otRelation:
      if FProjectCount = 2 then
        CustomDataToDataFileTree(AObject);
    otProject:
      Exit;
  end;
end;

procedure TValidateDoubleEntryFrame.ProjectTreeSelecting(Sender: TObject;
  const OldObject, NewObject: TEpiCustomBase; OldObjectType,
  NewObjectType: TEpiVTreeNodeObjectType; var Allowed: Boolean);
begin
  if OldObjectType <> otRelation then exit;
  if FProjectCount <> 2 then exit;

  if NewObjectType <> otRelation then
    begin
      Allowed := false;
      Exit;
    end;

  DataFileTreeToCustomData(OldObject);
end;

procedure TValidateDoubleEntryFrame.DataFileTreeToCustomData(
  const AObject: TEpiCustomBase);

  procedure UpdateCustomData(NewList: TList;
    Const Key: String);
  var
    List: TList;
  begin
    List := TList(AObject.FindCustomData(Key));
    List.Clear;
    List.Assign(NewList);
    NewList.Free;
  end;

var
  Op: TEpiToolsDblEntryValidateOptions;
  A: Integer;
begin
  UpdateCustomData(FKeyTree.SelectedList,     KEYTREE_CUSTOMDATA);
  UpdateCustomData(FCompareTree.SelectedList, COMPARETREE_CUSTOMDATA);
  Op := GetDEVOptions;
  A := Integer(Op);
  AObject.AddCustomData(DEV_OPTIONS_CUSTOMDATA, TObject(PtrInt(A)));
end;

procedure TValidateDoubleEntryFrame.CustomDataToDataFileTree(
  const AObject: TEpiCustomBase);

  procedure ApplyTree(Const DataformTree: TDataFormTreeViewFrame;
    Const CustomDataName: string);
  begin
    DataformTree.DataFile := TEpiMasterRelation(AObject).Datafile;
    DataformTree.SelectNone;
    DataformTree.SelectedList := TList(AObject.FindCustomData(CustomDataName));
  end;

var
  A: Integer;
begin
  ApplyTree(FKeyTree,     KEYTREE_CUSTOMDATA);
  ApplyTree(FCompareTree, COMPARETREE_CUSTOMDATA);
  A := Integer(PtrInt(AObject.FindCustomData(DEV_OPTIONS_CUSTOMDATA)));
  SetDEVOptions(TEpiToolsDblEntryValidateOptions(A));
end;

procedure TValidateDoubleEntryFrame.BumpProjectCount(const Value: Integer);
var
  Method: TEpiRelationListExCallBack;
  i: Integer;
  SelectList: TStringList;

  procedure BuildList;
  var
    i: integer;
    L: TList;
    DF: TEpiDataFile;
    F: TEpiField;
  begin
    for DF in FProjectTree.Documents[0].DataFiles do
    begin
      L := TList.Create;
      for F in DF.KeyFields do
        L.Add(F);
      FKeyTreeList.Add(L);

      L := TList.Create;
      for F in DF.Fields do
        if not DF.KeyFields.FieldExists(F) then
          L.Add(F);
      FCompareTreeList.Add(L);
    end;
  end;

  procedure ClearList;
  begin
    FKeyTreeList.Clear;
    FCompareTreeList.Clear;
  end;

begin
  Inc(FProjectCount, Value);

  if FProjectCount = 2 then
  begin
    BuildList;
    Method := @AddCustomDataWalk
  end else begin
    ClearList;
    Method := @RemoveCustomDataWalk;
  end;

  SelectList := FFileList.SelectedList;
  for i := 0 to SelectList.Count -1 do
    begin
      FListCounter := 0;
      TEpiDocument(SelectList.Objects[i]).Relations.OrderedWalk(Method);
    end;
  SelectList.Free;
end;

procedure TValidateDoubleEntryFrame.AddCustomDataWalk(
  const Relation: TEpiMasterRelation; const Depth: Cardinal;
  const Index: Cardinal; var aContinue: boolean; Data: Pointer);
begin
  Relation.AddCustomData(KEYTREE_CUSTOMDATA, FKeyTreeList[FListCounter]);
  Relation.AddCustomData(COMPARETREE_CUSTOMDATA, FCompareTreeList[FListCounter]);
  Inc(FListCounter);
end;

procedure TValidateDoubleEntryFrame.RemoveCustomDataWalk(
  const Relation: TEpiMasterRelation; const Depth: Cardinal;
  const Index: Cardinal; var aContinue: boolean; Data: Pointer);
begin
  Relation.RemoveCustomData(KEYTREE_CUSTOMDATA);
  Relation.RemoveCustomData(COMPARETREE_CUSTOMDATA);
end;

function TValidateDoubleEntryFrame.GetDEVOptions: TEpiToolsDblEntryValidateOptions;
begin
  Result := [];
  if OptionsChkGrp.Checked[0] then Include(Result, devIgnoreDeleted);
  if OptionsChkGrp.Checked[1] then Include(Result, devCaseSensitiveText);
  if OptionsChkGrp.Checked[2] then Include(Result, devIgnoreMissingRecords);
  if OptionsChkGrp.Checked[3] then Include(Result, devAddResultToField);
end;

procedure TValidateDoubleEntryFrame.SetDEVOptions(
  Options: TEpiToolsDblEntryValidateOptions);
begin
  OptionsChkGrp.Checked[0] := (devIgnoreDeleted        in Options);
  OptionsChkGrp.Checked[1] := (devCaseSensitiveText    in Options);
  OptionsChkGrp.Checked[2] := (devIgnoreMissingRecords in Options);
  OptionsChkGrp.Checked[3] := (devAddResultToField     in Options);
end;

procedure TValidateDoubleEntryFrame.AddDocumentFile(
  const DocumentFile: TEpiDocumentFile);
begin
  FFileList.AddDocument(DocumentFile);
end;

procedure TValidateDoubleEntryFrame.AddFiles(FileNames: TStrings);
begin
  FFileList.AddFiles(FileNames);
end;

procedure TValidateDoubleEntryFrame.ApplyReportOptions(Report: TReportBase);
var
  L: TList;
  MR: TEpiMasterRelation;
  MainDataFiles: TEpiDataFiles;
  DuplDataFiles: TEpiDataFiles;
  FieldList: TList;
  i: Integer;
  j: Integer;
  SelectFiles: TEpiDocumentFileList;
begin
  // Before anything else, make sure the lastes changed to the DataFileTree's
  // are applied to the custom data.
  DataFileTreeToCustomData(FProjectTree.SelectedObject);

  try
    L := FProjectTree.CheckList;
    if L.Count = 0 then exit;

    SetLength(FValidationOptions, L.Count);

    SelectFiles := FFileList.SelectedDocfileList;
    MainDataFiles := SelectFiles[0].Document.DataFiles;
    DuplDataFiles := SelectFiles[1].Document.DataFiles;

    MR := TEpiMasterRelation(L.Items[0]);
//    SwapDatafiles := false;

//    if MainDataFiles.IndexOf(MR.Datafile) < 0 then
//      SwapDatafiles := true;

    for i := 0 to L.Count - 1 do
      begin
        MR := TEpiMasterRelation(L.Items[i]);

        with FValidationOptions[i] do
        begin
          MainDF := MR.Datafile;
          DuplDF := TEpiDataFile(DuplDataFiles.GetItemByName(MR.Datafile.Name));

          Keyfields := TEpiFields.Create(nil);
          FieldList := TList(MR.FindCustomData(KEYTREE_CUSTOMDATA));
          for j := 0 to FieldList.Count - 1 do
            if TEpiCustomItem(FieldList[j]).InheritsFrom(TEpiField) then   // Sort out sections!
              KeyFields.AddItem(TEpiCustomItem(FieldList[j]));

          Comparefields := TEpiFields.Create(nil);
          FieldList := TList(MR.FindCustomData(COMPARETREE_CUSTOMDATA));
          for j := 0 to FieldList.Count - 1 do
            if TEpiCustomItem(FieldList[j]).InheritsFrom(TEpiField) then   // Sort out sections!
              Comparefields.AddItem(TEpiCustomItem(FieldList[j]));
        end;
      end;

    with TReportDoubleEntryValidation(Report) do
    begin
      DocumentFiles := SelectFiles;
      ReportOptions := FValidationOptions;
    end;

  finally
    L.Free;
//    SelectFiles.Free;
  end;

end;

constructor TValidateDoubleEntryFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FProjectCount        := 0;
  FValidationOptions   := nil;

  FKeyTreeList         := TObjectList.create(true);
  FCompareTreeList     := TObjectList.create(true);

  FFileList := TProjectFileListFrame.Create(Self);
  with FFileList do
  begin
    Align  := alClient;
    Parent := FilePanel;

    OnAfterAddToGrid         := @FileListAddDoc;
    OnDocumentIncludedChange := @FileListDocChange;
  end;

  FProjectTree := TEpiVProjectTreeViewFrame.Create(Self);
  with FProjectTree do
  begin
    Align              := alClient;
    Parent             := ProjectPanel;

    MinDocumentCount   := 2;
    MaxDocumentCount   := 2;

    AllowSelectProject := False;
    CheckType          := pctIndividual;
    DisplayMode        := pdmCommon;
    EditCaption        := False;
    EditStructure      := False;
    ShowCheckBoxes     := True;
    ShowHint           := True;
    ShowProject        := False;
    ShowRecordCount    := True;

    OnTreeNodeSelected := @ProjectTreeSelected;
    OnTreeNodeSelecting := @ProjectTreeSelecting;
    OnGetText := @ProjectGetText;
  end;

  FKeyTree := TDataFormTreeViewFrame.Create(Self);
  FKeyTree.Name := 'KeyTree';
  FKeyTree.Align := alClient;
  FKeyTree.Parent := KeyTab;
  FKeyTree.ShowHeadings := false;

  FCompareTree := TDataFormTreeViewFrame.Create(Self);
  FCompareTree.Name := 'CompareTree';
  FCompareTree.Align := alClient;
  FCompareTree.Parent := CompareTab;
  FCompareTree.ShowHeadings := false;

  SetDEVOptions([]);
  PageControl.ActivePage := KeyTab;
end;

function TValidateDoubleEntryFrame.GetCaption: string;
begin
  result := 'Double Entry Validation';
end;

function TValidateDoubleEntryFrame.CanPressOk: Boolean;
begin
  result :=
    (FProjectTree.DocumentCount = 2) and
    (FProjectTree.SelectedObjectType in [otProject, otRelation]);
end;

end.

