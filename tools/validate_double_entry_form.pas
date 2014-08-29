unit validate_double_entry_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, ComCtrls, StdCtrls, Buttons, epiv_projecttreeview_frame,
  epiv_dataform_treeview, projectfilelist_frame, epidocument, epicustombase,
  epirelations, contnrs, report_double_entry_validation, epitools_val_dbl_entry;

type

  { TValidateDoubleEntryForm }

  TValidateDoubleEntryForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Button1: TButton;
    CmpFAllNonKeyFBtn: TButton;
    CmpFExcludeTextFBtn: TButton;
    CmpFNoneBtn: TButton;
    Dlg: TOpenDialog;
    KFAutoIncBtn: TButton;
    KFIndexBtn: TButton;
    KFNoneBtn: TButton;
    OptionsChkGrp: TCheckGroup;
    PageControl: TPageControl;
    BottomPanel: TPanel;
    Panel1: TPanel;
    Panel2: TPanel;
    ProjectPanel: TPanel;
    Splitter1: TSplitter;
    FilePanel: TPanel;
    Splitter2: TSplitter;
    KeyTab: TTabSheet;
    CompareTab: TTabSheet;
    OptionsTab: TTabSheet;
    procedure BitBtn1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
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
      const Depth: Cardinal; const Index: Cardinal; var aContinue: boolean);
    procedure RemoveCustomDataWalk(const Relation: TEpiMasterRelation;
      const Depth: Cardinal; const Index: Cardinal; var aContinue: boolean);
  private
    { Options Handling }
    function  GetDEVOptions: TEpiToolsDblEntryValidateOptions;
    procedure SetDEVOptions(Options: TEpiToolsDblEntryValidateOptions);
  public
    constructor Create(TheOwner: TComponent); override;
    property ValidationOptions: TReportDoubleEntryValidationOptions read FValidationOptions;
    property FileListFrame: TProjectFileListFrame read FFileList;
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

{ TValidateDoubleEntryForm }

procedure TValidateDoubleEntryForm.Button1Click(Sender: TObject);
begin
  if not Dlg.Execute then exit;

  FFileList.AddFiles(Dlg.Files);
end;

procedure TValidateDoubleEntryForm.CmpFAllNonKeyFBtnClick(Sender: TObject);
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

procedure TValidateDoubleEntryForm.CmpFExcludeTextFBtnClick(Sender: TObject);
begin
  FCompareTree.SelectFieldTypes(StringFieldTypes, true);
end;

procedure TValidateDoubleEntryForm.CmpFNoneBtnClick(Sender: TObject);
begin
  FCompareTree.SelectNone;
end;

procedure TValidateDoubleEntryForm.BitBtn1Click(Sender: TObject);
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
end;

procedure TValidateDoubleEntryForm.FileListAddDoc(Sender: TObject;
  Document: TEpiDocument; const Filename: string; const RowNo: Integer);
begin
  BumpProjectCount(1);
  FProjectTree.AddDocument(Document);
  FProjectTree.CheckAll;
end;

procedure TValidateDoubleEntryForm.FileListDocChange(Sender: TObject;
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

procedure TValidateDoubleEntryForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if ManagerSettings.SaveWindowPositions and
     (CloseAction in [caHide, caFree])
  then
  begin
    SaveFormPosition(Self, 'DoubleEntryForm');
    SaveSplitterPosition(Splitter1, 'DoubleEntryForm_Splitter1');
    SaveSplitterPosition(Splitter2, 'DoubleEntryForm_Splitter2');
  end;
end;

procedure TValidateDoubleEntryForm.FormShow(Sender: TObject);
begin
  if ManagerSettings.SaveWindowPositions then
  begin
    LoadFormPosition(Self, 'DoubleEntryForm');
    LoadSplitterPosition(Splitter1, 'DoubleEntryForm_Splitter1');
    LoadSplitterPosition(Splitter2, 'DoubleEntryForm_Splitter2');
  end;
end;

procedure TValidateDoubleEntryForm.KFAutoIncBtnClick(Sender: TObject);
begin
  FKeyTree.SelectNone;
  FKeyTree.SelectFieldTypes([ftAutoInc], false);
end;

procedure TValidateDoubleEntryForm.KFIndexBtnClick(Sender: TObject);
begin
  FKeyTree.SelectKey;
end;

procedure TValidateDoubleEntryForm.KFNoneBtnClick(Sender: TObject);
begin
  FKeyTree.SelectNone;
end;

procedure TValidateDoubleEntryForm.ProjectGetText(Sender: TObject;
  const AObject: TEpiCustomBase; ObjectType: TEpiVTreeNodeObjectType;
  const StaticText: boolean; var NodeText: string);
begin
  if StaticText then Exit;

  if (ObjectType = otFake) then
    if (FProjectCount < 2) then
      NodeText := 'Too few projects. Select two to compare!'
    else
      NodeText := 'Too many projects. Select two to compare!'
end;

procedure TValidateDoubleEntryForm.ProjectTreeSelected(Sender: TObject;
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

procedure TValidateDoubleEntryForm.ProjectTreeSelecting(Sender: TObject;
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

procedure TValidateDoubleEntryForm.DataFileTreeToCustomData(
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

procedure TValidateDoubleEntryForm.CustomDataToDataFileTree(
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

procedure TValidateDoubleEntryForm.BumpProjectCount(const Value: Integer);
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

procedure TValidateDoubleEntryForm.AddCustomDataWalk(
  const Relation: TEpiMasterRelation; const Depth: Cardinal;
  const Index: Cardinal; var aContinue: boolean);
begin
  Relation.AddCustomData(KEYTREE_CUSTOMDATA, FKeyTreeList[FListCounter]);
  Relation.AddCustomData(COMPARETREE_CUSTOMDATA, FCompareTreeList[FListCounter]);
  Inc(FListCounter);
end;

procedure TValidateDoubleEntryForm.RemoveCustomDataWalk(
  const Relation: TEpiMasterRelation; const Depth: Cardinal;
  const Index: Cardinal; var aContinue: boolean);
begin
  Relation.RemoveCustomData(KEYTREE_CUSTOMDATA);
  Relation.RemoveCustomData(COMPARETREE_CUSTOMDATA);
end;

function TValidateDoubleEntryForm.GetDEVOptions: TEpiToolsDblEntryValidateOptions;
begin
  Result := [];
  if OptionsChkGrp.Checked[0] then Include(Result, devIgnoreDeleted);
  if OptionsChkGrp.Checked[1] then Include(Result, devCaseSensitiveText);
  if OptionsChkGrp.Checked[2] then Include(Result, devIgnoreMissingRecords);
  if OptionsChkGrp.Checked[3] then Include(Result, devAddResultToField);
end;

procedure TValidateDoubleEntryForm.SetDEVOptions(
  Options: TEpiToolsDblEntryValidateOptions);
begin
  OptionsChkGrp.Checked[0] := (devIgnoreDeleted        in Options);
  OptionsChkGrp.Checked[1] := (devCaseSensitiveText    in Options);
  OptionsChkGrp.Checked[2] := (devIgnoreMissingRecords in Options);
  OptionsChkGrp.Checked[3] := (devAddResultToField     in Options);
end;

constructor TValidateDoubleEntryForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FProjectCount        := 0;
  FValidationOptions   := nil;

  FKeyTreeList         := TObjectList.create(true);
  FCompareTreeList     := TObjectList.create(true);

  Dlg.InitialDir       := ManagerSettings.WorkingDirUTF8;
  Dlg.Filter           := GetEpiDialogFilter(dfEpiData);

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

end.

