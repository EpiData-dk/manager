unit validate_double_entry_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, ComCtrls, StdCtrls, Buttons, epiv_projecttreeview_frame,
  epiv_dataform_treeview, projectfilelist_frame, epidocument, epicustombase,
  epirelations, contnrs, report_double_entry_validation;

type

  { TValidateDoubleEntryForm }

  TValidateDoubleEntryForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Button1: TButton;
    Dlg: TOpenDialog;
    PageControl: TPageControl;
    BottomPanel: TPanel;
    ProjectPanel: TPanel;
    Splitter1: TSplitter;
    FilePanel: TPanel;
    Splitter2: TSplitter;
    KeyTab: TTabSheet;
    CompareTab: TTabSheet;
    OptionsTab: TTabSheet;
    procedure BitBtn1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FileListAddDoc(Sender: TObject; Document: TEpiDocument;
      const Filename: string; const RowNo: Integer);
    procedure FileListDocChange(Sender: TObject; Document: TEpiDocument;
      const Filename: string; const RowNo: Integer);
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
    FList: TObjectList;
    FListCounter: Integer;
    FValidationOptions: TReportDoubleEntryValidationOptions;
    procedure DataFileTreeToCustomData(Const AObject: TEpiCustomBase);
    procedure CustomDataToDataFileTree(Const AObject: TEpiCustomBase);
    procedure BumpProjectCount(Const Value: Integer);
    procedure AddCustomDataWalk(const Relation: TEpiMasterRelation;
      const Depth: Cardinal; const Index: Cardinal; var aContinue: boolean);
    procedure RemoveCustomDataWalk(const Relation: TEpiMasterRelation;
      const Depth: Cardinal; const Index: Cardinal; var aContinue: boolean);
  public
    constructor Create(TheOwner: TComponent); override;
    property ValidationOptions: TReportDoubleEntryValidationOptions read FValidationOptions;
    property FileListFrame: TProjectFileListFrame read FFileList;
  end;

implementation

{$R *.lfm}

uses
  settings2_var, epiv_documentfile, epidatafiles,
  epimiscutils;

const
  KEYTREE_CUSTOMDATA = 'KEYTREE_CUSTOMDATA';
  COMPARETREE_CUSTOMDATA = 'COMPARETREE_CUSTOMDATA';

{ TValidateDoubleEntryForm }

procedure TValidateDoubleEntryForm.Button1Click(Sender: TObject);
begin
  if not Dlg.Execute then exit;

  FFileList.AddFiles(Dlg.Files);
end;

procedure TValidateDoubleEntryForm.BitBtn1Click(Sender: TObject);
var
  L: TList;
  MR: TEpiMasterRelation;
  SelectFiles: TStringList;
  SwapDatafiles: Boolean;
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
    SwapDatafiles := false;

    if MainDataFiles.IndexOf(MR.Datafile) < 0 then
      SwapDatafiles := true;

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
      Document.Relations.OrderedWalk(@AddCustomDataWalk);
      FProjectTree.AddDocument(Document);
    end
  else
    begin
      FProjectTree.RemoveDocument(Document);
      Document.Relations.OrderedWalk(@RemoveCustomDataWalk);
      BumpProjectCount(-1);
    end;
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
  if ObjectType <> otRelation then exit;
  if FProjectCount <> 2 then exit;

  CustomDataToDataFileTree(AObject);
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

begin
  UpdateCustomData(FKeyTree.SelectedList,     KEYTREE_CUSTOMDATA);
  UpdateCustomData(FCompareTree.SelectedList, COMPARETREE_CUSTOMDATA);
end;

procedure TValidateDoubleEntryForm.CustomDataToDataFileTree(
  const AObject: TEpiCustomBase);

  procedure ApplyTree(Const DataformTree: TDataFormTreeViewFrame;
    Const CustomDataName: string);
  var
    L: TList;
  begin
    DataformTree.DataFile := TEpiMasterRelation(AObject).Datafile;
    DataformTree.SelectNone;
    DataformTree.SelectedList := TList(AObject.FindCustomData(CustomDataName));
  end;

begin
  ApplyTree(FKeyTree,     KEYTREE_CUSTOMDATA);
  ApplyTree(FCompareTree, COMPARETREE_CUSTOMDATA);
end;

procedure TValidateDoubleEntryForm.BumpProjectCount(const Value: Integer);
var
  Method: TEpiRelationListExCallBack;
  i: Integer;

  procedure BuildList;
  var
    i: integer;
  begin
    for i := 1 to (FProjectTree.Documents[0].DataFiles.Count * 2) do
      FList.Add(TList.Create);
  end;

  procedure ClearList;
  begin
    FList.Clear;
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

  for i := 0 to FFileList.DocList.Count -1 do
    begin
      FListCounter := 0;
      TEpiDocument(FFileList.DocList.Objects[i]).Relations.OrderedWalk(Method);
    end;
end;

procedure TValidateDoubleEntryForm.AddCustomDataWalk(
  const Relation: TEpiMasterRelation; const Depth: Cardinal;
  const Index: Cardinal; var aContinue: boolean);
begin
  Relation.AddCustomData(KEYTREE_CUSTOMDATA, FList[FListCounter]);
  Relation.AddCustomData(COMPARETREE_CUSTOMDATA, FList[FListCounter+1]);
  Inc(FListCounter, 2);
end;

procedure TValidateDoubleEntryForm.RemoveCustomDataWalk(
  const Relation: TEpiMasterRelation; const Depth: Cardinal;
  const Index: Cardinal; var aContinue: boolean);
begin
  Relation.RemoveCustomData(KEYTREE_CUSTOMDATA);
  Relation.RemoveCustomData(COMPARETREE_CUSTOMDATA);
end;

constructor TValidateDoubleEntryForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FProjectCount := 0;
  FValidationOptions := nil;
  FList         := TObjectList.create(true);
  Dlg.InitialDir := ManagerSettings.WorkingDirUTF8;
  Dlg.Filter     := GetEpiDialogFilter(dfEpiData);

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
end;

end.

