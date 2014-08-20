unit validate_double_entry_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, ComCtrls, StdCtrls, Buttons, epiv_projecttreeview_frame,
  epiv_dataform_treeview, projectfilelist_frame, epidocument, epicustombase;

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
  public
    constructor Create(TheOwner: TComponent); override;
  end;

var
  ValidateDoubleEntryForm: TValidateDoubleEntryForm;

implementation

{$R *.lfm}

uses
  settings2_var, epiv_documentfile, epirelations, epidatafiles,
  epimiscutils, report_double_entry_validation;

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
  ValOption: TReportDoubleEntryValidationOptions;
  L: TEpiVCheckList;
begin
  L := FProjectTree.CheckList;

  SetLength(ValOption, L.Count);



end;

procedure TValidateDoubleEntryForm.FileListAddDoc(Sender: TObject;
  Document: TEpiDocument; const Filename: string; const RowNo: Integer);
begin
  FProjectTree.AddDocument(Document);
  FProjectTree.CheckAll;

  Inc(FProjectCount);
end;

procedure TValidateDoubleEntryForm.FileListDocChange(Sender: TObject;
  Document: TEpiDocument; const Filename: string; const RowNo: Integer);
begin
  if FFileList.StructureGrid.Cells[FFileList.IncludeCol.Index + 1, RowNo] = FFileList.IncludeCol.ValueChecked
  then
    begin
      FProjectTree.AddDocument(Document);
      Inc(FProjectCount);
    end
  else
    begin
      FProjectTree.RemoveDocument(Document);
      Dec(FProjectCount);
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

  procedure ApplyTree(Const DataformTree: TDataFormTreeViewFrame;
    Const CustomDataName: string);
  var
    L: TList;
  begin
    DataformTree.DataFile := TEpiMasterRelation(AObject).Datafile;
    DataformTree.SelectNone;
    L := TList(AObject.RemoveCustomData(CustomDataName));
    DataformTree.SelectedList := L;
    L.Free;
  end;

begin
  if ObjectType <> otRelation then exit;
  if FProjectCount <> 2 then exit;

  ApplyTree(FKeyTree,     KEYTREE_CUSTOMDATA);
  ApplyTree(FCompareTree, COMPARETREE_CUSTOMDATA);
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

  OldObject.AddCustomData(KEYTREE_CUSTOMDATA,     FKeyTree.SelectedList);
  OldObject.AddCustomData(COMPARETREE_CUSTOMDATA, FCompareTree.SelectedList);
end;

constructor TValidateDoubleEntryForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FProjectCount := 0;
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

