  unit export_form2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, StdCtrls, EditBtn, Buttons, epiv_dataform_treeview,
  epiv_projecttreeview_frame, epiopenfile, epidocument, export_frame_types,
  types, epicustombase, epidatafiles, epirelations;

type

  { TExportForm2 }

  TExportForm2 = class(TForm)
    AllRecordRBtn: TRadioButton;
    OkBtn: TBitBtn;
    BitBtn2: TBitBtn;
    ProjectOptionsChkGrp: TCheckGroup;
    DirectoryEdit1: TDirectoryEdit;
    ExportTypeCombo: TComboBox;
    FieldListSheet: TTabSheet;
    FileNameEdit1: TFileNameEdit;
    FromRecordEdit: TEdit;
    DataformRecordGrpBox: TGroupBox;
    Label1: TLabel;
    ExportPG: TPageControl;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    OptionsTab: TTabSheet;
    DataformPageCtrl: TPageControl;
    Panel1: TPanel;
    Panel3: TPanel;
    ProjectPanel: TPanel;
    RangeRBtn: TRadioButton;
    ExportTabSheet: TTabSheet;
    Splitter2: TSplitter;
    ToRecordEdit: TEdit;
    procedure ExportTypeComboSelect(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
    procedure ProjectOptionsChkGrpItemClick(Sender: TObject; Index: integer);
  private
    { Document }
    FDocumentFile: TEpiDocumentFile;
    procedure SetDocumentFile(AValue: TEpiDocumentFile);
    procedure CreateCustomData(Const Relation: TEpiMasterRelation;
      Const Depth: Cardinal; Const Index: Cardinal; Var aContinue: boolean;
      Data: Pointer = nil);
  private
    { Project Tree }
    FProjectTree: TEpiVProjectTreeViewFrame;
    procedure ProjectTreeSelected(Sender: TObject;
      const AObject: TEpiCustomBase; ObjectType: TEpiVTreeNodeObjectType);
    procedure ProjectTreeSelecting(Sender: TObject; const OldObject,
      NewObject: TEpiCustomBase; OldObjectType,
      NewObjectType: TEpiVTreeNodeObjectType; var Allowed: Boolean);
  private
    { DataForm Tree }
    FDataFormViewer: TDataFormTreeViewFrame;
  private
    { Other }
    IFrame: IExportSettingsPresenterFrame;
    function GetExportFileName(Const DF: TEpiDataFile): string;
    procedure UpdateFileNameExtensions;
    procedure ChangeExt(const Relation: TEpiMasterRelation;
      const Depth: Cardinal; const Index: Cardinal; var aContinue: boolean;
      Data: Pointer = nil);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    property DocumentFile: TEpiDocumentFile read FDocumentFile write SetDocumentFile;
  end;

implementation

{$R *.lfm}

uses
  epidatafilestypes, epieximtypes, export_form, epiexportsettings,
  epimiscutils, settings2_var, settings2;

const
  EXPORT_CUSTOMDATA = 'EXPORT_CUSTOMDATA';

type
  TFrameRec = record
    CFC: TCustomFrameClass;
    ESC: TEpiExportSettingClass;
    Frame: TCustomFrame;
  end;
  PFrameRec = ^TFrameRec;

  TDatafileRec = class
    Filename: String;
    SelectedItems: TList;
    StartRec: Integer;
    EndRec: Integer;
  end;

{ TExportForm2 }

procedure TExportForm2.ExportTypeComboSelect(Sender: TObject);
const
  FActiveSheet: TTabSheet = nil;
var
  Frame: TCustomFrame;
  P: SizeInt;
begin
  if not (Showing) then exit;

  if Assigned(FActiveSheet) then FActiveSheet.TabVisible := false;

  P := ExportTypeCombo.ItemIndex;
  if P = -1 then exit;

  Frame := PFrameRec(ExportTypeCombo.Items.Objects[P])^.Frame;

  FActiveSheet := TTabSheet(Frame.Parent);
  FActiveSheet.TabVisible := true;

  if not Supports(Frame, IExportSettingsPresenterFrame, IFrame) then exit;

  UpdateFileNameExtensions;
  FDataFormViewer.ShowHeadings := IFrame.ExportHeadings;
  ProjectOptionsChkGrp.CheckEnabled[3] := IFrame.ExportRelated;
  if (ProjectOptionsChkGrp.Checked[3]) and (not IFrame.ExportRelated) then
  begin
    ProjectOptionsChkGrp.Checked[3] := false;
    ProjectOptionsChkGrpItemClick(ProjectOptionsChkGrp, 3);
  end;
end;

procedure TExportForm2.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if ManagerSettings.SaveWindowPositions then
    SaveFormPosition(Self, Self.ClassName);
end;

procedure TExportForm2.FormShow(Sender: TObject);
var
  S: String;
begin
  // SETUP ACCORDING TO MANAGERSETTINGS.
  // Export type:
  //  - Dirty way of doing, but works for now.
  case ManagerSettings.ExportType of
    0: S := 'Stata';
    1: S := 'CSV File';
    2: S := 'SPSS';
    3: S := 'SAS';
    4: S := 'DDI';
    5: S := 'EPX';
  end;
  ExportTypeCombo.AdjustSize;
  ExportTypeCombo.ItemIndex := ExportTypeCombo.Items.IndexOf(S);
  ExportTypeComboSelect(ExportTypeCombo);

  // Project options:
  ProjectOptionsChkGrp.Checked[0] := false;
  ProjectOptionsChkGrp.Checked[1] := ManagerSettings.ExportDeleted;
  ProjectOptionsChkGrp.Checked[2] := ManagerSettings.ExportCreateReport;
  ProjectOptionsChkGrp.Checked[3] := false;

  if ManagerSettings.SaveWindowPositions then
    LoadFormPosition(Self, Self.ClassName);
end;

procedure TExportForm2.ProjectOptionsChkGrpItemClick(Sender: TObject;
  Index: integer);
begin
  // If exporting structure there is no need to add options for ranges, etc.
  DataformRecordGrpBox.Enabled := (not ProjectOptionsChkGrp.Checked[0]);
  ProjectOptionsChkGrp.CheckEnabled[1] := (not ProjectOptionsChkGrp.Checked[0]);

  if (ProjectOptionsChkGrp.Checked[3]) then
  begin
    FProjectTree.CheckType :=  pctTriState;
    FDataFormViewer.KeyFieldsSelectState := kssAlwaysSelected;
  end else begin
    FProjectTree.CheckType :=  pctIndividual;
    FDataFormViewer.KeyFieldsSelectState := kssCustomSelected;
  end;
end;

procedure TExportForm2.SetDocumentFile(AValue: TEpiDocumentFile);
begin
  if FDocumentFile = AValue then Exit;
  FDocumentFile := AValue;

  DocumentFile.Document.Relations.OrderedWalk(@CreateCustomData);

  FProjectTree.AddDocument(FDocumentFile.Document);
  FProjectTree.CheckAll;

  FDataFormViewer.DataFile := FDocumentFile.Document.DataFiles[0];
  FDataFormViewer.SelectAll;
end;

procedure TExportForm2.ProjectTreeSelected(Sender: TObject;
  const AObject: TEpiCustomBase; ObjectType: TEpiVTreeNodeObjectType);
var
  Rec: TDatafileRec;
begin
  if (ObjectType <> otRelation) then exit;

  FDataFormViewer.DataFile := TEpiMasterRelation(AObject).Datafile;

  Rec := TDatafileRec(AObject.FindCustomData(EXPORT_CUSTOMDATA));
  FDataFormViewer.SelectedList := Rec.SelectedItems;
  FileNameEdit1.FileName       := Rec.Filename;
  if (Rec.StartRec = -1) and (Rec.EndRec = -1) then
  begin
    AllRecordRBtn.Checked := true;
  end else begin
    FromRecordEdit.Text := IntToStr(Rec.StartRec);
    ToRecordEdit.Text   := IntToStr(Rec.EndRec);
  end;
end;

procedure TExportForm2.ProjectTreeSelecting(Sender: TObject; const OldObject,
  NewObject: TEpiCustomBase; OldObjectType,
  NewObjectType: TEpiVTreeNodeObjectType; var Allowed: Boolean);
var
  Relation: TEpiMasterRelation;
  Rec: TDatafileRec;
begin
  Allowed := (NewObjectType = otRelation);

  if Not Allowed then
    Exit;

  if (OldObjectType <> otRelation) then
    Exit;

  Relation := TEpiMasterRelation(OldObject);

  Rec := TDatafileRec(Relation.FindCustomData(EXPORT_CUSTOMDATA));
  Rec.Filename      := FileNameEdit1.FileName;
  Rec.SelectedItems := FDataFormViewer.SelectedList;
  if AllRecordRBtn.Checked then
  begin
    Rec.StartRec := -1;
    Rec.EndRec   := -1;
  end else begin
    Rec.StartRec := StrToInt(FromRecordEdit.Text);
    Rec.EndRec   := StrToInt(ToRecordEdit.Text);
  end;
end;

procedure TExportForm2.CreateCustomData(const Relation: TEpiMasterRelation;
  const Depth: Cardinal; const Index: Cardinal; var aContinue: boolean;
  Data: Pointer);
var
  Rec: TDatafileRec;
  Item: TEpiCustomControlItem;
begin
  Rec := TDatafileRec.Create;

  Rec.Filename      := GetExportFileName(Relation.Datafile);
  Rec.StartRec      := -1;
  Rec.EndRec        := -1;
  Rec.SelectedItems := TList.Create;

  for Item in Relation.Datafile.ControlItems do
    Rec.SelectedItems.Add(Item);

  Relation.AddCustomData(EXPORT_CUSTOMDATA, TObject(Rec));
end;

function TExportForm2.GetExportFileName(const DF: TEpiDataFile): string;
begin
  Result :=
    ChangeFileExt(ExtractFileName(FDocumentFile.FileName), '') +
    '_' + DF.Caption.Text +
    '_' + IntToStr(FDocumentFile.Document.CycleNo) +
    '.tmp';
end;

procedure TExportForm2.UpdateFileNameExtensions;
var
  Ext: String;
  P: SizeInt;
begin
  Ext := GetEpiDialogFilterExt(IFrame.GetFileDialogExtensions);

  // Ext could contain multiple extensions, only use the first.
  P := Pos(';', Ext);
  if P > 0 then
    Delete(Ext, P, Length(Ext));

  // Delete the "*" part of "*.<ext>"
  Delete(Ext, 1, 1);

  FDocumentFile.Document.Relations.OrderedWalk(@ChangeExt, @Ext);
  FileNameEdit1.FileName := TDatafileRec(FProjectTree.SelectedObject.FindCustomData(EXPORT_CUSTOMDATA)).Filename;
end;

procedure TExportForm2.ChangeExt(const Relation: TEpiMasterRelation;
  const Depth: Cardinal; const Index: Cardinal; var aContinue: boolean;
  Data: Pointer);
var
  Ext: String;
  Rec: TDatafileRec;
begin
  Ext := String(Data^);

  Rec := TDatafileRec(Relation.FindCustomData(EXPORT_CUSTOMDATA));
  Rec.Filename := ChangeFileExt(Rec.Filename, Ext);
end;

constructor TExportForm2.Create(TheOwner: TComponent);
var
  Rec: PFrameRec;
  Tab: TTabSheet;
  Frame: TCustomFrame;
  DialogFilters: TEpiDialogFilters;
  i: Integer;
begin
  inherited Create(TheOwner);

  DirectoryEdit1.Directory := ManagerSettings.WorkingDirUTF8;
  DataformPageCtrl.ActivePage := FieldListSheet;

  FProjectTree := TEpiVProjectTreeViewFrame.Create(Self);
  with FProjectTree do
  begin
    Align              := alClient;
    Parent             := ProjectPanel;

    MinDocumentCount   := 1;
    MaxDocumentCount   := 1;

    AllowSelectProject := False;
    CheckType          := pctIndividual;
    DisplayMode        := pdmSeperate;
    EditCaption        := False;
    EditStructure      := False;
    ShowCheckBoxes     := True;
    ShowHint           := True;
    ShowProject        := False;
    ShowRecordCount    := True;

    OnTreeNodeSelected := @ProjectTreeSelected;
    OnTreeNodeSelecting := @ProjectTreeSelecting;
  end;

  FDataFormViewer := TDataFormTreeViewFrame.Create(Self);
  with FDataFormViewer do
  begin
    Align              := alClient;
    Parent             := FieldListSheet;

    ShowHeadings := true;
    ShowFieldTypes := AllFieldTypes;
  end;

  // Export types and their frames
  for i := 0 to RegisterList.Count - 1 do
  begin
    Rec := PFrameRec(RegisterList[i]);

    Tab := ExportPG.AddTabSheet;
    Tab.TabVisible := false;
    Frame := Rec^.CFC.Create(Tab);
    Frame.Parent := Tab;
    Frame.Align := alClient;
    Rec^.Frame := Frame;
    Frame.GetInterface(IExportSettingsPresenterFrame, IFrame);

    with IFrame do
    begin
      Tab.Caption := GetFrameCaption;
      ExportTypeCombo.AddItem(GetExportName, TObject(Rec));
      DialogFilters := DialogFilters + GetFileDialogExtensions;
    end;
  end;
end;

end.

