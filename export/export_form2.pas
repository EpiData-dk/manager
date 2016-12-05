  unit export_form2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, StdCtrls, EditBtn, Buttons, epiv_dataform_treeview,
  epiv_projecttreeview_frame, epiopenfile, export_frame_types,
  epicustombase, epidatafiles, epidatafilerelations, epiexportsettings;

type

  { TExportForm2 }

  TExportForm2 = class(TForm)
    AllRecordRBtn: TRadioButton;
    DataformFilenameEdit: TEdit;
    ProjectFileNameEdit: TEdit;
    OkBtn: TBitBtn;
    BitBtn2: TBitBtn;
    ProjectOptionsChkGrp: TCheckGroup;
    ExportFolderEdit: TDirectoryEdit;
    ExportTypeCombo: TComboBox;
    FieldListSheet: TTabSheet;
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
    procedure OkBtnClick(Sender: TObject);
    procedure ProjectOptionsChkGrpItemClick(Sender: TObject; Index: integer);
    procedure RangeEditEnter(Sender: TObject);
    procedure RangeEditKeyPress(Sender: TObject; var Key: char);
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
    FExportSetting: TEpiExportSetting;
    FActiveSheet: TTabSheet;
    IFrame: IExportSettingsPresenterFrame;
    function GetExportDirectory: string;
    function GetExportFileName(Const DF: TEpiDataFile): string;
    function GetExportReport: Boolean;
    function GetExportSingleFile: Boolean;
    procedure UpdateFileNameExtensions;
    procedure UpdateSelectedItems;
    procedure ChangeExt(const Relation: TEpiMasterRelation;
      const Depth: Cardinal; const Index: Cardinal; var aContinue: boolean;
      Data: Pointer = nil);
    procedure ChangeSelectedItems(const Relation: TEpiMasterRelation;
      const Depth: Cardinal; const Index: Cardinal; var aContinue: boolean;
      Data: Pointer = nil);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    class procedure RestoreDefaultPos;
    property DocumentFile: TEpiDocumentFile read FDocumentFile write SetDocumentFile;
    property ExportSetting: TEpiExportSetting read FExportSetting;
    property ExportReport: Boolean read GetExportReport;
    property ExportSingleFile: Boolean read GetExportSingleFile;
    property ExportDirectory: string read GetExportDirectory;
  end;

procedure RegisterExportFrame(CFC: TCustomFrameClass; ESC: TEpiExportSettingClass);

implementation

{$R *.lfm}

uses
  epidatafilestypes, epieximtypes, epidatafilerelations_helper, LazUTF8,
  epimiscutils, settings2_var, settings2, manager_types;

var
  RegisterList: TList = nil;

const
  EXPORT_CUSTOMDATA = 'EXPORT_CUSTOMDATA';

  ProjChkStructIdx  = 0;
  ProjChkDeletedIdx = 1;
  ProjChkReportIdx  = 2;
  ProjChkSingleIdx  = 3;


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
    AllRecord: Boolean;
    StartRec: Integer;
    EndRec: Integer;
  end;

procedure RegisterExportFrame(CFC: TCustomFrameClass;
  ESC: TEpiExportSettingClass);
var
  Rec: PFrameRec;
begin
  if not Assigned(RegisterList) then
    RegisterList := TList.Create;

  if (CFC <> nil) and (not Supports(CFC, IExportSettingsPresenterFrame)) then
    Exit;

  Rec := new(PFrameRec);
  Rec^.CFC := CFC;
  Rec^.ESC := ESC;
  RegisterList.Add(Rec);
end;

{ TExportForm2 }

procedure TExportForm2.ExportTypeComboSelect(Sender: TObject);
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
  UpdateSelectedItems;

  ProjectOptionsChkGrp.CheckEnabled[ProjChkSingleIdx] := IFrame.ExportRelated;
  if (ProjectOptionsChkGrp.Checked[ProjChkSingleIdx]) and (not IFrame.ExportRelated) then
  begin
    ProjectOptionsChkGrp.Checked[ProjChkSingleIdx] := false;
    ProjectOptionsChkGrpItemClick(ProjectOptionsChkGrp, ProjChkSingleIdx);
  end;
end;

procedure TExportForm2.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if ManagerSettings.SaveWindowPositions then
  begin
    SaveFormPosition(Self, Self.ClassName);
    SaveSplitterPosition(Splitter2, Self.ClassName);
  end;
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
  ProjectOptionsChkGrp.Checked[ProjChkStructIdx]  := false;
  ProjectOptionsChkGrp.Checked[ProjChkDeletedIdx] := ManagerSettings.ExportDeleted;
  ProjectOptionsChkGrp.Checked[ProjChkReportIdx]  := ManagerSettings.ExportCreateReport;
  ProjectOptionsChkGrp.Checked[ProjChkSingleIdx]  := false;

  if ManagerSettings.SaveWindowPositions then
  begin
    LoadFormPosition(Self, Self.ClassName);
    LoadSplitterPosition(Splitter2, Self.ClassName);
  end;
end;

procedure TExportForm2.OkBtnClick(Sender: TObject);
var
  Rec: PFrameRec;
  RelationList: TList;
  Relation: TEpiMasterRelation;
  DFSetting: TEpiExportDatafileSettings;
  DatafileRec: TDatafileRec;
  FakeBool: Boolean;
  i: Integer;
  j: Integer;
  ConflictFileNames: TStringList;
  Msg: String;
  Res: TModalResult;
begin
  ProjectTreeSelecting(nil, FProjectTree.SelectedObject, nil, otRelation, otRelation, FakeBool);

  Rec := PFrameRec(ExportTypeCombo.Items.Objects[ExportTypeCombo.ItemIndex]);

  FExportSetting := Rec^.ESC.Create;
  with FExportSetting do
  begin
    Doc           := FDocumentFile.Document;
    ExportDeleted := ProjectOptionsChkGrp.Checked[ProjChkDeletedIdx];
    Encoding      := ManagerSettings.ExportEncoding;

    RelationList := FProjectTree.CheckList;

    for i := 0 to RelationList.Count - 1 do
    begin
      Relation    := TEpiMasterRelation(RelationList[i]);
      DatafileRec := TDatafileRec(Relation.FindCustomData(EXPORT_CUSTOMDATA));

      DFSetting := TEpiExportDatafileSettings.Create;
      DFSetting.DatafileName   := Relation.Datafile.Name;

      if ProjectOptionsChkGrp.Checked[ProjChkStructIdx] then
      begin
        DFSetting.FromRecord   := 0;
        DFSetting.ToRecord     := -1;
      end else begin
        if DatafileRec.AllRecord then
        begin
          DFSetting.FromRecord := 0;
          DFSetting.ToRecord   := Relation.Datafile.Size - 1;
        end else begin
          DFSetting.FromRecord := DatafileRec.StartRec - 1;
          DFSetting.ToRecord   := DatafileRec.EndRec - 1;
        end;
      end;

      for j := 0 to DatafileRec.SelectedItems.Count - 1 do
        DFSetting.ExportItems.Add(TEpiCustomItem(DatafileRec.SelectedItems[j]).Name);

      // If exporting as single project (DDI, EPX) do not add filenames to DatafileSetting - this will
      // make sure that SanityCheck does not create unwanted filestreams.
      if (not ProjectOptionsChkGrp.Checked[ProjChkSingleIdx]) then
        DFSetting.ExportFileName := ExpandFileNameUTF8(ExportFolderEdit.Directory + DirectorySeparator + DatafileRec.Filename);

      DatafileSettings.Add(DFSetting);
    end;

    if FExportSetting.InheritsFrom(TEpiCustomCompleteProjectExportSetting) then
    with TEpiCustomCompleteProjectExportSetting(FExportSetting) do
    begin
      ExportFileName := ExpandFileNameUTF8(ExportFolderEdit.Directory + DirectorySeparator + ProjectFileNameEdit.Text);
      ExportCompleteProject := ProjectOptionsChkGrp.Checked[ProjChkSingleIdx];
    end;
  end;
  IFrame.UpdateExportSetting(FExportSetting);

  ConflictFileNames := TStringList.Create;

  if (ProjectOptionsChkGrp.Checked[ProjChkSingleIdx]) and
     (FileExistsUTF8(TEpiCustomCompleteProjectExportSetting(FExportSetting).ExportFileName))
  then
    ConflictFileNames.Add(ProjectFileNameEdit.Text);

  for i := 0 to FExportSetting.DatafileSettings.Count -1 do
  begin
    DFSetting := FExportSetting.DatafileSettings[i];

    while Assigned(DFSetting) do
    begin
      if FileExistsUTF8(DFSetting.ExportFileName) then
        ConflictFileNames.Add(ExtractFileName(DFSetting.ExportFileName));

      DFSetting := DFSetting.AdditionalExportSettings;
    end;
  end;

  if ConflictFileNames.Count > 0 then
  begin
    Msg :=
      'Replace existing file(s)?' + LineEnding +
      LineEnding +
      '(in folder): ' + ExportFolderEdit.Directory + LineEnding +
      LineEnding;


    for i := 0 to ConflictFileNames.Count - 1 do
      Msg += ConflictFileNames[i] + LineEnding;

    Res := MessageDlg(
      'Warning!',
      Msg,
      mtWarning,
      mbYesNo,
      0,
      mbNo);

    if Res = mrNo then
    begin
      ModalResult := mrNone;
      Exit;
    end;
  end;
end;

procedure TExportForm2.ProjectOptionsChkGrpItemClick(Sender: TObject;
  Index: integer);
var
  TmpChk: Boolean;
begin
  // If exporting structure there is no need to add options for ranges, etc.
  DataformRecordGrpBox.Enabled := (not ProjectOptionsChkGrp.Checked[ProjChkStructIdx]);

  ProjectOptionsChkGrp.CheckEnabled[ProjChkDeletedIdx] := (not ProjectOptionsChkGrp.Checked[ProjChkStructIdx]);
  DataformRecordGrpBox.Enabled := (not ProjectOptionsChkGrp.Checked[ProjChkStructIdx]);

  if (ProjectOptionsChkGrp.Checked[ProjChkStructIdx]) then
    ProjectOptionsChkGrp.Checked[ProjChkDeletedIdx] := false;

  if (Index = ProjChkSingleIdx) then
  begin
    TmpChk := ProjectOptionsChkGrp.Checked[ProjChkSingleIdx];

    if (TmpChk) then
    begin
      FProjectTree.CheckType :=  pctCascadeTopDown;
      FDataFormViewer.KeyFieldsSelectState := kssAlwaysSelected;
    end else begin
      FProjectTree.CheckType :=  pctIndividual;
      FDataFormViewer.KeyFieldsSelectState := kssCustomSelected;
    end;

    ProjectFileNameEdit.Visible  := TmpChk;
    DataformFileNameEdit.Visible := (not TmpChk);
  end;
end;

procedure TExportForm2.RangeEditEnter(Sender: TObject);
begin
  RangeRBtn.Checked := true;
end;

procedure TExportForm2.RangeEditKeyPress(Sender: TObject; var Key: char);
begin
  if not (Key in [Char('0')..Char('9'), #8]) then Key := #0;
end;

procedure TExportForm2.SetDocumentFile(AValue: TEpiDocumentFile);
begin
  if FDocumentFile = AValue then Exit;
  FDocumentFile := AValue;

  ExportFolderEdit.Directory   := ExtractFilePath(DocumentFile.FileName);

  DocumentFile.Document.Relations.OrderedWalk(@CreateCustomData);
  ProjectFileNameEdit.Text := GetExportFileName(nil);

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
  FDataFormViewer.SelectedList  := Rec.SelectedItems;
  DataformFileNameEdit.Text     := Rec.Filename;
  if (Rec.AllRecord) then
  begin
    AllRecordRBtn.Checked := true;
    FromRecordEdit.Text   := '';
    ToRecordEdit.Text     := '';
  end else begin
    RangeRBtn.Checked     := true;
    FromRecordEdit.Text   := IntToStr(Rec.StartRec);
    ToRecordEdit.Text     := IntToStr(Rec.EndRec);
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
  Rec.Filename      := DataformFileNameEdit.Text;
  Rec.SelectedItems := FDataFormViewer.SelectedList;
  if AllRecordRBtn.Checked then
  begin
    Rec.AllRecord := true;
  end else begin
    Rec.AllRecord := false;
    Rec.StartRec := StrToIntDef(FromRecordEdit.Text, 1);
    Rec.EndRec   := StrToIntDef(ToRecordEdit.Text,   Relation.Datafile.Size);
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
  Rec.AllRecord     := true;
  Rec.StartRec      := -1;
  Rec.EndRec        := -1;
  Rec.SelectedItems := TList.Create;

  Relation.AddCustomData(EXPORT_CUSTOMDATA, TObject(Rec));
end;

function TExportForm2.GetExportFileName(const DF: TEpiDataFile): string;
var
  LFormatSettings: TFormatSettings;
begin
  Result := ChangeFileExt(ExtractFileName(FDocumentFile.FileName), '');

  if (FDocumentFile.Document.DataFiles.Count > 1) and
     (Assigned(DF)) and
     (DF.Caption.Text <> '')
  then
    Result += '_' + DF.Caption.Text;

  case ManagerSettings.ExportPostFix of
    // Do nothing
    epProjectOnly: ;

    // Add cycle no:
    epAddCycle:
      Result += '_' + IntToStr(FDocumentFile.Document.CycleNo);

    // Add current date:
    epAddDate:
      begin
        LFormatSettings := DefaultFormatSettings;
        LFormatSettings.DateSeparator := '-';
        Result += '_' + DateToStr(Now, LFormatSettings);
      end;
  end;

  Result := CanonicalizeFileName(Result) + '.tmp';
end;

function TExportForm2.GetExportDirectory: string;
begin
  result := ExportFolderEdit.Directory;
end;

function TExportForm2.GetExportReport: Boolean;
begin
  result := ProjectOptionsChkGrp.Checked[ProjChkReportIdx];
end;

function TExportForm2.GetExportSingleFile: Boolean;
begin
  result := ProjectOptionsChkGrp.Checked[ProjChkSingleIdx];
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

  ProjectFileNameEdit.Text := ChangeFileExt(ProjectFileNameEdit.Text, Ext);
  FDocumentFile.Document.Relations.OrderedWalk(@ChangeExt, @Ext);

  DataformFileNameEdit.Text := TDatafileRec(FProjectTree.SelectedObject.FindCustomData(EXPORT_CUSTOMDATA)).Filename;
//  DataformFileNameEdit.Filter   := GetEpiDialogFilter(IFrame.GetFileDialogExtensions);
end;

procedure TExportForm2.UpdateSelectedItems;
begin
  FDocumentFile.Document.Relations.OrderedWalk(@ChangeSelectedItems);
  FDataFormViewer.SelectedList := TDatafileRec(FProjectTree.SelectedObject.FindCustomData(EXPORT_CUSTOMDATA)).SelectedItems;
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

procedure TExportForm2.ChangeSelectedItems(const Relation: TEpiMasterRelation;
  const Depth: Cardinal; const Index: Cardinal; var aContinue: boolean;
  Data: Pointer);
var
  Rec: TDatafileRec;
  Item: TEpiCustomControlItem;
begin
  Rec := TDatafileRec(Relation.FindCustomData(EXPORT_CUSTOMDATA));
  Rec.SelectedItems.Clear;

  // EPX format
  if IFrame.ExportHeadings then
    for Item in Relation.Datafile.ControlItems do
      Rec.SelectedItems.Add(Item)
  else
    for Item in Relation.Datafile.ControlItems do
      if (not Item.InheritsFrom(TEpiHeading)) then
        Rec.SelectedItems.Add(Item)
end;

constructor TExportForm2.Create(TheOwner: TComponent);
var
  Rec: PFrameRec;
  Tab: TTabSheet;
  Frame: TCustomFrame;
  i: Integer;
begin
  inherited Create(TheOwner);

  ProjectFileNameEdit.Visible  := False;
  DataformFileNameEdit.Visible := True;

  DataformPageCtrl.ActivePage  := FieldListSheet;

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
    ShowHeadings       := true;
    ShowFieldTypes     := AllFieldTypes;
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
    end;
  end;
end;

class procedure TExportForm2.RestoreDefaultPos;
var
  Aform: TForm;
begin
  Aform := TForm.Create(nil);
  Aform.Width := 600;
  Aform.Height := 650;
  Aform.top := (Screen.Monitors[0].Height - Aform.Height) div 2;
  Aform.Left := (Screen.Monitors[0].Width - Aform.Width) div 2;
  SaveFormPosition(Aform, TExportForm2.ClassName);
  AForm.free;
end;

finalization
  begin
    while RegisterList.Count > 0 do
      FreeMem(RegisterList.Extract(RegisterList.Last));
    RegisterList.Free;
  end;

end.

