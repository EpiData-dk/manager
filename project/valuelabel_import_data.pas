unit valuelabel_import_data;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ComCtrls, Grids, StdCtrls, projectfilelist_frame, epidatafiles,
  epidatafilestypes, epivaluelabels, epidocument, epiopenfile;

type

  { TValueLabelDataImport }

  TValueLabelDataImport = class(TForm)
    AddFilesBtn: TBitBtn;
    AddCBBtn: TBitBtn;
    BitBtn2: TBitBtn;
    OkBtn: TBitBtn;
    Panel1: TPanel;
    ProgressBar1: TProgressBar;
    procedure AddCBBtnClick(Sender: TObject);
    procedure AddFilesBtnClick(Sender: TObject);
    procedure AfterAddToGrid(Sender: TObject; Document: TEpiDocument;
      const Filename: string; const RowNo: Integer);
    procedure AfterFileImport(Sender: TObject; Document: TEpiDocument;
      const FileName: string);
    procedure CellHint(Sender: TObject; ACol, ARow: Integer;
      var HintText: String);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OkBtnClick(Sender: TObject);
    procedure SelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
  private
    FSuccesImportList: TStringList;
    FImportCount: Integer;
    FProjectFileListFrame: TProjectFileListFrame;
    function ShowDialog(out Files: TStrings): boolean;
    procedure DoAddfiles;
    function GetFieldList(Const DataFile: TEpiDataFile;
      FieldTypes: TEpiFieldTypes;
      CheckType: Byte  // 0=none, 1=Index, 2=Non-missing
      ): TStrings;
    procedure UpdateCaption;
  private
    { Filelist frame - grid}
    FDocFile: TEpiDocumentFile;
    FEditCol: Integer;
    FEditRow: Integer;
    FEditor: TPickListCellEditor;
    FValueFieldColumn: TGridColumn;
    FLabelFieldColumn: TGridColumn;
    FMissingFieldColumn: TGridColumn;
    procedure AsyncEditorMode(Data: PtrInt);
    function  GetValueLabelSets: TEpiValueLabelSets;
    procedure ProjectFileListCallBack(Sender: TObject; Document: TEpiDocument;
      const Filename: string; const RowNo: Integer);
    procedure SelectedItem(Sender: TObject);
    procedure SelectEditor(Sender: TObject; aCol, aRow: Integer;
      var Editor: TWinControl);
    procedure PrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure EditorCloseUp(Sender: TObject);
    procedure SetDocFile(AValue: TEpiDocumentFile);
    procedure LoadGlyphs;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    class procedure RestoreDefaultPos;
    property ValueLabelSets: TEpiValueLabelSets read GetValueLabelSets;
    property DocFile: TEpiDocumentFile read FDocFile write SetDocFile;
  end;

var
  ValueLabelDataImport: TValueLabelDataImport;

implementation

{$R *.lfm}
uses
  epimiscutils, settings2_var, settings2,
  LCLType, epitools_integritycheck,
  epistringutils, epiv_datamodule;

const
  FormName = 'ValueLabelDataImport';

type

  { TGridObject }

  TGridObject = class(TComponent)
  public
    FieldList: TStrings;
    SelectedField: TEpiField;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TFieldListEditor }

  TFieldListEditor = class(TPickListCellEditor)
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  private
    FValueDataImportForm: TValueLabelDataImport;
  public
    constructor Create(TheOwner: TComponent; AForm: TValueLabelDataImport);
    property OnCloseUp;
    property OnSelect;
  end;

constructor TGridObject.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FieldList := TStringList.Create;
end;

destructor TGridObject.Destroy;
begin
  FieldList.Destroy;
  inherited Destroy;
end;

{ TFieldListEditor }

procedure TFieldListEditor.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Shift = []) and
     (Key in [VK_UP, VK_DOWN]) and
     (not DroppedDown)
  then
    DroppedDown := true;

  if (Shift = []) and
     (Key = VK_ESCAPE)
  then
  begin
    Application.QueueAsyncCall(@FValueDataImportForm.AsyncEditorMode, 0);
    Key := VK_UNKNOWN;
  end;

  inherited KeyDown(Key, Shift);
end;

constructor TFieldListEditor.Create(TheOwner: TComponent;
  AForm: TValueLabelDataImport);
begin
  inherited Create(TheOwner);
  FValueDataImportForm := AForm;
  Style := csDropDownList;
end;

{ TValueLabelDataImport }

procedure TValueLabelDataImport.AddFilesBtnClick(Sender: TObject);
begin
  DoAddFiles;
end;

procedure TValueLabelDataImport.AfterAddToGrid(Sender: TObject;
  Document: TEpiDocument; const Filename: string; const RowNo: Integer);
var
  GO: TGridObject;
begin
  with FProjectFileListFrame do
  begin
    StructureGrid.Cells[IncludeCol.Index + 1, StructureGrid.RowCount - 1] := IncludeCol.ValueUnchecked;

    GO := TGridObject.Create(Self);
    GO.FieldList.Assign(GetFieldList(Document.DataFiles[0], ValueLabelFieldTypes, 1));
    StructureGrid.Objects[FValueFieldColumn.Index + 1, RowNo] := GO;

    GO := TGridObject.Create(Self);
    GO.FieldList.Assign(GetFieldList(Document.DataFiles[0], AllFieldTypes, 2));
    StructureGrid.Objects[FLabelFieldColumn.Index + 1, RowNo] := GO;

    GO := TGridObject.Create(Self);
    GO.FieldList.Assign(GetFieldList(Document.DataFiles[0], (BoolFieldTypes + IntFieldTypes + StringFieldTypes + FloatFieldTypes) - AutoFieldTypes, 0));
    StructureGrid.Objects[FMissingFieldColumn.Index + 1, RowNo] := GO;
  end;
end;

procedure TValueLabelDataImport.AfterFileImport(Sender: TObject;
  Document: TEpiDocument; const FileName: string);
begin
  if not ProgressBar1.Visible then exit;
  ProgressBar1.StepIt;
end;

procedure TValueLabelDataImport.CellHint(Sender: TObject; ACol, ARow: Integer;
  var HintText: String);
begin
  HintText := '';

  if ACol = (FValueFieldColumn.Index + 1) then
    HintText := 'Select field for category content';

  if ACol = (FLabelFieldColumn.Index + 1) then
    HintText := 'Select field containing descriptive label';

  if ACol = (FMissingFieldColumn.Index + 1) then
    HintText := 'Select field indicating missing (value is 1, "Y", "TRUE")';
end;

procedure TValueLabelDataImport.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  if ManagerSettings.SaveWindowPositions then
    SaveFormPosition(Self, FormName);
end;

procedure TValueLabelDataImport.AddCBBtnClick(Sender: TObject);
var
  Files: TStringList;
begin
  Files := TStringList.Create;
  Files.Add('');
  FProjectFileListFrame.AddFiles(Files);
  Files.Free;
end;

procedure TValueLabelDataImport.FormShow(Sender: TObject);
begin
  if ManagerSettings.SaveWindowPositions then
    LoadFormPosition(Self, FormName);

  UpdateCaption;
  FProjectFileListFrame.StructureGrid.AutoSizeColumns;
end;

procedure TValueLabelDataImport.MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ACol: Longint;
  ARow: Longint;
  SG: TStringGrid;
begin
  SG := FProjectFileListFrame.StructureGrid;
  SG.MouseToCell(X,Y, ACol, ARow);

  if (not (
           (ARow = FEditRow) and
           (ACol = FEditCol)
          )
     ) and
     SG.EditorMode
  then
    SG.EditorMode := false;
end;

procedure TValueLabelDataImport.AsyncEditorMode(Data: PtrInt);
begin
  FProjectFileListFrame.StructureGrid.EditorMode := Boolean(Data);

  if not Boolean(Data) then
  begin
    FEditCol := -1;
    FEditRow := -1;
  end;
end;

function TValueLabelDataImport.GetValueLabelSets: TEpiValueLabelSets;
begin
  result := nil;

  if Assigned(DocFile) then
    result := DocFile.Document.ValueLabelSets;
end;

procedure TValueLabelDataImport.OkBtnClick(Sender: TObject);
begin
  // Do Checks for correct choices
  FImportCount := 0;
  FSuccesImportList.Clear;
  FSuccesImportList.Add('Project: ' + ExtractFileName(FDocFile.FileName));

  Screen.Cursor := crHourGlass;
  Application.ProcessMessages;
  try
    FProjectFileListFrame.ForEachIncluded(@ProjectFileListCallBack);
  finally
    Screen.Cursor := crDefault;
    Application.ProcessMessages;
    FSuccesImportList.Insert(1, 'Successfully added ' + IntToStr(FImportCount) + ' Value Label Sets');

    if FImportCount > 0 then
      ShowMessage(FSuccesImportList.Text);
  end;
end;

procedure TValueLabelDataImport.SelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
begin
  CanSelect := false;

  if (aCol = FProjectFileListFrame.IncludeCol.Index + 1) then
  begin
    CanSelect := true;
    FProjectFileListFrame.StructureGrid.InvalidateRow(aRow);
  end;

  if (aCol = FValueFieldColumn.Index + 1) or
     (aCol = FLabelFieldColumn.Index + 1) or
     (aCol = FMissingFieldColumn.Index + 1)
  then
  with FProjectFileListFrame do
    CanSelect := StructureGrid.Cells[IncludeCol.Index + 1, aRow] = IncludeCol.ValueChecked;


  if (aCol = FValueFieldColumn.Index + 1) or
     (aCol = FLabelFieldColumn.Index + 1) or
     (aCol = FMissingFieldColumn.Index + 1)
  then
  begin
    if ((FEditCol <> aCol) or (FEditRow <> aRow)) and
       (FProjectFileListFrame.StructureGrid.EditorMode)
    then
      FProjectFileListFrame.StructureGrid.EditorMode := false;

    Application.QueueAsyncCall(@AsyncEditorMode, 1);
  end;
end;

procedure TValueLabelDataImport.SelectedItem(Sender: TObject);
var
  SG: TStringGrid;
  CB: TCustomComboBox absolute Sender;
begin
  SG := FProjectFileListFrame.StructureGrid;
  TGridObject(SG.Objects[FEditCol, FEditRow]).SelectedField := TEpiField(CB.Items.Objects[CB.ItemIndex]);
end;

procedure TValueLabelDataImport.ProjectFileListCallBack(Sender: TObject;
  Document: TEpiDocument; const Filename: string; const RowNo: Integer);
var
  ValIdx: Integer;
  LabIdx: Integer;
  MisIdx: Integer;
  FVal: TEpiField;
  FLab: TEpiField;
  FMis: TEpiField;
  VLSet: TEpiValueLabelSet;
  VL: TEpiCustomValueLabel;
  i: Integer;
  SG: TStringGrid;
  S: String;
  Ft: TEpiFieldType;
begin
  ValIdx := FValueFieldColumn.Index + 1;
  LabIdx := FLabelFieldColumn.Index + 1;
  MisIdx := FMissingFieldColumn.Index + 1;

  SG := FProjectFileListFrame.StructureGrid;
  FVal := TEpiField(TGridObject(SG.Objects[ValIdx, RowNo]).SelectedField);
  FLab := TEpiField(TGridObject(SG.Objects[LabIdx, RowNo]).SelectedField);
  FMis := TEpiField(TGridObject(SG.Objects[MisIdx, RowNo]).SelectedField);

  if (not Assigned(FVal)) or
     (not Assigned(FLab))
  then
    Exit;

  // We can read and understand uppercase fieldtypes, but a valuelabelset
  // can never be defined as uppercase.
  Ft := FVal.FieldType;
  if Ft = ftUpperString then
    Ft := ftString;

  VLSet := ValueLabelSets.NewValueLabelSet(Ft);
  VLSet.BeginUpdate;
  S := '_' + FVal.Name;
  i := 0;

  while (not ValueLabelSets.ValidateRename(VLSet, S)) do
  begin
    inc(i);
    S := '_' + FVal.Name + '_' + IntToStr(i);
  end;
  VLSet.Name := S;

  for i := 0 to FVal.Size -1 do
  begin
    VL := VLSet.NewValueLabel;
    VL.TheLabel.Text := FLab.AsString[i];
    case FVal.FieldType of
      ftInteger:
        TEpiIntValueLabel(VL).Value    := FVal.AsInteger[i];
      ftFloat:
        TEpiFloatValueLabel(VL).Value  := FVal.AsFloat[i];
      ftString, ftUpperString:
        TEpiStringValueLabel(VL).Value := FVal.AsString[i];
    end;
    if Assigned(FMis) then
      VL.IsMissingValue := FMis.AsBoolean[i] = 1;
  end;
  VLSet.EndUpdate;
  Inc(FImportCount);

  S := '  ' + IntToStr(FImportCount) + ') ' + VLSet.Name + ' - Fields: ' + FVal.Name + ', ' + FLab.Name;
  if Assigned(FMis) then
    S += ', ' + FMis.Name;
  S += ' from ' + SG.Cells[FProjectFileListFrame.FileNameCol.Index + 1, RowNo];

  FSuccesImportList.Add(S);
end;

procedure TValueLabelDataImport.EditorCloseUp(Sender: TObject);
begin
  Application.QueueAsyncCall(@AsyncEditorMode, 0);
end;

procedure TValueLabelDataImport.SetDocFile(AValue: TEpiDocumentFile);
begin
  if FDocFile = AValue then Exit;
  FDocFile := AValue;

  UpdateCaption;
end;

procedure TValueLabelDataImport.LoadGlyphs;
begin
  DM.Icons16.GetBitmap(19, AddFilesBtn.Glyph);
  DM.Icons16.GetBitmap(19, AddCBBtn.Glyph);
end;

procedure TValueLabelDataImport.PrepareCanvas(sender: TObject; aCol,
  aRow: Integer; aState: TGridDrawState);
var
  SG: TStringGrid;
begin
  if aRow = 0 then exit;

  SG := FProjectFileListFrame.StructureGrid;

  if (aCol = (FLabelFieldColumn.Index + 1)) or
     (aCol = (FValueFieldColumn.Index + 1))
  then
  begin
    if (not Assigned(TGridObject(SG.Objects[aCol, aRow]).SelectedField)) and
       (SG.Cells[FProjectFileListFrame.IncludeCol.Index + 1, aRow] = FProjectFileListFrame.IncludeCol.ValueChecked)
    then
      FProjectFileListFrame.StructureGrid.Canvas.Brush.Color := clBtnShadow;
  end;

  if (aCol = (FMissingFieldColumn.Index + 1))
  then
  begin
    if (not Assigned(TGridObject(SG.Objects[aCol, aRow]).SelectedField)) and
       (SG.Cells[FProjectFileListFrame.IncludeCol.Index + 1, aRow] = FProjectFileListFrame.IncludeCol.ValueChecked)
    then
      FProjectFileListFrame.StructureGrid.Canvas.Brush.Color := clBtnFace;
  end;
end;

procedure TValueLabelDataImport.SelectEditor(Sender: TObject; aCol, aRow: Integer;
  var Editor: TWinControl);
begin
  if (aCol = (FLabelFieldColumn.Index + 1)) or
     (aCol = (FValueFieldColumn.Index + 1)) or
     (aCol = (FMissingFieldColumn.Index + 1))
  then
  begin
    FEditor.Items.Assign(TGridObject(FProjectFileListFrame.StructureGrid.Objects[aCol, aRow]).FieldList);
    Editor := FEditor;

    FEditCol := aCol;
    FEditRow := aRow;
  end;
end;

function TValueLabelDataImport.ShowDialog(out Files: TStrings): boolean;
var
  Dlg: TOpenDialog;
begin
  // Result = true, is a confirmation that the dialog was execute
  // and the user selected some files
  Result := false;

  Files := nil;
  Dlg := nil;

  try
    Dlg := TOpenDialog.Create(Self);
    Dlg.InitialDir := ManagerSettings.WorkingDirUTF8;
    Dlg.Filter := GetEpiDialogFilter(dfImport + [dfCollection]);
    Dlg.Options := [ofAllowMultiSelect, ofFileMustExist, ofEnableSizing, ofViewDetail];
    if not Dlg.Execute then exit;

    Files := TStringList.Create;
    Files.Assign(Dlg.Files);
    Result := true;
  finally
    Dlg.Free;
  end;
end;

procedure TValueLabelDataImport.DoAddfiles;
var
  Files: TStrings;
begin
  if ShowDialog(Files) then
  begin
    ProgressBar1.Min := 0;
    ProgressBar1.Max := Files.Count;
    ProgressBar1.Step := 1;
    ProgressBar1.Visible := true;

    Screen.Cursor := crHourGlass;
    Application.ProcessMessages;

    FProjectFileListFrame.AddFiles(Files);
    Files.Free;
    ProgressBar1.Visible := false;

    Screen.Cursor := crDefault;
    Application.ProcessMessages;
  end;
end;

function TValueLabelDataImport.GetFieldList(const DataFile: TEpiDataFile;
  FieldTypes: TEpiFieldTypes; CheckType: Byte): TStrings;
var
  F: TEpiField;
  i: Integer;
  IndexChecker: TEpiIntegrityChecker;
  Fields: TEpiFields;
  FR: TBoundArray;
  FV: TBoundArray;
  UnsupportedTypes: TStringList;
  IndexFail: TStringList;
  S: String;
  j: Integer;
begin
  Result := TStringList.Create;
  Result.AddObject('', nil);

  IndexChecker := TEpiIntegrityChecker.Create;
  Fields := TEpiFields.Create(nil);

  UnsupportedTypes := TStringList.Create;
  IndexFail := TStringList.Create;


  for i := 0 to DataFile.Fields.Count - 1 do
  begin
    F := DataFile.Fields[i];
    S := F.Name;
    if F.Question.Text <> '' then
      S += ' (' + F.Question.Text + ')';

    if not (F.FieldType in FieldTypes) then
    begin
      UnsupportedTypes.Add(S);
      Continue;
    end;

    Case CheckType of
      0:
        Result.AddObject(S, F);
      1:
        begin
          Fields.AddItem(F);
          if (IndexChecker.IndexIntegrity(DataFile, FR, FV, true, Fields)) then
            Result.AddObject(S, F)
          else
            IndexFail.Add(S);

          Fields.RemoveItem(F);
        end;
      2:
        begin
          for j := 0 to F.Size - 1 do
            if F.IsMissing[j] then
              Break;

          if F.IsMissing[j] then
            IndexFail.Add(S)
          else
            Result.AddObject(S, F);
        end;
    end;
  end;
  if IndexFail.Count > 0 then
    Result.Add(' --- NON-UNIQUE FIELDS --- ');
  Result.AddStrings(IndexFail);

  if UnsupportedTypes.Count > 0 then
    Result.Add(' --- UNSUPPORTED TYPES --- ');
  Result.AddStrings(UnsupportedTypes);

  IndexFail.Free;
  UnsupportedTypes.Free;
  Fields.Free;
  IndexChecker.Free;
end;

procedure TValueLabelDataImport.UpdateCaption;
var
  S: String;
begin
  S := 'Import Value Labels';

  if Assigned(DocFile) then
    S += ' into: ' + DocFile.FileName;

  Caption := S;
end;

constructor TValueLabelDataImport.Create(TheOwner: TComponent);
var
  SG: TStringGrid;
begin
  inherited Create(TheOwner);
  LoadGlyphs;

  FProjectFileListFrame := TProjectFileListFrame.Create(Self);
  FProjectFileListFrame.Align := alClient;
  FProjectFileListFrame.Parent := Self;
  FProjectFileListFrame.OnAfterImportFile := @AfterFileImport;
  FProjectFileListFrame.OnAfterAddToGrid := @AfterAddToGrid;

  SG := FProjectFileListFrame.StructureGrid;
  SG.OnSelectEditor := @SelectEditor;
  SG.OnPrepareCanvas := @PrepareCanvas;
  SG.OnMouseDown := @MouseDown;
  SG.Options := SG.Options + [goCellHints];
  SG.ShowHint := true;
  SG.OnGetCellHint := @CellHint;

  FValueFieldColumn := TGridColumn(SG.Columns.Insert(2));
  FValueFieldColumn.ButtonStyle := cbsPickList;
  FValueFieldColumn.Title.Caption := 'Value Field';

  FLabelFieldColumn := TGridColumn(SG.Columns.Insert(3));
  FLabelFieldColumn.ButtonStyle := cbsPickList;
  FLabelFieldColumn.Title.Caption := 'Label Field';

  FMissingFieldColumn := TGridColumn(SG.Columns.Insert(4));
  FMissingFieldColumn.ButtonStyle := cbsPickList;
  FMissingFieldColumn.Title.Caption := 'Missing Field';

  FEditor := TFieldListEditor.Create(Self, Self);
  with TFieldListEditor(FEditor) do
  begin
    OnCloseUp := @EditorCloseUp;
    OnSelect := @SelectedItem;
  end;
  FEditor.AutoSize := false;

  SG.OnSelectCell := @SelectCell;
  FSuccesImportList := TStringList.Create;
end;

class procedure TValueLabelDataImport.RestoreDefaultPos;
var
  Aform: TForm;
begin
  Aform := TForm.Create(nil);
  Aform.Width := 600;
  Aform.Height := 480;
  Aform.top := (Screen.Monitors[0].Height - Aform.Height) div 2;
  Aform.Left := (Screen.Monitors[0].Width - Aform.Width) div 2;
  SaveFormPosition(Aform, FormName);
  AForm.free;
end;

end.

