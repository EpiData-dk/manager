unit valuelabel_import_data;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ComCtrls, Grids, StdCtrls, projectfilelist_frame, epidatafiles,
  epidatafilestypes, epivaluelabels, epidocument;

type

  { TValueLabelDataImport }

  TValueLabelDataImport = class(TForm)
    AddFilesBtn: TBitBtn;
    BitBtn2: TBitBtn;
    OkBtn: TBitBtn;
    Panel1: TPanel;
    ProgressBar1: TProgressBar;
    procedure AddFilesBtnClick(Sender: TObject);
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OkBtnClick(Sender: TObject);
    procedure SelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
  private
    FImportCount: Integer;
    FProjectFileListFrame: TProjectFileListFrame;
    function ShowDialog(out Files: TStrings): boolean;
    procedure DoAddfiles;
    function GetFieldList(Const DataFile: TEpiDataFile;
      FieldTypes: TEpiFieldTypes;
      CheckIndex: boolean): TStrings;
  private
    { Filelist frame - grid}
    FEditCol: Integer;
    FEditRow: Integer;
    FValueFieldColumn: TGridColumn;
    FLabelFieldColumn: TGridColumn;
    FMissingFieldColumn: TGridColumn;
    FValueLabelSets: TEpiValueLabelSets;
    procedure AsyncEditorMode(Data: PtrInt);
    procedure ProjectFileListCallBack(Sender: TObject; Document: TEpiDocument;
      const Filename: string; const RowNo: Integer);
    procedure SelectedItem(Sender: TObject);
    procedure SelectEditor(Sender: TObject; aCol, aRow: Integer;
      var Editor: TWinControl);
    procedure PrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure EditorCloseUp(Sender: TObject);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    property ValueLabelSets: TEpiValueLabelSets read FValueLabelSets write FValueLabelSets;
  end;

var
  ValueLabelDataImport: TValueLabelDataImport;

implementation

{$R *.lfm}
uses
  epimiscutils, settings2_var,
  LCLType, epitools_integritycheck,
  epistringutils, LMessages;

type

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
end;

procedure TValueLabelDataImport.OkBtnClick(Sender: TObject);
begin
  // Do Checks for correct choices
  FImportCount := 0;

  Screen.Cursor := crHourGlass;
  Application.ProcessMessages;
  try
    FProjectFileListFrame.ForEachIncluded(@ProjectFileListCallBack);
  finally
    Screen.Cursor := crDefault;
    Application.ProcessMessages;
    if FImportCount > 0 then
      ShowMessage('Successfully created ' + IntToStr(FImportCount) + ' Value Label Sets');
  end;
end;

procedure TValueLabelDataImport.SelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
begin
  if (aCol = FValueFieldColumn.Index + 1) or
     (aCol = FLabelFieldColumn.Index + 1) or
     (aCol = FMissingFieldColumn.Index + 1) or
     (aCol = FProjectFileListFrame.IncludeCol.Index + 1)
  then
    CanSelect := true
  else
    CanSelect := false;


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
  SG.Objects[FEditCol, FEditRow] := CB.Items.Objects[CB.ItemIndex];
end;

procedure TValueLabelDataImport.ProjectFileListCallBack(Sender: TObject;
  Document: TEpiDocument; const Filename: string; const RowNo: Integer);
var
  ValIdx: Integer;
  LabIdx: Integer;
  MisIdx: Integer;
  SVal: String;
  SLab: String;
  SMis: String;
  FVal: TEpiField;
  FLab: TEpiField;
  FMis: TEpiField;
  VLSet: TEpiValueLabelSet;
  VL: TEpiCustomValueLabel;
  i: Integer;
begin
  ValIdx := FValueFieldColumn.Index + 1;
  LabIdx := FLabelFieldColumn.Index + 1;
  MisIdx := FMissingFieldColumn.Index + 1;

  FVal := TEpiField(FProjectFileListFrame.StructureGrid.Objects[ValIdx, RowNo]);
  FLab := TEpiField(FProjectFileListFrame.StructureGrid.Objects[LabIdx, RowNo]);
  FMis := TEpiField(FProjectFileListFrame.StructureGrid.Objects[MisIdx, RowNo]);

  if (not Assigned(FVal)) or
     (not Assigned(FLab))
  then
    Exit;

  VLSet := ValueLabelSets.NewValueLabelSet(FVal.FieldType);
  VLSet.BeginUpdate;
  VLSet.Name := '_' + FVal.Name;

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
end;

procedure TValueLabelDataImport.EditorCloseUp(Sender: TObject);
begin
  FProjectFileListFrame.StructureGrid.EditorMode := false;
  FEditCol := -1;
  FEditRow := -1;
end;

procedure TValueLabelDataImport.PrepareCanvas(sender: TObject; aCol,
  aRow: Integer; aState: TGridDrawState);
begin
  if aRow = 0 then exit;

  if (aCol = (FLabelFieldColumn.Index + 1)) or
     (aCol = (FValueFieldColumn.Index + 1)) or
     (aCol = (FMissingFieldColumn.Index + 1))
  then
  begin
//    FProjectFileListFrame.StructureGrid.Canvas.Brush.Color := clYellow;
  end;
end;

procedure TValueLabelDataImport.SelectEditor(Sender: TObject; aCol, aRow: Integer;
  var Editor: TWinControl);
var
  NewEditor: TFieldListEditor;
  PickEditor: TPickListCellEditor absolute Editor;
  FieldTypes: TEpiFieldTypes;
  List: TStrings;
begin
  if (aCol = (FLabelFieldColumn.Index + 1)) or
     (aCol = (FValueFieldColumn.Index + 1)) or
     (aCol = (FMissingFieldColumn.Index + 1))
  then
  begin
    NewEditor := TFieldListEditor.Create(Self, Self);
    NewEditor.OnCloseUp := @EditorCloseUp;
    NewEditor.OnSelect := @SelectedItem;
    NewEditor.AutoSize := false;

    if aCol = (FLabelFieldColumn.Index + 1) then
      FieldTypes := StringFieldTypes;
    if aCol = (FValueFieldColumn.Index + 1) then
      FieldTypes := ValueLabelFieldTypes;
    if aCol = (FMissingFieldColumn.Index + 1) then
      FieldTypes := (BoolFieldTypes + IntFieldTypes + StringFieldTypes + FloatFieldTypes) - AutoFieldTypes;

    List := GetFieldList(
      TEpiDocument(FProjectFileListFrame.DocList.Objects[aRow-1]).DataFiles[0],
      FieldTypes,
      aCol = (FValueFieldColumn.Index + 1)
    );

    NewEditor.Items.Assign(List);
    List.Free;
    NewEditor.DropDownCount := 7;
    Editor := NewEditor;

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
    ProgressBar1.Visible := true;
    ProgressBar1.Min := 0;
    ProgressBar1.Max := Files.Count;
    ProgressBar1.Step := 1;

    Application.ProcessMessages;
    FProjectFileListFrame.AddFiles(Files);
    Files.Free;

    ProgressBar1.Visible := false;
  end;
end;

function TValueLabelDataImport.GetFieldList(const DataFile: TEpiDataFile;
  FieldTypes: TEpiFieldTypes; CheckIndex: boolean): TStrings;
var
  F: TEpiField;
  i: Integer;
  Checker: TEpiIntegrityChecker;
  Fields: TEpiFields;
  FR: TBoundArray;
  FV: TBoundArray;
  UnsupportedTypes: TStringList;
  IndexFail: TStringList;
  S: String;
begin
  Result := TStringList.Create;
  Result.AddObject('', nil);

  Checker := TEpiIntegrityChecker.Create;
  Fields := TEpiFields.Create(nil);

  UnsupportedTypes := TStringList.Create;
  IndexFail := TStringList.Create;


  for i := 0 to DataFile.Fields.Count - 1 do
  begin
    F := DataFile.Fields[i];
    S := EpiCutString(F.Name + ' (' + F.Question.Text + ')', 20);

    if not (F.FieldType in FieldTypes) then
    begin
      UnsupportedTypes.Add(S);
      Continue;
    end;

    if CheckIndex then
    begin
      Fields.AddItem(F);
      if (Checker.IndexIntegrity(DataFile, FR, FV, true, Fields)) then
        Result.AddObject(S, F)
      else
        IndexFail.Add(S);

      Fields.RemoveItem(F);
    end
    else
      Result.AddObject(S, F);

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
  Checker.Free;
end;

constructor TValueLabelDataImport.Create(TheOwner: TComponent);
var
  SG: TStringGrid;
begin
  inherited Create(TheOwner);
  FProjectFileListFrame := TProjectFileListFrame.Create(Self);
  FProjectFileListFrame.Align := alClient;
  FProjectFileListFrame.Parent := Self;

//  FProjectFileListFrame.IncludeCol.Visible := false;
  SG := FProjectFileListFrame.StructureGrid;
  SG.OnSelectEditor := @SelectEditor;
  SG.OnPrepareCanvas := @PrepareCanvas;
  SG.OnMouseDown := @MouseDown;

  FValueFieldColumn := TGridColumn(SG.Columns.Insert(2));
  FValueFieldColumn.ButtonStyle := cbsPickList;
  FValueFieldColumn.Title.Caption := 'Value Field';

  FLabelFieldColumn := TGridColumn(SG.Columns.Insert(3));
  FLabelFieldColumn.ButtonStyle := cbsPickList;
  FLabelFieldColumn.Title.Caption := 'Label Field';

  FMissingFieldColumn := TGridColumn(SG.Columns.Insert(4));
  FMissingFieldColumn.ButtonStyle := cbsPickList;
  FMissingFieldColumn.Title.Caption := 'Missing Field';

  SG.OnSelectCell := @SelectCell;
end;

end.

