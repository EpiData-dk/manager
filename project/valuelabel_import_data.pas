unit valuelabel_import_data;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ComCtrls, Grids, projectfilelist_frame, epidatafiles, epidatafilestypes;

type

  { TValueLabelDataImport }

  TValueLabelDataImport = class(TForm)
    AddFilesBtn: TBitBtn;
    BitBtn2: TBitBtn;
    OkBtn: TBitBtn;
    Panel1: TPanel;
    ProgressBar1: TProgressBar;
    procedure AddFilesBtnClick(Sender: TObject);
  private
    FProjectFileListFrame: TProjectFileListFrame;
    function ShowDialog(out Files: TStrings): boolean;
    procedure DoAddfiles;
    function GetFieldList(Const DataFile: TEpiDataFile;
      FieldTypes: TEpiFieldTypes): TStrings;
  private
    { Filelist frame - grid}
    FValueFieldColumn: TGridColumn;
    FLabelFieldColumn: TGridColumn;
    procedure SelectEditor(Sender: TObject; aCol, aRow: Integer;
      var Editor: TWinControl);
    procedure PrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure EditorCloseUp(Sender: TObject);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
  end;

var
  ValueLabelDataImport: TValueLabelDataImport;

implementation

{$R *.lfm}
uses
  epidocument, epimiscutils, settings2_var, StdCtrls,
  LCLType, epitools_integritycheck;

type

  { TFieldListEditor }

  TFieldListEditor = class(TPickListCellEditor)
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(TheOwner: TComponent); override;
    property OnCloseUp;
  end;

{ TFieldListEditor }

procedure TFieldListEditor.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Shift = []) and
     (Key in [VK_UP, VK_DOWN]) and
     (not DroppedDown)
  then
    DroppedDown := true;

  inherited KeyDown(Key, Shift);
end;

constructor TFieldListEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Style := csDropDownList;
end;

{ TValueLabelDataImport }

procedure TValueLabelDataImport.AddFilesBtnClick(Sender: TObject);
begin
  DoAddFiles;
end;

procedure TValueLabelDataImport.EditorCloseUp(Sender: TObject);
begin
  FProjectFileListFrame.StructureGrid.EditorMode := false;
end;

procedure TValueLabelDataImport.PrepareCanvas(sender: TObject; aCol,
  aRow: Integer; aState: TGridDrawState);
begin
  if aRow = 0 then exit;

  if (aCol = (FLabelFieldColumn.Index + 1)) or
     (aCol = (FValueFieldColumn.Index + 1))
  then
  begin
    FProjectFileListFrame.StructureGrid.Canvas.Brush.Color := clYellow;
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
     (aCol = (FValueFieldColumn.Index + 1))
  then
  begin
    NewEditor := TFieldListEditor.Create(Self);
    NewEditor.OnCloseUp := @EditorCloseUp;
    NewEditor.AutoSize := false;

    if aCol = (FLabelFieldColumn.Index + 1) then
      FieldTypes := StringFieldTypes
    else
      FieldTypes := ValueLabelFieldTypes;

    List := GetFieldList(
      TEpiDocument(FProjectFileListFrame.DocList.Objects[aRow-1]).DataFiles[0],
      FieldTypes
    );

    NewEditor.Items.Assign(List);
    List.Free;
    NewEditor.DropDownCount := 7;
    Editor := NewEditor;
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
  FieldTypes: TEpiFieldTypes): TStrings;
var
  F: TEpiField;
  i: Integer;
  Checker: TEpiIntegrityChecker;
  Fields: TEpiFields;
  FR: TBoundArray;
  FV: TBoundArray;
begin
  Result := TStringList.Create;
  Result.AddObject('', nil);

  Checker := TEpiIntegrityChecker.Create;
  Fields := TEpiFields.Create(nil);


  for i := 0 to DataFile.Fields.Count - 1 do
  begin
    F := DataFile.Fields[i];
    Fields.AddItem(F);

    if (F.FieldType in FieldTypes) and
       (Checker.IndexIntegrity(DataFile, FR, FV, true, Fields))
    then
      Result.AddObject(F.Name, F);

    Fields.RemoveItem(F);
  end;
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

  FProjectFileListFrame.IncludeCol.Visible := false;
  SG := FProjectFileListFrame.StructureGrid;
  SG.OnSelectEditor := @SelectEditor;
  SG.OnPrepareCanvas := @PrepareCanvas;

  FValueFieldColumn := TGridColumn(SG.Columns.Insert(2));
  FValueFieldColumn.ButtonStyle := cbsPickList;
  FValueFieldColumn.Title.Caption := 'Value Field';

  FLabelFieldColumn := TGridColumn(SG.Columns.Insert(3));
  FLabelFieldColumn.ButtonStyle := cbsPickList;
  FLabelFieldColumn.Title.Caption := 'Label Field';
end;

end.

