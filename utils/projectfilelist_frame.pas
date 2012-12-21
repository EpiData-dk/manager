unit projectfilelist_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, Grids,
  epidocument, epidatafiles;

type

  TProjectListFileEvent = procedure (Sender: TObject; Document: TEpiDocument; Const FileName: string) of object;

  { TProjectFileListFrame }

  TProjectFileListFrame = class(TFrame)
    ErrorListBox: TListBox;
    StructureGrid: TStringGrid;
    procedure StructureGridCheckboxToggled(sender: TObject; aCol,
      aRow: Integer; aState: TCheckboxState);
    procedure StructureGridColRowMoved(Sender: TObject; IsColumn: Boolean;
      sIndex, tIndex: Integer);
  private
    { private declarations }
    FCurrentFile: string;
    FDocList: TStringList;
    FOnAfterImportFile: TProjectListFileEvent;
    FOnBeforeImportFile: TProjectListFileEvent;
    procedure  AddDocumentToGrid(Const FileName: string; Const Doc: TEpiDocument);
    function   GetSelectedList: TStringList;
    procedure  ImportFile(Const FileName: string);
    procedure  ReportError(Const Msg: string);
    procedure  SetOnAfterImportFile(const AValue: TProjectListFileEvent);
    procedure  SetOnBeforeImportFile(const AValue: TProjectListFileEvent);
  protected
    procedure  DoBeforeImportFile(Document: TEpiDocument; Const FileName: string);
    procedure  DoAfterImportFile(Document: TEpiDocument; Const FileName: string);
    procedure  RecImportPassword(Sender: TObject; var Login: string; var Password: string);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   AddFiles(Const Files: TStrings);
    procedure   AddDocument(Const FileName: string; Const Doc: TEpiDocument);
    property    OnBeforeImportFile: TProjectListFileEvent read FOnBeforeImportFile write SetOnBeforeImportFile;
    property    OnAfterImportFile: TProjectListFileEvent read FOnAfterImportFile write SetOnAfterImportFile;
    property    SelectedList: TStringList read GetSelectedList;
    property    DocList: TStringList read FDocList;
  private
    { columns }
    FCreatedCol: TGridColumn;
    FFieldsCol: TGridColumn;
    FFileNameCol: TGridColumn;
    FIncludeCol: TGridColumn;
    FLastEditCol: TGridColumn;
    FNameCol: TGridColumn;
    FRecordsCol: TGridColumn;
    FSectionsCol: TGridColumn;
  public
    property FileNameCol: TGridColumn read FFileNameCol;
    property IncludeCol:  TGridColumn read FIncludeCol;
    property CreatedCol:  TGridColumn read FCreatedCol;
    property LastEditCol: TGridColumn read FLastEditCol;
    property NameCol:     TGridColumn read FNameCol;
    property SectionsCol: TGridColumn read FSectionsCol;
    property FieldsCol:   TGridColumn read FFieldsCol;
    property RecordsCol:  TGridColumn read FRecordsCol;
  end;

implementation

{$R *.lfm}

uses
  epiimport, LCLProc, epimiscutils, Dialogs, managerprocs;

{ TProjectFileListFrame }

procedure TProjectFileListFrame.StructureGridColRowMoved(Sender: TObject;
  IsColumn: Boolean; sIndex, tIndex: Integer);
begin
  if IsColumn then exit;

  FDocList.Move(sIndex - 1, tIndex - 1);
end;

procedure TProjectFileListFrame.AddDocumentToGrid(const FileName: string;
  const Doc: TEpiDocument);
var
  Idx: Integer;
  Ext: String;
begin
  Ext := ExtractFileExt(UTF8LowerCase(FileName));
  with StructureGrid do
  begin
    Idx := RowCount;
    RowCount := RowCount + 1;

    Cells[FileNameCol.Index + 1, Idx] := ExtractFileName(FileName);                           // Filename column.
    Cells[IncludeCol.Index + 1, Idx]  := '1';                                                 // Include row.
    if (ext = '.epx') or (ext ='.epz') then
    begin
      Cells[CreatedCol.Index + 1, Idx]  := DateToStr(Doc.Study.Created); //FormatDateTime('YYYY/MM/DD HH:NN', Doc.Study.Created);                      // Created
      Cells[LastEditCol.Index + 1, Idx] := DateToStr(Doc.Study.ModifiedDate); //FormatDateTime('YYYY/MM/DD HH:NN', Doc.Study.ModifiedDate);                 // Edited
    end else begin
      Cells[CreatedCol.Index + 1, Idx]  := 'N/A';                                                                          // Created
      Cells[LastEditCol.Index + 1, Idx] := DateToStr(FileDateToDateTime(FileAgeUTF8(FileName)));// FormatDateTime('YYYY/MM/DD HH:NN', FileDateToDateTime(FileAgeUTF8(FileName)));  // Edited
    end;
    with Doc.DataFiles[0] do
    begin
      Cells[NameCol.Index + 1, Idx]     := Caption.Text;                                      // Info
      Cells[SectionsCol.Index + 1, Idx] := IntToStr(Sections.Count);                          // Sections
      Cells[FieldsCol.Index + 1, Idx]   := IntToStr(Fields.Count);                            // Fields
      Cells[RecordsCol.Index + 1, Idx]  := IntToStr(Size);                                    // Records
    end;
  end;
  FDocList.AddObject(FileName, Doc);
end;

procedure TProjectFileListFrame.ImportFile(const FileName: string);
var
  Importer: TEpiImport;
  Ext: String;
  Doc: TEpiDocument;
  St: TMemoryStream;
  Idx: Integer;
  DataFile: TEpiDataFile;
begin
  Importer := TEpiImport.Create;
  FCurrentFile := FileName;
  Ext := ExtractFileExt(UTF8LowerCase(FileName));

  try
    Doc := TEpiDocument.Create('en');
    if (ext = '.rec') or (ext = '.dta') then
    begin
      DataFile := Doc.DataFiles.NewDataFile;
      DoBeforeImportFile(Doc, FileName);
      if (ext = '.dta') then
        Importer.ImportStata(FileName, Doc, DataFile, true)
      else begin
        Importer.OnRequestPassword := @RecImportPassword;
        Importer.ImportRec(FileName , DataFile, true);
      end;
      DoAfterImportFile(Doc, FileName);
    end
    else if (ext = '.epx') or (ext = '.epz') then
    begin
      DoBeforeImportFile(Doc, FileName);
      TOpenEpiDoc.OpenDoc(Doc, FileName);
      DoAfterImportFile(Doc, FileName);
    end;

    AddDocumentToGrid(FileName, Doc);
  except
    on E: Exception do
      begin
        ReportError('Failed to read file "' + ExtractFileName(FileName) + '": ' + E.Message);
        Doc.Free;
      end;
  end;
  Importer.Free;
  FCurrentFile := '';
end;

function TProjectFileListFrame.GetSelectedList: TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;

  for i := 1 to StructureGrid.RowCount - 1 do
    if StructureGrid.Cells[IncludeCol.Index + 1, i] <> '0' then
      Result.AddObject(FDocList[i - 1], FDocList.Objects[i - 1]);
end;

procedure TProjectFileListFrame.ReportError(const Msg: string);
begin
  if not ErrorListBox.Visible then
    ErrorListBox.Visible := true;

  ErrorListBox.Items.Add(Msg);
end;

procedure TProjectFileListFrame.SetOnAfterImportFile(
  const AValue: TProjectListFileEvent);
begin
  if FOnAfterImportFile = AValue then exit;
  FOnAfterImportFile := AValue;
end;

procedure TProjectFileListFrame.SetOnBeforeImportFile(
  const AValue: TProjectListFileEvent);
begin
  if FOnBeforeImportFile = AValue then exit;
  FOnBeforeImportFile := AValue;
end;

procedure TProjectFileListFrame.DoBeforeImportFile(Document: TEpiDocument;
  const FileName: string);
begin
  if Assigned(FOnBeforeImportFile) then
    FOnBeforeImportFile(Self, Document, FileName);
end;

procedure TProjectFileListFrame.DoAfterImportFile(Document: TEpiDocument;
  const FileName: string);
begin
  if Assigned(FOnAfterImportFile) then
    FOnAfterImportFile(Self, Document, FileName);
end;

procedure TProjectFileListFrame.RecImportPassword(Sender: TObject;
  var Login: string; var Password: string);
begin
  Login := '';
  Password :=
    PasswordBox('IMPORTANT!',
    ' Password needed for "' + ExtractFileName(FCurrentFile) + '":');
end;

procedure TProjectFileListFrame.StructureGridCheckboxToggled(sender: TObject;
  aCol, aRow: Integer; aState: TCheckboxState);
begin
  // Include the file in the list?
end;

constructor TProjectFileListFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FDocList := TStringList.Create;

  // Preserve ADD order!
  FFileNameCol := StructureGrid.Columns.Add;
  FFileNameCol.Title.Caption := 'Filename';
  FFileNameCol.ReadOnly := true;

  FIncludeCol := StructureGrid.Columns.Add;
  FIncludeCol.Title.Caption := 'Include';
  FIncludeCol.ButtonStyle := cbsCheckboxColumn;

  FFieldsCol := StructureGrid.Columns.Add;
  FFieldsCol.Title.Caption := 'Fields';
  FFieldsCol.ReadOnly := true;

  FSectionsCol := StructureGrid.Columns.Add;
  FSectionsCol.Title.Caption := 'Sections';
  FSectionsCol.ReadOnly := true;

  FRecordsCol := StructureGrid.Columns.Add;
  FRecordsCol.Title.Caption := 'Records';
  FRecordsCol.ReadOnly := true;

  FCreatedCol := StructureGrid.Columns.Add;
  FCreatedCol.Title.Caption := 'Created';
  FCreatedCol.ReadOnly := true;

  FLastEditCol := StructureGrid.Columns.Add;
  FLastEditCol.Title.Caption := 'Last edited';
  FLastEditCol.ReadOnly := true;

  FNameCol := StructureGrid.Columns.Add;
  FNameCol.Title.Caption := 'Project Title';
  FNameCol.ReadOnly := true;
end;

destructor TProjectFileListFrame.Destroy;
begin
  FDocList.Free;
  inherited Destroy;
end;

procedure TProjectFileListFrame.AddFiles(const Files: TStrings);
var
  i: Integer;
begin
  if Assigned(Files) then
    for i := 0 to Files.Count -1 do
      ImportFile(Files[i]);
  StructureGrid.AutoAdjustColumns;
end;

procedure TProjectFileListFrame.AddDocument(const FileName: string;
  const Doc: TEpiDocument);
begin
  AddDocumentToGrid(FileName, Doc);
end;

end.

