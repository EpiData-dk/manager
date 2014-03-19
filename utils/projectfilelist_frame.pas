unit projectfilelist_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, Grids, ComCtrls,
  ExtCtrls, epidocument, epidatafiles, epicustombase;

type

  TProjectListFileEvent = procedure (Sender: TObject; Document: TEpiDocument;
    Const FileName: string) of object;
  TProjectFileListGridEvent = procedure (Sender: TObject; Document: TEpiDocument;
    Const Filename: string; Const RowNo: Integer) of object;

  { TProjectFileListFrame }

  TProjectFileListFrame = class(TFrame)
    ErrorListBox: TListBox;
    Panel1: TPanel;
    ProgressBar1: TProgressBar;
    StructureGrid: TStringGrid;
    procedure Progress(const Sender: TEpiCustomBase;
      ProgressType: TEpiProgressType; CurrentPos, MaxPos: Cardinal;
      var Canceled: Boolean);
    procedure StructureGridCheckboxToggled(sender: TObject; aCol,
      aRow: Integer; aState: TCheckboxState);
    procedure StructureGridColRowMoved(Sender: TObject; IsColumn: Boolean;
      sIndex, tIndex: Integer);
    procedure ClipboardRead(ClipBoardLine: TStrings);
  private
    { private declarations }
    FCurrentFile: string;
    FDocList: TStringList;
    FOnAfterAddToGrid: TProjectFileListGridEvent;
    FOnAfterImportFile: TProjectListFileEvent;
    FOnBeforeImportFile: TProjectListFileEvent;
    FOnSelectionChanged: TNotifyEvent;
    procedure  AddDocumentToGrid(Const FileName: string; Const Doc: TEpiDocument);
    function   GetSelectedList: TStringList;
    procedure  ImportFile(Const FileName: string);
    procedure  DoReportError(Const Msg: string);
    procedure  SetOnAfterImportFile(const AValue: TProjectListFileEvent);
    procedure  SetOnBeforeImportFile(const AValue: TProjectListFileEvent);
  protected
    procedure  DoAfterGridEvent(Const Filename: string; Const Document: TEpiDocument;
      Const RowNo: Integer);
    procedure  DoSelectionChanged;
    procedure  DoBeforeImportFile(Document: TEpiDocument; Const FileName: string);
    procedure  DoAfterImportFile(Document: TEpiDocument; Const FileName: string);
    procedure  RecImportPassword(Sender: TObject; var Login: string; var Password: string);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   AddFiles(Const Files: TStrings);
    procedure   AddDocument(Const FileName: string; Const Doc: TEpiDocument);
    procedure   ForEachIncluded(CallBackMethod: TProjectFileListGridEvent);
    procedure   ReportError(Const Msg: string);
    property    OnBeforeImportFile: TProjectListFileEvent read FOnBeforeImportFile write SetOnBeforeImportFile;
    property    OnAfterImportFile: TProjectListFileEvent read FOnAfterImportFile write SetOnAfterImportFile;
    property    OnSelectionChanged: TNotifyEvent read FOnSelectionChanged write FOnSelectionChanged;
    property    OnAfterAddToGrid: TProjectFileListGridEvent read FOnAfterAddToGrid write FOnAfterAddToGrid;
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
    FVersionCol: TGridColumn;
    FCycleCol: TGridColumn;
  public
    property FileNameCol: TGridColumn read FFileNameCol;
    property IncludeCol:  TGridColumn read FIncludeCol;
    property CreatedCol:  TGridColumn read FCreatedCol;
    property LastEditCol: TGridColumn read FLastEditCol;
    property NameCol:     TGridColumn read FNameCol;
    property SectionsCol: TGridColumn read FSectionsCol;
    property FieldsCol:   TGridColumn read FFieldsCol;
    property RecordsCol:  TGridColumn read FRecordsCol;
    property VersionCol:  TGridColumn read FVersionCol;
    property CycleCol:    TGridColumn read FCycleCol;
  end;

implementation

{$R *.lfm}

uses
  epiimport, LCLProc, epimiscutils, Dialogs, managerprocs, epiv_documentfile,
  settings2_var, Clipbrd, LCLIntf, LCLType;

{ TProjectFileListFrame }

procedure TProjectFileListFrame.StructureGridColRowMoved(Sender: TObject;
  IsColumn: Boolean; sIndex, tIndex: Integer);
begin
  if IsColumn then exit;

  FDocList.Move(sIndex - 1, tIndex - 1);
end;

procedure TProjectFileListFrame.ClipboardRead(ClipBoardLine: TStrings);
var
  MS: TMemoryStream;
begin
  MS := TMemoryStream.Create;
  if Clipboard.GetFormat(PredefinedClipboardFormat(pcfText), MS) then
  begin
    Ms.Position := 0;
    ClipBoardLine.LoadFromStream(MS);
  end;
  MS.Free;
end;

procedure TProjectFileListFrame.AddDocumentToGrid(const FileName: string;
  const Doc: TEpiDocument);
var
  Idx: Integer;
  Ext: String;
  i: Integer;
  LastDate: TDateTime;
begin
  Ext := ExtractFileExt(UTF8LowerCase(FileName));
  with StructureGrid do
  begin
    Idx := RowCount;
    RowCount := RowCount + 1;

    // Filename column.
    if FileName = '' then
      Cells[FileNameCol.Index + 1, Idx] := '(Clipboard)'
    else
      Cells[FileNameCol.Index + 1, Idx] := ExtractFileName(FileName);
    // Include row - checkbox
    Cells[IncludeCol.Index + 1, Idx]  := '1';
    if (ext = '.epx') or (ext ='.epz') then
    begin
      // Created
      Cells[CreatedCol.Index + 1, Idx]  := DateToStr(Doc.Study.Created);
      // Edited
      LastDate := Doc.Study.ModifiedDate;
      for i := 0 to Doc.DataFiles.Count - 1 do
      begin
        if Doc.DataFiles[i].RecModifiedDate > LastDate then
          LastDate := Doc.DataFiles[i].RecModifiedDate;
        if Doc.DataFiles[i].StructureModifiedDate > LastDate then
          LastDate := Doc.DataFiles[i].StructureModifiedDate;
      end;
      Cells[LastEditCol.Index + 1, Idx] := DateToStr(LastDate);
    end else begin
      // Created
      Cells[CreatedCol.Index + 1, Idx]  := 'N/A';
      // Edited
      if FileExistsUTF8(FileName) then
        Cells[LastEditCol.Index + 1, Idx] := DateToStr(FileDateToDateTime(FileAgeUTF8(FileName)))
      else
        Cells[LastEditCol.Index + 1, Idx] := 'N/A';
    end;
    with Doc.DataFiles[0] do
    begin
      // Sections
      Cells[SectionsCol.Index + 1, Idx] := IntToStr(Sections.Count);
      // Fields
      Cells[FieldsCol.Index + 1, Idx]   := IntToStr(Fields.Count);
      // Records
      Cells[RecordsCol.Index + 1, Idx]  := IntToStr(Size);
      // Version info
      Cells[VersionCol.Index + 1, Idx]  := Doc.Study.Version;
      // Cycle
      Cells[CycleCol.Index + 1, Idx]    := IntToStr(Doc.CycleNo);
      // Info
      Cells[NameCol.Index + 1, Idx]     := Caption.Text;
    end;
  end;
  FDocList.AddObject(FileName, Doc);
  DoAfterGridEvent(Filename, Doc, Idx);
  DoSelectionChanged;
end;

procedure TProjectFileListFrame.ImportFile(const FileName: string);
var
  Importer: TEpiImport;
  Ext: String;
  Doc: TEpiDocument;
  St: TMemoryStream;
  Idx: Integer;
  DataFile: TEpiDataFile;
  DocFile: TDocumentFile;
  Res: Boolean;
begin
  Importer := TEpiImport.Create;
  Importer.ImportCasing := ManagerSettings.ImportCasing;
  Importer.OnProgress := @Progress;
  Importer.OnClipBoardRead := @ClipboardRead;
  FCurrentFile := FileName;
  Ext := ExtractFileExt(UTF8LowerCase(FileName));

  try
    DocFile := TDocumentFile.Create;
    Res := False;

    if (ext = '.rec') or (ext = '.dta') then
    begin
      Doc := DocFile.CreateNewDocument('en');

      DataFile := Doc.DataFiles.NewDataFile;
      DoBeforeImportFile(Doc, FileName);
      if (ext = '.dta') then
        Importer.ImportStata(FileName, Doc, DataFile, true)
      else begin
        Importer.OnRequestPassword := @RecImportPassword;
        Importer.ImportRec(FileName , DataFile, true);
      end;
      DoAfterImportFile(Doc, FileName);
      Res := true;
    end
    else if (ext = '.epx') or (ext = '.epz') then
    begin
      DoBeforeImportFile(nil, FileName);
      DocFile.OnProgress := @Progress;
      Res := DocFile.OpenFile(FileName, true);
      DoAfterImportFile(DocFile.Document, FileName);
    end
    else if (ext = '.csv') or
            (ext = '.txt') or
            (FileName = '') // import from clipboard
    then
    begin
      Doc := DocFile.CreateNewDocument('en');
      DataFile := Doc.DataFiles.NewDataFile;
      DoBeforeImportFile(Doc, FileName);
      Importer.ImportTxt(FileName, DataFile, true);
      DoAfterImportFile(Doc, FileName);
      Res := true;
    end;

    if Res then
      AddDocumentToGrid(FileName, DocFile.Document)
    else
      DoReportError('Failed to read file: ' + ExtractFileName(FileName));
  except
    on E: Exception do
      begin
        DoReportError('Failed to read file "' + ExtractFileName(FileName) + '": ' + E.Message);
        DocFile.Free;
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

procedure TProjectFileListFrame.DoReportError(const Msg: string);
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

procedure TProjectFileListFrame.DoAfterGridEvent(const Filename: string;
  const Document: TEpiDocument; const RowNo: Integer);
begin
  if Assigned(FOnAfterAddToGrid) then
    FOnAfterAddToGrid(Self, Document, Filename, RowNo);
end;

procedure TProjectFileListFrame.DoSelectionChanged;
begin
  if Assigned(FOnSelectionChanged) then
    FOnSelectionChanged(Self);
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
  DoSelectionChanged;
end;

procedure TProjectFileListFrame.Progress(const Sender: TEpiCustomBase;
  ProgressType: TEpiProgressType; CurrentPos, MaxPos: Cardinal;
  var Canceled: Boolean);
Const
  LastUpdate: Cardinal = 0;
  ProgressUpdate: Cardinal = 0;
begin
  case ProgressType of
    eptInit:
      begin
        ProgressUpdate := MaxPos div 50;
        ProgressBar1.Position := CurrentPos;
        ProgressBar1.Visible := true;
        ProgressBar1.Max := MaxPos;
        Application.ProcessMessages;
      end;
    eptFinish:
      begin
        ProgressBar1.Visible := false;
        Application.ProcessMessages;
        LastUpdate := 0;
      end;
    eptRecords:
      begin
        if CurrentPos > (LastUpdate + ProgressUpdate) then
        begin
          ProgressBar1.Position := CurrentPos;
          {$IFNDEF MSWINDOWS}
          Application.ProcessMessages;
          {$ENDIF}
          LastUpdate := CurrentPos;
        end;
      end;
  end;
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

  FVersionCol := StructureGrid.Columns.Add;
  FVersionCol.Title.Caption := 'Version';
  FVersionCol.ReadOnly := true;

  FCycleCol := StructureGrid.Columns.Add;
  FCycleCol.Title.Caption := 'Cycle';
  FCycleCol.ReadOnly := true;

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

procedure TProjectFileListFrame.ForEachIncluded(
  CallBackMethod: TProjectFileListGridEvent);
var
  IncludeIdx: Integer;
  i: Integer;
begin
  if not Assigned(CallBackMethod) then exit;

  IncludeIdx := IncludeCol.Index + 1;

  for i := 1 to StructureGrid.RowCount - 1 do
  begin
    if StructureGrid.Cells[IncludeIdx, i] = IncludeCol.ValueChecked then
      CallBackMethod(Self, TEpiDocument(FDocList.Objects[i-1]), FDocList[i-1], i);
  end;
end;

procedure TProjectFileListFrame.ReportError(const Msg: string);
begin
  DoReportError(Msg);
end;

end.

