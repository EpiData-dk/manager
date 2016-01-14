unit projectfilelist_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, Grids, ComCtrls,
  ExtCtrls, epidocument, epidatafiles, epicustombase, epiopenfile, epiadmin,
  epieximtypes;

type

  TProjectListFileEvent = procedure (Sender: TObject; Document: TEpiDocument;
    Const FileName: string) of object;
  TProjectFileListGridEvent = procedure (Sender: TObject; Document: TEpiDocument;
    Const Filename: string; Const RowNo: Integer) of object;
  TProjectFileListGridMoveEvent = procedure (Sender: TObject; Document: TEpiDocument;
    Const FromRow, ToRow: Integer) of object;

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
    FDocFileList: TList;
    FDocList: TStringList;
    FOnAfterAddToGrid: TProjectFileListGridEvent;
    FOnAfterImportFile: TProjectListFileEvent;
    FOnBeforeImportFile: TProjectListFileEvent;
    FOnControlItemPosition: TEpiControlItemPosition;
    FOnDocumentIncludedChange: TProjectFileListGridEvent;
    FOnSelectionChanged: TNotifyEvent;
    FOnDocumentMoved: TProjectFileListGridMoveEvent;
    FRequiredRights: TEpiManagerRights;
    procedure  AddDocumentToGrid(Const FileName: string; Const Doc: TEpiDocument);
    function   GetSelectedDocfileList: TEpiDocumentFileList;
    function   GetSelectedList: TStringList;
    procedure  ImportFile(Const FileName: string);
    procedure  DoReportError(Const Msg: string);
    procedure  SetOnAfterImportFile(const AValue: TProjectListFileEvent);
    procedure  SetOnBeforeImportFile(const AValue: TProjectListFileEvent);
  protected
    procedure ControlPosition(const Sender: TObject;
      const ControlItem: TEpiCustomControlItem; var ATop, ALeft: Integer);
    procedure  DoAfterGridEvent(Const Filename: string; Const Document: TEpiDocument;
      Const RowNo: Integer);
    procedure  DoGridMoveEvent(Document: TEpiDocument; Const FromRow, ToRow: Integer);
    procedure  DoIncludedChange(Const RowNo: Integer);
    procedure  DoSelectionChanged;
    procedure  DoBeforeImportFile(Document: TEpiDocument; Const FileName: string);
    procedure  DoAfterImportFile(Document: TEpiDocument; Const FileName: string);
    function  RecImportPassword(Sender: TObject;
      RequestType: TEpiRequestPasswordType;
      RequestNo:   Integer;
      var Login: UTF8String; var Password: UTF8String): TEpiRequestPasswordResponse;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   AddFiles(Const Files: TStrings);
    procedure   AddDocument(Const FileName: string; Const Doc: TEpiDocument); overload;
    procedure   AddDocument(Const DocFile: TEpiDocumentFile); overload;
    procedure   ForEachIncluded(CallBackMethod: TProjectFileListGridEvent);
    procedure   ReportError(Const Msg: string);
    property    OnBeforeImportFile: TProjectListFileEvent read FOnBeforeImportFile write SetOnBeforeImportFile;
    property    OnAfterImportFile: TProjectListFileEvent read FOnAfterImportFile write SetOnAfterImportFile;
    property    OnSelectionChanged: TNotifyEvent read FOnSelectionChanged write FOnSelectionChanged;
    property    OnDocumentIncludedChange: TProjectFileListGridEvent read FOnDocumentIncludedChange write FOnDocumentIncludedChange;
    property    OnAfterAddToGrid: TProjectFileListGridEvent read FOnAfterAddToGrid write FOnAfterAddToGrid;
    property    OnControlItemPosition: TEpiControlItemPosition read FOnControlItemPosition write FOnControlItemPosition;
    property    OnDocumentMoved: TProjectFileListGridMoveEvent read FOnDocumentMoved write FOnDocumentMoved;
    property    SelectedList: TStringList read GetSelectedList;
    property    SelectedDocfileList: TEpiDocumentFileList read GetSelectedDocfileList;
    property    DocList: TStringList read FDocList;
    property    DocFileList: TList read FDocFileList;
    property    RequiredRights: TEpiManagerRights read FRequiredRights write FRequiredRights;
  private
    { columns }
    FCreatedCol: TGridColumn;
    FDataFormCol: TGridColumn;
    FFileNameCol: TGridColumn;
    FIncludeCol: TGridColumn;
    FLastEditCol: TGridColumn;
    FStudyTitleCol: TGridColumn;
    FVersionCol: TGridColumn;
    FCycleCol: TGridColumn;
  public
    property FileNameCol:   TGridColumn read FFileNameCol;
    property IncludeCol:    TGridColumn read FIncludeCol;
    property DataFormCol:   TGridColumn read FDataFormCol;
    property CreatedCol:    TGridColumn read FCreatedCol;
    property LastEditCol:   TGridColumn read FLastEditCol;
    property StudyTitleCol: TGridColumn read FStudyTitleCol;
    property VersionCol:    TGridColumn read FVersionCol;
    property CycleCol:      TGridColumn read FCycleCol;
  end;

implementation

{$R *.lfm}

uses
  epiimport, LCLProc, epimiscutils, Dialogs, managerprocs, epiv_documentfile,
  settings2_var, Clipbrd, LCLIntf, LCLType, admin_authenticator;


type

  { TImportedDocumentFile }

  TImportedDocumentFile = class(TDocumentFile)
  private
    FImportedFileName: string;
  protected
    function GetFileName: string; override;
  public
    constructor Create; override;
  end;

{ TImportedDocumentFile }

function TImportedDocumentFile.GetFileName: string;
begin
  if FImportedFileName = '' then
    Result := inherited GetFileName
  else
    Result := FImportedFileName;
end;

constructor TImportedDocumentFile.Create;
begin
  inherited Create;
  FImportedFileName := '';
end;


{ TProjectFileListFrame }

procedure TProjectFileListFrame.StructureGridColRowMoved(Sender: TObject;
  IsColumn: Boolean; sIndex, tIndex: Integer);
var
  Doc: TEpiDocument;
begin
  if IsColumn then exit;

  Doc := TEpiDocument(FDocList.Objects[sIndex - 1]);
  FDocList.Move(sIndex - 1, tIndex - 1);
  FDocFileList.Move(sIndex - 1, tIndex - 1);

  DoGridMoveEvent(Doc, sIndex, tIndex);
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
    Cells[IncludeCol.Index + 1, Idx]  := IncludeCol.ValueChecked;
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
    with Doc do
    begin
      // Dataforms
      Cells[DataFormCol.Index + 1, Idx]   := IntToStr(DataFiles.Count);
      // Fields
//      Cells[FieldsCol.Index + 1, Idx]   := IntToStr(Fields.Count);
      // Records
//      Cells[RecordsCol.Index + 1, Idx]  := IntToStr(Size);
      // Version info
      Cells[VersionCol.Index + 1, Idx]    := Study.Version;
      // Cycle
      Cells[CycleCol.Index + 1, Idx]      := IntToStr(Doc.CycleNo);
      // Info
      Cells[StudyTitleCol.Index + 1, Idx] := Study.Title.Text;
    end;
  end;
  FDocList.AddObject(FileName, Doc);
  DoAfterGridEvent(Filename, Doc, Idx);
  DoSelectionChanged;
end;

function TProjectFileListFrame.GetSelectedDocfileList: TEpiDocumentFileList;
var
  i: Integer;
begin
  Result := TEpiDocumentFileList.Create;

  for i := 1 to StructureGrid.RowCount - 1 do
    if StructureGrid.Cells[IncludeCol.Index + 1, i] = IncludeCol.ValueChecked then
      Result.Add(TEpiDocumentFile(FDocFileList[i - 1]));
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
  Auth: TAuthenticator;
begin
  Importer := TEpiImport.Create;
  Importer.ImportCasing := ManagerSettings.ImportCasing;
  Importer.OnProgress := @Progress;
  Importer.OnClipBoardRead := @ClipboardRead;
  Importer.OnControlItemPosition := @ControlPosition;

  FCurrentFile := FileName;
  Ext := ExtractFileExt(UTF8LowerCase(FileName));

  try
    DocFile := TImportedDocumentFile.Create;
    Auth := nil;
    Res := False;

    if (ext = '.rec') or (ext = '.dta') then
    begin
      Doc := DocFile.CreateNewDocument('en');
      Doc.Study.Title.Text := ExtractFileName(FileName);

      DataFile := Doc.DataFiles.NewDataFile;
      Doc.Relations.NewMasterRelation.Datafile := DataFile;

      DoBeforeImportFile(Doc, FileName);
      if (ext = '.dta') then
        Importer.ImportStata(FileName, Doc, DataFile, true)
      else begin
        Importer.OnRequestPassword := @RecImportPassword;
        Importer.ImportRec(FileName , DataFile, true);
      end;
      DoAfterImportFile(Doc, FileName);
      TImportedDocumentFile(DocFile).FImportedFileName := FileName;
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
      Doc.Study.Title.Text := ExtractFileName(FileName);

      DataFile := Doc.DataFiles.NewDataFile;
      Doc.Relations.NewMasterRelation.Datafile := DataFile;

      DoBeforeImportFile(Doc, FileName);
      Importer.ImportTxt(FileName, DataFile, true);
      DoAfterImportFile(Doc, FileName);

      if FileName = '' then
        TImportedDocumentFile(DocFile).FImportedFileName := '(from clipboard)'
      else
        TImportedDocumentFile(DocFile).FImportedFileName := FileName;
      Res := true;
    end;

    Auth := TAuthenticator.Create(DocFile);

    if (not Auth.IsAuthorized(RequiredRights)) then
    begin
      Res := false;
      ShowMessage('You are not authorised to open the file: ' + LineEnding +
                  DocFile.FileName);
    end;

    if Res then
    begin
      FDocFileList.Add(DocFile);
      AddDocumentToGrid(FileName, DocFile.Document);
    end
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
{  if not ErrorListBox.Visible then
    ErrorListBox.Visible := true;

  ErrorListBox.Items.Add(Msg);    }
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

procedure TProjectFileListFrame.ControlPosition(const Sender: TObject;
  const ControlItem: TEpiCustomControlItem; var ATop, ALeft: Integer);
begin
  if Assigned(OnControlItemPosition) then
    OnControlItemPosition(Sender, ControlItem, ATop, ALeft)
end;

procedure TProjectFileListFrame.DoAfterGridEvent(const Filename: string;
  const Document: TEpiDocument; const RowNo: Integer);
begin
  if Assigned(FOnAfterAddToGrid) then
    FOnAfterAddToGrid(Self, Document, Filename, RowNo);
end;

procedure TProjectFileListFrame.DoGridMoveEvent(Document: TEpiDocument;
  const FromRow, ToRow: Integer);
begin
  if Assigned(OnDocumentMoved) then
    OnDocumentMoved(Self, Document, FromRow, ToRow);
end;

procedure TProjectFileListFrame.DoIncludedChange(const RowNo: Integer);
begin
  if Assigned(OnDocumentIncludedChange) then
    OnDocumentIncludedChange(Self, TEpiDocument(FDocList.Objects[RowNo - 1]), FDocList[RowNo - 1], RowNo);
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

function TProjectFileListFrame.RecImportPassword(Sender: TObject;
  RequestType: TEpiRequestPasswordType; RequestNo: Integer;
  var Login: UTF8String; var Password: UTF8String): TEpiRequestPasswordResponse;
begin
  Login := '';
  Password :=
    PasswordBox('IMPORTANT!',
    ' Password needed for "' + ExtractFileName(FCurrentFile) + '":');

  if (RequestNo < 3) then
    Result := rprAskOnFail
  else
    Result := rprStopOnFail;
end;

procedure TProjectFileListFrame.StructureGridCheckboxToggled(sender: TObject;
  aCol, aRow: Integer; aState: TCheckboxState);
begin
  DoIncludedChange(aRow);
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

  RequiredRights := [];

  FDocList := TStringList.Create;
  FDocFileList := TList.Create;

  // Preserve ADD order!
  FFileNameCol := StructureGrid.Columns.Add;
  FFileNameCol.Title.Caption := 'Filename';
  FFileNameCol.ReadOnly := true;

  FIncludeCol := StructureGrid.Columns.Add;
  FIncludeCol.Title.Caption := 'Include';
  FIncludeCol.ButtonStyle := cbsCheckboxColumn;

  FDataFormCol := StructureGrid.Columns.Add;
  FDataFormCol.Title.Caption := 'Dataforms';
  FDataFormCol.ReadOnly := true;

{  FSectionsCol := StructureGrid.Columns.Add;
  FSectionsCol.Title.Caption := 'Sections';
  FSectionsCol.ReadOnly := true;

  FRecordsCol := StructureGrid.Columns.Add;
  FRecordsCol.Title.Caption := 'Records';
  FRecordsCol.ReadOnly := true;   }

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

  FStudyTitleCol := StructureGrid.Columns.Add;
  FStudyTitleCol.Title.Caption := 'Project Title';
  FStudyTitleCol.ReadOnly := true;
end;

destructor TProjectFileListFrame.Destroy;
begin
  FDocFileList.Free;
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

procedure TProjectFileListFrame.AddDocument(const DocFile: TEpiDocumentFile);
begin
  AddDocumentToGrid(DocFile.FileName, DocFile.Document);
  FDocFileList.Add(Docfile);
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

