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
  end; 

implementation

{$R *.lfm}

uses
  epiimport, LCLProc, epimiscutils, Dialogs;

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
    Cells[1, Idx] := ExtractFileName(FileName);                           // Filename column.
    Cells[2, Idx] := '1';                                                 // Include row.
    if (ext = '.epx') or (ext ='.epz') then
    begin
      Cells[3, Idx] := FormatDateTime('YYYY/MM/DD HH:NN', Doc.Study.Created);                      // Created
      Cells[4, Idx] := FormatDateTime('YYYY/MM/DD HH:NN', Doc.Study.ModifiedDate);                 // Edited
    end else begin
      Cells[3, Idx] := 'N/A';                                             // Created
      Cells[4, Idx] := FormatDateTime('YYYY/MM/DD HH:NN', FileDateToDateTime(FileAgeUTF8(FileName)));  // Edited
    end;
    with Doc.DataFiles[0] do
    begin
      Cells[5, Idx] := Caption.Text;                                         // Info
      Cells[6, Idx] := IntToStr(Sections.Count);                          // Sections
      Cells[7, Idx] := IntToStr(Fields.Count);                            // Fields
      Cells[8, Idx] := IntToStr(Size);
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
    else if ext = '.epx' then
    begin
      DoBeforeImportFile(Doc, FileName);
      Doc.OnPassword := @RecImportPassword;
      Doc.LoadFromFile(FileName);
      DoAfterImportFile(Doc, FileName);
    end
    else if ext = '.epz' then
    begin
      St := TMemoryStream.Create;
      ZipFileToStream(St, FileName);
      DoBeforeImportFile(Doc, FileName);
      St.Position := 0;
      Doc.OnPassword := @RecImportPassword;
      Doc.LoadFromStream(St);
      DoAfterImportFile(Doc, FileName);
      St.Free;
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
    if StructureGrid.Cells[2, i] <> '0' then
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

