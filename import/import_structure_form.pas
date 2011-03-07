unit import_structure_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Buttons, ActnList, Grids, StdCtrls, ShellCtrls, epicustombase,
  epidatafiles;

type

  { TImportStructureForm }

  TImportStructureForm = class(TForm)
    OpenAction: TAction;
    CancelAction: TAction;
    ErrorListBox: TListBox;
    OkAction: TAction;
    ActionList1: TActionList;
    CancelBtn: TBitBtn;
    OkApplyPanel: TPanel;
    OkBtn: TBitBtn;
    OpenBtn: TBitBtn;
    StructureGrid: TStringGrid;
    procedure CancelActionExecute(Sender: TObject);
    procedure OkActionExecute(Sender: TObject);
    procedure OpenActionExecute(Sender: TObject);
    procedure StructureGridColRowMoved(Sender: TObject; IsColumn: Boolean;
      sIndex, tIndex: Integer);
  private
    { private declarations }
    FSelectedDocuments: TList;
    FDocList: TList;
    FLastRecYPos: integer;
    FLastRecCtrl: TEpiCustomControlItem;
    DataFile: TEpiDatafile;
    procedure ImportHook(Sender: TObject; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
    procedure  ReadFiles(Const Files: TStrings);
    procedure  ImportFile(Const FileName: string);
    procedure  ReportError(Const Msg: string);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; Const Files: TStrings);
    property    SelectedDocuments: TList read FSelectedDocuments;
  end; 

implementation

{$R *.lfm}

uses
  epiimport, LCLProc, epidocument, epimiscutils, settings2_var;

{ TImportStructureForm }

procedure TImportStructureForm.OkActionExecute(Sender: TObject);
var
  i: Integer;
begin
  for i := 1 to StructureGrid.RowCount - 1 do
    if StructureGrid.Cells[2, i] <> '0' then
      FSelectedDocuments.Add(FDocList.Items[i - 1]);

  ModalResult := mrOk;
end;

procedure TImportStructureForm.OpenActionExecute(Sender: TObject);
var
  Dlg: TOpenDialog;
begin
  Dlg := TOpenDialog.Create(Self);
  Dlg.InitialDir := ManagerSettings.WorkingDirUTF8;
  Dlg.Filter := GetEpiDialogFilter(true, true, true, false, false, false,
    true, false, false, true, false);
  Dlg.Options := [ofAllowMultiSelect, ofFileMustExist, ofEnableSizing, ofViewDetail];
  if not Dlg.Execute then exit;

  ReadFiles(Dlg.Files);
end;

procedure TImportStructureForm.StructureGridColRowMoved(Sender: TObject;
  IsColumn: Boolean; sIndex, tIndex: Integer);
begin
  if IsColumn then exit;

  FDocList.Move(sIndex - 1, tIndex - 1);
end;

procedure TImportStructureForm.ImportHook(Sender: TObject; EventGroup: TEpiEventGroup;
  EventType: Word; Data: Pointer);
var
  Cls: TControlClass;
  Pt: TPoint;
begin
  if (Sender is TEpiFields) then
  begin
    if (EventGroup = eegCustomBase) and (EventType = Ord(ecceAddItem)) then
      TEpiField(Data).RegisterOnChangeHook(@ImportHook, false);
    exit;
  end;

  if (Sender is TEpiHeadings) then
  begin
    if (EventGroup = eegCustomBase) and (EventType = Ord(ecceAddItem)) then
      TEpiHeading(Data).RegisterOnChangeHook(@ImportHook, false);
    exit;
  end;

  if (Sender is TEpiCustomControlItem) and (EventGroup = eegCustomBase) and (EventType = Ord(ecceUpdate)) then
  begin
    TEpiCustomControlItem(Sender).UnRegisterOnChangeHook(@ImportHook);
{
    Cls := TDesignField;
    if Sender is TEpiHeading then
      Cls := TDesignHeading;
    Pt := FindNewPosition(FActiveDockSite, Cls);

    if (not (FLastRecYPos = -1)) and (FLastRecYPos = TEpiCustomControlItem(Sender).Top) then
    begin
      Pt.Y := FLastRecCtrl.Top;
      if (FLastRecCtrl is TDesignField) and (Sender is TEpiField) then
        Pt.X := FLastRecCtrl.Left + FLastRecCtrl.Width + 5 +                                   // This calculates right side of previous placed control (with 5px margin)
                FDesignerBox.Canvas.GetTextWidth(TEpiField(Sender).Question.Text) + 5 +        // This gives a rough estimate of the width of the Question text (5px margin)
                FDesignerBox.Canvas.GetTextWidth(TEpiField(Sender).Name) + 5                   // This gives a rough estimate of the width of the Name (if shown).
      else
        Pt.X := FLastRecCtrl.Left + FLastRecCtrl.Width + 10;
    end;

    FLastRecYPos := TEpiCustomControlItem(Sender).Top;
    FLastRecCtrl := NewDesignControl(Cls, FActiveDockSite, Pt, TEpiCustomControlItem(Sender));}
  end;
end;

procedure TImportStructureForm.ReadFiles(const Files: TStrings);
var
  i: Integer;
begin
  if Assigned(Files) then
    for i := 0 to Files.Count -1 do
      ImportFile(Files[i]);
  StructureGrid.AutoAdjustColumns;
end;

procedure TImportStructureForm.CancelActionExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TImportStructureForm.ImportFile(const FileName: string);
var
  Importer: TEpiImport;
  Ext: String;
  Doc: TEpiDocument;
  St: TMemoryStream;
  Idx: Integer;
begin
  Importer := TEpiImport.Create;
  Ext := ExtractFileExt(UTF8LowerCase(FileName));

  try
    // Needed for rec/dta file import.
    FLastRecYPos := -1;
    FLastRecCtrl := nil;

    Doc := TEpiDocument.Create('en');
    if ext = '.rec' then
    begin
      DataFile := Doc.DataFiles.NewDataFile;
//      DataFile.MainSection.Fields.RegisterOnChangeHook(@ImportHook, false);
//      DataFile.MainSection.Headings.RegisterOnChangeHook(@ImportHook, false);
      Importer.ImportRec(FileName , DataFile, false);
//      DataFile.MainSection.Fields.UnRegisterOnChangeHook(@ImportHook);
//      DataFile.MainSection.Headings.UnRegisterOnChangeHook(@ImportHook);
    end
    else if ext = '.dta' then
    begin
      DataFile := Doc.DataFiles.NewDataFile;
//      DataFile.MainSection.Fields.RegisterOnChangeHook(@ImportHook, false);
//      DataFile.MainSection.Headings.RegisterOnChangeHook(@ImportHook, false);
      Importer.ImportStata(FileName, DataFile, false);
//      DataFile.MainSection.Fields.UnRegisterOnChangeHook(@ImportHook);
//      DataFile.MainSection.Headings.UnRegisterOnChangeHook(@ImportHook);
    end
    else if ext = '.epx' then
      Doc.LoadFromFile(FileName)
    else if ext = '.epz' then
    begin
      St := TMemoryStream.Create;
      ZipFileToStream(St, FileName);
      Doc.LoadFromStream(St);
      St.Free;
    end;

    with StructureGrid do
    begin
      RowCount := RowCount + 1;
      Idx := RowCount - 1;
      Cells[1, Idx] := ExtractFileName(FileName);  // Filename column.
      Cells[2, Idx] := '1';                        // Include row.
      Cells[3, Idx] := Doc.DataFiles[0].Name.Text; // Info
    end;
    FDocList.Add(Doc);
  except
    on E: Exception do
      ReportError('Failed to read file "' + ExtractFileName(FileName) + '": ' + E.Message);
  end;
  Importer.Free;
end;

procedure TImportStructureForm.ReportError(const Msg: string);
begin
  if not ErrorListBox.Visible then
    ErrorListBox.Visible := true;

  ErrorListBox.Items.Add(Msg);
end;

constructor TImportStructureForm.Create(TheOwner: TComponent;
  const Files: TStrings);
begin
  inherited Create(TheOwner);
  FSelectedDocuments := TList.Create;
  FDocList := TList.Create;

  ReadFiles(Files);
end;

end.

