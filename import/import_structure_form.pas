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
    AddFilesAction: TAction;
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
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
    procedure OkActionExecute(Sender: TObject);
    procedure AddFilesActionExecute(Sender: TObject);
    procedure StructureGridColRowMoved(Sender: TObject; IsColumn: Boolean;
      sIndex, tIndex: Integer);
  private
    { private declarations }
    FSelectedDocuments: TList;
    FDocList: TList;
    FLastRecYPos: Integer;
    FLastEpiCtrl: TEpiCustomControlItem;
    DataFile: TEpiDatafile;
    FDesignerBox: TScrollBox;
    procedure ImportHook(Sender: TObject; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
    procedure  ReadFiles(Const Files: TStrings);
    procedure  ImportFile(Const FileName: string);
    procedure  ReportError(Const Msg: string);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; Const Files: TStrings);
    destructor Destroy; override;
    class procedure RestoreDefaultPos;
    property    SelectedDocuments: TList read FSelectedDocuments;
  end; 

implementation

{$R *.lfm}

uses
  epiimport, LCLProc, epidocument, epimiscutils, settings2_var,
  epidatafilestypes, settings2;

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

procedure TImportStructureForm.AddFilesActionExecute(Sender: TObject);
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
  S: Char;
const
  FieldHeigth =
    {$IFDEF WINDOWS}
      21
    {$ELSE}
      {$IFDEF DARWIN}
      22
      {$ELSE}
        {$IFDEF LINUX}
      27
        {$ENDIF}
    {$ENDIF}
  {$ENDIF};

  HeadingHeigth =
    {$IFDEF WINDOWS}
      14
    {$ELSE}
      {$IFDEF DARWIN}
      17
      {$ELSE}
        {$IFDEF LINUX}
      18
        {$ENDIF}
    {$ENDIF}
  {$ENDIF};
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

    if FLastEpiCtrl = nil then
    begin
      Pt := Point(ManagerSettings.DefaultLabelPosition, 5);
      if Sender is TEpiField then
        Pt.X := ManagerSettings.DefaultRightPosition;
    end else begin
      if (FLastRecYPos <> -1) and (FLastRecYPos = TEpiCustomControlItem(Sender).Top) then
      begin
        Pt.Y := FLastEpiCtrl.Top;

        if (FLastEpiCtrl is TEpiField) then
        begin
          S := '4';
          if TEpiField(FLastEpiCtrl).FieldType in StringFieldTypes then
            S := 'W';

          if (Sender is TEpiField) then
            Pt.X := FLastEpiCtrl.Left +
                    FDesignerBox.Canvas.GetTextWidth(S) * TEpiField(FLastEpiCtrl).Length + 15 +   // This calculates right side of previous placed control (with 5px margin)
                    FDesignerBox.Canvas.GetTextWidth(TEpiField(Sender).Question.Text) + 5 +        // This gives a rough estimate of the width of the Question text (5px margin)
                    FDesignerBox.Canvas.GetTextWidth(TEpiField(Sender).Name) + 5                   // This gives a rough estimate of the width of the Name (if shown).
          else
            Pt.X := FLastEpiCtrl.Left +
                    FDesignerBox.Canvas.GetTextWidth(S) * TEpiField(FLastEpiCtrl).Length + 15;    // This calculates right side of previous placed control (with 5px margin)
        end else begin
          if (Sender is TEpiField) then
            Pt.X := FLastEpiCtrl.Left +
                    FDesignerBox.Canvas.GetTextWidth(TEpiHeading(FLastEpiCtrl).Caption.Text) + 10 +
                    FDesignerBox.Canvas.GetTextWidth(TEpiField(Sender).Question.Text) + 5 +        // This gives a rough estimate of the width of the Question text (5px margin)
                    FDesignerBox.Canvas.GetTextWidth(TEpiField(Sender).Name) + 5                   // This gives a rough estimate of the width of the Name (if shown).
          else
            Pt.X := FLastEpiCtrl.Left +
                    FDesignerBox.Canvas.GetTextWidth(TEpiHeading(FLastEpiCtrl).Caption.Text) + 10;
        end;
      end else begin
        if (FLastEpiCtrl is TEpiField) then
        begin
          Pt.Y := FLastEpiCtrl.Top + FieldHeigth;
          if (Sender is TEpiField) then
          begin
            Pt.Y := Pt.Y  + ManagerSettings.SpaceBtwFieldField;
            Pt.X := ManagerSettings.DefaultRightPosition;
          end else begin
            Pt.Y := Pt.Y  + ManagerSettings.SpaceBtwFieldLabel;
            Pt.X := ManagerSettings.DefaultLabelPosition;
          end;
        end;
        if (FLastEpiCtrl is TEpiHeading) then
        begin
          Pt.Y := FLastEpiCtrl.Top + HeadingHeigth;
          if (Sender is TEpiField) then
          begin
            Pt.Y := Pt.Y + ManagerSettings.SpaceBtwFieldLabel;
            Pt.X := ManagerSettings.DefaultRightPosition;
          end else begin
            Pt.Y := Pt.Y + ManagerSettings.SpaceBtwLabelLabel;
            Pt.X := ManagerSettings.DefaultLabelPosition;
          end;
        end;
      end;
    end;

    with TEpiCustomControlItem(Sender) do
    begin
      // Top not yet adjusted for pixel position...
      FLastRecYPos := Top;

      BeginUpdate;
      Top := Pt.Y;
      Left := Pt.X;
      EndUpdate;
    end;
    FLastEpiCtrl := TEpiCustomControlItem(Sender);
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

procedure TImportStructureForm.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  if ManagerSettings.SaveWindowPositions then
    SaveFormPosition(Self, 'ImportStructureForm');
  CanClose := true;
end;

procedure TImportStructureForm.FormShow(Sender: TObject);
begin
  if ManagerSettings.SaveWindowPositions then
    LoadFormPosition(Self, 'ImportStructureForm');
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
    FLastEpiCtrl := nil;

    Doc := TEpiDocument.Create('en');
    if ext = '.rec' then
    begin
      DataFile := Doc.DataFiles.NewDataFile;
      DataFile.MainSection.Fields.RegisterOnChangeHook(@ImportHook, false);
      DataFile.MainSection.Headings.RegisterOnChangeHook(@ImportHook, false);
      Importer.ImportRec(FileName , DataFile, false);
      DataFile.MainSection.Fields.UnRegisterOnChangeHook(@ImportHook);
      DataFile.MainSection.Headings.UnRegisterOnChangeHook(@ImportHook);
    end
    else if ext = '.dta' then
    begin
      DataFile := Doc.DataFiles.NewDataFile;
      DataFile.MainSection.Fields.RegisterOnChangeHook(@ImportHook, false);
      DataFile.MainSection.Headings.RegisterOnChangeHook(@ImportHook, false);
      Importer.ImportStata(FileName, DataFile, false);
      DataFile.MainSection.Fields.UnRegisterOnChangeHook(@ImportHook);
      DataFile.MainSection.Headings.UnRegisterOnChangeHook(@ImportHook);
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
        Cells[5, Idx] := Name.Text;                                         // Info
        Cells[6, Idx] := IntToStr(Sections.Count);                          // Sections
        Cells[7, Idx] := IntToStr(Fields.Count);                            // Fields
      end;
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

  if TheOwner is TScrollBox then
    FDesignerBox := TScrollBox(TheOwner);

  FSelectedDocuments := TList.Create;
  FDocList := TList.Create;

  ReadFiles(Files);
end;

destructor TImportStructureForm.Destroy;
var
  I: TObject;
begin
  while FDocList.Count > 0 do
  begin
    I := TObject(FDocList.Last);
    FDocList.Remove(I);
    I.Free;
  end;
  FDocList.Free;
  FSelectedDocuments.Free;
  inherited Destroy;
end;

class procedure TImportStructureForm.RestoreDefaultPos;
var
  Aform: TForm;
begin
  Aform := TForm.Create(nil);
  Aform.Width := 600;
  Aform.Height := 600;
  Aform.top := (Screen.Monitors[0].Height - Aform.Height) div 2;
  Aform.Left := (Screen.Monitors[0].Width - Aform.Width) div 2;
  SaveFormPosition(Aform, 'ImportStructureForm');
  AForm.free;
end;

end.

