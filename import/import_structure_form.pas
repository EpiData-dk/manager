unit import_structure_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Buttons, ActnList, Grids, StdCtrls, ShellCtrls, epicustombase,
  epidatafiles, projectfilelist_frame, epidocument;

type

  { TImportStructureForm }

  TImportStructureForm = class(TForm)
    AddFilesAction: TAction;
    OpenCBBtn: TBitBtn;
    CancelAction: TAction;
    Label1: TLabel;
    OkAction: TAction;
    ActionList1: TActionList;
    CancelBtn: TBitBtn;
    OkApplyPanel: TPanel;
    OkBtn: TBitBtn;
    OpenBtn: TBitBtn;
    OptionsPanel: TPanel;
    FieldsRenameGrpBox: TRadioGroup;
    ValueLabelsRenameGrpBox: TRadioGroup;
    procedure CancelActionExecute(Sender: TObject);
    procedure DocListHook(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
    procedure OkActionExecute(Sender: TObject);
    procedure AddFilesActionExecute(Sender: TObject);
    procedure OpenCBBtnClick(Sender: TObject);
  private
    { Data Import }
    FImportDataSelectedIndex: integer;
    FDataCol: TGridColumn;
    FImportData: boolean;
    FOldCheckBoxToggle: TToggledCheckboxEvent;
    FOldColRowMoved: TGridOperationEvent;
  private
    { private declarations }
    FInitialFiles: TStringList;
    FSelectedDocuments: TStringList;
    FLastRecYPos: Integer;
    FLastEpiCtrl: TEpiCustomControlItem;
    DataFile: TEpiDatafile;
    FDesignerBox: TScrollBox;
    FProjectList: TProjectFileListFrame;
    procedure ImportDataCheckBoxToogle(sender: TObject; aCol, aRow: Integer;
      aState: TCheckboxState);
    procedure ImportDataColRowMoved(Sender: TObject; IsColumn: Boolean; sIndex,
      tIndex: Integer);
    procedure ImportHook(Const Sender, Initiator: TEpiCustomBase;
      EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
    procedure BeforeLoad(Sender: TObject; Doc: TEpiDocument; Const FN: string);
    procedure AfterLoad(Sender: TObject; Doc: TEpiDocument; Const FN: string);
    procedure SetImportData(AValue: boolean);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent);
    destructor Destroy; override;
    procedure  AddInitialFiles(Const Files: TStrings);
    class procedure RestoreDefaultPos;
    property    SelectedDocuments: TStringList read FSelectedDocuments;
    property    ImportData: boolean read FImportData write SetImportData;
    property    ImportDataIndex: Integer read FImportDataSelectedIndex;
  end; 

implementation

{$R *.lfm}

uses
  epiimport, LCLProc, epimiscutils, settings2_var,
  epidatafilestypes, settings2;

{ TImportStructureForm }

procedure TImportStructureForm.OkActionExecute(Sender: TObject);
var
  i: Integer;
  DataIndex: Integer;
begin
  DataIndex := -1;

  FSelectedDocuments := FProjectList.SelectedList;

  if FImportDataSelectedIndex <> -1 then
  begin
    for i := 1 to FProjectList.StructureGrid.RowCount - 1 do
      if (FProjectList.StructureGrid.Cells[2, i] <> '0') then
        if (FProjectList.StructureGrid.Cells[FDataCol.Index + 1, i] = '1') then
        begin
          Inc(DataIndex);
          Break;
        end else
          Inc(DataIndex);
  end;

  FImportDataSelectedIndex := DataIndex;

  ModalResult := mrOk;
end;

procedure TImportStructureForm.AddFilesActionExecute(Sender: TObject);
var
  Dlg: TOpenDialog;
begin
  Dlg := TOpenDialog.Create(Self);
  Dlg.InitialDir := ManagerSettings.WorkingDirUTF8;
  Dlg.Filter := GetEpiDialogFilter(dfImport + [dfCollection]);
  Dlg.Options := [ofAllowMultiSelect, ofFileMustExist, ofEnableSizing, ofViewDetail];
  if not Dlg.Execute then exit;

  FProjectList.AddFiles(Dlg.Files);
end;

procedure TImportStructureForm.OpenCBBtnClick(Sender: TObject);
var
  Files: TStringList;
begin
  Files := TStringList.Create;
  Files.Add('');
  FProjectList.AddFiles(Files);
  Files.Free;
end;

procedure TImportStructureForm.ImportHook(const Sender,
  Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
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

  InImportHook: boolean = false;

begin
  if not (
    (Initiator is TEpiField) or
    (Initiator is TEpiHeading)
    )
  then
    Exit;

  if not (
     (EventGroup = eegCustomBase) and
     (EventType = Ord(ecceUpdate))
    )
  then
    Exit;

  If InImportHook then exit;
  InImportHook := true;

  if FLastEpiCtrl = nil then
  begin
    Pt := Point(ManagerSettings.DefaultLabelPosition, 5);
    if Initiator is TEpiField then
      Pt.X := ManagerSettings.DefaultRightPosition;
  end else begin
    if (FLastRecYPos <> -1) and (FLastRecYPos = TEpiCustomControlItem(Initiator).Top) then
    begin
      Pt.Y := FLastEpiCtrl.Top;

      if (FLastEpiCtrl is TEpiField) then
      begin
        S := '4';
        if TEpiField(FLastEpiCtrl).FieldType in StringFieldTypes then
          S := 'W';

        if (Initiator is TEpiField) then
          Pt.X := FLastEpiCtrl.Left +
                  FDesignerBox.Canvas.GetTextWidth(S) * TEpiField(FLastEpiCtrl).Length + 15 +   // This calculates right side of previous placed control (with 5px margin)
                  FDesignerBox.Canvas.GetTextWidth(TEpiField(Initiator).Question.Text) + 5 +        // This gives a rough estimate of the width of the Question text (5px margin)
                  FDesignerBox.Canvas.GetTextWidth(TEpiField(Initiator).Name) + 5                   // This gives a rough estimate of the width of the Name (if shown).
        else
          Pt.X := FLastEpiCtrl.Left +
                  FDesignerBox.Canvas.GetTextWidth(S) * TEpiField(FLastEpiCtrl).Length + 15;    // This calculates right side of previous placed control (with 5px margin)
      end else begin
        if (Initiator is TEpiField) then
          Pt.X := FLastEpiCtrl.Left +
                  FDesignerBox.Canvas.GetTextWidth(TEpiHeading(FLastEpiCtrl).Caption.Text) + 10 +
                  FDesignerBox.Canvas.GetTextWidth(TEpiField(Initiator).Question.Text) + 5 +        // This gives a rough estimate of the width of the Question text (5px margin)
                  FDesignerBox.Canvas.GetTextWidth(TEpiField(Initiator).Name) + 5                   // This gives a rough estimate of the width of the Name (if shown).
        else
          Pt.X := FLastEpiCtrl.Left +
                  FDesignerBox.Canvas.GetTextWidth(TEpiHeading(FLastEpiCtrl).Caption.Text) + 10;
      end;
    end else begin
      if (FLastEpiCtrl is TEpiField) then
      begin
        Pt.Y := FLastEpiCtrl.Top + FieldHeigth;
        if (Initiator is TEpiField) then
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
        if (Initiator is TEpiField) then
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

  with TEpiCustomControlItem(Initiator) do
  begin
    // Top not yet adjusted for pixel position...
    FLastRecYPos := Top;

    BeginUpdate;
    Top := Pt.Y;
    Left := Pt.X;
    EndUpdate;
  end;
  FLastEpiCtrl := TEpiCustomControlItem(Initiator);

  InImportHook := false;
end;

procedure TImportStructureForm.BeforeLoad(Sender: TObject; Doc: TEpiDocument;
  const FN: string);
var
  Ext: String;
begin
  Ext := ExtractFileExt(FN);
  if (Ext = '.epx') or (Ext = '.epz') then exit;

  FLastRecYPos := -1;
  FLastEpiCtrl := nil;

  DataFile := Doc.DataFiles[Doc.DataFiles.Count - 1];
//  DataFile.MainSection.Fields.RegisterOnChangeHook(@ImportHook, false);
//  DataFile.MainSection.Headings.RegisterOnChangeHook(@ImportHook, false);
  DataFile.MainSection.RegisterOnChangeHook(@ImportHook);
end;

procedure TImportStructureForm.AfterLoad(Sender: TObject; Doc: TEpiDocument;
  const FN: string);
var
  Ext: String;
  i: Integer;
begin
  Ext := ExtractFileExt(FN);
  if (Ext = '.epx') or (Ext = '.epz') then exit;

  DataFile.MainSection.Fields.UnRegisterOnChangeHook(@ImportHook);
  DataFile.MainSection.Headings.UnRegisterOnChangeHook(@ImportHook);
end;

procedure TImportStructureForm.SetImportData(AValue: boolean);
begin
  if FImportData = AValue then Exit;
  FImportData := AValue;
end;

procedure TImportStructureForm.CancelActionExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TImportStructureForm.DocListHook(Sender: TObject);
begin
  TStringGrid(FDataCol.Grid).Cells[FDataCol.Index+1, TStringGrid(FDataCol.Grid).RowCount-1] := '0';
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

  if ImportData then
  begin
    Caption := 'Add structure/Import Data';

    FOldCheckBoxToggle := FProjectList.StructureGrid.OnCheckboxToggled;
    FProjectList.StructureGrid.OnCheckboxToggled := @ImportDataCheckBoxToogle;

    FOldColRowMoved := FProjectList.StructureGrid.OnColRowMoved;
    FProjectList.StructureGrid.OnColRowMoved := @ImportDataColRowMoved;
    FImportDataSelectedIndex := -1;
  end else begin
    Caption := 'Add structure';

    FDataCol.ReadOnly := true;
    FDataCol.Color := clSilver;
  end;


  if Assigned(FInitialFiles) then
  begin
    Screen.Cursor := crHourGlass;
    Application.ProcessMessages;

    FProjectList.AddFiles(FInitialFiles);

    Screen.Cursor := crDefault;
    Application.ProcessMessages;

    if ImportData and
       (FProjectList.StructureGrid.RowCount > 1)
    then
    begin
      FProjectList.StructureGrid.Cells[FDataCol.Index + 1, 1] := '1';
      FImportDataSelectedIndex := 0;
    end;
  end;
end;

procedure TImportStructureForm.ImportDataColRowMoved(Sender: TObject;
  IsColumn: Boolean; sIndex, tIndex: Integer);
begin
  // Always send event on to others!
  if Assigned(FOldColRowMoved) then
    FOldColRowMoved(Sender, IsColumn, sIndex, tIndex);

  if IsColumn then exit;
  if FImportDataSelectedIndex = -1 then exit;

  if sIndex = FImportDataSelectedIndex then
  begin
    FImportDataSelectedIndex := tIndex;
    Exit;
  end;

  if (
      (sIndex > FImportDataSelectedIndex) and
      (tIndex > FImportDataSelectedIndex)
     ) or (
      (sIndex < FImportDataSelectedIndex) and
      (tIndex < FImportDataSelectedIndex)
     )
  then
    // Row moved happend above or below index => do nothing.
    Exit;

  if sIndex > tIndex then
    // Row moved below data index => increase value.
    Inc(FImportDataSelectedIndex)
  else
    // Row moved above data index => decrease value.
    Dec(FImportDataSelectedIndex);
end;

procedure TImportStructureForm.ImportDataCheckBoxToogle(sender: TObject; aCol,
  aRow: Integer; aState: TCheckboxState);
begin
  // Always send event on to others!
  if Assigned(FOldCheckBoxToggle) then
    FOldCheckBoxToggle(sender, aCol, aRow, aState);

  // Exit if this check-toggle does not apply to use.
  if FDataCol.Index <> (aCol - 1) then exit;

  if (aRow = FImportDataSelectedIndex) and
     (aState = cbUnchecked)
  then
  begin
    FImportDataSelectedIndex := -1;
    Exit;
  end;

  if (aState = cbChecked) then
  begin
    if FImportDataSelectedIndex <> -1 then
      TStringGRid(FDataCol.Grid).Cells[FDataCol.Index+1, FImportDataSelectedIndex] := '0';

    FImportDataSelectedIndex := aRow;
  end;
end;

constructor TImportStructureForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FProjectList := TProjectFileListFrame.Create(Self);
  with FProjectList do
  begin
    OnBeforeImportFile := @BeforeLoad;
    OnAfterImportFile  := @AfterLoad;

    StructureGrid.Columns[1].Title.Caption := 'Structure';

    FDataCol := TGridColumn(StructureGrid.Columns.Insert(2));
    FDataCol.Title.Caption := 'Data';
    FDataCol.ButtonStyle := cbsCheckboxColumn;

    Align := alClient;
    Parent := Self;
  end;

  if TheOwner is TScrollBox then
    FDesignerBox := TScrollBox(TheOwner);

  FProjectList.DocList.OnChange := @DocListHook;
end;

destructor TImportStructureForm.Destroy;
begin
  FSelectedDocuments.Free;
  inherited Destroy;
end;

procedure TImportStructureForm.AddInitialFiles(const Files: TStrings);
begin
  if Assigned(Files) and (Files.Count > 0) then
  begin
    FInitialFiles := TStringList.Create;
    FInitialFiles.Assign(Files);
  end;
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

