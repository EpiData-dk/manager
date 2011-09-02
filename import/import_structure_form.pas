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
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
    procedure OkActionExecute(Sender: TObject);
    procedure AddFilesActionExecute(Sender: TObject);
  private
    { private declarations }
    FSelectedDocuments: TStringList;
    FLastRecYPos: Integer;
    FLastEpiCtrl: TEpiCustomControlItem;
    DataFile: TEpiDatafile;
    FDesignerBox: TScrollBox;
    FProjectList: TProjectFileListFrame;
    procedure ImportHook(Sender: TObject; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
    procedure BeforeLoad(Sender: TObject; Doc: TEpiDocument; Const FN: string);
    procedure AfterLoad(Sender: TObject; Doc: TEpiDocument; Const FN: string);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; Const Files: TStrings);
    destructor Destroy; override;
    class procedure RestoreDefaultPos;
    property    SelectedDocuments: TStringList read FSelectedDocuments;
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
begin
  FSelectedDocuments := FProjectList.SelectedList;

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
  DataFile.MainSection.Fields.RegisterOnChangeHook(@ImportHook, false);
  DataFile.MainSection.Headings.RegisterOnChangeHook(@ImportHook, false);
end;

procedure TImportStructureForm.AfterLoad(Sender: TObject; Doc: TEpiDocument;
  const FN: string);
var
  Ext: String;
begin
  Ext := ExtractFileExt(FN);
  if (Ext = '.epx') or (Ext = '.epz') then exit;

  DataFile.MainSection.Fields.UnRegisterOnChangeHook(@ImportHook);
  DataFile.MainSection.Headings.UnRegisterOnChangeHook(@ImportHook);
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

constructor TImportStructureForm.Create(TheOwner: TComponent;
  const Files: TStrings);
begin
  inherited Create(TheOwner);
  FProjectList := TProjectFileListFrame.Create(Self);
  with FProjectList do
  begin
    FProjectList.OnBeforeImportFile := @BeforeLoad;
    FProjectList.OnAfterImportFile  := @AfterLoad;

    FProjectList.Align := alClient;
    FProjectList.Parent := Self;
  end;

  if TheOwner is TScrollBox then
    FDesignerBox := TScrollBox(TheOwner);

  FProjectList.AddFiles(Files);
end;

destructor TImportStructureForm.Destroy;
begin
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

