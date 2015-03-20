unit export_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, EditBtn, Buttons, CheckLst, ExtCtrls, VirtualTrees,
  epiexportsettings, export_stata_frame, epidatafiles, epidocument,
  epicustombase, epirelations, epiv_dataform_treeview, export_frame_types;

type

  { TExportForm }

  TExportForm = class(TForm)
    AllBitBtn: TBitBtn;
    DataFileComboBox: TComboBox;
    DFTreeViewFrame: TDataFormTreeViewFrame;
    ExportReportChkBox: TCheckBox;
    NoneBitBtn: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    ExportDeletedChkBox: TCheckBox;
    ExportTypeCombo: TComboBox;
    EncodingCmbBox: TComboBox;
    FromRecordEdit: TEdit;
    NoneRecordsRadioBtn: TRadioButton;
    ToRecordEdit: TEdit;
    ExportFileNameEdit: TFileNameEdit;
    GroupBox1: TGroupBox;
    FieldGrpBox: TGroupBox;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label5: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    AllRecordRBtn: TRadioButton;
    RangeRBtn: TRadioButton;
    BasicSheet: TTabSheet;
    procedure BitBtn3Click(Sender: TObject);
    procedure DataFileComboBoxDropDown(Sender: TObject);
    procedure DataFileComboBoxSelect(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
    procedure FromRecordEditClick(Sender: TObject);
    procedure NoneBitBtnClick(Sender: TObject);
    procedure ExportTypeComboSelect(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FromRecordEditKeyPress(Sender: TObject; var Key: char);
  private
    FOldDataFormIndex: Integer;
    FExportSetting: TEpiExportSetting;
    FActiveSheet: TTabSheet;
    IFrame: IExportSettingsPresenterFrame;
    FDoc: TEpiDocument;
    FFileName: string;
    procedure UpdateDataFileCombo;
    procedure UpdateExportFileName;
    procedure LoadGlyphs;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; Const Doc: TEpiDocument; Const FileName: string);
    property ExportSetting: TEpiExportSetting read FExportSetting;
  public
    class procedure RestoreDefaultPos;
  end;


procedure RegisterExportFrame(CFC: TCustomFrameClass; ESC: TEpiExportSettingClass);

var
  RegisterList: TList = nil;

implementation

{$R *.lfm}

uses
  epieximtypes, epimiscutils,
  settings2, settings2_var, LCLType, epidatafilestypes,
  epiv_datamodule, LazUTF8;

type
  TFrameRec = record
    CFC: TCustomFrameClass;
    ESC: TEpiExportSettingClass;
    Frame: TCustomFrame;
  end;
  PFrameRec = ^TFrameRec;

type
  TAccessFileNameEdit = class(TFileNameEdit)
  public
    property Button;
  end;

procedure RegisterExportFrame(CFC: TCustomFrameClass; ESC: TEpiExportSettingClass
  );
var
  Rec: PFrameRec;
begin
  if not Assigned(RegisterList) then
    RegisterList := TList.Create;

  if (CFC <> nil) and (not Supports(CFC, IExportSettingsPresenterFrame)) then
    Exit;

  Rec := new(PFrameRec);
  Rec^.CFC := CFC;
  Rec^.ESC := ESC;
  RegisterList.Add(Rec);
end;

{ TExportForm }

procedure TExportForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  i: Integer;
  Rec: PFrameRec;
  Node: PVirtualNode;
  CI: TEpiCustomControlItem;
  SelectedDF: TEpiDataFile;
begin
  if ModalResult <> mrOK then exit;

  Rec := PFrameRec(ExportTypeCombo.Items.Objects[ExportTypeCombo.ItemIndex]);
  SelectedDF := TEpiDataFile(DataFileComboBox.Items.Objects[DataFileComboBox.ItemIndex]);

  FExportSetting := Rec^.ESC.Create;
  with FExportSetting do
  begin
    Doc := FDoc;
{    ExportFileName := ExportFileNameEdit.Text;
    DataFileIndex := FDoc.DataFiles.IndexOf(SelectedDF);
    ExportDeleted := ExportDeletedChkBox.Checked;

    if RangeRBtn.Checked then
    begin
      FromRecord := StrToInt(FromRecordEdit.Text) - 1;  // -1 because the record count in Core
      ToRecord   := StrToInt(ToRecordEdit.Text) - 1;    // expect the numbers 0-indexed.
    end;

    if NoneRecordsRadioBtn.Checked then
    begin
      FromRecord := 1;
      ToRecord   := 0;
    end;
    Encoding := TEpiEncoding(PtrUInt(EncodingCmbBox.Items.Objects[EncodingCmbBox.ItemIndex]));
    Fields.Free;

    Fields := DFTreeViewFrame.SelectedList;}
  end;
  (Rec^.Frame as IExportSettingsPresenterFrame).UpdateExportSetting(FExportSetting);
end;

procedure TExportForm.FromRecordEditKeyPress(Sender: TObject; var Key: char);
begin
  if not (Key in [Char('0')..Char('9')]) then Key := #0;
end;

procedure TExportForm.UpdateDataFileCombo;
var
  OrderedDataFiles: TEpiDataFiles;
  DF: TEpiDataFile;
begin
  OrderedDataFiles := FDoc.Relations.GetOrderedDataFiles;

  for DF in OrderedDataFiles do
    DataFileComboBox.AddItem(DF.Caption.Text, DF);

  OrderedDataFiles.Free;
end;

procedure TExportForm.UpdateExportFileName;
begin

end;

procedure TExportForm.LoadGlyphs;
begin
  DM.Icons16.GetBitmap(19, TAccessFileNameEdit(ExportFileNameEdit).Button.Glyph);
end;

procedure TExportForm.ExportTypeComboSelect(Sender: TObject);
var
  Frame: TCustomFrame;
  Ext: String;
  P: SizeInt;
begin
  if not (Showing) then exit;

  if Assigned(FActiveSheet) then FActiveSheet.TabVisible := false;

  P := ExportTypeCombo.ItemIndex;
  if P = -1 then exit;

  Frame := PFrameRec(ExportTypeCombo.Items.Objects[P])^.Frame;

  FActiveSheet := TTabSheet(Frame.Parent);
  FActiveSheet.TabVisible := true;

  if not Supports(Frame, IExportSettingsPresenterFrame, IFrame) then exit;

  if ExportFileNameEdit.Text <> '' then
  begin
    Ext := GetEpiDialogFilterExt(IFrame.GetFileDialogExtensions);

    // Ext could contain multiple extensions, only use the first.
    P := Pos(';', Ext);
    if P > 0 then
      Delete(Ext, P, Length(Ext));

    // Delete the "*" part of "*.<ext>"
    Delete(Ext, 1, 1);

    ExportFileNameEdit.Text := ChangeFileExt(ExportFileNameEdit.Text, Ext)
  end;

  DFTreeViewFrame.ShowHeadings := IFrame.ExportHeadings;
end;

procedure TExportForm.NoneBitBtnClick(Sender: TObject);
var
  State: TCheckState;
begin
  if Sender = NoneBitBtn then
    DFTreeViewFrame.SelectNone
  else
    DFTreeViewFrame.SelectAll;
end;

procedure TExportForm.FromRecordEditClick(Sender: TObject);
begin
  RangeRBtn.Checked := true;
end;

procedure TExportForm.FormShow(Sender: TObject);
var
  S: String;
begin
  // SETUP ACCORDING TO MANAGERSETTINGS.
  // Export type:
  //  - Dirty way of doing, but works for now.
  case ManagerSettings.ExportType of
    0: S := 'Stata';
    1: S := 'CSV File';
    2: S := 'SPSS';
    3: S := 'SAS';
    4: S := 'DDI';
    5: S := 'EPX';
  end;
  ExportTypeCombo.AdjustSize;
  ExportTypeCombo.ItemIndex := ExportTypeCombo.Items.IndexOf(S);
  ExportTypeComboSelect(ExportTypeCombo);

  // Encoding:
  EncodingCmbBox.ItemIndex := EncodingCmbBox.Items.IndexOfObject(TObject(PtrUInt(ManagerSettings.ExportEncoding)));
  // Export Deleted:
  ExportDeletedChkBox.Checked := ManagerSettings.ExportDeleted;
  ExportReportChkBox.Checked := ManagerSettings.ExportCreateReport;

  if ManagerSettings.SaveWindowPositions then
    LoadFormPosition(Self, Self.ClassName);

  DataFileComboBox.ItemIndex := 0;
  DataFileComboBoxSelect(nil);
end;

procedure TExportForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  Rec: PFrameRec;
  ErrorText: string;
begin
  if ModalResult = mrOk then
  begin
    Rec := PFrameRec(ExportTypeCombo.Items.Objects[ExportTypeCombo.ItemIndex]);
    CanClose := (Rec^.Frame as IExportSettingsPresenterFrame).CheckExportAllowed(FExportSetting, FDoc, ErrorText);

    if not CanClose then
      ShowMessage(ErrorText);
  end;

  if ManagerSettings.SaveWindowPositions then
    SaveFormPosition(Self, Self.ClassName);
end;

procedure TExportForm.BitBtn3Click(Sender: TObject);
var
  Msg: String;
  Res: TModalResult;
begin
  if FileExistsUTF8(ExportFileNameEdit.FileName) then
  begin
    Msg :=
      'A file named "' + ExtractFileName(ExportFileNameEdit.FileName) + '" already exists!' + LineEnding +
      'Replacing this file will also replace any additional files this export may create (eg. .csv/.log files)' + LineEnding +
      LineEnding +
      'Do you wish to replace the existing file(s)?';

    Res := MessageDlg(
      'Warning!',
      Msg,
      mtWarning,
      mbYesNo,
      0,
      mbNo);

    if Res = mrNo then
    begin
      ModalResult := mrNone;
      Exit;
    end;
  end;
end;

procedure TExportForm.DataFileComboBoxDropDown(Sender: TObject);
begin
  FOldDataFormIndex := DataFileComboBox.ItemIndex;
end;

procedure TExportForm.DataFileComboBoxSelect(Sender: TObject);
var
  DF: TEpiDataFile;
  DFCaption: String;
  P: SizeInt;
  FName: TCaption;
begin
  DFTreeViewFrame.DataFile := TEpiDataFile(DataFileComboBox.Items.Objects[DataFileComboBox.ItemIndex]);

  //  Add dataform captions:
  if FOldDataFormIndex < 0 then exit;

  FName := ExportFileNameEdit.Text;
  DF := TEpiDataFile(DataFileComboBox.Items.Objects[FOldDataFormIndex]);
  DFCaption := '_' + StringReplace(DF.Caption.Text, ' ', '_', [rfReplaceAll]);


  P := Pos(DFCaption, FName);
  if P > 0 then
  begin
    Delete(FName, P, Length(DFCaption));

    DF := TEpiDataFile(DataFileComboBox.Items.Objects[DataFileComboBox.ItemIndex]);
    DFCaption := '_' + StringReplace(DF.Caption.Text, ' ', '_', [rfReplaceAll]);
    Insert(DFCaption, FName, P);

    ExportFileNameEdit.Text := FName;
  end;
end;

constructor TExportForm.Create(TheOwner: TComponent; const Doc: TEpiDocument;
  const FileName: string);
var
  Rec: PFrameRec;
  Tab: TTabSheet;
  Frame: TCustomFrame;
  i: Integer;
  DialogFilters: TEpiDialogFilters;
  S: String;
begin
  inherited Create(TheOwner);

  LoadGlyphs;

  FOldDataFormIndex := -1;
  FExportSetting := nil;
  FActiveSheet := nil;
  DialogFilters := [];

  FDoc := Doc;
  FFileName := FileName;

  // Export File Name
  ExportFileNameEdit.Filter := GetEpiDialogFilter(DialogFilters + [dfAll]);
  ExportFileNameEdit.InitialDir := ManagerSettings.WorkingDirUTF8;

  if (Doc.DataFiles.Count > 1) then
    ExportFileNameEdit.FileName := ChangeFileExt(FFileName,
      '_' + StringReplace(Doc.DataFiles[0].Caption.Text, ' ', '_', [rfReplaceAll]) +
      '_' + IntToStr(FDoc.CycleNo) + '.test')
  else
    ExportFileNameEdit.FileName := ChangeFileExt(FFileName,
      '_' + IntToStr(FDoc.CycleNo) + '.test');

  // Export types and their frames
  for i := 0 to RegisterList.Count - 1 do
  begin
    Rec := PFrameRec(RegisterList[i]);

    Tab := PageControl1.AddTabSheet;
    Tab.TabVisible := false;
    Frame := Rec^.CFC.Create(Tab);
    Frame.Parent := Tab;
    Frame.Align := alClient;
    Rec^.Frame := Frame;
    Frame.GetInterface(IExportSettingsPresenterFrame, IFrame);

    with IFrame do
    begin
      Tab.Caption := GetFrameCaption;
      ExportTypeCombo.AddItem(GetExportName, TObject(Rec));
      DialogFilters := DialogFilters + GetFileDialogExtensions;
    end;
  end;

  // Encodings
  with EncodingCmbBox.Items do
  begin
    Clear;
    AddObject('Unicode (UTF-8)',        TObject(eeUTF8));
    AddObject('Central Europe (CP1250)',TObject(eeCP1250));
    AddObject('Cyrillic (CP1251)',      TObject(eeCP1251));
    AddObject('Latin 1(CP1252)',        TObject(eeCP1252));
    AddObject('Greek (CP1253)',         TObject(eeCP1253));
    AddObject('Turkish (CP1254)',       TObject(eeCP1254));
    AddObject('Hebrew (CP1255)',        TObject(eeCP1255));
    AddObject('Arabic (CP1256)',        TObject(eeCP1256));
    AddObject('Baltic (CP1257)',        TObject(eeCP1257));
    AddObject('Vietnamese (CP1258)',    TObject(eeCP1258));
    AddObject('Thai (CP874)',           TObject(eeCP874));
    AddObject('Russian (KOI8)',         TObject(eeKOI8));
    AddObject('Chinese simple (CP936)', TObject(eeCP936));
    AddObject('Chinese complex (CP950)',TObject(eeCP950));
    AddObject('Korean (CP949)',         TObject(eeCP949));
    AddObject('Japanes (CP932)',        TObject(eeCP932));
  end;

  UpdateDataFileCombo;
end;

class procedure TExportForm.RestoreDefaultPos;
var
  Aform: TForm;
begin
  Aform := TForm.Create(nil);
  Aform.Width := 600;
  Aform.Height := 650;
  Aform.top := (Screen.Monitors[0].Height - Aform.Height) div 2;
  Aform.Left := (Screen.Monitors[0].Width - Aform.Width) div 2;
  SaveFormPosition(Aform, TExportForm.ClassName);
  AForm.free;
end;

finalization
  begin
    while RegisterList.Count > 0 do
      FreeMem(RegisterList.Extract(RegisterList.Last));
    RegisterList.Free;
  end;

end.

