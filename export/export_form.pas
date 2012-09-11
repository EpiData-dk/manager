unit export_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, EditBtn, Buttons, CheckLst, ExtCtrls, epiexportsettings,
  export_stata_frame, epidatafiles, epidocument;

type

  { TExportForm }

  TExportForm = class(TForm)
    AllBitBtn: TBitBtn;
    NoneBitBtn: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    ExportDeletedChkBox: TCheckBox;
    FieldsChkListBox: TCheckListBox;
    ExportTypeCombo: TComboBox;
    EncodingCmbBox: TComboBox;
    FromRecordEdit: TEdit;
    ToRecordEdit: TEdit;
    ExportFileNameEdit: TFileNameEdit;
    GroupBox1: TGroupBox;
    FieldGrpBox: TGroupBox;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    QuestionLabel: TLabel;
    Label5: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    AllRecordRBtn: TRadioButton;
    RangeRBtn: TRadioButton;
    BasicSheet: TTabSheet;
    procedure FormShow(Sender: TObject);
    procedure FromRecordEditClick(Sender: TObject);
    procedure NoneBitBtnClick(Sender: TObject);
    procedure ExportTypeComboSelect(Sender: TObject);
    procedure FieldsChkListBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FromRecordEditKeyPress(Sender: TObject; var Key: char);
  private
    FExportSetting: TEpiExportSetting;
    FActiveSheet: TTabSheet;
    FDoc: TEpiDocument;
    { private declarations }
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; Const Doc: TEpiDocument; Const FileName: string);
    property ExportSetting: TEpiExportSetting read FExportSetting;
  end;


procedure RegisterExportFrame(CFC: TCustomFrameClass; ESC: TEpiExportSettingClass);

implementation

{$R *.lfm}

uses
  epieximtypes, export_frame_types, epimiscutils,
  settings2_var, LCLType;

type
  TFrameRec = record
    CFC: TCustomFrameClass;
    ESC: TEpiExportSettingClass;
    Frame: TCustomFrame;
  end;
  PFrameRec = ^TFrameRec;

var
  RegisterList: TList = nil;

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
begin
  if ModalResult <> mrOK then exit;

  Rec := PFrameRec(ExportTypeCombo.Items.Objects[ExportTypeCombo.ItemIndex]);

  FExportSetting := Rec^.ESC.Create;
  with FExportSetting do
  begin
    Doc := FDoc;
    ExportFileName := ExportFileNameEdit.Text;
    DataFileIndex := 0;
    if RangeRBtn.Checked then
    begin
      FromRecord := StrToInt(FromRecordEdit.Text) - 1;  // -1 because the record count in Cores
      ToRecord   := StrToInt(ToRecordEdit.Text) - 1;    // expect the numbers 0-indexed.
    end;
    Encoding := TEpiEncoding(PtrUInt(EncodingCmbBox.Items.Objects[EncodingCmbBox.ItemIndex]));
    for i := 0 to FieldsChkListBox.Items.Count - 1 do
      if FieldsChkListBox.Checked[i] then
        Fields.Add(FieldsChkListBox.Items.Objects[i]);
  end;
  (Rec^.Frame as IExportSettingsPresenterFrame).UpdateExportSetting(FExportSetting);
end;

procedure TExportForm.FromRecordEditKeyPress(Sender: TObject; var Key: char);
begin
  if not (Key in [Char('0')..Char('9')]) then Key := #0;
end;

procedure TExportForm.ExportTypeComboSelect(Sender: TObject);
var
  Frame: TCustomFrame;
  Ext: String;
  P: SizeInt;
begin
  if not (Showing) then exit;

  if Assigned(FActiveSheet) then FActiveSheet.TabVisible := false;

  Frame := PFrameRec(ExportTypeCombo.Items.Objects[ExportTypeCombo.ItemIndex])^.Frame;

  FActiveSheet := TTabSheet(Frame.Parent);
  FActiveSheet.TabVisible := true;

  if ExportFileNameEdit.Text <> '' then
  begin
    Ext := GetEpiDialogFilterExt((Frame as IExportSettingsPresenterFrame).GetFileDialogExtensions);

    // Ext could contain multiple extensions, only use the first.
    P := Pos(';', Ext);
    if P > 0 then
      Delete(Ext, P, Length(Ext));
    // Delete the "*" part of "*.<ext>"
    Delete(Ext, 1, 1);

    ExportFileNameEdit.Text := ChangeFileExt(ExportFileNameEdit.Text, Ext);
  end;
end;

procedure TExportForm.NoneBitBtnClick(Sender: TObject);
var
  State: TCheckBoxState;
begin
  if Sender = NoneBitBtn then
    State := cbUnchecked
  else
    State := cbChecked;
  FieldsChkListBox.CheckAll(State, false, false);
end;

procedure TExportForm.FromRecordEditClick(Sender: TObject);
begin
  RangeRBtn.Checked := true;
end;

procedure TExportForm.FormShow(Sender: TObject);
begin
  // Forces a change of filename extension and frame!
  ExportTypeComboSelect(ExportTypeCombo);
end;

procedure TExportForm.FieldsChkListBoxMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  Idx: Integer;
begin
  Idx := FieldsChkListBox.GetIndexAtXY(X,Y);
  if Idx <> -1 then
    QuestionLabel.Caption := TEpiField(FieldsChkListBox.Items.Objects[Idx]).Question.Text;
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
  FExportSetting := nil;
  FActiveSheet := nil;
  DialogFilters := [];

  FDoc := Doc;

  // Export File Name
  ExportFileNameEdit.Filter := GetEpiDialogFilter(DialogFilters + [dfAll]);
  ExportFileNameEdit.InitialDir := ManagerSettings.WorkingDirUTF8;
  ExportFileNameEdit.FileName := FileName;

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

    with (Frame as IExportSettingsPresenterFrame) do
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
    AddObject('Centel Europe (CP1250)', TObject(eeCP1250));
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

  // Fields:
  // TODO : Using only for datafile until more df's are supported.
  for i := 0 to Doc.DataFiles[0].Fields.Count - 1 do
    FieldsChkListBox.AddItem(Doc.DataFiles[0].Fields[i].Name, Doc.DataFiles[0].Fields[i]);
  FieldsChkListBox.CheckAll(cbChecked, false, false);

  // SETUP ACCORDING TO MANAGERSETTINGS.
  // Export type:
  //  - Dirty way of doing, but works for now.
  case ManagerSettings.ExportType of
    0: S := 'Stata';
    1: S := 'CSV File';
    2: S := 'SPSS';
    3: S := 'SAS';
  end;
  ExportTypeCombo.ItemIndex := ExportTypeCombo.Items.IndexOf(S);
  // Encoding:
  EncodingCmbBox.ItemIndex := EncodingCmbBox.Items.IndexOfObject(TObject(PtrUInt(ManagerSettings.ExportEncoding)));
  // Export Deleted:
  ExportDeletedChkBox.Checked := ManagerSettings.ExportDeleted;
end;

end.

