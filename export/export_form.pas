unit export_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, EditBtn, Buttons, CheckLst, ExtCtrls, VirtualTrees,
  epiexportsettings, export_stata_frame, epidatafiles, epidocument,
  epicustombase, export_frame_types;

type

  { TExportForm }

  TExportForm = class(TForm)
    AllBitBtn: TBitBtn;
    ExportReportChkBox: TCheckBox;
    ImageList1: TImageList;
    NoneBitBtn: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    ExportDeletedChkBox: TCheckBox;
    FieldsChkListBox: TCheckListBox;
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
    DataFileTree: TVirtualStringTree;
    procedure BitBtn3Click(Sender: TObject);
    procedure DataFileTreeGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
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
    IFrame: IExportSettingsPresenterFrame;
    FDoc: TEpiDocument;
    FFileName: string;
    procedure PopulateListBox(Const Datafile: TEpiDataFile);
  private
    { VirtualStringTree }
    FDataFile: TEpiDataFile;
    function GetCustomItemFromNode(Const Node: PVirtualNode): TEpiCustomControlItem;
    procedure DataFileTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure DataFileTreeInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure PopulateTree(Const Datafile: TEpiDataFile);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; Const Doc: TEpiDocument; Const FileName: string);
    property ExportSetting: TEpiExportSetting read FExportSetting;
  public
    class procedure RestoreDefaultPos;
  end;


procedure RegisterExportFrame(CFC: TCustomFrameClass; ESC: TEpiExportSettingClass);

implementation

{$R *.lfm}

uses
  epieximtypes, epimiscutils,
  settings2, settings2_var, LCLType, epidatafilestypes;

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
  Node: PVirtualNode;
  CI: TEpiCustomControlItem;
begin
  if ModalResult <> mrOK then exit;

  Rec := PFrameRec(ExportTypeCombo.Items.Objects[ExportTypeCombo.ItemIndex]);

  FExportSetting := Rec^.ESC.Create;
  with FExportSetting do
  begin
    Doc := FDoc;
    ExportFileName := ExportFileNameEdit.Text;
    DataFileIndex := 0;
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

    Node := DataFileTree.GetFirst();
    while Assigned(Node) do
    begin
      CI := GetCustomItemFromNode(Node);

      if (DataFileTree.CheckState[Node] in [csMixedNormal, csCheckedNormal]) and
         (DataFileTree.IsVisible[Node])
      then
        Fields.Add(GetCustomItemFromNode(Node));
      Node := DataFileTree.GetNext(Node, true);
    end;
  end;
  (Rec^.Frame as IExportSettingsPresenterFrame).UpdateExportSetting(FExportSetting);
end;

procedure TExportForm.FromRecordEditKeyPress(Sender: TObject; var Key: char);
begin
  if not (Key in [Char('0')..Char('9')]) then Key := #0;
end;

procedure TExportForm.PopulateListBox(const Datafile: TEpiDataFile);
var
  i: Integer;
begin
  // Fields:
  for i := 0 to DataFile.Fields.Count - 1 do
    FieldsChkListBox.AddItem(DataFile.Fields[i].Name, DataFile.Fields[i]);
  FieldsChkListBox.CheckAll(cbChecked, false, false);
end;

function TExportForm.GetCustomItemFromNode(const Node: PVirtualNode
  ): TEpiCustomControlItem;
begin
  result := TEpiCustomControlItem(DataFileTree.GetNodeData(Node)^);
end;

procedure TExportForm.PopulateTree(const Datafile: TEpiDataFile);
var
  MainNode: PVirtualNode;
  CI: TEpiCustomControlItem;
  CurrentNode: PVirtualNode;
  i: Integer;
begin
  DataFileTree.OnGetText  := @DataFileTreeGetText;
  DataFileTree.OnInitNode := @DataFileTreeInitNode;

  DataFileTree.Clear;
  DataFileTree.BeginUpdate;
  DataFileTree.NodeDataSize := SizeOf(TEpiCustomControlItem);

  MainNode := DataFileTree.AddChild(nil, Datafile.MainSection);

  for i := 1 to Datafile.ControlItems.Count - 1 do
  begin
    CI := Datafile.ControlItem[i];

    if (CI.Owner.Owner = Datafile.MainSection) or
       (CI is TEpiSection)
    then
      CurrentNode := MainNode;

    if CI is TEpiSection then
      CurrentNode := DataFileTree.AddChild(CurrentNode, CI)
    else
      DataFileTree.AddChild(CurrentNode, CI);
  end;
  DataFileTree.EndUpdate;
  DataFileTree.ReinitChildren(MainNode, true);
  DataFileTree.ToggleNode(MainNode);
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

  DataFileTree.ReinitNode(DataFileTree.GetFirst(), true);
end;

procedure TExportForm.NoneBitBtnClick(Sender: TObject);
var
  State: TCheckState;
begin
  if Sender = NoneBitBtn then
    State := csUncheckedNormal
  else
    State := csCheckedNormal;

  DataFileTree.CheckState[DataFileTree.GetFirst()] := State;
//  FieldsChkListBox.CheckAll(State, false, false);
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
  ExportTypeCombo.ItemIndex := ExportTypeCombo.Items.IndexOf(S);
  ExportTypeComboSelect(ExportTypeCombo);
  PopulateTree(FDoc.DataFiles[0]);

  // Encoding:
  EncodingCmbBox.ItemIndex := EncodingCmbBox.Items.IndexOfObject(TObject(PtrUInt(ManagerSettings.ExportEncoding)));
  // Export Deleted:
  ExportDeletedChkBox.Checked := ManagerSettings.ExportDeleted;
  ExportReportChkBox.Checked := ManagerSettings.ExportCreateReport;

  if ManagerSettings.SaveWindowPositions then
    LoadFormPosition(Self, Self.ClassName);
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

procedure TExportForm.DataFileTreeGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  CI: TEpiCustomControlItem;
begin
  if Column = 1 then exit;

  CI := GetCustomItemFromNode(Node);

  if CI is TEpiHeading then
    ImageIndex := 8;
  if CI is TEpiSection then
    ImageIndex := 9;

  if CI is TEpiField then
  case TEpiField(CI).FieldType of
    ftBoolean:
      ImageIndex := 0;
    ftInteger,
    ftAutoInc:
      ImageIndex := 1;
    ftFloat:
      ImageIndex := 2;
    ftDMYDate,
    ftDMYAuto:
      ImageIndex := 3;
    ftMDYDate,
    ftMDYAuto:
      ImageIndex := 4;
    ftYMDDate,
    ftYMDAuto:
      ImageIndex := 5;
    ftTime,
    ftTimeAuto:
      ImageIndex := 6;
    ftString,
    ftUpperString:
      ImageIndex := 7;
  end;
end;

procedure TExportForm.BitBtn3Click(Sender: TObject);
var
  Msg: String;
  Res: TModalResult;
begin
  if FileExistsUTF8(ExportFileNameEdit.FileName) then
  begin
    Msg :=
      'A file named "' + ExtractFileName(ExportFileNameEdit.FileName) + '" already exits!' + LineEnding +
      'Replacing this file will also replace any addition files this export may create (eg. .csv/.log files)' + LineEnding +
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

procedure TExportForm.DataFileTreeGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  CI: TEpiCustomControlItem;
begin
  CI := GetCustomItemFromNode(Node);

  if Column = 0 then
    CellText := CI.Name
  else begin
    if CI is TEpiSection then
      CellText := TEpiSection(CI).Caption.Text;
    if CI is TEpiHeading then
      CellText := TEpiHeading(CI).Caption.Text;
    if CI is TEpiField then
      CellText := TEpiField(CI).Question.Text;
  end;
end;

procedure TExportForm.DataFileTreeInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  CI: TEpiCustomControlItem;
begin
  Sender.CheckType[Node]  := ctTriStateCheckBox;
  Sender.CheckState[Node] := csCheckedNormal;

  CI := GetCustomItemFromNode(Node);
  if (CI is TEpiHeading) and
     (not IFrame.ExportHeadings)
  then
    Sender.IsVisible[Node] := false
  else
    Sender.IsVisible[Node] := true;
end;

procedure TExportForm.FieldsChkListBoxMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  Idx: Integer;
begin
{  Idx := FieldsChkListBox.GetIndexAtXY(X,Y);
  if Idx <> -1 then
    QuestionLabel.Caption := TEpiField(FieldsChkListBox.Items.Objects[Idx]).Question.Text;         }
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
  FFileName := FileName;

  // Export File Name
  ExportFileNameEdit.Filter := GetEpiDialogFilter(DialogFilters + [dfAll]);
  ExportFileNameEdit.InitialDir := ManagerSettings.WorkingDirUTF8;
  ExportFileNameEdit.FileName := ChangeFileExt(FFileName, '_' + IntToStr(FDoc.CycleNo) + '.test');

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

  // TODO : Using only for datafile until more df's are supported.
//  PopulateListBox(Doc.DataFiles[0]);
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

