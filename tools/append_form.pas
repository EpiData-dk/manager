unit append_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, CheckLst, Grids,
  epidocument, epidatafiles, epiopenfile, epitools_append,
  projectfilelist_frame;

type

  { TAppendForm }

  TAppendForm = class(TForm)
    AddFilesBtn: TBitBtn;
    OkBtn: TBitBtn;
    CancelBtn: TBitBtn;
    GroupBox3: TGroupBox;
    AFCheckList: TCheckListBox;
    AFAllBtn: TButton;
    AFNoneBtn: TButton;
    Label7: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Splitter1: TSplitter;
    procedure AddFilesBtnClick(Sender: TObject);
    procedure AFAllBtnClick(Sender: TObject);
    procedure AFNoneBtnClick(Sender: TObject);
  private
    FAppendProject: TEpiDocumentFile;
    FMainProject: TEpiDocumentFile;
    procedure SetMainProject(AValue: TEpiDocumentFile);
    procedure UpdateFieldList;
    procedure UpdateOkBtn;
    procedure UpdateAppendProject;
  private
    { File Frame }
    FFileFrame: TProjectFileListFrame;
    FSelectedIndex: Integer;
    procedure FileFrameAddToGrid(Sender: TObject; Document: TEpiDocument;
      const Filename: string; const RowNo: Integer);
    procedure FileFremCheckBoxToggled(sender: TObject; aCol, aRow: Integer;
      aState: TCheckboxState);
    procedure FileFramePrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure FileFrameSetCheckBoxState(Sender: TObject; ACol, ARow: Integer;
      const Value: TCheckboxState);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure CreateSelectedList(var ResultList: TStrings);
    property MainProject: TEpiDocumentFile read FMainProject write SetMainProject;
    property AppendProject: TEpiDocumentFile read FAppendProject;
  end;

  { TAppendHandler }

  TAppendHandler = class
  private
    FMainForm: TForm;
  public
    constructor Create(Const MainForm: TForm);
    procedure AppendError(Sender: TObject; Const Msg: string);
    procedure AppendWarning(Sender: TObject; Const Msg: string;
      out Result: TEpiToolWarningResult);
  end;

var
  AppendForm: TAppendForm;

implementation

{$R *.lfm}

uses
  epimiscutils, settings2_var, epiv_documentfile,
  LazUTF8, math, strutils, main;

{ TAppendForm }

procedure TAppendForm.AFNoneBtnClick(Sender: TObject);
begin
  AFCheckList.CheckAll(cbUnchecked, false, false);
end;

procedure TAppendForm.AFAllBtnClick(Sender: TObject);
begin
  AFCheckList.CheckAll(cbChecked, false, false);
end;

procedure TAppendForm.AddFilesBtnClick(Sender: TObject);
var
  Dlg: TOpenDialog;
begin
  Dlg := TOpenDialog.Create(self);
  Dlg.Filter := GetEpiDialogFilter(dfImport);
  Dlg.InitialDir := ManagerSettings.WorkingDirUTF8;
  Dlg.Options := Dlg.Options + [ofAllowMultiSelect];

  try
    if Dlg.Execute then
      FFileFrame.AddFiles(Dlg.Files);
  finally
    Dlg.Free;
  end;
end;

procedure TAppendForm.SetMainProject(AValue: TEpiDocumentFile);
begin
  if FMainProject = AValue then Exit;
  FMainProject := AValue;

  FFileFrame.AddDocument(AValue);
  FFileFrame.StructureGrid.AutoAdjustColumns;

  UpdateOkBtn;
end;


procedure TAppendForm.UpdateFieldList;
var
  MainDF: TEpiDataFile;
  AppendDF: TEpiDataFile;
  F: TEpiField;
  W: Integer;
  i: Integer;
begin
  AFCheckList.Clear;

  if (not Assigned(MainProject)) or
     (not Assigned(AppendProject))
  then
    Exit;

  AFCheckList.Items.BeginUpdate;

  MainDF   := MainProject.Document.DataFiles[0];
  AppendDF := AppendProject.Document.DataFiles[0];

  W := 0;
  for i := 0 to MainDF.Fields.Count - 1 do
    if AppendDF.Fields.ItemExistsByName(MainDF.Field[i].Name) then
      W := Max(W, UTF8Length(MainDF.Field[i].Name));

  for i := 0 to MainDF.Fields.Count - 1 do
  begin
    F := MainDF.Fields[i];

    if AppendDF.Fields.ItemExistsByName(F.Name) then
      AFCheckList.AddItem(F.Name + DupeString(' ', W - UTF8Length(F.Name)) + ' - ' + F.Question.Text,
      F);
  end;

  AFCheckList.Items.EndUpdate;
  AFCheckList.CheckAll(cbChecked, false, false);
end;

procedure TAppendForm.UpdateOkBtn;
begin
  OkBtn.Enabled := (FSelectedIndex >= 0);
end;

procedure TAppendForm.UpdateAppendProject;
begin
  if FSelectedIndex < 0 then
    FAppendProject := nil
  else
    FAppendProject := TDocumentFile(FFileFrame.DocFileList[FSelectedIndex - 1]);
end;

procedure TAppendForm.FileFrameAddToGrid(Sender: TObject;
  Document: TEpiDocument; const Filename: string; const RowNo: Integer);
var
  SG: TStringGrid;
begin
  if RowNo = 1 then exit;

  SG := FFileFrame.StructureGrid;

  if FSelectedIndex >= 0 then
    SG.Cells[FFileFrame.IncludeCol.Index + 1, RowNo] := FFileFrame.IncludeCol.ValueUnchecked
  else
    FSelectedIndex := RowNo;

  UpdateAppendProject;
  UpdateOkBtn;
  UpdateFieldList;
end;

procedure TAppendForm.FileFremCheckBoxToggled(sender: TObject; aCol,
  aRow: Integer; aState: TCheckboxState);
begin
  // Skip if first row
  if aRow = 1 then exit;

  if FSelectedIndex >= 0 then
    FFileFrame.StructureGrid.Cells[FFileFrame.IncludeCol.Index + 1, FSelectedIndex] := FFileFrame.IncludeCol.ValueUnchecked;

  if aState = cbUnchecked then
    FSelectedIndex := -1
  else
    FSelectedIndex := aRow;

  UpdateAppendProject;
  UpdateOkBtn;
  UpdateFieldList;
end;

procedure TAppendForm.FileFramePrepareCanvas(sender: TObject; aCol,
  aRow: Integer; aState: TGridDrawState);
begin
  if (aRow = 1) and
     (aCol >= 1)
  then
    TStringGrid(Sender).Canvas.Brush.Color := clBtnShadow;
end;

procedure TAppendForm.FileFrameSetCheckBoxState(Sender: TObject; ACol,
  ARow: Integer; const Value: TCheckboxState);
var
  SG: TStringGrid absolute Sender;
begin
  // Main file cannot be de-selected
  if ARow = 1 then Exit;

  case Value of
    cbUnchecked:
      SG.Cells[ACol, ARow] := FFileFrame.IncludeCol.ValueUnchecked;
    cbChecked:
      SG.Cells[ACol, ARow] := FFileFrame.IncludeCol.ValueChecked;
    cbGrayed: ;
  end;
end;

constructor TAppendForm.Create(TheOwner: TComponent);
var
  SG: TStringGrid;
begin
  inherited Create(TheOwner);
  FSelectedIndex := -1;

  FFileFrame := TProjectFileListFrame.Create(self);
  FFileFrame.Align := alClient;
  FFileFrame.Parent := Panel2;
  FFileFrame.OnAfterAddToGrid := @FileFrameAddToGrid;

  SG := FFileFrame.StructureGrid;
  SG.OnPrepareCanvas := @FileFramePrepareCanvas;
  SG.OnSetCheckboxState := @FileFrameSetCheckBoxState;
  SG.Options := SG.Options - [goRowMoving];
  SG.OnCheckboxToggled := @FileFremCheckBoxToggled;
end;

procedure TAppendForm.CreateSelectedList(var ResultList: TStrings);
var
  i: Integer;
begin
  if not Assigned(ResultList) then
    ResultList := TStringList.Create;

  if (not Assigned(MainProject)) or
     (not Assigned(AppendProject))
  then
    Exit;


  for i := 0 to AFCheckList.Count - 1 do
    if AFCheckList.Checked[i] then
      ResultList.Add(TEPiField(AFCheckList.Items.Objects[i]).Name);
end;

{ TAppendHandler }

constructor TAppendHandler.Create(const MainForm: TForm);
begin
  FMainForm := MainForm;
end;

procedure TAppendHandler.AppendError(Sender: TObject; const Msg: string);
begin
  ShowMessage('Error: ' + Msg);
end;

procedure TAppendHandler.AppendWarning(Sender: TObject; const Msg: string; out
  Result: TEpiToolWarningResult);
var
  Res: TModalResult;
begin
  Res :=
    MessageDlg('Warning',
      Msg,
      mtWarning,
      mbYesNo,
      0,
      mbNo
      );
  if Res = mrNo then
    Result := wrStop
  else
    Result := wrContinue;
end;

end.

