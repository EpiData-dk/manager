unit append_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, CheckLst,
  epidocument, epidatafiles, epiopenfile, epitools_append;

type

  { TAppendForm }

  TAppendForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    FileNameLabel: TLabel;
    FileNameLabel1: TLabel;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    AFCheckList: TCheckListBox;
    AFAllBtn: TButton;
    AFNoneBtn: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label7: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    SpeedButton1: TSpeedButton;
    TitleLabel: TLabel;
    TitleLabel1: TLabel;
    procedure AFAllBtnClick(Sender: TObject);
    procedure AFNoneBtnClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    FAppendProject: TEpiDocumentFile;
    FMainProject: TEpiDocumentFile;
    procedure SetMainProject(AValue: TEpiDocumentFile);
    procedure DoOpenFile(Const FileName: string);
    procedure UpdateFieldList;
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

procedure TAppendForm.SpeedButton1Click(Sender: TObject);
var
  Dlg: TOpenDialog;
begin

  try
    Dlg := TOpenDialog.Create(self);
    Dlg.InitialDir := ManagerSettings.WorkingDirUTF8;
    Dlg.Filter := GetEpiDialogFilter([dfEPX, dfEPZ]);

    if not Dlg.Execute then exit;

    DoOpenFile(Dlg.FileName);
  finally
    Dlg.Free;
  end;
end;

procedure TAppendForm.AFNoneBtnClick(Sender: TObject);
begin
  AFCheckList.CheckAll(cbUnchecked, false, false);
end;

procedure TAppendForm.AFAllBtnClick(Sender: TObject);
begin
  AFCheckList.CheckAll(cbChecked, false, false);
end;

procedure TAppendForm.SetMainProject(AValue: TEpiDocumentFile);
begin
  if FMainProject = AValue then Exit;
  FMainProject := AValue;

  FileNameLabel.Caption := FMainProject.FileName;
  TitleLabel.Caption := FMainProject.Document.Study.Title.Text;
end;

procedure TAppendForm.DoOpenFile(const FileName: string);
var
  S: String;
begin
  // TODO: Show Error?
  if not FileExistsUTF8(FileName) then
  begin
    S := 'Warning: File "' + FileName + '" not found!';

    if Assigned(FAppendProject) then
      S := S + LineEnding +
        'Content shown is from "' + FAppendProject.FileName + '"';

    ShowMessage(S);
    Exit;
  end;


  if Assigned(FAppendProject) then
  begin
    if FAppendProject.FileName = FileName then exit;

    FreeAndNil(FAppendProject);
  end;

  FAppendProject := TDocumentFile.Create;
  if not FAppendProject.OpenFile(FileName, true) then
  begin
    FreeAndNil(FAppendProject);
    Exit;
  end;

  FileNameLabel1.Caption := FAppendProject.FileName;
  TitleLabel1.Caption := FAppendProject.Document.Study.Title.Text;

  UpdateFieldList;
end;

procedure TAppendForm.UpdateFieldList;
var
  MainDF: TEpiDataFile;
  AppendDF: TEpiDataFile;
  F: TEpiField;
  W: Integer;
  i: Integer;
begin

  if (not Assigned(MainProject)) or
     (not Assigned(AppendProject))
  then
    Exit;

  AFCheckList.Clear;
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

constructor TAppendForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FileNameLabel1.Caption := '(press button to open file)';
  TitleLabel1.Caption := 'N/A';
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

