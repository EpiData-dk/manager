unit archive_form;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  EditBtn, Buttons, FileCtrl, ComboEx, CheckLst, ComCtrls, FileUtil;

type

  { TArchiveForm }

  TArchiveForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    FilterChkList: TCheckListBox;
    Label7: TLabel;
    Label8: TLabel;
    ProgressBar1: TProgressBar;
    ProgressBar2: TProgressBar;
    SubDirChkBox: TCheckBox;
    EncryptCheckBox: TCheckBox;
    FolderEdit: TDirectoryEdit;
    PasswordEdit: TEdit;
    RepeatPasswordEdit: TEdit;
    SaveAsEdit: TFileNameEdit;
    SingleFileEdit: TFileNameEdit;
    ArchiveBox: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Panel1: TPanel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    procedure RadioButton1Change(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure EncryptCheckBoxChange(Sender: TObject);
    procedure FolderEditAcceptDirectory(Sender: TObject; var Value: String);
    procedure SingleFileEditAcceptFileName(Sender: TObject; var Value: String);
    procedure FolderEditEditingDone(Sender: TObject);
  private
    LastUpdate: QWord;
    FHintWindow: THintWindow;
    function ShowError(Ctrl: TControl; Const Msg: UTF8String): boolean;
    function SanityCheck: boolean;
    procedure ToolProgress(Sender: TObject; OverallProgress, Fileprogress: Integer;
      const Filename: String; out Cancel: boolean);
    procedure UpdateSaveAsFileName;
    procedure AsyncUpdateSaveAsFileName(Data: PtrInt);
    procedure FileSearcherFileFound(FileIterator: TFileIterator);
    function GetFilterList(Const Seperator: UTF8String): UTF8String;
    procedure ToolCompressError(Sender: TObject);
    procedure ToolEncryptionError(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
  end;

var
  ArchiveForm: TArchiveForm;

implementation

{$R *.lfm}

uses
  epimiscutils, epitools_archieve, types, LazFileUtils;

type

  { TArchiveFileSearcher }

  TArchiveFileSearcher = class(TFileSearcher)
  private
    FCompressTool: TEpiToolCompressor;
  public
    property CompressTool: TEpiToolCompressor read FCompressTool write FCompressTool;
  end;


{ TArchiveForm }

procedure TArchiveForm.RadioButton1Change(Sender: TObject);
begin
  case (TComponent(Sender).Tag) of
    // All files
    1: begin
         FolderEdit.Enabled      := true;
         FilterChkList.Enabled   := false;
         SingleFileEdit.Enabled  := false;
         SubDirChkBox.Enabled    := true;
       end;

    // Filter files
    2: begin
         FolderEdit.Enabled      := true;
         FilterChkList.Enabled   := true;
         SingleFileEdit.Enabled  := false;
         SubDirChkBox.Enabled    := true;
       end;

    // Single file
    3: begin
         FolderEdit.Enabled      := false;
         FilterChkList.Enabled   := false;
         SingleFileEdit.Enabled  := true;
         SubDirChkBox.Enabled    := false;
       end;
  end;

  UpdateSaveAsFileName;
end;

procedure TArchiveForm.BitBtn1Click(Sender: TObject);
var
  Tool: TEpiToolCompressor;
  FileSearcher: TArchiveFileSearcher;
  S: String;
  Res: Boolean;
begin
  if (not SanityCheck) then
    begin
      ModalResult := mrNone;
      Exit;
    end;

  Tool := TEpiToolCompressor.Create;

  if (RadioButton1.Checked or RadioButton2.Checked) then
    begin
      Tool.RootDir := FolderEdit.Text;

      FileSearcher := TArchiveFileSearcher.Create;
      FileSearcher.CompressTool := Tool;
      FileSearcher.OnFileFound := @FileSearcherFileFound;

      if RadioButton1.Checked then
        S := '*.*'
      else
        S := GetFilterList(FileSearcher.MaskSeparator);

      FileSearcher.Search(Tool.RootDir, S, SubDirChkBox.Checked);
    end;

  if (RadioButton3.Checked) then
    begin
      Tool.RootDir := ExtractFilePath(SingleFileEdit.Text);
      Tool.Files.Add(SingleFileEdit.Text);
    end;

  if (EncryptCheckBox.Checked) then
    Tool.Password := PasswordEdit.Text;

  ProgressBar1.Max     := Tool.Files.Count;
  ProgressBar1.Visible := true;
  ProgressBar2.Visible := true;
  Label7.Visible       := true;
  Label8.Visible       := true;
  LastUpdate           := 0;

  Tool.OnCompressError := @ToolCompressError;
  Tool.OnEncryptionError := @ToolEncryptionError;
  Tool.OnProgress := @ToolProgress;

  Screen.Cursor := crHourGlass;
  Application.ProcessMessages;
  Res := Tool.CompressToFile(SaveAsEdit.FileName);
  Tool.Free;
  Screen.Cursor := crDefault;

  if (not Res) then
    begin
      ShowMessage('Compression was canceled!');
      ModalResult := mrNone;
    end
  else
    begin
      ShowMessage('Successfull created archive: ' + LineEnding +
                  SaveAsEdit.FileName);
    end;

  ProgressBar1.Visible := false;
  ProgressBar2.Visible := false;
  Label7.Visible       := false;
  Label8.Visible       := false;
end;

procedure TArchiveForm.EncryptCheckBoxChange(Sender: TObject);
begin
  PasswordEdit.Enabled       := EncryptCheckBox.Checked;
  RepeatPasswordEdit.Enabled := EncryptCheckBox.Checked;
  Label5.Enabled             := EncryptCheckBox.Checked;
  Label6.Enabled             := EncryptCheckBox.Checked;

  if (EncryptCheckBox.Checked) then
    begin
      if (PasswordEdit.CanFocus) then
        PasswordEdit.SetFocus;

      UpdateSaveAsFileName;
    end;
end;

procedure TArchiveForm.FolderEditAcceptDirectory(Sender: TObject;
  var Value: String);
begin
  Application.QueueAsyncCall(@AsyncUpdateSaveAsFileName, 0);
end;

procedure TArchiveForm.SingleFileEditAcceptFileName(Sender: TObject;
  var Value: String);
begin
  Application.QueueAsyncCall(@AsyncUpdateSaveAsFileName, 0);
end;

procedure TArchiveForm.FolderEditEditingDone(Sender: TObject);
begin
  UpdateSaveAsFileName;
end;

function TArchiveForm.ShowError(Ctrl: TControl; const Msg: UTF8String): boolean;
var
  R: TRect;
  P: TPoint;
begin
  R := FHintWindow.CalcHintRect(0, Msg, nil);
  P := Ctrl.ClientToScreen(Point(0, Ctrl.Height + 2));
  OffsetRect(R, P.X, P.Y);
  FHintWindow.ActivateHint(R, Msg);

  Result := false;
end;

function TArchiveForm.SanityCheck: boolean;
var
  i: Integer;
begin
  result := false;

  // Folder check
  if (RadioButton1.Checked) or (RadioButton2.Checked) then
    begin
      if (FolderEdit.Directory = '') then
        Exit(ShowError(FolderEdit, 'Folder cannot be empty!'));

      if (not DirectoryExistsUTF8(FolderEdit.Directory)) then
        Exit(ShowError(FolderEdit, 'Folder does not exist!'));
    end;

  // Filter Check
  if RadioButton2.Checked then
    begin
      i := 0;
      while i < FilterChkList.Count do
        if FilterChkList.Checked[PostInc(i)] then break;

      if (i = FilterChkList.Count) then
        Exit(ShowError(FilterChkList, 'No filters selected!'));
    end;

  // Single File
  if RadioButton3.Checked then
    begin
      if (SingleFileEdit.FileName = '') then
        Exit(ShowError(SingleFileEdit, 'No file selected!'));

      if (not FileExistsUTF8(SingleFileEdit.FileName)) then
        Exit(ShowError(SingleFileEdit, 'File does not exist!'));
    end;

  // Save as??
  if (SaveAsEdit.FileName = '') then
    Exit(ShowError(SaveAsEdit, 'No file selected!'));

  // Encryptions
  if EncryptCheckBox.Checked then
    begin
      if (PasswordEdit.Text = '') then
        Exit(ShowError(PasswordEdit, 'No password entered'));

      if (PasswordEdit.Text <> RepeatPasswordEdit.Text) then
        Exit(ShowError(RepeatPasswordEdit, 'Passwords do not match!'));
    end;

  result := true;
end;

procedure TArchiveForm.ToolProgress(Sender: TObject; OverallProgress,
  Fileprogress: Integer; const Filename: String; out Cancel: boolean);
begin
  ProgressBar1.Position := OverallProgress;
  ProgressBar2.Position := Fileprogress;

  if (GetTickCount64 - LastUpdate) > 50 then
    begin
      Application.ProcessMessages;
      LastUpdate := GetTickCount64;
    end;

  Cancel := (ModalResult = mrCancel);
end;

procedure TArchiveForm.UpdateSaveAsFileName;
var
  S: String;
begin
  S := '';

  // Folder compression
  if ((RadioButton1.Checked) or (RadioButton2.Checked)) and
     (FolderEdit.Text <> '')
  then
    begin
      S := FolderEdit.Text;
      if S[Length(S)] = DirectorySeparator then
        Delete(S, Length(S), 1);
      S := S + '_' + FormatDateTime('YYYY-MM-DD', Now);
    end;

  if (RadioButton3.Checked) and
     (SingleFileEdit.Text <> '')
  then
    begin
      S := SingleFileEdit.Text;
      if ExtractFileExt(S) <> '' then
        S := ChangeFileExt(S, '');
    end;

  if S <> '' then
    begin
      if EncryptCheckBox.Checked then
        S := S + '.zky'
      else
        S := S + '.zip';
    end;

  SaveAsEdit.Text := S;
end;

procedure TArchiveForm.AsyncUpdateSaveAsFileName(Data: PtrInt);
begin
  UpdateSaveAsFileName;
end;

procedure TArchiveForm.FileSearcherFileFound(FileIterator: TFileIterator);
begin
  TArchiveFileSearcher(FileIterator).CompressTool.Files.Add(FileIterator.FileName);
end;

function TArchiveForm.GetFilterList(const Seperator: UTF8String): UTF8String;
var
  i: Integer;
begin
  Result := '';

  for i := 0 to FilterChkList.Count - 1 do
    begin
      if FilterChkList.Checked[i] then
        result := result + Seperator + GetEpiDialogFilterExt([TEpiDialogFilter(PtrInt(FilterChkList.Items.Objects[i]))]);
    end;

  Delete(Result, 1, 1);
end;

procedure TArchiveForm.ToolCompressError(Sender: TObject);
begin

end;

procedure TArchiveForm.ToolEncryptionError(Sender: TObject);
begin

end;

constructor TArchiveForm.Create(TheOwner: TComponent);
var
  Item: TEpiDialogFilter;
  Items: TEpiDialogFilters;
begin
  inherited Create(TheOwner);

  Items := dfAllFilters - [dfCollection, dfAll];
  for Item in Items do
    FilterChkList.AddItem(GetEpiDialogFilterName([Item]), TObject(PtrInt(Item)));

  FilterChkList.AutoSize := true;

  FHintWindow := THintWindow.Create(Self);
  FHintWindow.AutoHide := true;
  FHintWindow.HideInterval := 2000;
end;

end.
