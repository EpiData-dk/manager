unit unarchive_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, EditBtn,
  ButtonPanel, ComCtrls, archive_progressform;

type

  { TUnArchiveForm }

  TUnArchiveForm = class(TForm)
    Label1: TLabel;
    InputFileNameEdit: TFileNameEdit;
    DecryptChkBox: TCheckBox;
    UnzipChkBox: TCheckBox;
    PasswordEdit: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    ReplaceChkBox: TCheckBox;
    DestinationFolderEdit: TDirectoryEdit;
    ButtonPanel1: TButtonPanel;
    OutputFileNameEdit: TFileNameEdit;
    procedure OKButtonClick(Sender: TObject);
    procedure DecryptChkBoxChange(Sender: TObject);
    procedure UnzipChkBoxChange(Sender: TObject);
    procedure InputFileNameEditEditingDone(Sender: TObject);
    procedure InputFileNameEditAcceptFileName(Sender: TObject; var Value: String
      );
  private
    FProgressForm: TArchiveProgressForm;
    FErrorHandled: boolean;
    FHintWindow: THintWindow;
    function ShowError(Ctrl: TControl; Const Msg: UTF8String): boolean;
    function SanityCheck: boolean;
    procedure DecompressError(Sender: TObject; const Msg: String);
    procedure ToolTotalFileCount(Sender: TObject; TotalCount: Integer);
    procedure DestinationFilenameChange;
    procedure UpdateDestinationFilename(Data: PtrInt = 0);
  public
    constructor Create(TheOwner: TComponent); override;
  end;

var
  UnArchiveForm: TUnArchiveForm;

implementation

{$R *.lfm}

uses
  epitools_archieve, LazFileUtils, types, Clipbrd;

{ TUnArchiveForm }

procedure TUnArchiveForm.OKButtonClick(Sender: TObject);
var
  Tool: TEpiToolDeCompressor;
  S: String;
begin
  if (not SanityCheck) then
    begin
      ModalResult := mrNone;
      Exit;
    end;

  FErrorHandled := false;
  Tool := TEpiToolDeCompressor.Create;
  Tool.DestinationDir := DestinationFolderEdit.Directory;
  Tool.Password := PasswordEdit.Text;
  Tool.OnDecompressionError := @DecompressError;
  Tool.OnDecryptionError    := @DecompressError;

  if UnzipChkBox.Checked then
  begin
    FProgressForm := TArchiveProgressForm.Create(Self);
    Tool.OnProgress           := @FProgressForm.Progress;
    Tool.OnTotalFileCount     := @ToolTotalFileCount;
    Tool.ReplaceFiles         := ReplaceChkBox.Checked;

    try
      if (not Tool.DecompressFromFile(InputFileNameEdit.FileName)) then
        begin
          if (not FErrorHandled) then
            ShowMessage('Decompression was canceled!');
          ModalResult := mrNone;
        end
      else
        begin
          S := 'Successfull extracted archive!' + LineEnding +
               LineEnding +
               'Source: ' + InputFileNameEdit.FileName + LineEnding +
               'Destination: ' + DestinationFolderEdit.Directory + LineEnding +
               '(files: ' + IntToStr(Tool.ExtractedFiles + Tool.SkippedFiles) + ')';

          if (not ReplaceChkBox.Checked) then
            S := S + LineEnding + '(skipped: ' + IntToStr(Tool.SkippedFiles) + ')';

          ShowMessage(S);

        end;
    finally
      FProgressForm.Free;
    end;
  end;

  if DecryptChkBox.Checked and (not UnzipChkBox.Checked) then
    if (not Tool.DecryptFromFile(InputFileNameEdit.FileName, OutputFileNameEdit.FileName)) then
      begin
        if (not FErrorHandled) then
          ShowMessage('Decryption failed!');
        ModalResult := mrNone;
      end
    else
      begin
        S := 'Decryption completed!' + LineEnding +
             LineEnding +
             'Source: ' + InputFileNameEdit.FileName + LineEnding +
             'Destination: ' + OutputFileNameEdit.FileName;

        ShowMessage(S);
      end;

  Clipboard.AsText := S;

  Tool.Free;
end;

procedure TUnArchiveForm.DecryptChkBoxChange(Sender: TObject);
begin
  PasswordEdit.Enabled := DecryptChkBox.Checked;
  DestinationFilenameChange;
end;

procedure TUnArchiveForm.UnzipChkBoxChange(Sender: TObject);
begin
  DestinationFilenameChange;
end;

procedure TUnArchiveForm.InputFileNameEditEditingDone(Sender: TObject);
begin
  UpdateDestinationFilename;
end;

procedure TUnArchiveForm.InputFileNameEditAcceptFileName(Sender: TObject;
  var Value: String);
begin
  Application.QueueAsyncCall(@UpdateDestinationFilename, 0);
end;

function TUnArchiveForm.ShowError(Ctrl: TControl; const Msg: UTF8String
  ): boolean;
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

function TUnArchiveForm.SanityCheck: boolean;
begin
  if (InputFileNameEdit.FileName = '') then
    Exit(ShowError(InputFileNameEdit, 'No archive selected!'));

  if (not FileExistsUTF8(InputFileNameEdit.FileName)) then
    Exit(ShowError(InputFileNameEdit, 'File does not exist!'));

  if (PasswordEdit.Enabled) and
     (PasswordEdit.Text = '')
  then
    Exit(ShowError(PasswordEdit, 'Empty passwords not allowed!'));

  if (UnzipChkBox.Checked) then
    begin
      if (DestinationFolderEdit.Directory = '') then
        Exit(ShowError(DestinationFolderEdit, 'No destination directory selected!'));

      if (not DirectoryExistsUTF8(DestinationFolderEdit.Directory)) then
        Exit(ShowError(DestinationFolderEdit, 'Destination directory does not exist!'));
    end;

  if (OutputFileNameEdit.Enabled) then
    begin
      if (OutputFileNameEdit.FileName = '') then
        Exit(ShowError(OutputFileNameEdit, 'No filename specified!'));
    end;
end;

procedure TUnArchiveForm.DecompressError(Sender: TObject; const Msg: String);
begin
  ShowMessage(Msg);
  FErrorHandled := true;
end;

procedure TUnArchiveForm.ToolTotalFileCount(Sender: TObject; TotalCount: Integer
  );
begin
  FProgressForm.MaxFileCount := TotalCount;
  FProgressForm.Show;
end;

procedure TUnArchiveForm.DestinationFilenameChange;
begin
  Label3.Enabled                := UnzipChkBox.Checked;
  DestinationFolderEdit.Visible := UnzipChkBox.Checked or (not DecryptChkBox.Checked);
  DestinationFolderEdit.Enabled := UnzipChkBox.Checked;
  ReplaceChkBox.Visible         := UnzipChkBox.Checked or (not DecryptChkBox.Checked);
  ReplaceChkBox.Enabled         := UnzipChkBox.Checked;

  OutputFileNameEdit.Visible    := DecryptChkBox.Checked and (not UnzipChkBox.Checked);
  OutputFileNameEdit.Enabled    := DecryptChkBox.Checked and (not UnzipChkBox.Checked);

  ButtonPanel1.OKButton.Enabled := DecryptChkBox.Checked or UnzipChkBox.Checked;

  UpdateDestinationFilename;
end;

procedure TUnArchiveForm.UpdateDestinationFilename(Data: PtrInt);
begin
  if (InputFileNameEdit.FileName = '') then
    Exit;

  if DestinationFolderEdit.Enabled then
    begin
      DestinationFolderEdit.Directory := ExtractFilePath(InputFileNameEdit.FileName);
    end;

  if OutputFileNameEdit.Enabled then
    begin
      OutputFileNameEdit.FileName := ChangeFileExt(InputFileNameEdit.FileName, '.zip');
    end;
end;

constructor TUnArchiveForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  ButtonPanel1.OKButton.OnClick := @OKButtonClick;

  FHintWindow := THintWindow.Create(Self);
  FHintWindow.AutoHide := true;
  FHintWindow.HideInterval := 2000;
end;

end.

