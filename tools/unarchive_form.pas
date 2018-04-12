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
  private
    FProgressForm: TArchiveProgressForm;
    FErrorHandled: boolean;
    FHintWindow: THintWindow;
    function ShowError(Ctrl: TControl; Const Msg: UTF8String): boolean;
    function SanityCheck: boolean;
    procedure DecompressError(Sender: TObject; const Msg: String);
    procedure ExtractProgress(Sender: TObject; FileNo, FileProgress: Integer;
      const Filename: String; out Cancel: boolean);
    procedure ToolTotalFileCount(Sender: TObject; TotalCount: Integer);
    procedure DestinationFilenameChange;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

var
  UnArchiveForm: TUnArchiveForm;

implementation

{$R *.lfm}

uses
  epitools_archieve, LazFileUtils, types;

{ TUnArchiveForm }

procedure TUnArchiveForm.OKButtonClick(Sender: TObject);
var
  Tool: TEpiToolDeCompressor;
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

    try
      if (not Tool.DecompressFromFile(InputFileNameEdit.FileName)) then
        begin
          if (not FErrorHandled) then
            ShowMessage('Decompression was canceled!');
          ModalResult := mrNone;
        end
      else
        begin
          ShowMessage('Successfull extracted archive!');
        end;
    finally
      FProgressForm.Free;
    end;
  end;

  if DecryptChkBox.Checked and (not UnzipChkBox.Checked) then
    if Tool.DecryptFromFile(InputFileNameEdit.FileName, OutputFileNameEdit.FileName) then
      begin
        ShowMessage('Decryption failed!');
        ModalResult := mrNone;
      end
    else
      begin
        ShowMessage('Decryption completed');
      end;

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

procedure TUnArchiveForm.ExtractProgress(Sender: TObject; FileNo,
  FileProgress: Integer; const Filename: String; out Cancel: boolean);
begin
  //
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
end;

constructor TUnArchiveForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FHintWindow := THintWindow.Create(Self);
  FHintWindow.AutoHide := true;
  FHintWindow.HideInterval := 2000;
end;

end.

