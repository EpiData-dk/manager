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
    FileNameEdit1: TFileNameEdit;
    DecryptChkBox: TCheckBox;
    UnzipChkBox: TCheckBox;
    PasswordEdit: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    ReplaceChkBox: TCheckBox;
    DestinationFolderEdit: TDirectoryEdit;
    ButtonPanel1: TButtonPanel;
    procedure OKButtonClick(Sender: TObject);
    procedure DecryptChkBoxChange(Sender: TObject);
    procedure UnzipChkBoxChange(Sender: TObject);
  private
    FProgressForm: TArchiveProgressForm;
    FHintWindow: THintWindow;
    function ShowError(Ctrl: TControl; Const Msg: UTF8String): boolean;
    function SanityCheck: boolean;
    procedure DecompressError(Sender: TObject; const Msg: String);
    procedure ExtractProgress(Sender: TObject; FileNo, FileProgress: Integer;
      const Filename: String; out Cancel: boolean);
    procedure ToolTotalFileCount(Sender: TObject; TotalCount: Integer);
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
      if (not Tool.DecompressFromFile(FileNameEdit1.FileName)) then
        begin
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
    if Tool.DecryptFromFile(FileNameEdit1.FileName, '') then
      begin

      end
    else
      begin

      end;

  Tool.Free;
end;

procedure TUnArchiveForm.DecryptChkBoxChange(Sender: TObject);
begin
  PasswordEdit.Enabled := DecryptChkBox.Checked;
end;

procedure TUnArchiveForm.UnzipChkBoxChange(Sender: TObject);
begin
  Label3.Enabled                := UnzipChkBox.Checked;
  DestinationFolderEdit.Enabled := UnzipChkBox.Checked;
  ReplaceChkBox.Enabled         := UnzipChkBox.Checked;
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
  if (FileNameEdit1.FileName = '') then
    Exit(ShowError(FileNameEdit1, 'No archive selected!'));

  if (FileExistsUTF8(FileNameEdit1.FileName)) then
    Exit(ShowError(FileNameEdit1, 'File does not exist!'));


  if (UnzipChkBox.Checked) then
    begin
      if (DestinationFolderEdit.Directory = '') then
        Exit(ShowError(DestinationFolderEdit, 'No destination directory selected!'));

      if (DirectoryExistsUTF8(DestinationFolderEdit.Directory)) then
        Exit(ShowError(DestinationFolderEdit, 'Destination directory does not exist!'));
    end;
end;

procedure TUnArchiveForm.DecompressError(Sender: TObject; const Msg: String);
begin
  //
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

constructor TUnArchiveForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FHintWindow := THintWindow.Create(Self);
  FHintWindow.AutoHide := true;
  FHintWindow.HideInterval := 2000;
end;

end.

