unit unarchive_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, EditBtn,
  ButtonPanel, ComCtrls;

type

  { TUnArchiveForm }

  TUnArchiveForm = class(TForm)
    Label1: TLabel;
    FileNameEdit1: TFileNameEdit;
    DecryptChkBox: TCheckBox;
    UnzipChkBox: TCheckBox;
    Edit1: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    CheckBox3: TCheckBox;
    DirectoryEdit1: TDirectoryEdit;
    ButtonPanel1: TButtonPanel;
    procedure OKButtonClick(Sender: TObject);
  private
    FHintWindow: THintWindow;
    function ShowError(Ctrl: TControl; Const Msg: UTF8String): boolean;
    function SanityCheck: boolean;
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
  //
  Tool := TEpiToolDeCompressor.Create;
  Tool.DestinationDir := DirectoryEdit1.Directory;
  Tool.Password := Edit1.Text;

  if UnzipChkBox.Checked then
    Tool.DecompressFromFile(FileNameEdit1.FileName);

  if DecryptChkBox.Checked and (not UnzipChkBox.Checked) then
    Tool.DecryptFromFile(FileNameEdit1.FileName, '');

  Tool.Free;
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
      if (DirectoryEdit1.Directory = '') then
        Exit(ShowError(DirectoryEdit1, 'No destination directory selected!'));

      if (DirectoryExistsUTF8(DirectoryEdit1.Directory)) then
        Exit(ShowError(DirectoryEdit1, 'Destination directory does not exist!'));
    end;
end;

constructor TUnArchiveForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FHintWindow := THintWindow.Create(Self);
  FHintWindow.AutoHide := true;
  FHintWindow.HideInterval := 2000;
end;

end.

