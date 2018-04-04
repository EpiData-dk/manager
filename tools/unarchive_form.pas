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
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Edit1: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    CheckBox3: TCheckBox;
    DirectoryEdit1: TDirectoryEdit;
    ButtonPanel1: TButtonPanel;
    procedure OKButtonClick(Sender: TObject);
  private

  public

  end;

var
  UnArchiveForm: TUnArchiveForm;

implementation

{$R *.lfm}

uses
  epitools_archieve;

{ TUnArchiveForm }

procedure TUnArchiveForm.OKButtonClick(Sender: TObject);
var
  Tool: TEpiToolDeCompressor;
begin
  //
  Tool := TEpiToolDeCompressor.Create;
  Tool.DestinationDir := DirectoryEdit1.Directory;
  Tool.Password := Edit1.Text;

  if CheckBox2.Checked then
    Tool.DecompressFromFile(FileNameEdit1.FileName);

  if CheckBox1.Checked and (not CheckBox2.Checked) then
    Tool.DecryptFromFile(FileNameEdit1.FileName);

  Tool.Free;
end;

end.

