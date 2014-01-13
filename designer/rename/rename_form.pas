unit rename_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, epidatafiles;

type

  { TRenameForm }

  TRenameForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Label4: TLabel;
    Panel1: TPanel;
    Shape1: TShape;
    procedure BitBtn1Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
  private
    FDataFile: TEpiDataFile;
  public
    constructor Create(TheOwner: TComponent; Const DataFile: TEpiDataFile);
  end;

implementation

{$R *.lfm}

{ TRenameForm }

procedure TRenameForm.CheckBox1Change(Sender: TObject);
begin
  case TCheckBox(Sender).Tag of
    1: Edit1.Enabled := TCheckBox(Sender).Checked;
    2: Edit2.Enabled := TCheckBox(Sender).Checked;
    3: Edit3.Enabled := TCheckBox(Sender).Checked;
  end;
end;

procedure TRenameForm.BitBtn1Click(Sender: TObject);
var
  i: Integer;
begin
  if (not CheckBox1.Checked) and
     (not CheckBox2.Checked) and
     (not CheckBox3.Checked)
  then
  begin
    ShowMessage('No items selected for renaming!');
    Exit;
  end;

  case
    MessageDlg('Warning!',
      'This will rename all selected items!' + LineEnding +
      'This cannot be undone!' + LineEnding +
      LineEnding +
      'Do you wish to continue?',
      mtWarning,
      mbYesNoCancel,
      0,
      mbCancel
    )
  of
   mrNo: Exit;
   mrCancel:
     begin
       ModalResult := mrNone;
       Exit;
     end;
  end;

  FDataFile.BeginUpdate;

  if CheckBox1.Checked then
  begin
    for i := 0 to FDataFile.Fields.Count -1 do
      FDataFile.Field[i].Name := '@rename' + IntToStr(i);
    for i := 0 to FDataFile.Fields.Count -1 do
      FDataFile.Field[i].Name := Edit1.Text + IntToStr(i + 1);
  end;

  if CheckBox2.Checked then
  begin
    for i := 0 to FDataFile.Sections.Count -1 do
      FDataFile.Section[i].Name := '@rename' + IntToStr(i);
    for i := 0 to FDataFile.Sections.Count -1 do
      FDataFile.Section[i].Name := Edit2.Text + IntToStr(i + 1);
  end;

  if CheckBox3.Checked then
  begin
    for i := 0 to FDataFile.Headings.Count -1 do
      FDataFile.Heading[i].Name := '@rename' + IntToStr(i);
    for i := 0 to FDataFile.Headings.Count -1 do
      FDataFile.Heading[i].Name := Edit3.Text + IntToStr(i + 1);
  end;
  FDataFile.EndUpdate;
end;

constructor TRenameForm.Create(TheOwner: TComponent;
  const DataFile: TEpiDataFile);
begin
  inherited Create(TheOwner);
  FDataFile := DataFile;
end;

end.

