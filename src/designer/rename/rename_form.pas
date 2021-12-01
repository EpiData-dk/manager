unit rename_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, epidatafiles, epicustombase;

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
    procedure CloseQuery(Sender: TObject; var CanClose: boolean);
    procedure ShowForm(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent; Const DataFile: TEpiDataFile);
    class procedure RestoreDefaultPos;
  end;

implementation

{$R *.lfm}

uses
  settings2_var, settings2;

{ TRenameForm }

procedure TRenameForm.CheckBox1Change(Sender: TObject);
begin
  case TCheckBox(Sender).Tag of
    1: Edit1.Enabled := TCheckBox(Sender).Checked;
    2: Edit2.Enabled := TCheckBox(Sender).Checked;
    3: Edit3.Enabled := TCheckBox(Sender).Checked;
  end;
end;

procedure TRenameForm.CloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if ManagerSettings.SaveWindowPositions then
    SaveFormPosition(Self, Self.ClassName);
end;

procedure TRenameForm.ShowForm(Sender: TObject);
begin
  if ManagerSettings.SaveWindowPositions then
    LoadFormPosition(Self, Self.ClassName);
end;

procedure TRenameForm.BitBtn1Click(Sender: TObject);
var
  i: Integer;
  S: String;
  CI: TEpiCustomControlItem;
  F: TEpiField;
begin
  if (not CheckBox1.Checked) and
     (not CheckBox2.Checked) and
     (not CheckBox3.Checked)
  then
  begin
    ShowMessage('No items selected for renaming!');
    Exit;
  end;

  S := '';
  if CheckBox1.Checked then
    S += '/Variables';
  if CheckBox2.Checked then
    S += '/Sections';
  if CheckBox3.Checked then
    S += '/Headings';
  Delete(S, 1, 1);

  case
    MessageDlg('Warning!',
      'Sequential renaming of all:' + LineEnding +
      S + LineEnding +
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

  // First rename all controls selected for renaming
  // - this is if two different types (eg. field and section)
  //   is going to have a name-clash if rename seperately.
  for i := 0 to FDataFile.ControlItems.Count - 1 do
  begin
    CI := FDataFile.ControlItem[i];

    // Do not rename main section
    if CI = FDataFile.MainSection then continue;

    // Do not rename key fields
    if FDataFile.KeyFields.IndexOf(CI) >= 0 then Continue;

    if CI.InheritsFrom(TEpiField) and CheckBox1.Checked then
      CI.Name := '@RenameF' + IntToStr(i);

    if CI.InheritsFrom(TEpiSection) and CheckBox2.Checked then
      CI.Name := '@RenameS' + IntToStr(i);

    if CI.InheritsFrom(TEpiHeading) and CheckBox3.Checked then
      CI.Name := '@RenameH' + IntToStr(i);
  end;

  // Rename controls sequentially. One by one...
  if CheckBox1.Checked then
  begin
    for i := 0 to FDataFile.Fields.Count -1 do
    begin
      F := FDataFile.Fields[i];
      if FDataFile.KeyFields.IndexOf(F) >= 0 then Continue;

      F.Name := Edit1.Text + IntToStr(i + 1);
    end;
  end;

  if CheckBox2.Checked then
  begin
    for i := 0 to FDataFile.Sections.Count -1 do
      if FDataFile.Section[i] = FDataFile.MainSection then
        Continue
      else
        FDataFile.Section[i].Name := Edit2.Text + IntToStr(i + 1);
  end;

  if CheckBox3.Checked then
  begin
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

  OnShow := @ShowForm;
  OnCloseQuery := @CloseQuery;
end;

class procedure TRenameForm.RestoreDefaultPos;
var
  F: TForm;
begin
  F := TForm.Create(nil);
  with F do
  begin
    Width := 300;
    Height := 230;
    Left := 400;
    Top := 400;
  end;
  SaveFormPosition(F, TRenameForm.ClassName);
  F.Free;
end;

end.

