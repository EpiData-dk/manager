unit prepare_double_entry_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, EditBtn, epidocument;

type

  { TPrepareDoubleEntryForm }

  TPrepareDoubleEntryForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    FileNameEdit: TFileNameEdit;
    FileNameLabel: TLabel;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Panel2: TPanel;
    Panel1: TPanel;
    TitleEdit: TEdit;
    TitleLabel: TLabel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
  private
    { private declarations }
    FDoc: TEpiDocument;
    FFileName: string;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; Const Doc: TEpiDocument;
      Const FileName: String);
  end;

procedure PrepareDoubleEntry(Const Doc: TEpiDocument; Const FileName: string);

implementation

{$R *.lfm}

{ TPrepareDoubleEntryForm }

procedure TPrepareDoubleEntryForm.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
var
  NewDoc: TEpiDocument;
  i: Integer;
  Res: Integer;
  FN: TCaption;
begin
  if ModalResult  = mrCancel then exit;

  FN := FileNameEdit.Text;
  if FileExistsUTF8(FN) then
  begin
    case MessageDlg('Warning:',
      'Warning: File exists. Do you want to overwrite?',
      mtWarning,
      mbYesNoCancel,
      0,
      mbCancel) of

      mrCancel:
        begin
          CanClose := false;
          Exit;
        end;
      mrNo:
        Exit;
    end;
  end;

  NewDoc := nil;
  try
    NewDoc := TEpiDocument(FDoc.Clone);
    if TitleEdit.Text <> '' then
      NewDoc.Study.Title.Text := TitleEdit.Text;

    for i := 0 to NewDoc.DataFiles.Count - 1 do
      NewDoc.DataFiles[i].Size := 0;

    NewDoc.SaveToFile(FN);
  except
    MessageDlg('Error Saving File!',
      'This file ' + FN + ' could not be save.',
      mtError,
      [mbAbort], 0);
    CanClose := false;
    NewDoc.Free;
    Exit;
  end;
  ShowMessage('Double Entry file save successfully:' + LineEnding +
    FN);
  NewDoc.Free;
end;

constructor TPrepareDoubleEntryForm.Create(TheOwner: TComponent; const Doc: TEpiDocument;
  const FileName: String);
begin
  inherited Create(TheOwner);
  FDoc := Doc;
  FFileName := FileName;

  FileNameLabel.Caption := FFileName;
  TitleLabel.Caption := FDoc.Study.Title.Text;

  FileNameEdit.Text := ChangeFileExt(FFileName, '.double.epx');
  TitleEdit.Text    := TitleLabel.Caption + ' (double entry file)';
end;

procedure PrepareDoubleEntry(const Doc: TEpiDocument; const FileName: string);
var
  F: TPrepareDoubleEntryForm;
begin
  F := TPrepareDoubleEntryForm.Create(nil, Doc, FileName);
  F.ShowModal;
end;

end.

