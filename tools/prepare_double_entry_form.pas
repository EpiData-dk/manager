unit prepare_double_entry_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, EditBtn, epidocument, epidatafiles, epiopenfile;

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

procedure PrepareDoubleEntry(Const Doc: TEpiDocumentFile);

implementation

{$R *.lfm}

uses
  epidatafilestypes, epiv_documentfile;

{ TPrepareDoubleEntryForm }

procedure TPrepareDoubleEntryForm.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
var
  NewDoc: TEpiDocument;
  i: Integer;
  Res: Integer;
  FN: TCaption;
  F: TEpiField;
  NewName: String;
  S: TEpiSection;
  j: Integer;
  NewF: TEpiField;
  DocFile: TDocumentFile;
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

  try
    DocFile := TDocumentFile.Create;
    NewDoc  := DocFile.CreateClonedDocument(FDoc);

    if TitleEdit.Text <> '' then
      NewDoc.Study.Title.Text := TitleEdit.Text;

    for i := 0 to NewDoc.DataFiles.Count - 1 do
    with NewDoc.DataFiles[i] do
    begin
      Size := 0;

      // Convert IDNUM fields to Integer!
      for j := 0 to Fields.Count - 1 do
      begin
        if Fields[j].FieldType <> ftAutoInc then continue;

        // TODO -cRuntimeDesigner: Check up on workign after rewriting TEpiCustomBase.Assign.
        F := Fields[j];
        NewF := F.Section.NewField(ftInteger);
        NewF.Assign(F);
        NewName := F.Name;
        F.Free;
        NewF.Name := NewName;
      end;
    end;

    DocFile.SaveFile(FN);
  except
    MessageDlg('Error Saving File!',
      'This file ' + FN + ' could not be save.',
      mtError,
      [mbAbort], 0);
    CanClose := false;
    DocFile.Free;
    Exit;
  end;
  ShowMessage('Double Entry file save successfully:' + LineEnding +
    FN);
  DocFile.Free;
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

procedure PrepareDoubleEntry(const Doc: TEpiDocumentFile);
var
  F: TPrepareDoubleEntryForm;
begin
  F := TPrepareDoubleEntryForm.Create(nil, Doc.Document, Doc.FileName);
  F.ShowModal;
  F.Free;
end;

end.

