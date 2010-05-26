unit import_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  EditBtn, StdCtrls, Buttons, epidatafiles, epidatafilestypes;

type

  { TImportForm }

  TImportForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    ImportValueLabelsChkBox: TCheckBox;
    ImportDataChkBox: TCheckBox;
    ImportFileEdit: TFileNameEdit;
    GroupBox1: TGroupBox;
    ClearDataFormChkBox: TCheckBox;
    Label1: TLabel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure ImportFileEditAcceptFileName(Sender: TObject; var Value: String);
    procedure ImportFileEditEditingDone(Sender: TObject);
  private
    { private declarations }
    FDataFile: TEpiDataFile;
    procedure SetDataFile(const AValue: TEpiDataFile);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    property    DataFile: TEpiDataFile read FDataFile write SetDataFile;
  end; 

implementation

{$R *.lfm}

uses
  epiimport;

{ TImportForm }

procedure TImportForm.ImportFileEditAcceptFileName(Sender: TObject;
  var Value: String);
var
  Ext: String;
begin
  Ext := ExtractFileExt(Value);
end;

procedure TImportForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  Importer: TEpiImport;
  ImpDf: TEpiDataFile;
  i: Integer;
begin
  if ModalResult <> mrOK then exit;

  Importer := TEpiImport.Create;
  ImpDf := nil;
  Importer.ImportRec(ImportFileEdit.FileName, ImpDf);

  if not (ImportValueLabelsChkBox.Checked) then
    for i := ImpDf.ValueLabels.Count - 1 downto 0 do
      ImpDf.ValueLabels.DeleteItem(i).Free;

  if not (ImportDataChkBox.Checked) then
    ImpDf.Size := 0;
end;

procedure TImportForm.ImportFileEditEditingDone(Sender: TObject);
var
  Fn: String;
begin
  Fn := ImportFileEdit.FileName;
  ImportFileEditAcceptFileName(Sender, Fn);
end;

procedure TImportForm.SetDataFile(const AValue: TEpiDataFile);
begin
  FDataFile := AValue;
end;

constructor TImportForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

end.

