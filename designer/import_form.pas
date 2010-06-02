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
  FDataFile := nil;
end;

end.

