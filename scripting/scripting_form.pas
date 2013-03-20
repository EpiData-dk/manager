unit scripting_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, Buttons, epidatafiles, typetable;

type

  { TScriptingForm }

  TScriptingForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    ComboBox1: TComboBox;
    PageControl1: TPageControl;
    Panel1: TPanel;
    BeforeRecSynEdit: TSynEdit;
    BeforeRecTabSheet: TTabSheet;
    BeforeFieldSynEdit: TSynEdit;
    AfterFieldSynEdit: TSynEdit;
    AfterRecSynEdit: TSynEdit;
    Panel2: TPanel;
    Panel3: TPanel;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure BitBtn1Click(Sender: TObject);
    procedure ComboBox1Select(Sender: TObject);
  private
    FGlobalSymbolTable: TTypeTable;
    FDataFile: TEpiDataFile;
    FCurrentField: TEpiField;
    procedure SetDataFile(AValue: TEpiDataFile);
    { private declarations }
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    property DataFile: TEpiDataFile read FDataFile write SetDataFile;
  end;

implementation

uses
  epiparser, AST;

{$R *.lfm}

{ TScriptingForm }

procedure TScriptingForm.BitBtn1Click(Sender: TObject);
var
  Parser: TEpiParser;
  StatementList: TStatementList;
  Msg: String;
begin
  // Validate script.
  FGlobalSymbolTable.Clear;

  Parser := TEpiParser.Create;
  Parser.DataFile := DataFile;
  Parser.SymbolTable := FGlobalSymbolTable;
  if not Parser.Parse(BeforeRecSynEdit.Lines, StatementList) then
  begin
    ShowMessage('Parsing failed for "Before Record"');
    ModalResult := mrCancel;
    Exit;
  end;
  Parser.Free;
  if not StatementList.TypeCheck(Msg) then
  begin
    ShowMessage('Typechecking failed for "Before Record": ' + Msg);
    ModalResult := mrCancel;
  end;

{  Parser := TEpiParser.Create;
  Parser.DataFile := DataFile;
  Parser.SymbolTable := FGlobalSymbolTable;
  if not Parser.Parse(AfterRecSynEdit.Lines, StatementList) then
  begin
    ShowMessage('Parsing failed for "After Record"');
    ModalResult := mrCancel;
  end;
  Parser.Free;
  if StatementList.TypeCheck(Msg) then
  begin
    ShowMessage('Typechecking failed for "After Record"');
    ModalResult := mrCancel;
  end;        }

  if ModalResult = mrOK then
  begin;
    FDataFile.BeforeRecordScript.Assign(BeforeRecSynEdit.Lines);
    FDataFile.AfterRecordScript.Assign(AfterRecSynEdit.Lines);
  end;
end;

procedure TScriptingForm.ComboBox1Select(Sender: TObject);
var
  F: TEpiField;
begin
  F := TEpiField(ComboBox1.Items.Objects[ComboBox1.ItemIndex]);

  if (F <> FCurrentField) and
     (Assigned(FCurrentField))
  then
  begin
    FCurrentField.BeforeEntryScript.Assign(BeforeFieldSynEdit.Lines);
    FCurrentField.AfterEntryScript.Assign(AfterFieldSynEdit.Lines);
  end;

  BeforeFieldSynEdit.Lines.Assign(F.BeforeEntryScript);
  AfterFieldSynEdit.Lines.Assign(F.AfterEntryScript);
  FCurrentField := F;
end;

procedure TScriptingForm.SetDataFile(AValue: TEpiDataFile);
var
  i: Integer;
begin
  if FDataFile = AValue then Exit;
  FDataFile := AValue;

  BeforeRecSynEdit.Lines.Assign(FDataFile.BeforeRecordScript);
  AfterRecSynEdit.Lines.Assign(FDataFile.AfterRecordScript);

  ComboBox1.Clear;
  ComboBox1.Items.BeginUpdate;
  for i := 0 to FDataFile.Fields.Count - 1 do
    ComboBox1.AddItem(FDataFile.Field[i].Name, FDataFile.Field[i]);
  ComboBox1.Items.EndUpdate;
  if FDataFile.Fields.Count > 0 then
  begin
    ComboBox1.ItemIndex := 0;
    ComboBox1Select(nil);
  end;
end;

constructor TScriptingForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FGlobalSymbolTable := TTypeTable.Create;
end;

end.

