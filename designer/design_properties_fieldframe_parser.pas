unit design_properties_fieldframe_parser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epi_script_AST, epidatafiles, contnrs, epi_parser_types,
  epidatafilestypes;

type

  { TScriptParser }

  TScriptParser = class(TObject, IEpiScriptParser, IEpiScriptExecutor)
  private
    FOnError: TExecutorError;
    FVariables: TFPObjectHashTable;
    FDataFile: TEpiDataFile;
  public
    constructor Create(Const DataFile: TEpiDataFile);
    procedure ParseError(Const Msg: string; Const LineNo, ColNo: integer;
      Const TextFound: string);
    function  VariableExists(Const Ident: string): boolean;
    procedure AddVariable(Const Variable: TCustomVariable);
    function  FindVariable(Const Ident: string): TCustomVariable;
    function  RecordIndex: Integer;
    property OnError: TExecutorError read FOnError write FOnError;
    procedure SetFieldValue(Const Sender: TObject; Const F: TEpiField; Const Value: Variant);
    function GetFieldValue(Const Sender: TObject; Const F: TEpiField): Variant;
  end;

implementation

{ TScriptParser }

constructor TScriptParser.Create(const DataFile: TEpiDataFile);
var
  F: TEpiField;
  i: Integer;
begin
  FVariables := TFPObjectHashTable.Create(False);
  FDataFile := DataFile;

  // Assigned a TFieldVariable for all Fields;
  for i := 0 to FDataFile.Fields.Count - 1 do
  begin
    F := FDataFile.Field[i];
    AddVariable(TFieldVariable.Create(F, Self));
  end;
end;

procedure TScriptParser.ParseError(const Msg: string; const LineNo,
  ColNo: integer; const TextFound: string);
begin
  if Assigned(OnError) then
    FOnError(Msg, LineNo, ColNo, TextFound);
end;

function TScriptParser.VariableExists(const Ident: string): boolean;
begin
  result := Assigned(FindVariable(Ident));
end;

procedure TScriptParser.AddVariable(const Variable: TCustomVariable);
begin
  FVariables.Add(Variable.Ident, Variable);
end;

function TScriptParser.FindVariable(const Ident: string): TCustomVariable;
begin
  Result := TCustomVariable(FVariables.Items[Ident]);
end;

function TScriptParser.RecordIndex: Integer;
begin
  result := 0;
end;

procedure TScriptParser.SetFieldValue(const Sender: TObject;
  const F: TEpiField; const Value: Variant);
begin

end;

function TScriptParser.GetFieldValue(const Sender: TObject; const F: TEpiField
  ): Variant;
begin

end;

end.

