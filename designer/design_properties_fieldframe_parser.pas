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
    property OnError: TExecutorError read FOnError write FOnError;
  public
    { IEpiScriptParser }
    procedure ParseError(Const Msg: string; Const LineNo, ColNo: integer; Const TextFound: string);
    function  VariableExists(Const Ident: string): boolean;
    procedure AddVariable(Const Variable: TCustomVariable);
    function  FindVariable(Const Ident: string): TCustomVariable;
    function  CreateFunction(const FunctionName: string; const ParamList: TParamList): TFunction;
  public
    { IEpiScriptExecutor }
    procedure SetFieldValue(Const Sender: TObject; Const F: TEpiField; Const Value: EpiBool); overload;
    procedure SetFieldValue(Const Sender: TObject; Const F: TEpiField; Const Value: EpiInteger); overload;
    procedure SetFieldValue(Const Sender: TObject; Const F: TEpiField; Const Value: EpiFloat); overload;
    procedure SetFieldValue(Const Sender: TObject; Const F: TEpiField; Const Value: EpiString); overload;
    function GetFieldValueBool(Const Sender: TObject; Const F: TEpiField): EpiBool;
    function GetFieldValueInt(Const Sender: TObject; Const F: TEpiField): EpiInteger;
    function GetFieldValueFloat(Const Sender: TObject; Const F: TEpiField): EpiFloat;
    function GetFieldValueString(Const Sender: TObject; Const F: TEpiField): EpiString;
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

function TScriptParser.CreateFunction(const FunctionName: string;
  const ParamList: TParamList): TFunction;
begin
  result := nil;
end;

procedure TScriptParser.SetFieldValue(const Sender: TObject;
  const F: TEpiField; const Value: EpiBool);
begin

end;

procedure TScriptParser.SetFieldValue(const Sender: TObject;
  const F: TEpiField; const Value: EpiInteger);
begin

end;

procedure TScriptParser.SetFieldValue(const Sender: TObject;
  const F: TEpiField; const Value: EpiFloat);
begin

end;

procedure TScriptParser.SetFieldValue(const Sender: TObject;
  const F: TEpiField; const Value: EpiString);
begin

end;

function TScriptParser.GetFieldValueBool(const Sender: TObject;
  const F: TEpiField): EpiBool;
begin

end;

function TScriptParser.GetFieldValueInt(const Sender: TObject;
  const F: TEpiField): EpiInteger;
begin

end;

function TScriptParser.GetFieldValueFloat(const Sender: TObject;
  const F: TEpiField): EpiFloat;
begin

end;

function TScriptParser.GetFieldValueString(const Sender: TObject;
  const F: TEpiField): EpiString;
begin

end;



end.

