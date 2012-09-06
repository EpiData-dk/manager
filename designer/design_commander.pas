unit design_commander;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls;

type

  { TCustomCommand }

  TCustomCommand = class
  public
    procedure Undo; virtual; abstract;
    procedure ReDo; virtual; abstract;
  end;

  { TCustomCommandList }

  TCustomCommandList = class(TCustomCommand)
  private
    FList: TFPList;
  protected
    procedure FreeCommand(data, arg: pointer);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AddCommand(Command: TCustomCommand); virtual;
    procedure Undo; override;
    procedure ReDo; override;
  end;

  { TCommander }

  TCommander = class(TCustomCommandList)
  private
    FRedoList: TFPList;
    function GetCanRedo: boolean;
    function GetCanUndo: boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure AddCommand(Command: TCustomCommand); override;
    procedure Undo; override;
    procedure ReDo; override;
    property CanUndo: boolean read GetCanUndo;
    property CanRedo: boolean read GetCanRedo;
  end;

implementation

{ TCustomCommandList }

procedure TCustomCommandList.FreeCommand(data, arg: pointer);
begin
  TCustomCommand(data).Free;
end;

constructor TCustomCommandList.Create;
begin
  FList := TFPList.Create;
end;

destructor TCustomCommandList.Destroy;
begin
  FList.ForEachCall(@FreeCommand, nil);
  inherited Destroy;
end;

procedure TCustomCommandList.AddCommand(Command: TCustomCommand);
begin
  FList.Add(Command);
end;

procedure TCustomCommandList.Undo;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    TCustomCommand(FList[i]).Undo;
end;

procedure TCustomCommandList.ReDo;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    TCustomCommand(FList[i]).ReDo;
end;

{ TCommander }

function TCommander.GetCanRedo: boolean;
begin
  result := FRedoList.Count > 0;
end;

function TCommander.GetCanUndo: boolean;
begin
  result := FList.Count > 0;
end;

constructor TCommander.Create;
begin
  inherited Create;
  FRedoList := TFPList.Create;
end;

destructor TCommander.Destroy;
begin
  FRedoList.ForEachCall(@FreeCommand, nil);
  FRedoList.Free;
  inherited Destroy;
end;

procedure TCommander.AddCommand(Command: TCustomCommand);
begin
  inherited AddCommand(Command);

  FRedoList.ForEachCall(@FreeCommand, nil);
  FRedoList.Clear;
end;

procedure TCommander.Undo;
var
  Cmd: TCustomCommand;
begin
  if FList.Count = 0 then exit;

  Cmd := TCustomCommand(FList.Last);
  FList.Remove(Cmd);
  Cmd.Undo;

  FRedoList.Add(Cmd);
end;

procedure TCommander.ReDo;
var
  Cmd: TCustomCommand;
begin
  if FRedoList.Count = 0 then exit;

  Cmd := TCustomCommand(FRedoList.Last);
  FRedoList.Remove(Cmd);
  Cmd.Redo;

  FList.Add(Cmd);
end;

end.

