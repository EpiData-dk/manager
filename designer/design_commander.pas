unit design_commander;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls;

type

  { TCustomCommand }

  TCustomCommand = class(TComponent)
  public
    procedure Undo; virtual; abstract;
    procedure ReDo; virtual; abstract;
  end;

  { TCustomCommandList }

  TCustomCommandList = class(TCustomCommand)
  private
    FFreeOnEmpty: boolean;
    FList: TFPList;
    procedure SetFreeOnEmpty(AValue: boolean);
  protected
    procedure FreeCommand(data, arg: pointer);
    procedure Notification(AComponent: TComponent; Operation: TOperation);
       override;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AddCommand(Command: TCustomCommand); virtual;
    procedure Undo; override;
    procedure ReDo; override;
    property  FreeOnEmpty: boolean read FFreeOnEmpty write SetFreeOnEmpty;
  end;

  { TCommander }

  TCommander = class(TCustomCommandList)
  private
    FRedoList: TFPList;
    function GetCanRedo: boolean;
    function GetCanUndo: boolean;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);
       override;
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

procedure TCustomCommandList.SetFreeOnEmpty(AValue: boolean);
begin
  if FFreeOnEmpty = AValue then Exit;
  FFreeOnEmpty := AValue;
end;

procedure TCustomCommandList.FreeCommand(data, arg: pointer);
begin
  with TCustomCommand(data) do
  begin
    RemoveFreeNotification(Self);
    Free;
  end;
end;

procedure TCustomCommandList.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if csDestroying in ComponentState then exit;

  if FList.IndexOf(AComponent) > -1 then
    FList.Remove(AComponent);

  if FreeOnEmpty and (FList.Count = 0) then
    Free;
end;

constructor TCustomCommandList.Create;
begin
  FList := TFPList.Create;
  FFreeOnEmpty := false;
end;

destructor TCustomCommandList.Destroy;
begin
  FList.ForEachCall(@FreeCommand, nil);
  FList.Free;
  inherited Destroy;
end;

procedure TCustomCommandList.AddCommand(Command: TCustomCommand);
begin
  Command.FreeNotification(Self);
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

procedure TCommander.Notification(AComponent: TComponent; Operation: TOperation
  );
begin
  inherited Notification(AComponent, Operation);
  if csDestroying in ComponentState then exit;

  if FRedoList.IndexOf(AComponent) > -1 then
    FRedoList.Remove(AComponent);
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

