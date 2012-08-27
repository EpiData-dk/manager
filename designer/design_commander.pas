unit design_commander;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls;

type

  { TCustomCommand }

  TCustomCommand = class
  public
    procedure Execute; virtual; abstract;
  end;

  { TCommandList }

  TCommandList = class
  private
    FList: TFPList;

  public
    constructor Create;
    procedure AddCommand(Command: TCustomCommand);
    procedure Undo;
    procedure ReDo;
  end;

implementation

{ TCommandList }

constructor TCommandList.Create;
begin
  FList := TFPList.Create;
end;

procedure TCommandList.AddCommand(Command: TCustomCommand);
begin
  FList.Add(Command);
end;

procedure TCommandList.Undo;
begin
  if FList.Count = 0 then exit;

  TCustomCommand(FList.Last).Execute;
  FList.Remove(FList.Last);
end;

procedure TCommandList.ReDo;
begin

end;


end.

