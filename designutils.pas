unit designutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, LCLIntf;

  function GetFormRelativeMousePosition(Form: TCustomForm): TPoint;

implementation


function GetFormRelativeMousePosition(Form: TCustomForm): TPoint;
var
  FormClientOrigin: TPoint;
begin
  Result.X:=0;
  Result.Y:=0;
  GetCursorPos(Result);
  FormClientOrigin:=Form.ClientOrigin;
  dec(Result.X,FormClientOrigin.X);
  dec(Result.Y,FormClientOrigin.Y);
end;

end.

