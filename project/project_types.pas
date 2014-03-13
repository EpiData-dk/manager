unit project_types;

{$mode objfpc}{$H+}
{$interfaces corba}

interface

uses
  Classes, SysUtils;

type
  IProjectFrame = interface['IProjectFrame']
    procedure UpdateFrame;
    procedure Activate;
    function DeActivate(aHide: boolean): boolean;
  end;


implementation

end.

