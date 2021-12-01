unit project_types;

{$mode objfpc}{$H+}
{$interfaces corba}

interface

uses
  Classes, SysUtils;

type

  { IProjectFrame }

  IProjectFrame = interface['IProjectFrame']
    procedure UpdateFrame;
    procedure UpdateStatusbar;
    procedure Activate;
    function DeActivate(aHide: boolean): boolean;
    procedure AssignActionLinks;
  end;

const
  PROJECT_RUNTIMEFRAME_KEY = 'PROJECT_RUNTIMEFRAME_KEY';
  PROJECT_RELATION_KEYFIELD_CHILD_KEY = 'PROJECT_RELATION_KEYFIELD_KEY';

implementation

end.

