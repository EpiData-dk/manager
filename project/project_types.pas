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

const
  PROJECT_TREE_NODE_KEY = 'PROJECT_TREE_NODE_KEY';
  PROJECT_RUNTIMEFRAME_KEY = 'PROJECT_RUNTIMEFRAME_KEY';
  PROJECT_RELATION_KEY = 'PROJECT_RELATION_KEY';

implementation

end.

