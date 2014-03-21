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
    procedure Activate;
    function DeActivate(aHide: boolean): boolean;
    procedure AssignActionLinks;
  end;

const
  PROJECT_TREE_NODE_KEY = 'PROJECT_TREE_NODE_KEY';
  PROJECT_RUNTIMEFRAME_KEY = 'PROJECT_RUNTIMEFRAME_KEY';
  PROJECT_RELATION_KEY = 'PROJECT_RELATION_KEY';
  PROJECT_RELATION_KEYFIELD_CHILD_KEY = 'PROJECT_RELATION_KEYFIELD_KEY';
  PROJECT_RELATION_KEYFIELD_PARENT_KEY = 'PROJECT_RELATION_KEYFIELD_PARENT_KEY';

implementation

end.

