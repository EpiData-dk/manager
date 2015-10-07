unit manager_types;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  epiadmin, epicustombase;

type
  ICanCloseQuery = interface['ICanCloseQuery']
    function CanClose: boolean;
  end;

  TExportPostFix = (
    epProjectOnly,
    epAddCycle,
    epAddDate
  );

  TGetUserEvent = function (Sender: TObject): TEpiUser of object;
  TUpdateStatusBarContent = procedure (Sender: TObject;
    SelectionList: TEpiCustomList) of object;

const
  ExportPostFixCaptions: array[TExportPostFix] of string = (
    'Project Only',
    'Add Cycle',
    'Add Date'
  );

implementation

end.

