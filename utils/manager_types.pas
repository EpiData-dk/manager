unit manager_types;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

type
  ICanCloseQuery = interface['ICanCloseQuery']
    function CanClose: boolean;
  end;

  TExportPostFix = (
    epProjectOnly,
    epAddCycle,
    epAddDate
  );


const
  ExportPostFixCaptions: array[TExportPostFix] of string = (
    'Project Only',
    'Add Cycle',
    'Add Date'
  );

implementation

end.

