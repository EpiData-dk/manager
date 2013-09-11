unit manager_types;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

type
  ICanCloseQuery = interface['ICanCloseQuery']
    function CanClose: boolean;
  end;

implementation

end.

