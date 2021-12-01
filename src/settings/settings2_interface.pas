unit settings2_interface;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, settings2_var;

type

  ISettingsFrame = interface ['{7C311CAC-5BD2-4364-9866-8051270F52D6}']
    procedure SetSettings(Data: PManagerSettings);
    function  ApplySettings: boolean;
  end;


implementation

end.

