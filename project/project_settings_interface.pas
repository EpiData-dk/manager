unit project_settings_interface;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, episettings;

type

  { IProjectSettingsFrame }

  IProjectSettingsFrame = interface ['{A04D7652-EAA0-49B4-A14E-07F974207DB9}']
    procedure SetProjectSettings(AValue: TEpiProjectSettings);
    function  ApplySettings: boolean;
  end;

implementation

end.

