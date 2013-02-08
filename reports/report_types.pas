unit report_types;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, Forms, report_base;

type
  IReportFrameProvider = interface ['IReportFrameProvider']
    function GetFrameClass: TCustomFrameClass;
  end;

  IReportOptionFrame = interface ['IReportOptionFrame']
    function  GetFrameCaption: string;
    procedure UpdateFrame(Selection: TStrings);
    procedure ApplyReportOptions(Report: TReportBase);
    function CanClose: boolean;
  end;

implementation

end.

