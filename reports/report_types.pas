unit report_types;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, Forms, report_base, projectfilelist_frame;

type
  IReportFrameProvider = interface ['IReportFrameProvider']
    function GetFrameClass: TCustomFrameClass;
  end;

  IReportOptionFrame = interface ['IReportOptionFrame']
    function  GetFrameCaption: string;
    procedure UpdateFrame(Selection: TStrings);
    procedure ApplyReportOptions(Report: TReportBase);
    function OkToAdvance(ProjectList: TProjectFileListFrame): boolean;
    function OkToAdvanceText: string;
    function CanClose: boolean;
  end;

implementation

end.

