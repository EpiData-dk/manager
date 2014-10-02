unit report_types;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, Forms, report_base, projectfilelist_frame, epiopenfile;

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
  end;

  { IReportFrame }

  IReportFrame = interface ['IReportFrame']
    procedure AddDocumentFile(Const DocumentFile: TEpiDocumentFile);
    procedure AddFiles(FileNames: TStrings);
    procedure ApplyReportOptions(Report: TReportBase);
    function  GetCaption: string;
    function  CanPressOk: Boolean;
  end;

implementation

end.

