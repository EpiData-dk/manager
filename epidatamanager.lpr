program epidatamanager;

{$codepage UTF8}
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
    {$IFDEF UseCThreads}
    cthreads,
    {$ENDIF}
    cwstring,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main,
  settings,
  {$IFNDEF EPI_DEBUG}
  warning,
  Controls,
  {$ENDIF EPI_DEBUG}
  datafile_documentation_form, design_autoalign_form, design_field_frame,
  design_frame, design_label_form, fieldedit, editormain, managerprocs, 
  workflow_frame, managertypes;


{$IFNDEF EPI_DEBUG}
var
  mr: integer;
{$ENDIF}

{$R epidatamanager.res}

begin
  Application.Initialize;
  {$IFNDEF EPI_DEBUG}
  Application.CreateForm(TWarningForm, WarningForm);
  mr := WarningForm.ShowModal;
  if mr <> mrOk then
    exit;
  WarningForm.Free;
  {$ENDIF EPI_DEBUG}
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
