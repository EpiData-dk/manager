program epidatamanager;

{$codepage UTF8}
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, LResources, main,
  settings,
  {$IFNDEF EPI_DEBUG}
  warning,
  Controls,
  {$ENDIF EPI_DEBUG}
  datafile_documentation_form, design_autoalign_form, design_field_frame,
  design_frame, design_label_form, fieldedit, editormain, managerprocs;

{$IFDEF WINDOWS}
  {$R epidatamanager.rc}
{$ENDIF}

{$IFNDEF EPI_DEBUG}
var
  mr: integer;
{$ENDIF}
begin
  {$I epidatamanager.lrs}
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

