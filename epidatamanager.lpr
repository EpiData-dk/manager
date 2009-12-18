program epidatamanager;

{$codepage UTF8}
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, LResources, main, design_frame, fieldedit, Design_Field_Frame,
  settings, design_label_form, design_autoalign_form,
  {$IFNDEF EPI_DEBUG}
  warning,
  Controls,
  {$ENDIF EPI_DEBUG}
  datafile_documentation_form;

{$IFDEF WINDOWS}
  {$IFDEF WIN32}
    {$R epidatamanager.rc}
  {$ENDIF}
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

