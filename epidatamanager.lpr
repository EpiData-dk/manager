program epidatamanager;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
    {$IFDEF UseCThreads}
    cthreads,
    {$ENDIF}
  cwstring,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main, project_frame, design_frame, workflow_frame, managerprocs, sysutils,
  project_settings, project_settings_field_frame, project_settings_interface,
  about, epidatacore, project_settings_general_frame, settings2,
  settings_visualdesign_frame, settings_fielddefinitions_frame,
  settings_advanced_frame, settings2_interface, settings2_var, structure_form,
  structure_datafile_frame, valuelabelseditor_form,
  structure_valuelabelset_frame, design_controls, UniqueInstanceRaw;

{$R *.res}

function EpiDataApplicationName: string;
begin
  result := 'epidatamanager';
end;

begin
  OnGetApplicationName := @EpiDataApplicationName;
  if InstanceRunning(EpiDataApplicationName) then exit;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

