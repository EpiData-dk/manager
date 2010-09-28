program epidatamanager;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main, project_frame, design_frame, workflow_frame, 
  design_field, design_heading, design_section, design_custombase, import_form, 
  settings, managerprocs, sysutils, project_settings, 
project_settings_field_frame, project_settings_interface, about, epidatacore, 
project_settings_general_frame
  { you can add units after this };

{$R *.res}

function EpiDataApplicationName: string;
begin
  result := 'epidatamanager';
end;

begin
  OnGetApplicationName := @EpiDataApplicationName;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

