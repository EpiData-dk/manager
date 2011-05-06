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
  Forms, main, project_frame, design_frame, workflow_frame, managerprocs,
  sysutils, about, epidatacore,

  // Settings - import order defines order!
  settings2,
  settings2_interface,
  settings2_var,
  settings_general_frame,
  settings_advanced_frame,
  settings_fielddefinitions_frame,
  settings_visualdesign_frame,

  project_settings,
  project_settings_interface,
  project_settings_general_frame,
  project_settings_field_frame,
  project_settings_study_frame,
  project_settings_study_contentdesc_frame,
  project_settings_study_ownership_frame,

  // No more settings
  structure_form, structure_datafile_frame,
  valuelabelseditor_form, structure_valuelabelset_frame, design_controls,
  UniqueInstanceRaw, copyobject, design_fieldproperties_frame, 
  design_sectionproperties_frame, design_headingproperties_frame,
  design_propertiesbase_frame, design_types, import_structure_form, 
  manager_messages;

{$R *.res}

function EpiDataApplicationName: string;
begin
  result := 'epidatamanager';
end;

begin
  OnGetApplicationName := @EpiDataApplicationName;
  // Load ini before anything else - it contains start-up info.
  LoadIniFile;
  {$IFDEF EPI_RELEASE}
  if (not ManagerSettings.MultipleInstances) and
     InstanceRunning(EpiDataApplicationName) then exit;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

