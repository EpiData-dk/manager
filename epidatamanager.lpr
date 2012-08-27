program epidatamanager;

{$mode objfpc}{$H+}

uses
//  heaptrc,
  {$IFDEF UNIX}
    {$IFDEF EPI_USEIPC}
    cthreads,
    {$ENDIF}
  cwstring,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, fpvectorialpkg, printer4lazarus, virtualtreeview_package,
  jvRuntimeDesign, main, project_frame, workflow_frame,
  managerprocs, sysutils, about,

  // Settings - import order defines order!
  settings2,
  settings2_interface,
  settings2_var,
  settings_general_frame,
  settings_advanced_frame,
  settings_fielddefinitions_frame,
  settings_visualdesign_frame,

  project_settings, project_settings_interface, project_settings_general_frame,
  project_settings_field_frame, project_settings_study_frame,
  project_settings_study_contentdesc_frame,
  project_settings_study_ownership_frame, valuelabelseditor_form,
  UniqueInstanceRaw, epidatacore,
  design_sectionproperties_frame, design_properties_headingframe,
  design_propertiesbase_frame, design_types, import_structure_form,
  manager_messages, toolsform, reportgenerator, report_fieldlist, viewer_form,
  report_valuelabellist, report_combinedlist, report_base,
  projectfilelist_frame, staticreports_form, report_fieldlist_extended,
  report_project_overview, shortcuts, field_valuelabelseditor_form,
  valuelabelgrid_frame, valuelabelseditor_form2, settings_export, export_form,
  export_stata_frame, export_customvaluelabel_frame, export_frame_types,
  export_csv_frame, export_customtext_frame, export_spss_frame,
  export_sas_frame, datasetviewer_frame, prepare_double_entry_form,
  project_keyfields_form, manager_globals, validate_double_entry_form,
  report_double_entry_validation, design_runtimedesigner, design_control_field,
  design_control_heading, design_control_section, design_designpanel,
  design_properties_form, design_designcontroller,
  design_fieldproperties_frame,
design_properties_fieldframe, design_properties_sectionframe, 
design_designmessenger, design_commander;

{$R *.res}

function EpiDataApplicationName: string;
begin
  result := 'epidatamanager';
end;

begin
  Application.Title := 'EpiData Manager';
  OnGetApplicationName := @EpiDataApplicationName;
  // Load ini before anything else - it contains start-up info.
  LoadIniFile;
  {$IFNDEF EPI_DEBUG}
  if (not ManagerSettings.MultipleInstances) and
     InstanceRunning(EpiDataApplicationName) then exit;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

