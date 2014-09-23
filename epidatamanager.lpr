program epidatamanager;

{$mode objfpc}{$H+}

uses
  {$IFDEF MEM_LEAK_FINDER}
  heaptrc, LazFileUtils,
  {$ENDIF}

  {$IFDEF UNIX}
    {$IFDEF EPI_USEIPC}
    cthreads,
    {$ENDIF}
  clocale,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, printer4lazarus, virtualtreeview_package,
  jvRuntimeDesign, main, project_frame,
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
  project_settings_field_frame, UniqueInstanceRaw,
  epidatacore, design_properties_headingframe, design_properties_baseframe,
  design_types, import_structure_form, manager_messages, toolsform,
  reportgenerator, report_fieldlist, viewer_form, report_valuelabellist,
  report_combinedlist, report_base, projectfilelist_frame, staticreports_form,
  report_fieldlist_extended, report_project_overview, shortcuts,
  field_valuelabelseditor_form, valuelabelgrid_frame, valuelabelseditor_form2,
  settings_export, export_form, export_stata_frame,
  export_customvaluelabel_frame, export_frame_types, export_csv_frame,
  export_customtext_frame, export_spss_frame, export_sas_frame,
  prepare_double_entry_form, project_keyfields_form, manager_globals,
  validate_double_entry_form, report_double_entry_validation,
  design_runtimedesigner, design_control_field, design_control_heading,
  design_control_section, design_designpanel, design_properties_form,
  design_designcontroller, design_properties_fieldframe,
  design_properties_sectionframe, design_designmessenger, design_commander,
  design_properties_emptyframe, design_designmover, design_designsizer,
  design_designbander, align_form, report_counts, report_optionframe_counts,
  report_types, dataset_form, ok_cancel_form, validate_double_entry_frame,
  project_settings_autoincrement_frame, settings_font_frame, report_codebook,
  export_ddi_frame, design_control_extender, manager_types, export_epx_frame,
  report_project_validation, report_project_validation_frame, recode_form,
  rename_form, report_export, valuelabel_import_external,
  valuelabel_import_data, project_types, project_studyunit_frame,
  design_properties_dataformframe, append_form, append_form2, 
  epiv_projecttreeview_frame, epiv_datamodule, count_by_id_form, 
report_project_validation_frame2, reports_form;

{$R *.res}

function EpiDataApplicationName: string;
begin
  result := 'epidatamanager';
//  result := '';
end;

function EpiDataVendorName: string;
begin
  result := 'epidata';
end;

{$IFDEF MEM_LEAK_FINDER}
const
  MemTraceFileName = '/tmp/manager.trc';
{$ENDIF}

begin
  {$IFDEF MEM_LEAK_FINDER}
  HaltOnError := false;
  if FileExistsUTF8(MemTraceFileName) then
    DeleteFileUTF8(MemTraceFileName);
  SetHeapTraceOutput(MemTraceFileName);
  {$ENDIF}

  Application.Title := 'EpiData Manager';
  OnGetApplicationName := @EpiDataApplicationName;
  OnGetVendorName := @EpiDataVendorName;

  // Initialize the application (and widgetset), we may
  // need it during commandline options (windows doesn't have
  // a console, so help/versioninfo is displayed in a window).
  Application.Initialize;

  // Parse commandline options!
  ParseCommandLineOpts;

  // Load ini before anything else - it contains start-up info.
  LoadIniFiles;

  {$IFNDEF EPI_DEBUG}
  if (not ManagerSettings.MultipleInstances) and
     InstanceRunning(EpiDataApplicationName) then exit;
  {$ENDIF}
  Application.CreateForm(TDM, DM);
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

