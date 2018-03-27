program epidatamanager;

{$mode objfpc}{$H+}

uses
  {$IFDEF MEM_LEAK_FINDER}
  heaptrc, LazFileUtils,
  {$ENDIF}

  {$IFDEF UNIX}
  cthreads,
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
  design_properties_headingframe, design_properties_baseframe, design_types,
  import_structure_form, manager_messages, toolsform, reportgenerator,
  report_fieldlist, viewer_form, report_valuelabellist, report_combinedlist,
  report_base, projectfilelist_frame, staticreports_form,
  report_fieldlist_extended, report_project_overview, shortcuts,
  field_valuelabelseditor_form, valuelabelgrid_frame, valuelabelseditor_form2,
  settings_export, export_stata_frame, export_customvaluelabel_frame,
  export_frame_types, export_csv_frame, export_customtext_frame,
  export_spss_frame, export_sas_frame, prepare_double_entry_form,
  project_keyfields_form, manager_globals, report_double_entry_validation,
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
  epiv_projecttreeview_frame, epiv_datamodule, report_project_validation_frame2,
  reports_form, report_counts_frame, lazcontrols, design_control_memo,
  export_form2, admin_form, admin_user_form, admin_group_form,
  admin_authenticator, design_properties_groupassign_frame, core_logger,
  admin_groups_form, admin_users_form, admin_entryrights_form,
  admin_users_accum_rights_frame, project_statusbar,
  design_properties_dataform_statusbarframe, settings_statusbar,
  admin_logviewer_frame, empty_form, report_admin, report_logoverview,
  epicustomlist_helper, epidatacore, project_settings_extended_access,
  export_securitylog_form, archive_form;

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
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

