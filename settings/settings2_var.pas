unit settings2_var;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidatafilestypes, Graphics, epieximtypes,
  epiexportsettings, Forms;

type
  TManagerSettings = record
    // General:
    SaveWindowPositions:   boolean;
    ShowWorkToolBar:       boolean;
    ShowA4GuideLines:      boolean;
    MultipleInstances:     boolean;
    PasteSpecialType:      byte;     // Index into list:
                                     //   0: Heading
                                     //   1: Int
                                     //   2: Float
                                     //   3: String
    SaveType:              byte;     // Index into list:
                                     //   0: .epx
                                     //   1: .epz
    ImportCasing:          TEpiFieldNamingCase;
    CheckForUpdates:       Boolean;
    DaysBetweenChecks:     Byte;
    LastUpdateCheck:       TDateTime;

    // Visual design:
    DefaultRightPosition:  Integer;
    DefaultLabelPosition:  Integer;
    SnapFields:            boolean;
    SnappingThresHold:     Integer;
    SpaceBtwFieldField:    Integer;
    SpaceBtwFieldLabel:    Integer;
    SpaceBtwLabelLabel:    Integer;
    ReportOutputFormat:    Byte;     // 0: HTML
                                     // 1: Text

    // Field definitions:
    IntFieldLength:        Integer;
    FloatIntLength:        Integer;
    FloatDecimalLength:    Integer;
    StringFieldLength:     Integer;
    DefaultDateType:       TEpiFieldType;
    FieldNamePrefix:       string;
//    FieldNamingStyle:      TFieldNaming;

    // Advanced:
    WorkingDirUTF8:        string;
    TutorialDirUTF8:       string;
    TutorialURLUTF8:       string;
    EntryClientDirUTF8:    string;
    FieldFont:             TFont;
    HeadingFont1:          TFont;
    HeadingFont2:          TFont;
    HeadingFont3:          TFont;
    HeadingFont4:          TFont;
    HeadingFont5:          TFont;
    SectionFont:           TFont;

    // Export:
    ExportType:            Integer;
    ExportDeleted:         boolean;
    ExportEncoding:        TEpiEncoding;
    ExportCreateReport:    boolean;

    // - Stata:
    ExportStataVersion:    TEpiStataVersion;
    ExportStataFieldCase:  TEpiFieldNamingCase;
    ExportStataValueLabels: boolean;

    // - CSV
    ExportCSVFieldName:    boolean;
    ExportCSVQuote:        string;
    ExportCSVFieldSep:     string;
    ExportCSVDateSep:      string;
    ExportCSVTimeSep:      string;
    ExportCSVDecSep:       string;
    ExportCSVNewLine:      integer;

    // - SAS
    ExportSASValueLabels:  boolean;
    // - SPSS
    ExportSPSSValueLabels: boolean;
    ExportSPSSDelimiter:   char;
    // - DDI
    ExportDDIValueLabels:  boolean;
    ExportDDIRemoveMissingVL: boolean;
    ExportDDIFilterTagIsUserId: boolean;
    ExportDDIRenameVariables: boolean;
    ExportDDIRenameVariablesPrefix: string;
    ExportDDISectionCaptionIsQText: boolean;
    ExportDDILanguageISO: string;
    // - EPX
    ExportEPXValueLabels:  boolean;

    // Project Defaults
    // - general:
    TimedRecoveryInterval: integer;
    SaveBackup:            boolean;
    AutoIncStart:          integer;
    // - Fields:
    ShowNames:             boolean;
    ShowBorders:           boolean;
    ShowValuelabelText:    boolean;
    // - Study:
    StudyTitle:            string;
    StudyIndent:           string;
    StudyLang:             string;
    StudyVersion:          string;
    // - Content Desc:
    ContKeywords:          string;
    ContPurpose:           string;
    ContAbstract:          string;
    ContCitation:          string;
    ContGeoCover:          string;
    ContTimeCover:         string;
    ContPopulation:        string;
    // - Ownership:
    OwnAgency:             string;
    OwnAuthers:            string;
    OwnRights:             string;
    OwnPublisher:          string;
    OwnFunding:            string;

    // Not shown in dialog.
    SelectedControlColour: Integer;
    LabelNamePrefix:       string;
  end;
  PManagerSettings = ^TManagerSettings;

var
  ManagerSettings: TManagerSettings;

var
  RecentFiles: TStringList;
  // Startup files are a list of files that the program should
  // open. This list is gathered during parsing commandline options.
  // Maintained from "managerprocs.pas"
  StartupFiles: TStringList = nil;

implementation

uses
  settings2;

initialization
  RestoreSettingsDefaults;

end.

