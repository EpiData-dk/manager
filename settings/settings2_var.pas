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
    ShowWelcome:           boolean;
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

    // Visual design:
    DefaultRightPosition:  Integer;
    DefaultLabelPosition:  Integer;
    SnapFields:            boolean;
    SnappingThresHold:     Integer;
    SpaceBtwFieldField:    Integer;
    SpaceBtwFieldLabel:    Integer;
    SpaceBtwLabelLabel:    Integer;

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
    HeadingFont:           TFont;
    SectionFont:           TFont;

    // Export:
    ExportType:            Integer;
    ExportDeleted:         boolean;
    ExportEncoding:        TEpiEncoding;

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
    ExportSASValueLabels: boolean;
    // - SPSS
    ExportSPSSValueLabels: boolean;

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
    IniFileName:           string;
  end;
  PManagerSettings = ^TManagerSettings;

var
  ManagerSettings: TManagerSettings = (
    // General:
    SaveWindowPositions:   true;
    ShowWelcome:           true;
    ShowWorkToolBar:       true;
    ShowA4GuideLines:      true;
    MultipleInstances:     false;
    PasteSpecialType:      1;     // Heading.
    SaveType:              0;     // epx format.
    ImportCasing:          fncLower;     // lowercase

    // Visual design:
    DefaultRightPosition:  400;
    DefaultLabelPosition:  20;
    SnapFields:            true;
    SnappingThresHold:     10;
    SpaceBtwFieldField:    10;
    SpaceBtwFieldLabel:    25;
    SpaceBtwLabelLabel:    5;

    // Field definitions:
    IntFieldLength:        2;
    FloatIntLength:        5;
    FloatDecimalLength:    2;
    StringFieldLength:     20;
    DefaultDateType:       ftDMYDate;
    FieldNamePrefix:       'V';
//    FieldNamingStyle:      fnFirstWord;

    // Advanced:
    WorkingDirUTF8:        '';
    TutorialDirUTF8:       '';
    TutorialURLUTF8:       'http://epidata.dk/documentation.php';
    EntryClientDirUTF8:    '';
    FieldFont:             nil;
    HeadingFont:           nil;
    SectionFont:           nil;

    // Export:
    ExportType:            0;     // 0 = Stata
                                  // 1 = CSV
                                  // 2 = SPSS
                                  // 3 = SAS
    ExportDeleted:         false;
    ExportEncoding:        eeUTF8;

    // - Stata:
    ExportStataVersion:    dta8;   // Default to Version 8
    ExportStataFieldCase:  fncAsIs;
    ExportStataValueLabels: true;

    // - CSV
    ExportCSVFieldName:    true;
    ExportCSVQuote:        '"';
    ExportCSVFieldSep:     ',';
    ExportCSVDateSep:      '-';
    ExportCSVTimeSep:      ':';
    ExportCSVDecSep:       '.';
    ExportCSVNewLine:      0;

    // - SAS
    ExportSASValueLabels:  true;
    // - SPSS
    ExportSPSSValueLabels: true;

    // Project Defaults
    // - general:
    TimedRecoveryInterval: 10;
    SaveBackup:            true;
    AutoIncStart:          1;
    // - Fields:
    ShowNames:             false;
    ShowBorders:           true;
    ShowValuelabelText:    true;
    // - Study:
    StudyTitle:            'Untitled Project';
    StudyIndent:           '';
    StudyLang:             'en';
    StudyVersion:          '1';
    // - Content Desc:
    ContKeywords:          '';
    ContPurpose:           '';
    ContAbstract:          '';
    ContCitation:          '';
    ContGeoCover:          '';
    ContTimeCover:         '';
    ContPopulation:        '';
    // - Ownership:
    OwnAgency:             '';
    OwnAuthers:            '';
    OwnRights:             '';
    OwnPublisher:          '';
    OwnFunding:            '';



    // Not shown in dialog.
    SelectedControlColour: $00B6F5F5;
    LabelNamePrefix:       'label_';
    IniFileName:           '';
  );

var
  RecentFiles: TStringList;

implementation

end.

