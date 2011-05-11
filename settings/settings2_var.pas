unit settings2_var;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidatafilestypes, Graphics;

type
  TManagerSettings = record
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
    SaveWindowPositions:   boolean;
    ShowWelcome:           boolean;
    ShowWorkToolBar:       boolean;
    MultipleInstances:     boolean;
    WorkingDirUTF8:        string;
    TutorialDirUTF8:       string;
    TutorialURLUTF8:       string;
    EntryClientDirUTF8:    string;
    PasteSpecialType:      byte;     // Index into list:
                                     //   0: QES
                                     //   1: Heading
                                     //   2: Int
                                     //   3: Float
                                     //   4: String
    SaveType:              byte;     // Index into list:
                                     //   0: .epx
                                     //   1: .epz
    DesignerFont:          TFont;

    // Project Defaults
    // - general:
    TimedRecoveryInterval: integer;
    SaveBackup:            boolean;
    AutoIncStart:          integer;
    // - Fields:
    ShowNames:             boolean;
    ShowBorders:           boolean;
    // - Study:
    StudyTitle:            string;
    StudyIndent:           string;
    StudyLang:             string;
    StudyVersion:          string;
    // - Content Desc:
    ContPurpose:           string;
    ContAbstract:          string;
    ContCitation:          string;
    ContGeoCover:          string;
    ContTimeCover:         string;
    // - Ownership:
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
    // Visual design:
    DefaultRightPosition:  200;
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
    SaveWindowPositions:   true;
    ShowWelcome:           true;
    ShowWorkToolBar:       true;
    MultipleInstances:     false;
    WorkingDirUTF8:        '';
    TutorialDirUTF8:       '';
    TutorialURLUTF8:       'http://epidata.dk/documentation.php';
    EntryClientDirUTF8:    '';
    PasteSpecialType:      1;     // Heading.
    SaveType:              0;     // epx format.
    DesignerFont:          nil;


    // Project Defaults
    // - general:
    TimedRecoveryInterval: 10;
    SaveBackup:            true;
    AutoIncStart:          1;
    // - Fields:
    ShowNames:             false;
    ShowBorders:           true;
    // - Study:
    StudyTitle:            'Untitled Project';
    StudyIndent:           '';
    StudyLang:             'en';
    StudyVersion:          '1';
    // - Content Desc:
    ContPurpose:           '';
    ContAbstract:          '';
    ContCitation:          '';
    ContGeoCover:          '';
    ContTimeCover:         '';
    // - Ownership:
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

