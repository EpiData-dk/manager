unit settings2_var;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidatafilestypes;

type
  TManagerSettings = record
    // Visual design:
    DefaultRightPostion:   Integer;
    DefaultLabelPostion:   Integer;
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
    PasteSpecialType:      byte;     // Index into list:
                                     //   0: QES
                                     //   1: Heading
                                     //   2: Int
                                     //   3: Float
                                     //   4: String
    SaveType:              byte;     // Index into list:
                                     //   0: .epx
                                     //   1: .epz

    // Not shown in dialog.
    SelectedControlColour: Integer;
    LabelNamePrefix:       string;
    IniFileName:           string;
  end;
  PManagerSettings = ^TManagerSettings;

var
  ManagerSettings: TManagerSettings = (
    // Visual design:
    DefaultRightPostion:   200;
    DefaultLabelPostion:   20;
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
    PasteSpecialType:      1;     // Heading.
    SaveType:              0;     // epx format.

    // Not shown in dialog.
    SelectedControlColour: $00B6F5F5;
    LabelNamePrefix:       'label_';
    IniFileName:           '';
  );

var
  RecentFiles: TStringList;

implementation

end.

