unit settings2_var;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidatafilestypes;

type
  TManagerSettings = record
    // Visual design:
    DefaultRightPostion:   Integer;
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
  ManagerSettings2: TManagerSettings = (
    // Visual design:
    DefaultRightPostion:   200;
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
    PasteSpecialType:      1;     // Heading.
    SaveType:              0;     // epx format.

    // Not shown in dialog.
    SelectedControlColour: $00B6F5F5;
    LabelNamePrefix:       'label_';
    IniFileName:           '';
  );

implementation

end.

