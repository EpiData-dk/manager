unit core_logger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, epicustombase, epidocument, epiadmin, episettings, epistudy,
  epidatafiles, epiranges, epivaluelabels;

type

  { TCoreLogger }

  TCoreLogger = class(TForm)
    StringGrid1: TStringGrid;
  private
    FCurrentInitiator: TEpiCustomBase;
    function GetNewGridRow: Integer;
  private
    { private declarations }
    procedure DoEvent(Event: TEpiCustomChangeEventType;     Data: Pointer); overload;
    procedure DoEvent(Event: TEpiDocumentChangeEvent;       Data: Pointer); overload;
    procedure DoEvent(Event: TEpiSettingChangeEvent;        Data: Pointer); overload;
    procedure DoEvent(Event: TEpiProjectSettingChangeEvent; Data: Pointer); overload;
    procedure DoEvent(Event: TEpiAdminChangeEventType;      Data: Pointer); overload;
    procedure DoEvent(Event: TEpiStudyChangeEvent;          Data: Pointer); overload;
    procedure DoEvent(Event: TEpiDataFileChangeEventType;   Data: Pointer); overload;
    procedure DoEvent(Event: TEpiSectionsChangeEventType;   Data: Pointer); overload;
    procedure DoEvent(Event: TEpiFieldsChangeEventType;     Data: Pointer); overload;
    procedure DoEvent(Event: TEpiHeadingChangeEvent;        Data: Pointer); overload;
//    procedure DoEvent(Event: TEpigroupsFieldsChangeEventType;     Data: Pointer); overload;
    procedure DoEvent(Event: TEpiRangeChangeEventType;      Data: Pointer); overload;
    procedure DoEvent(Event: TEpiValueLabelChangeEvent;     Data: Pointer); overload;
    procedure DoEvent(Event: TEpiValueLabelSetChangeEvent;  Data: Pointer); overload;
//    procedure DoEvent(Event: TEpirelationFieldsChangeEventType;     Data: Pointer); overload;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure DocumentHook(const Sender: TEpiCustomBase;
      const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
  end;

implementation

{$R *.lfm}

uses
  typinfo;

{ TCoreLogger }

function TCoreLogger.GetNewGridRow: Integer;
begin
  result := StringGrid1.RowCount;
  StringGrid1.RowCount := Result + 1;
end;

procedure TCoreLogger.DoEvent(Event: TEpiCustomChangeEventType; Data: Pointer);
begin
  StringGrid1.InsertRowWithValues(StringGrid1.RowCount, [
    IntToStr(StringGrid1.RowCount),
    FCurrentInitiator.ClassName,
    'CustomBase',
    GetEnumName(TypeInfo(TEpiCustomChangeEventType), Integer(Event)),
    hexStr(Data)
  ]);
end;

procedure TCoreLogger.DoEvent(Event: TEpiDocumentChangeEvent; Data: Pointer);
begin

end;

procedure TCoreLogger.DoEvent(Event: TEpiSettingChangeEvent; Data: Pointer);
begin

end;

procedure TCoreLogger.DoEvent(Event: TEpiProjectSettingChangeEvent;
  Data: Pointer);
begin

end;

procedure TCoreLogger.DoEvent(Event: TEpiAdminChangeEventType; Data: Pointer);
begin

end;

procedure TCoreLogger.DoEvent(Event: TEpiStudyChangeEvent; Data: Pointer);
begin

end;

procedure TCoreLogger.DoEvent(Event: TEpiDataFileChangeEventType; Data: Pointer
  );
begin

end;

procedure TCoreLogger.DoEvent(Event: TEpiSectionsChangeEventType; Data: Pointer
  );
begin

end;

procedure TCoreLogger.DoEvent(Event: TEpiFieldsChangeEventType; Data: Pointer);
begin

end;

procedure TCoreLogger.DoEvent(Event: TEpiHeadingChangeEvent; Data: Pointer);
begin

end;

procedure TCoreLogger.DoEvent(Event: TEpiRangeChangeEventType; Data: Pointer);
begin

end;

procedure TCoreLogger.DoEvent(Event: TEpiValueLabelChangeEvent; Data: Pointer);
begin

end;

procedure TCoreLogger.DoEvent(Event: TEpiValueLabelSetChangeEvent; Data: Pointer
  );
begin

end;

constructor TCoreLogger.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  StringGrid1.Cells[0, 0] := '#';
  StringGrid1.Cells[1, 0] := 'Initiator';
  StringGrid1.Cells[2, 0] := 'Event Group';
  StringGrid1.Cells[3, 0] := 'Event Type';
  StringGrid1.Cells[4, 0] := 'Data';
end;

procedure TCoreLogger.DocumentHook(const Sender: TEpiCustomBase;
  const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
begin
  if not Showing then exit;

  FCurrentInitiator := Initiator;

  case EventGroup of
    eegCustomBase:
      DoEvent(TEpiCustomChangeEventType(EventType), Data);

    eegDocument:
      DoEvent(TEpiDocumentChangeEvent(EventType), Data);

    eegXMLSetting:
      DoEvent(TEpiSettingChangeEvent(EventType), Data);

    eegProjectSettings:
      DoEvent(TEpiProjectSettingChangeEvent(EventType), Data);

    eegAdmin:
      DoEvent(TEpiAdminChangeEventType(EventType), Data);

    eegStudy:
      DoEvent(TEpiStudyChangeEvent(EventType), Data);

    eegDataFiles:
      DoEvent(TEpiDataFileChangeEventType(EventType), Data);

    eegSections:
      DoEvent(TEpiSectionsChangeEventType(EventType), Data);

    eegFields:
      DoEvent(TEpiFieldsChangeEventType(EventType), Data);

    eegGroups:                                              ;
//      DoEvent(TEpiCustomChangeEventType(EventType), Data);

    eegHeading:
      DoEvent(TEpiHeadingChangeEvent(EventType), Data);

    eegRange:
      DoEvent(TEpiRangeChangeEventType(EventType), Data);

    eegValueLabel:
      DoEvent(TEpiValueLabelChangeEvent(EventType), Data);

    eegValueLabelSet:
      DoEvent(TEpiValueLabelSetChangeEvent(EventType), Data);

    eegRelations:
//      DoEvent(TEpiCustomChangeEventType(EventType), Data);
      ;
  else
    ;

  end;
end;

end.

