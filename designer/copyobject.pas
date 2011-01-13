unit copyobject;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidatafiles, epicustombase, design_controls, Controls;


type
  TCopyType = (ctField, ctSection, ctHeading, ctSelection);


  { TCopyObject }

  TCopyObject = class
  private
    FCopyType: TCopyType;
    FData: PtrInt;
  public
    property CopyType: TCopyType read FCopyType write FCopyType;
    property Data: PtrInt read FData write FData;
  end;

  { TFieldCopyObject }

  TFieldCopyObject = class
  private
    FField: TEpiField;
    procedure FieldHook(Sender: TObject; EventGrp: TEpiEventGroup; Event: Word; Data: Pointer);
  public
    constructor Create(Const Field: TEpiField);
    property Field: TEpiField read FField;
  end;

  { TSectionCopyObject }

  TSectionCopyObject = class
  private
    FSection: TEpiSection;
  public
    constructor Create(const Section: TEpiSection);
    property Section: TEpiSection read FSection;
  end;

  { THeadingCopyObject }

  THeadingCopyObject = class
  private
    FHeading: TEpiHeading;
  public
    constructor Create(Const Heading: TEpiHeading);
    property Heading: TEpiHeading read FHeading;
  end;

  { TSelectionCopyObject }

{  TSelectionCopyObject = class
  private
    FItem: TFPList;
    FTypes: TFPList;
    function GetCount: Integer;
    function GetItems(const index: integer): TEpiCustomControlItem;
    function GetTypes(const index: integer): TEpiCustomControlItemClass;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   Add(Item: TEpiCustomControlItem);
    property    Count: Integer read GetCount;
    property    Items[const index: integer]: TEpiCustomControlItem read GetItems;
    property    Types[const index: integer]: TEpiCustomControlItemClass read GetTypes;
  end;
                  }



function NewCopyObject(Const Ctrl: TControl): TCopyObject;
function GetCopyObject: TCopyObject;

const
  CopyCompatibleTable: array[TDesignerControlType] of array [TCopyType] of boolean = (
    (true, false, false, false),
    (true, true, true, true),
    (false, false, true, false)
  );


implementation

var
  TheCopyObject: TCopyObject = nil;

function NewCopyObject: TCopyObject;
begin
  if Assigned(TheCopyObject) then
  begin
    case TheCopyObject.CopyType of
      ctField:     TFieldCopyObject(TheCopyObject.Data).Free;
      ctSection:   TSectionCopyObject(TheCopyObject.Data).Free;
      ctHeading:   THeadingCopyObject(TheCopyObject.Data).Free;
      ctSelection: ; // TODO : Assign when TSelectionCopyObject is working.;
    end;
    TheCopyObject.Free;
  end;
  TheCopyObject := TCopyObject.Create;
end;

function NewCopyObject(const Ctrl: TControl): TCopyObject;
begin
  result := NewCopyObject;
  case DesignControlTypeFromControl(Ctrl) of
    dctField:
      begin
        Result.Data := PtrInt(TFieldCopyObject.Create(TEpiField((Ctrl as IDesignEpiControl).EpiControl)));
      end;
    dctSection: ;
    dctHeading: ;
  end;
end;

function GetCopyObject: TCopyObject;
begin
  result := TheCopyObject;
end;

{ TFieldCopyObject }

procedure TFieldCopyObject.FieldHook(Sender: TObject; EventGrp: TEpiEventGroup;
  Event: Word; Data: Pointer);
begin
  if (EventGrp = eegCustomBase) and (Event = Word(ecceDestroy)) then
    Self.Free;
end;

constructor TFieldCopyObject.Create(const Field: TEpiField);
begin
  FField := Field;
  FField.RegisterOnChangeHook(@FieldHook);
end;

{ TSectionCopyObject }

constructor TSectionCopyObject.Create(const Section: TEpiSection);
begin

end;

{ THeadingCopyObject }

constructor THeadingCopyObject.Create(const Heading: TEpiHeading);
begin

end;

{ TSelectionCopyObject }
{
function TSelectionCopyObject.GetCount: Integer;
begin
  result := FItem.Count;
end;

function TSelectionCopyObject.GetItems(const index: integer
  ): TEpiCustomControlItem;
begin
  Result := TEpiCustomControlItem(FItem[Index]);
end;

function TSelectionCopyObject.GetTypes(const index: integer
  ): TEpiCustomControlItemClass;
begin
  result := TEpiCustomControlItemClass(FTypes[index]);
end;

constructor TSelectionCopyObject.Create;
begin
  FItem := TFPList.Create;
  FTypes := TFPList.Create;
end;

destructor TSelectionCopyObject.Destroy;
begin
  FItem.Free;
  inherited Destroy;
end;

procedure TSelectionCopyObject.Add(Item: TEpiCustomControlItem);
var
  I: LongInt;
begin
  FItem.Add(Item);
  FTypes.Add(Item.ClassType);
end;     }

end.

