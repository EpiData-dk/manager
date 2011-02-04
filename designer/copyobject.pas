unit copyobject;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidatafiles, epicustombase, Controls,
  design_types;


type
  TCopyType = (ctField, ctSection, ctHeading, ctSelection);


  { TCopyObject }

  TCopyObject = class
  private
    FCopyType: TCopyType;
    FData: PtrInt;
    constructor Create;
  protected
    constructor Create(Const Ctrl: TControl);
  public
    destructor Destroy; override;
    function ContainsControl(Const Ctrl: TControl): boolean;
    property CopyType: TCopyType read FCopyType;
    property Data: PtrInt read FData;
  end;

  { TFieldCopyObject }

  TFieldCopyObject = class
  private
    FField: TEpiField;
  public
    constructor Create(Const Field: TEpiField);
    destructor Destroy; override;
    property Field: TEpiField read FField;
  end;

  { TSectionCopyObject }

  TSectionCopyObject = class
  private
    FSection: TEpiSection;
  public
    constructor Create(const Section: TEpiSection);
    destructor Destroy; override;
    property Section: TEpiSection read FSection;
  end;

  { THeadingCopyObject }

  THeadingCopyObject = class
  private
    FHeading: TEpiHeading;
  public
    constructor Create(Const Heading: TEpiHeading);
    destructor Destroy; override;
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

uses
  LCLProc, design_controls;

type

  { TDestroyerObject }

  TDestroyerObject = class
  public
    procedure CustomItemHook(Sender: TObject; EventGrp: TEpiEventGroup; Event: Word; Data: Pointer);
  end;

var
  TheCopyObject: TCopyObject = nil;
  TheDestroyer: TDestroyerObject;

function NewCopyObject(const Ctrl: TControl): TCopyObject;
begin
  if not Assigned(TheDestroyer) then
    TheDestroyer := TDestroyerObject.Create;

  if Assigned(TheCopyObject) then
    TheCopyObject.Free;

  TheCopyObject := TCopyObject.Create(Ctrl);
  Result := TheCopyObject;
end;

function GetCopyObject: TCopyObject;
begin
  result := TheCopyObject;
end;

{ TDestroyerObject }

procedure TDestroyerObject.CustomItemHook(Sender: TObject;
  EventGrp: TEpiEventGroup; Event: Word; Data: Pointer);
begin
  if (EventGrp = eegCustomBase) and (Event = Word(ecceDestroy)) then
    FreeThenNil(TheCopyObject);
end;

{ TCopyObject }

constructor TCopyObject.Create;
begin
  // Disallows multiple instances.
end;

constructor TCopyObject.Create(const Ctrl: TControl);
begin
  case DesignControlTypeFromControl(Ctrl) of
    dctField:
      begin
        FData := PtrInt(TFieldCopyObject.Create(TEpiField((Ctrl as IDesignEpiControl).EpiControl)));
        FCopyType := ctField;
      end;
    dctSection:
      begin
        FData := PtrInt(TSectionCopyObject.Create(TEpiSection((Ctrl as IDesignEpiControl).EpiControl)));
        FCopyType := ctSection;
      end;
    dctHeading:
      begin
        FData := PtrInt(THeadingCopyObject.Create(TEpiHeading((Ctrl as IDesignEpiControl).EpiControl)));
        FCopyType := ctHeading;
      end;
  end;
end;

destructor TCopyObject.Destroy;
begin
  case TheCopyObject.CopyType of
    ctField:     TFieldCopyObject(TheCopyObject.Data).Free;
    ctSection:   TSectionCopyObject(TheCopyObject.Data).Free;
    ctHeading:   THeadingCopyObject(TheCopyObject.Data).Free;
    ctSelection: ; // TODO : Assign when TSelectionCopyObject is working.;
  end;
  inherited Destroy;
end;

function TCopyObject.ContainsControl(const Ctrl: TControl): boolean;
var
  EpiCtrl: TEpiCustomControlItem;
begin
  case CopyType of
    ctField:     EpiCtrl := TFieldCopyObject(Data).Field;
    ctSection:   EpiCtrl := TSectionCopyObject(Data).Section;
    ctHeading:   EpiCtrl := THeadingCopyObject(Data).Heading;
    ctSelection: ;
  end;
  result := (Ctrl as IDesignEpiControl).EpiControl = EpiCtrl;
end;

{ TFieldCopyObject }

constructor TFieldCopyObject.Create(const Field: TEpiField);
begin
  FField := Field;
  FField.RegisterOnChangeHook(@TheDestroyer.CustomItemHook);
end;

destructor TFieldCopyObject.Destroy;
begin
  FField.UnRegisterOnChangeHook(@TheDestroyer.CustomItemHook);
  inherited Destroy;
end;

{ TSectionCopyObject }


constructor TSectionCopyObject.Create(const Section: TEpiSection);
begin
  FSection := Section;
  FSection.RegisterOnChangeHook(@TheDestroyer.CustomItemHook);
end;

destructor TSectionCopyObject.Destroy;
begin
  FSection.UnRegisterOnChangeHook(@TheDestroyer.CustomItemHook);
  inherited Destroy;
end;

{ THeadingCopyObject }

constructor THeadingCopyObject.Create(const Heading: TEpiHeading);
begin
  FHeading := Heading;
  FHeading.RegisterOnChangeHook(@TheDestroyer.CustomItemHook);
end;

destructor THeadingCopyObject.Destroy;
begin
  FHeading.UnRegisterOnChangeHook(@TheDestroyer.CustomItemHook);
  inherited Destroy;
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

