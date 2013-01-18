unit design_control_section;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, epicustombase, epidatafiles, design_types,
  Forms, Controls;

type
  { TDesignSection }

  TDesignSection = Class(TGroupBox, IDesignEpiControl)
  private
    FSection: TEpiSection;
    function GetTotalWidth: integer;
    procedure OnChange(Sender: TObject; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
    procedure UpdateHint;
    procedure UpdateEpiControl;
    function  GetEpiControl: TEpiCustomControlItem;
    procedure SetEpiControl(const AValue: TEpiCustomControlItem);
    procedure ReadSection(Stream: TStream);
    procedure WriteSection(Stream: TStream);
  protected
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure SetParent(NewParent: TWinControl); override;
  public
    constructor Create(AOwner: TComponent); Override;
    destructor Destroy; override;
    function  DesignFrameClass: TCustomFrameClass;
    procedure UpdateControl;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: integer); override;
    procedure FixupCopyControl;
    property  EpiControl: TEpiCustomControlItem read GetEpiControl write SetEpiControl;
    property  TotalWidth: integer read GetTotalWidth;
  end;

implementation

uses
  settings2_var,
  design_properties_sectionframe, epistringutils,
  manager_globals;

{ TDesignSection }

function TDesignSection.GetEpiControl: TEpiCustomControlItem;
begin
  Result := FSection;
end;

procedure TDesignSection.ReadSection(Stream: TStream);
var
  CopySection: TEpiSection;
begin
  Stream.Read(CopySection, SizeOf(Pointer));
  FSection := TEpiSection(CopySection.Clone(nil));
end;

procedure TDesignSection.WriteSection(Stream: TStream);
var
  CopySection: TEpiSection;
begin
  CopySection := TEpiSection(FSection.Clone(nil));
  CopySection.Fields.ClearAndFree;
  CopySection.Headings.ClearAndFree;
  GlobalCopyList.Add(CopySection);
  Stream.Write(CopySection, Sizeof(Pointer));
end;

procedure TDesignSection.OnChange(Sender: TObject; EventGroup: TEpiEventGroup;
  EventType: Word; Data: Pointer);
begin
  case TEpiCustomChangeEventType(EventType) of
    ecceDestroy:
      begin
        FSection.UnRegisterOnChangeHook(@OnChange);
        FSection := nil;
      end;
    ecceName: ;
    ecceAddItem: ;
    ecceDelItem: ;
    ecceSetItem: ;
    ecceSetTop: ;
    ecceSetLeft: ;
    ecceUpdate,
    ecceText:
      begin
        Caption := TEpiSection(Sender).Caption.Text;
        UpdateControl;
      end;
  end;
end;

function TDesignSection.GetTotalWidth: integer;
begin
  result := Width;
end;

procedure TDesignSection.SetEpiControl(const AValue: TEpiCustomControlItem);
begin
  FSection := TEpiSection(Avalue);
  FSection.RegisterOnChangeHook(@OnChange);
  FSection.AddCustomData(DesignControlCustomDataKey, self);
  UpdateEpiControl;
end;

procedure TDesignSection.UpdateHint;
begin

end;

procedure TDesignSection.UpdateEpiControl;
begin
  if not Assigned(FSection) then exit;

  with FSection do
  begin
    BeginUpdate;
    Top := Self.Top;
    Left := Self.Left;
    Width := Self.Width;
    Height := Self.Height;
    EndUpdate;
  end;
end;

procedure TDesignSection.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I : Integer;
  Control : TControl;
begin
  for I := 0 to ControlCount-1 do
    if Supports(Controls[i], IDesignEpiControl) then
      Proc(Controls[i]);
end;

procedure TDesignSection.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('EpiSection', @ReadSection, @WriteSection, Assigned(FSection));
end;

procedure TDesignSection.SetParent(NewParent: TWinControl);
var
  FixupEpiControl: Boolean;
  DataFile: TEpiDataFile;
  i: Integer;
begin
  FixupEpiControl := false;
  if (Parent = nil) and
     (NewParent <> nil) and
     (FSection <> nil)
  then
    FixupEpiControl := true;

  // On Pasting we could recieve a NewParent = TDesignSection
  // => not allowed, try with the NewParent.Parent!
  if (NewParent is TDesignSection) then
    NewParent := NewParent.Parent;

  inherited SetParent(NewParent);
  if [csDestroying, csLoading] * ComponentState <> [] then exit;

  if FixupEpiControl then
    begin
      DataFile := TEpiSection((NewParent as IDesignEpiControl).EpiControl).DataFile;
      if not Datafile.ValidateRename(FSection.Name) then
        FSection.Name := DataFile.Sections.GetUniqueItemName(TEpiSection);
      DataFile.Sections.AddItem(FSection);

      for i := 0 to ControlCount - 1 do
        if Supports(Controls[i], IDesignEpiControl) then
          (Controls[i] as IDesignEpiControl).FixupCopyControl;

      SetEpiControl(FSection);
    end;
end;

constructor TDesignSection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ShowHint := true;
  ParentColor := true;
  Font.Assign(ManagerSettings.SectionFont);
  Caption := '';
end;

destructor TDesignSection.Destroy;
begin
  if Assigned(FSection) then
    begin
      FSection.UnRegisterOnChangeHook(@OnChange);
      FSection.Free;
    end;
  inherited Destroy;
end;

function TDesignSection.DesignFrameClass: TCustomFrameClass;
begin
  result := TSectionPropertiesFrame;
end;

procedure TDesignSection.UpdateControl;
begin
  Font.Assign(ManagerSettings.SectionFont);
end;

procedure TDesignSection.SetBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  UpdateEpiControl;
end;

procedure TDesignSection.FixupCopyControl;
begin
  // Do nothing - untill we decide to implement Section-in-Section.
end;

initialization
  RegisterClass(TDesignSection);

end.

