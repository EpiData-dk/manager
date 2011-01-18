unit design_sectionproperties_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, StdCtrls, Buttons,
  design_controls, epicustombase, epidatafiles, epiadmin;


type

  { TSectionPropertiesFrame }

  TSectionPropertiesFrame = class(TDesignPropertiesFrame)
    GroupAssignedListBox: TListBox;
    GroupAvailableListBox: TListBox;
    GrpRightsMoveLeft: TSpeedButton;
    GrpRightsMoveRight: TSpeedButton;
    HeightEdit: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    SectionAdvancedSheet: TTabSheet;
    SectionBasicSheet: TTabSheet;
    SectionGroupAccessGroupBox: TGroupBox;
    SectionNameEdit: TEdit;
    SectionPageControl: TPageControl;
    WidthEdit: TEdit;
    procedure GrpRightsMoveLeftClick(Sender: TObject);
    procedure GrpRightsMoveRightClick(Sender: TObject);
  private
    { private declarations }
    FAdmin: TEpiAdmin;
    function GetSection: TEpiSection;
  protected
    procedure SetEpiControl(const AValue: TEpiCustomControlItem); override;
    procedure ShiftToTabSheet(const SheetNo: Byte); override;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; AAdmin: TEpiAdmin);
    function    ValidateControl: boolean; override;
    procedure   UpdateFormContent; override;
    procedure   ForceShow; override;
    property    Section: TEpiSection read GetSection;
  end;

implementation

{$R *.lfm}

{ TSectionPropertiesFrame }

procedure TSectionPropertiesFrame.GrpRightsMoveRightClick(Sender: TObject);
var
  Grp: TEpiGroup;
  Idx: LongInt;
begin
  if GroupAvailableListBox.ItemIndex < 0 then exit;

  Idx := GroupAvailableListBox.ItemIndex;
  Grp := TEpiGroup(GroupAvailableListBox.Items.Objects[Idx]);
  GroupAssignedListBox.Items.AddObject(Grp.Name.Text, Grp);
  GroupAvailableListBox.Items.Delete(Idx);
end;

function TSectionPropertiesFrame.GetSection: TEpiSection;
begin
  result := TEpiSection(EpiControl);
end;

procedure TSectionPropertiesFrame.GrpRightsMoveLeftClick(Sender: TObject);
var
  Idx: LongInt;
  Grp: TEpiGroup;
begin
  if GroupAssignedListBox.ItemIndex < 0 then exit;

  Idx := GroupAssignedListBox.ItemIndex;
  Grp := TEpiGroup(GroupAssignedListBox.Items.Objects[Idx]);
  GroupAvailableListBox.Items.AddObject(Grp.Name.Text, Grp);
  GroupAssignedListBox.Items.Delete(Idx);
end;

procedure TSectionPropertiesFrame.SetEpiControl(
  const AValue: TEpiCustomControlItem);
begin
  inherited SetEpiControl(AValue);
  UpdateFormContent;
end;

procedure TSectionPropertiesFrame.ShiftToTabSheet(const SheetNo: Byte);
begin
  if SheetNo = 0 then exit;
  if SheetNo > SectionPageControl.PageCount then exit;
  SectionPageControl.ActivePageIndex := SheetNo - 1;
end;

constructor TSectionPropertiesFrame.Create(TheOwner: TComponent;
  AAdmin: TEpiAdmin);
begin
  inherited Create(TheOwner);
  FAdmin := AAdmin;
end;

function TSectionPropertiesFrame.ValidateControl: boolean;
var
  FSection: TEpiSection;
  i: Integer;
begin
  result := false;

  FSection := TEpiSection(EpiControl);
  FSection.BeginUpdate;

  // Basic Page
  FSection.Name.Text := SectionNameEdit.Text;
  for i := 0 to GroupAssignedListBox.Count - 1 do
    if not FSection.Groups.ItemExistsById(TEpiGroup(GroupAssignedListBox.Items.Objects[i]).Id) then
      FSection.Groups.AddItem(TEpiGroup(GroupAssignedListBox.Items.Objects[i]));

  // Advanced Page
  FSection.Width := StrToInt(WidthEdit.Text);
  FSection.Height := StrToInt(HeightEdit.Text);

  FSection.EndUpdate;

  Result := true;
end;

procedure TSectionPropertiesFrame.UpdateFormContent;
var
  LocalGroups: TEpiGroups;
  i: Integer;
begin
  Caption := 'Section Properties';

  // Basic Page
  SectionNameEdit.Text := Section.Name.Text;

  {$IFNDEF EPI_DEBUG}
  SectionGroupAccessGroupBox.Visible := false;
  SectionGroupAccessGroupBox.Enabled := false;
  {$ELSE}
  GroupAssignedListBox.Items.BeginUpdate;
  GroupAvailableListBox.Items.BeginUpdate;
  GroupAssignedListBox.Clear;
  GroupAvailableListBox.Clear;
  LocalGroups := FAdmin.Groups;
  for i := 0 to LocalGroups.Count - 1 do
  begin
    if Section.Groups.ItemExistsById(LocalGroups[i].Id) then
      GroupAssignedListBox.Items.AddObject(LocalGroups[i].Name.Text, LocalGroups[i])
    else
      GroupAvailableListBox.Items.AddObject(LocalGroups[i].Name.Text, LocalGroups[i]);
  end;
  GroupAvailableListBox.Items.EndUpdate;
  GroupAssignedListBox.Items.EndUpdate;
  {$ENDIF}

  // Advanced Page
  WidthEdit.Text := IntToStr(Section.Width);
  HeightEdit.Text := IntToStr(Section.Height);
end;

procedure TSectionPropertiesFrame.ForceShow;
begin
  inherited ForceShow;
  SectionNameEdit.SetFocus;
end;

end.

