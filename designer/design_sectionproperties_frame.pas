unit design_sectionproperties_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, StdCtrls, Buttons,
  epicustombase, epidatafiles, epiadmin, design_propertiesbase_frame;


type

  { TSectionPropertiesFrameOld }

  TSectionPropertiesFrameOld = class(TDesignPropertiesFrame)
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
    Label9: TLabel;
    SectionBasicSheet: TTabSheet;
    SectionGroupAccessGroupBox: TGroupBox;
    NameEdit: TEdit;
    CaptionEdit: TEdit;
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
    procedure UpdateCaption(const S: String); override;
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

{ TSectionPropertiesFrameOld }

procedure TSectionPropertiesFrameOld.GrpRightsMoveRightClick(Sender: TObject);
var
  Grp: TEpiGroup;
  Idx: LongInt;
begin
  if GroupAvailableListBox.ItemIndex < 0 then exit;

  Idx := GroupAvailableListBox.ItemIndex;
  Grp := TEpiGroup(GroupAvailableListBox.Items.Objects[Idx]);
  GroupAssignedListBox.Items.AddObject(Grp.Caption.Text, Grp);
  GroupAvailableListBox.Items.Delete(Idx);
end;

function TSectionPropertiesFrameOld.GetSection: TEpiSection;
begin
  result := TEpiSection(EpiControl);
end;

procedure TSectionPropertiesFrameOld.GrpRightsMoveLeftClick(Sender: TObject);
var
  Idx: LongInt;
  Grp: TEpiGroup;
begin
  if GroupAssignedListBox.ItemIndex < 0 then exit;

  Idx := GroupAssignedListBox.ItemIndex;
  Grp := TEpiGroup(GroupAssignedListBox.Items.Objects[Idx]);
  GroupAvailableListBox.Items.AddObject(Grp.Caption.Text, Grp);
  GroupAssignedListBox.Items.Delete(Idx);
end;

procedure TSectionPropertiesFrameOld.SetEpiControl(
  const AValue: TEpiCustomControlItem);
begin
  inherited SetEpiControl(AValue);
  UpdateFormContent;
end;

procedure TSectionPropertiesFrameOld.ShiftToTabSheet(const SheetNo: Byte);
begin
  if SheetNo = 0 then exit;
  if SheetNo > SectionPageControl.PageCount then exit;
  SectionPageControl.ActivePageIndex := SheetNo - 1;
end;

procedure TSectionPropertiesFrameOld.UpdateCaption(const S: String);
var
  T: String;
begin
  T := 'Section Properties: ' + Section.Name;
  inherited UpdateCaption(T);
end;

constructor TSectionPropertiesFrameOld.Create(TheOwner: TComponent;
  AAdmin: TEpiAdmin);
begin
  inherited Create(TheOwner);
  FAdmin := AAdmin;
end;

function TSectionPropertiesFrameOld.ValidateControl: boolean;

function DoError(Const Msg: string; Ctrl: TWinControl): boolean;
  var
    P: TWinControl;
  begin
    P := Ctrl.Parent;
    while not (P is TTabSheet) do
      P := P.Parent;
    TPageControl(P.Parent).ActivePage := TTabSheet(P);
    Ctrl.SetFocus;
    ShowHintMsg(Msg, Ctrl);
    Result := false;
  end;

var
  FSection: TEpiSection;
  i: Integer;
begin
  result := false;

  FSection := TEpiSection(EpiControl);
  try
    FSection.BeginUpdate;

    // Basic Page
    if (not FSection.ValidateRename(NameEdit.Text, true)) then
    begin
      DoError('Name already exists or invalid identifier', NameEdit);
      Exit;
    end;

    FSection.Caption.Text := CaptionEdit.Text;
    for i := 0 to GroupAssignedListBox.Count - 1 do
      if not FSection.Groups.ItemExistsByName(TEpiGroup(GroupAssignedListBox.Items.Objects[i]).Name) then
        FSection.Groups.AddItem(TEpiGroup(GroupAssignedListBox.Items.Objects[i]));

    // Advanced Page
    FSection.Width := StrToInt(WidthEdit.Text);
    FSection.Height := StrToInt(HeightEdit.Text);

  finally
    FSection.EndUpdate;
  end;

  Result := true;
  UpdateCaption('');
end;

procedure TSectionPropertiesFrameOld.UpdateFormContent;
var
  LocalGroups: TEpiGroups;
  i: Integer;
begin
  Caption := 'Section Properties';

  // Basic Page
  NameEdit.Text := Section.Name;
  NameEdit.Enabled := Section.DataFile.MainSection <> Section;

  CaptionEdit.Text := Section.Caption.Text;

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
      GroupAssignedListBox.Items.AddObject(LocalGroups[i].Caption.Text, LocalGroups[i])
    else
      GroupAvailableListBox.Items.AddObject(LocalGroups[i].Caption.Text, LocalGroups[i]);
  end;
  GroupAvailableListBox.Items.EndUpdate;
  GroupAssignedListBox.Items.EndUpdate;
  {$ENDIF}

  // Advanced Page
  WidthEdit.Text := IntToStr(Section.Width);
  HeightEdit.Text := IntToStr(Section.Height);
  UpdateCaption('');
end;

procedure TSectionPropertiesFrameOld.ForceShow;
begin
  inherited ForceShow;
  CaptionEdit.SetFocus;
end;

end.

