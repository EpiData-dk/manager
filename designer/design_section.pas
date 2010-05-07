unit design_section;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, ExtCtrls, epidatafiles, epicustombase, design_custombase;

type

  { TDesignSection }

  TDesignSection = Class(TGroupBox, IDesignEpiControl)
  private
    FSection: TEpiSection;
    function GetEpiControl: TEpiCustomControlItem;
    procedure SetEpiControl(const AValue: TEpiCustomControlItem);
    procedure OnChange(Sender: TObject; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
    procedure UpdateHint;
  public
    constructor Create(AOwner: TComponent); Override;
    destructor Destroy; override;
    property EpiControl: TEpiCustomControlItem read GetEpiControl write SetEpiControl;
  end;

  { TDesignSectionForm }

  TDesignSectionForm = class(TDesignCustomForm)
    OkBtn: TBitBtn;
    CancelBtn: TBitBtn;
    GroupBox1: TGroupBox;
    GrpRightsMoveLeft: TSpeedButton;
    GrpRightsMoveRight: TSpeedButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    NameEdit: TEdit;
    IdEdit: TEdit;
    GroupAvailableListBox: TListBox;
    GroupAssignedListBox: TListBox;
    Panel1: TPanel;
  private
    { private declarations }
    FSection: TEpiSection;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure GrpRightsMoveLeftClick(Sender: TObject);
    procedure GrpRightsMoveRightClick(Sender: TObject);
  protected
    function GetEpiControl: TEpiCustomControlItem; override;
    procedure SetEpiControl(const AValue: TEpiCustomControlItem); override;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end; 

var
  DesignSectionForm: TDesignSectionForm;

implementation

{$R *.lfm}

uses
  epidocument, epiadmin;

{ TDesignSection }

function TDesignSection.GetEpiControl: TEpiCustomControlItem;
begin
  result := FSection;
end;

procedure TDesignSection.SetEpiControl(const AValue: TEpiCustomControlItem);
begin
  FSection := TEpiSection(AValue);
  FSection.RegisterOnChangeHook(@OnChange);
  Name := FSection.Id;
  Caption := '';
end;

procedure TDesignSection.OnChange(Sender: TObject; EventGroup: TEpiEventGroup;
  EventType: Word; Data: Pointer);
begin
  case EventGroup of
    eegCustomBase:
      case TEpiCustomChangeEventType(EventType) of
        ecceSetLeft: Left := FSection.Left;
        ecceSetTop:  Top  := FSection.Top;
        ecceText:    Caption := EpiTextToControlText(FSection.Name.Text);
        ecceUpdate:
          begin
            Left := FSection.Left;
            Top  := FSection.Top;
            Caption := EpiTextToControlText(FSection.Name.Text);
            Width := FSection.Width;
            Height := FSection.Height;
          end;
      end;
    eegSections:
      begin
        Case TEpiSectionsChangeEventType(EventType) of
          esceWidth:  Width := FSection.Width;
          esceHeight: Height := FSection.Height;
        end;
      end;
  end;
  UpdateHint;
end;

procedure TDesignSection.UpdateHint;
var
  S: String;
  i: Integer;
begin
  S := '';
  for i := 0 to FSection.Groups.Count - 1 do
    S += FSection.Groups[i].Name.Text + ',';
  Delete(S, Length(S), 1);

  With FSection do
    Hint := WideFormat(
      'Id: %s' + LineEnding +
      'Name: %s' + LineEnding +
      'Groups: %s' + LineEnding +
      'X: %d, Y: %d' + LineEnding +
      'W: %d, H: %d',
      [UTF8Decode(Id), UTF8Decode(Name.Text), UTF8Decode(S),
       Left, Top, Width, Height]
    );
end;

constructor TDesignSection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DragKind := dkDock;
  DragMode := dmAutomatic;
  DockSite := true;
  ShowHint := true;
  ParentColor := false;
end;

destructor TDesignSection.Destroy;
begin
  inherited Destroy;
end;

{ TDesignSectionForm }

procedure TDesignSectionForm.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
var
  i: Integer;
begin
  if ModalResult <> mrOK then exit;

  FSection.BeginUpdate;

  FSection.Id := IdEdit.Text;
  FSection.Name.Text := NameEdit.Text;
  for i := 0 to GroupAssignedListBox.Count - 1 do
    if not FSection.Groups.ItemExistsById(TEpiGroup(GroupAssignedListBox.Items.Objects[i]).Id) then
      FSection.Groups.AddItem(TEpiGroup(GroupAssignedListBox.Items.Objects[i]));

  FSection.EndUpdate;
end;

procedure TDesignSectionForm.GrpRightsMoveLeftClick(Sender: TObject);
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

procedure TDesignSectionForm.GrpRightsMoveRightClick(Sender: TObject);
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

function TDesignSectionForm.GetEpiControl: TEpiCustomControlItem;
begin
  Result := FSection;
end;

procedure TDesignSectionForm.SetEpiControl(const AValue: TEpiCustomControlItem
  );
var
  LocalGroups: TEpiGroups;
  i: Integer;
begin
  FSection := TEpiSection(AValue);

  IdEdit.Text := FSection.Id;
  if FSection.DataFile.MainSection = FSection then
    IdEdit.Enabled := false
  else
    IdEdit.Enabled := true;
  NameEdit.Text := FSection.Name.Text;

  LocalGroups := TEpiDocument(FSection.RootOwner).Admin.Groups;
  for i := 0 to LocalGroups.Count - 1 do
  begin
    if FSection.Groups.ItemExistsById(LocalGroups[i].Id) then
      GroupAssignedListBox.Items.AddObject(LocalGroups[i].Name.Text, LocalGroups[i])
    else
      GroupAvailableListBox.Items.AddObject(LocalGroups[i].Name.Text, LocalGroups[i]);
  end;
end;

constructor TDesignSectionForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  OnCloseQuery := @FormCloseQuery;
  GrpRightsMoveLeft.OnClick := @GrpRightsMoveLeftClick;
  GrpRightsMoveRight.OnClick := @GrpRightsMoveRightClick;
end;

destructor TDesignSectionForm.Destroy;
begin
  inherited Destroy;
end;

end.

