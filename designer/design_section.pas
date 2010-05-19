unit design_section;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, ExtCtrls, ComCtrls, epidatafiles, epicustombase,
  design_custombase, AVL_Tree;

type

  { TDesignSection }

  TDesignSection = Class(TGroupBox, IDesignEpiControl, IPositionHandler)
  private
    // IDesignEpiControl
    FSection: TEpiSection;
    FXTreeNode: TAVLTreeNode;
    FYTreeNode: TAVLTreeNode;
    function GetEpiControl: TEpiCustomControlItem;
    function GetXTreeNode: TAVLTreeNode;
    function GetYTreeNode: TAVLTreeNode;
    procedure SetEpiControl(const AValue: TEpiCustomControlItem);
    procedure SetXTreeNode(const AValue: TAVLTreeNode);
    procedure SetYTreeNode(const AValue: TAVLTreeNode);
  private
    // IPositionHandler
    FXTree: TAVLTree;
    FYTree: TAVLTree;
    function GetXTree: TAVLTree;
    function GetYTree: TAVLTree;
  private
    procedure OnChange(Sender: TObject; EventGroup: TEpiEventGroup; EventType: Word; Data: Pointer);
    procedure UpdateHint;
  public
    constructor Create(AOwner: TComponent); Override;
    destructor Destroy; override;
    // IDesignEpiControl
    property EpiControl: TEpiCustomControlItem read GetEpiControl write SetEpiControl;
    property XTreeNode: TAVLTreeNode read GetXTreeNode write SetXTreeNode;
    property YTreeNode: TAVLTreeNode read GetYTreeNode write SetYTreeNode;
    // IPositionHandler
    property XTree: TAVLTree read GetXTree;
    property YTree: TAVLTree read GetYTree;
  end;

  { TDesignSectionForm }

  TDesignSectionForm = class(TDesignCustomForm)
    CancelBtn: TBitBtn;
    GroupBox1: TGroupBox;
    GrpRightsMoveLeft: TSpeedButton;
    GrpRightsMoveRight: TSpeedButton;
    WidthEdit: TEdit;
    HeightEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    NameEdit: TEdit;
    IdEdit: TEdit;
    GroupAvailableListBox: TListBox;
    GroupAssignedListBox: TListBox;
    OkBtn: TBitBtn;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
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

function TDesignSection.GetXTreeNode: TAVLTreeNode;
begin
  result := FXTreeNode;
end;

function TDesignSection.GetYTreeNode: TAVLTreeNode;
begin
  result := FYTreeNode;
end;

procedure TDesignSection.SetEpiControl(const AValue: TEpiCustomControlItem);
begin
  FSection := TEpiSection(AValue);
  FSection.RegisterOnChangeHook(@OnChange);
  Name := FSection.Id;
  Caption := '';
end;

procedure TDesignSection.SetXTreeNode(const AValue: TAVLTreeNode);
begin
  FXTreeNode := AValue;
end;

procedure TDesignSection.SetYTreeNode(const AValue: TAVLTreeNode);
begin
  FYTreeNode := AValue;
end;

function TDesignSection.GetXTree: TAVLTree;
begin
  result := FXTree;
end;

function TDesignSection.GetYTree: TAVLTree;
begin
  result := FYTree;
end;

procedure TDesignSection.OnChange(Sender: TObject; EventGroup: TEpiEventGroup;
  EventType: Word; Data: Pointer);
begin
  case EventGroup of
    eegCustomBase:
      case TEpiCustomChangeEventType(EventType) of
        ecceDestroy: exit;
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

  FXTree := TAVLTree.Create(@XTreeSort);
  FYTree := TAVLTree.Create(@YTreeSort);

  DragKind := dkDock;
  DragMode := dmAutomatic;
  DockSite := true;
  ShowHint := true;
  ParentColor := false;
end;

destructor TDesignSection.Destroy;
begin
  FXTree.Free;
  FYTree.Free;
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

  // Basic Page
  FSection.Id := IdEdit.Text;
  FSection.Name.Text := NameEdit.Text;
  for i := 0 to GroupAssignedListBox.Count - 1 do
    if not FSection.Groups.ItemExistsById(TEpiGroup(GroupAssignedListBox.Items.Objects[i]).Id) then
      FSection.Groups.AddItem(TEpiGroup(GroupAssignedListBox.Items.Objects[i]));

  // Advanced Page
  FSection.Width := StrToInt(WidthEdit.Text);
  FSection.Height := StrToInt(HeightEdit.Text);

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

  // Basic Page
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

  // Advanced Page
  // - use width = hieght = 0 to indicate a new section!
  if (FSection.Width = 0) and (FSection.Height = 0) then
  PageControl1.ShowTabs := false;
  PageControl1.ActivePage := TabSheet1;
  WidthEdit.Text := IntToStr(FSection.Width);
  HeightEdit.Text := IntToStr(FSection.Height);
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

