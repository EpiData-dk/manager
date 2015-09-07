unit admin_entryrights_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, ComCtrls, epiv_projecttreeview_frame, epidocument,
  epicustombase, design_properties_groupassign_frame, epidatafilerelations;

type

  { TDefineEntryRightsForm }

  TDefineEntryRightsForm = class(TForm)
    Panel4: TPanel;
    BitBtn1: TBitBtn;
    Panel2: TPanel;
    Splitter1: TSplitter;
    Panel3: TPanel;
    procedure ProjectTreeDelete(const Relation: TEpiMasterRelation);
  private
    FDocument: TEpiDocument;
    procedure SetDocument(AValue: TEpiDocument);


  { Project Tree View }
  private
    FProjectTreeView: TEpiVProjectTreeViewFrame;

    { Events }
    procedure ProjectTreeNodeSelected(Sender: TObject;
      const AObject: TEpiCustomBase; ObjectType: TEpiVTreeNodeObjectType);
    procedure ProjectTreeNodeSelecting(Sender: TObject; const OldObject,
      NewObject: TEpiCustomBase; OldObjectType,
      NewObjectType: TEpiVTreeNodeObjectType; var Allowed: Boolean);

  { Group Rights }
  private
    FGroupRightsFrame: TGroupsAssignFrame;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property Document: TEpiDocument read FDocument write SetDocument;
  end;

procedure ShowDefineEntryRightsForm(Owner: TComponent; Document: TEpiDocument);

implementation

{$R *.lfm}

uses
  Themes, epiadmin;

var
  DefineEntryRightsForm: TDefineEntryRightsForm = nil;

procedure ShowDefineEntryRightsForm(Owner: TComponent; Document: TEpiDocument);
begin
  if (not Assigned(DefineEntryRightsForm)) then
    DefineEntryRightsForm := TDefineEntryRightsForm.Create(Owner);

  DefineEntryRightsForm.Document := Document;
  DefineEntryRightsForm.Show;
end;

{ TDefineEntryRightsForm }

procedure TDefineEntryRightsForm.ProjectTreeDelete(
  const Relation: TEpiMasterRelation);
begin
  //
end;

procedure TDefineEntryRightsForm.SetDocument(AValue: TEpiDocument);
begin
  if FDocument = AValue then Exit;
  FDocument := AValue;

  FProjectTreeView.AddDocument(FDocument);
  FGroupRightsFrame.Admin := FDocument.Admin;
  FProjectTreeView.SelectedObject := FDocument.Relations[0];
end;

procedure TDefineEntryRightsForm.ProjectTreeNodeSelected(Sender: TObject;
  const AObject: TEpiCustomBase; ObjectType: TEpiVTreeNodeObjectType);
var
  MR: TEpiMasterRelation;
begin
  MR := TEpiMasterRelation(AObject);
  FGroupRightsFrame.DataFileRelation := MR;
end;

procedure TDefineEntryRightsForm.ProjectTreeNodeSelecting(Sender: TObject;
  const OldObject, NewObject: TEpiCustomBase; OldObjectType,
  NewObjectType: TEpiVTreeNodeObjectType; var Allowed: Boolean);
begin
  Allowed := true;
end;

constructor TDefineEntryRightsForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FProjectTreeView := TEpiVProjectTreeViewFrame.Create(Self);
  with FProjectTreeView do
  begin
    ShowCheckBoxes := false;
    ShowHint := false;
    ShowProject := false;
    ShowRecordCount := false;

    MinDocumentCount := 1;
    MaxDocumentCount := 1;

    AllowSelectProject := false;
    DisplayMode := pdmCommon;
    EditCaption := false;
    EditStructure := false;

    OnDelete := @ProjectTreeDelete;
    OnTreeNodeSelected := @ProjectTreeNodeSelected;
    OnTreeNodeSelecting := @ProjectTreeNodeSelecting;

    Align := alClient;
    Parent := Panel2;
  end;

  FGroupRightsFrame := TGroupsAssignFrame.Create(Self);
  with FGroupRightsFrame do
  begin
    Align := alClient;
    Parent := Panel3;
  end;
end;

destructor TDefineEntryRightsForm.Destroy;
begin
  DefineEntryRightsForm := nil;
  inherited Destroy;
end;

end.

