unit admin_group_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, CheckBoxThemed, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, StdCtrls, EditBtn, Buttons, ComCtrls, CheckLst, epiadmin;

type

  { TAdminGroupForm }

  TAdminGroupForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Panel1: TPanel;
    ProjectContentDesignChkGrp: TCheckGroup;
    AssignProjectrightsChkGrp: TCheckGroup;
    UserManagementChkGrp: TCheckGroup;
    DataAccessChkGrp: TCheckGroup;
    CaptionEdit: TEdit;
    Label3: TLabel;
    Panel2: TPanel;
    procedure BitBtn1Click(Sender: TObject);
  private
    FGroup: TEpiGroup;
    procedure FillRights;
    procedure FormShow(Sender: TObject);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    property Group: TEpiGroup read FGroup write FGroup;
  end;

var
  AdminGroupForm: TAdminGroupForm;

implementation

{$R *.lfm}

uses
  admin_authenticator;

{ TAdminGroupForm }

procedure TAdminGroupForm.BitBtn1Click(Sender: TObject);

  procedure AccumulateRights(var Rights: TEpiManagerRights; Const ChkGrp: TCheckGroup);
  var
    i: Integer;
    Item: TEpiManagerRight;
  begin
    for i := 0 to ChkGrp.Items.Count - 1 do
    begin
      Item := TEpiManagerRight(PtrInt(ChkGrp.Items.Objects[I]));
      if ChkGrp.Checked[i] then
        Include(Rights, Item);
    end;
  end;

var
  lRights: TEpiManagerRights;
begin
  Group.Caption.Text := CaptionEdit.Text;

  lRights := [];
  AccumulateRights(lRights, ProjectContentDesignChkGrp);
  AccumulateRights(lRights, AssignProjectrightsChkGrp);
  AccumulateRights(lRights, UserManagementChkGrp);
  AccumulateRights(lRights, DataAccessChkGrp);

  Group.ManageRights := lRights;
end;

procedure TAdminGroupForm.FillRights;
var
  R: TEpiGroupRelation;
  P: TEpiGroupRelation;
  ParentGroup: TEpiGroup;

  procedure FillChkGrp(Const ChkGrp: TCheckGroup);
  var
    I: Integer;
    Item: TEpiManagerRight;
  begin
    for I := 0 to ChkGrp.Items.Count - 1 do
    begin
      Item := TEpiManagerRight(PtrInt(ChkGrp.Items.Objects[I]));

      ChkGrp.Checked[I]      := (Item in Group.ManageRights);

                                            // AuthedUserInGroup will return false if ParentGroup = nil,
                                            // hence evaluation after "and" is NOT done.
      ChkGrp.CheckEnabled[I] := Authenticator.AuthedUserInGroup(ParentGroup, true) and
                                            (Item in ParentGroup.ManageRights);
    end;
  end;

begin
  R := Authenticator.RelationFromGroup(Group);
  P := R.ParentRelation;

  ParentGroup := nil;
  if Assigned(P) then
    ParentGroup := P.Group;


  FillChkGrp(ProjectContentDesignChkGrp);
  FillChkGrp(AssignProjectrightsChkGrp);
  FillChkGrp(UserManagementChkGrp);
  FillChkGrp(DataAccessChkGrp);
end;

procedure TAdminGroupForm.FormShow(Sender: TObject);
begin
  CaptionEdit.Text := Group.Caption.Text;

  FillRights;

  if CaptionEdit.CanFocus then
    CaptionEdit.SetFocus;
end;

constructor TAdminGroupForm.Create(TheOwner: TComponent);
var
  Item: TEpiManagerRight;
begin
  inherited Create(TheOwner);

  ProjectContentDesignChkGrp.Items.AddObject(EpiManagerRightCaptions[earDefineProject], TObject(earDefineProject));
  ProjectContentDesignChkGrp.Items.AddObject(EpiManagerRightCaptions[earPrepareDoubleEntry], TObject(earPrepareDoubleEntry));
  ProjectContentDesignChkGrp.Items.AddObject(EpiManagerRightCaptions[earTranslate], TObject(earTranslate));

  AssignProjectrightsChkGrp.Items.AddObject(EpiManagerRightCaptions[earGroups], TObject(earGroups));

  UserManagementChkGrp.Items.AddObject(EpiManagerRightCaptions[earUsers], TObject(earUsers));
  UserManagementChkGrp.Items.AddObject(EpiManagerRightCaptions[earPassword], TObject(earPassword));

  DataAccessChkGrp.Items.AddObject(EpiManagerRightCaptions[earExport], TObject(earExport));
  DataAccessChkGrp.Items.AddObject(EpiManagerRightCaptions[earExtentendedData], TObject(earExtentendedData));
  DataAccessChkGrp.Items.AddObject(EpiManagerRightCaptions[earViewData], TObject(earViewData));
  DataAccessChkGrp.Items.AddObject(EpiManagerRightCaptions[earReport], TObject(earReport));

  OnShow := @FormShow;
end;

end.

