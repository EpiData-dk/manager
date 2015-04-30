unit admin_authenticator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epiopenfile, epiadmin;

type

  { TAuthenticator }

  TAuthenticator = class
  private
    FDocumentFile: TEpiDocumentFile;
    function GetAuthedUser: TEpiUser;
    function RelationFromGroup(Const Group: TEpiGroup): TEpiGroupRelation;
    procedure InitGroupWalk(
        Const Relation: TEpiGroupRelation;
        Const Depth: Cardinal;
        Const Index: Cardinal;
        var aContinue: boolean;
        Data: Pointer = nil);
  public
    constructor Create(Const ADocumentFile: TEpiDocumentFile);
    function    IsAuthorized(Const RequiredRights: TEpiManagerRights): Boolean;
    function    UserInGroup(Const Group: TEpiGroup; Const CheckInheritance: boolean): boolean;

  public
    property DocumentFile: TEpiDocumentFile read FDocumentFile;
    property AuthedUser: TEpiUser read GetAuthedUser;
  end;

var
  Authenticator: TAuthenticator;



implementation

uses
  epigrouprelation_helper;

const
  AUTH_GROUP_KEY = 'AUTH_GROUP_KEY';

{ TAuthenticator }

function TAuthenticator.GetAuthedUser: TEpiUser;
begin
  result := DocumentFile.AuthedUser;
end;

function TAuthenticator.RelationFromGroup(const Group: TEpiGroup
  ): TEpiGroupRelation;
begin
  result := TEpiGroupRelation(Group.FindCustomData(AUTH_GROUP_KEY));
end;

procedure TAuthenticator.InitGroupWalk(const Relation: TEpiGroupRelation;
  const Depth: Cardinal; const Index: Cardinal; var aContinue: boolean;
  Data: Pointer);
begin
  Relation.Group.AddCustomData(AUTH_GROUP_KEY, Relation);
end;

constructor TAuthenticator.Create(const ADocumentFile: TEpiDocumentFile);
var
  R: TEpiGroupRelation;
begin
  FDocumentFile := ADocumentFile;

  R := DocumentFile.Document.Admin.AdminRelation;
  R.Group.AddCustomData(AUTH_GROUP_KEY, R);

  R.GroupRelations.OrderedWalk(@InitGroupWalk, nil);
end;

function TAuthenticator.IsAuthorized(const RequiredRights: TEpiManagerRights
  ): Boolean;
begin
  result := true;
  if not Assigned(AuthedUser) then exit;

  result := AuthedUser.Groups.HasRights(RequiredRights);
end;

function TAuthenticator.UserInGroup(const Group: TEpiGroup;
  const CheckInheritance: boolean): boolean;
var
  Parent: TEpiGroupRelation;
begin
  result := false;

  if (not Assigned(AuthedUser)) then
    Exit;

  result := (AuthedUser.Groups.IndexOf(Group) >= 0);

  if CheckInheritance then
  begin
    Parent := RelationFromGroup(Group).ParentRelation;

    if Assigned(Parent) then
      Result := Result or UserInGroup(Parent.Group, CheckInheritance);
  end;
end;

end.

