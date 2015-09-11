unit admin_authenticator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epiopenfile, epiadmin, epirights, epicustombase, epidocument,
  epidatafiles;

type

  { TAuthenticator }

  TAuthenticator = class
  private
    FDocument: TEpiDocument;
    FDocumentFile: TEpiDocumentFile;
    function GetAdmin: TEpiAdmin;
    function GetAuthedUser: TEpiUser;
    procedure InitGroupWalk(
        Const Relation: TEpiGroupRelation;
        Const Depth: Cardinal;
        Const Index: Cardinal;
        var aContinue: boolean;
        Data: Pointer = nil);
    procedure NewRelationHook(const Sender: TEpiCustomBase;
      const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
    procedure DocumentHook(const Sender: TEpiCustomBase;
      const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
  public
    constructor Create(Const ADocumentFile: TEpiDocumentFile);
    destructor Destroy; override;
    // Check if Master user is in a higher group than OtherUser.
    //   If AllGroups = false: Returns true if at least one such hierachy is found.
    //   If AllGroups = true:  Returns true if all such hierachies are found.
    function    CheckUserHierachy(Const MasterUser, OtherUser: TEpiUser;
      Const AllGroups: boolean): boolean; overload;
    // Check if Authenticated user is in a higher group than OtherUser.
    //   If AllGroups = false: Returns true if at least one such hierachy is found.
    //   If AllGroups = true:  Returns true if all such hierachies are found.
    function    CheckAuthedUserHierachy(Const OtherUser: TEpiUser;
      Const AllGroups: boolean): boolean; overload;

    // Check if Master is higher in the hierachy that Other user, just
    // for this group. MasterUser MUST belong (or be in a higher group) to Group.
    function    CheckUserHierachy(Const MasterUser, OtherUser: TEpiUser;
      Const Group: TEpiGroup): boolean; overload;
    function    CheckAuthedUserHierachy(Const OtherUser: TEpiUser;
      Const Group: TEpiGroup): boolean; overload;

    function    IsAuthorized(Const RequiredRights: TEpiManagerRights): Boolean;
    function    UserInGroup(Const User: TEpiUser; Const Group: TEpiGroup;
        Const CheckInheritance: Boolean; Const InhertanceUpTree: boolean = true): boolean;
    function    AuthedUserInGroup(Const Group: TEpiGroup; Const CheckInheritance: boolean;
        Const InhertanceUpTree: boolean = true): boolean;
    function    RelationFromGroup(Const Group: TEpiGroup): TEpiGroupRelation;

    // Get a list of effective rights for a user based on all groups
    // - if abbriviated, the list will contain a comma delimited list of single characters
    function  PrintUserRights(Const User: TEpiUser; Abbriviated: boolean): string;

    // Get a list of rights for a group
    // - if abbriviated, the list will contain a comma delimited list of single characters
    function  PrintGroupRights(Const Group: TEpiGroup; Abbriviated: boolean): string;
  public
    property DocumentFile: TEpiDocumentFile read FDocumentFile;
    property AuthedUser: TEpiUser read GetAuthedUser;
    property Admin: TEpiAdmin read GetAdmin;
  end;

var
  Authenticator: TAuthenticator;


implementation

uses
  epicustomrelations, epigrouprelation_helper;

const
  AUTH_GROUP_KEY = 'AUTH_GROUP_KEY';

{ TAuthenticator }

function TAuthenticator.GetAdmin: TEpiAdmin;
begin
  result := FDocumentFile.Document.Admin;
end;

function TAuthenticator.GetAuthedUser: TEpiUser;
begin
  result := nil;
  if (Self = nil) then exit;

  if Assigned(DocumentFile) then
    result := DocumentFile.AuthedUser;
end;

function TAuthenticator.RelationFromGroup(const Group: TEpiGroup
  ): TEpiGroupRelation;
begin
  result := TEpiGroupRelation(Group.FindCustomData(AUTH_GROUP_KEY));
end;

function TAuthenticator.PrintUserRights(const User: TEpiUser;
  Abbriviated: boolean): string;
var
  G: TEpiGroup;
  Rights: TEpiManagerRights;
  R: TEpiManagerRight;
begin
  for G in User.Groups do
    Rights := Rights + G.ManageRights;

  Result := '';
  if Abbriviated then
    for R in Rights do
      Result += EpiManagerRightCaptionsShort[R] + ', '
  else
    for R in Rights do
      Result += EpiManagerRightCaptions[R] + LineEnding;

  if Abbriviated then
    Delete(Result, Length(Result) - 1, 2)
  else
    Result := TrimRight(Result);
end;

function TAuthenticator.PrintGroupRights(const Group: TEpiGroup;
  Abbriviated: boolean): string;
var
  R: TEpiManagerRight;
begin
  Result := '';
  if Abbriviated then
    for R in Group.ManageRights do
      Result += EpiManagerRightCaptionsShort[R] + ', '
  else
    for R in Group.ManageRights do
      Result += EpiManagerRightCaptions[R] + LineEnding;

  if Abbriviated then
    Delete(Result, Length(Result) - 1, 2)
  else
    Result := TrimRight(Result);
end;

procedure TAuthenticator.InitGroupWalk(const Relation: TEpiGroupRelation;
  const Depth: Cardinal; const Index: Cardinal; var aContinue: boolean;
  Data: Pointer);
begin
  Relation.Group.AddCustomData(AUTH_GROUP_KEY, Relation);
end;

procedure TAuthenticator.NewRelationHook(const Sender: TEpiCustomBase;
  const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
var
  Group: TEpiGroup;
begin
  if not (Initiator is TEpiGroupRelation) then exit;
  if (EventGroup <> eegRelations) then exit;
  if (TEpiCustomRelationEvent(EventType) <> ecreAssignObject) then exit;

  Group := TEpiGroup(PEpiCustomRelationAssignObjectData(Data)^.ObjectData);

  if Assigned(Group) then
    Group.AddCustomData(AUTH_GROUP_KEY, Initiator);
end;

procedure TAuthenticator.DocumentHook(const Sender: TEpiCustomBase;
  const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
begin
  if (Initiator <> FDocument) then exit;
  if (EventGroup <> eegCustomBase) then exit;
  if (TEpiCustomChangeEventType(EventType) <> ecceDestroy) then exit;

  Self.Free;
end;

constructor TAuthenticator.Create(const ADocumentFile: TEpiDocumentFile);
var
  R: TEpiGroupRelation;
begin
  FDocumentFile := ADocumentFile;
  FDocument     := FDocumentFile.Document;

  FDocument.RegisterOnChangeHook(@DocumentHook, true);

  R := DocumentFile.Document.Admin.AdminRelation;
  R.Group.AddCustomData(AUTH_GROUP_KEY, R);

  R.GroupRelations.OrderedWalk(@InitGroupWalk, nil);
  R.RegisterOnChangeHook(@NewRelationHook, true);
end;

destructor TAuthenticator.Destroy;
begin
  Authenticator := nil;
  FDocument.UnRegisterOnChangeHook(@DocumentHook);
  inherited Destroy;
end;

function TAuthenticator.CheckUserHierachy(const MasterUser,
  OtherUser: TEpiUser; const AllGroups: boolean): boolean;
var
  G: TEpiGroup;
begin
  if AllGroups then
    begin
      Result := true;
      for G in OtherUser.Groups do
        result := result and
                  UserInGroup(MasterUser, G, true);
    end
  else
    begin
      // If Other user is not member of a group, ALL other users
      // are potential "owners".
      result := (OtherUser.Groups.Count = 0);
      for G in OtherUser.Groups do
        result := result or
                  UserInGroup(MasterUser, G, true);
    end;
end;

function TAuthenticator.CheckAuthedUserHierachy(const OtherUser: TEpiUser;
  const AllGroups: boolean): boolean;
begin
  result := CheckUserHierachy(AuthedUser, OtherUser, AllGroups);
end;

function TAuthenticator.CheckUserHierachy(const MasterUser,
  OtherUser: TEpiUser; const Group: TEpiGroup): boolean;
begin
  result := UserInGroup(MasterUser, Group, true, true) and
            UserInGroup(OtherUser, Group, true, false);
end;

function TAuthenticator.CheckAuthedUserHierachy(const OtherUser: TEpiUser;
  const Group: TEpiGroup): boolean;
begin
  result := CheckUserHierachy(AuthedUser, OtherUser, Group);
end;

function TAuthenticator.IsAuthorized(const RequiredRights: TEpiManagerRights
  ): Boolean;
begin
  result := true;
  if (Self = nil) then exit;

  if not Assigned(AuthedUser) then exit;

  result := AuthedUser.Groups.HasRights(RequiredRights);
end;

function TAuthenticator.UserInGroup(const User: TEpiUser;
  const Group: TEpiGroup; const CheckInheritance: Boolean;
  const InhertanceUpTree: boolean): boolean;
var
  R: TEpiGroupRelation;
begin
  result := false;
  if (Self = nil) then exit;

  if (not Assigned(User)) then
    Exit;

  if (not Assigned(Group)) then
    Exit;

  result := (User.Groups.IndexOf(Group) >= 0);

  if CheckInheritance then
  begin
    if InhertanceUpTree
    then
      begin
        R := RelationFromGroup(Group).ParentRelation;

        if Assigned(R) then
          Result := Result or UserInGroup(User, R.Group, CheckInheritance, InhertanceUpTree);
      end
    else
      begin
        for R in RelationFromGroup(Group).GroupRelations do
          Result := Result or UserInGroup(User, R.Group, CheckInheritance, InhertanceUpTree);

      end;
  end;
end;

function TAuthenticator.AuthedUserInGroup(const Group: TEpiGroup;
  const CheckInheritance: boolean; const InhertanceUpTree: boolean): boolean;
begin
  result := false;
  if (Self = nil) then exit;

  if (not Assigned(AuthedUser)) then
    Exit;

  Result := UserInGroup(AuthedUser, Group, CheckInheritance, InhertanceUpTree);
end;

end.

