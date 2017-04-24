unit report_admin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, report_base, epidocument, epidatafiles, epidatafilerelations,
  epiopenfile, epiadmin;

type

  { TReportAdmin }

  TReportAdmin = class(TReportFileListBase)
  private
    procedure DoGroupRelation(GroupRel: TEpiGroupRelation; Lvl: Integer; var Idx: Integer);
    procedure DoAdminReport(Document: TEpiDocument);
  protected
    function GetTitle: string; override;
    procedure DoDocumentReport(const ADocumentFile: TEpiDocumentFile;
      const Index: Integer); override;
  end;


implementation

uses
  admin_authenticator, strutils, epigrouprelation_helper, epidatafilerelations_helper,
  epireport_types, epirights;

{ TReportAdmin }

procedure TReportAdmin.DoGroupRelation(GroupRel: TEpiGroupRelation;
  Lvl: Integer; var Idx: Integer);
var
  G: TEpiGroup;
  i: Integer;
begin
  G := GroupRel.Group;
  Generator.TableCell(DupeString('-', Lvl) + G.Caption.Text,   0, Idx);
  Generator.TableCell(DateTimeToStr(G.Created),                1, Idx);
  Generator.TableCell(DateTimeToStr(G.Modified),               2, Idx);
  Generator.TableCell(Authenticator.PrintGroupRights(G, true), 3, Idx);

{  S := '';
  for U in G.Users do
    S := U.Login + LineEnding;

  Generator.TableCell(S,                                       4, Idx);}

  Inc(Lvl);
  for i := 0 to GroupRel.GroupRelations.Count - 1 do
    begin
      Inc(Idx);
      DoGroupRelation(GroupRel.GroupRelation[i], Lvl, Idx);
    end;
end;

procedure TReportAdmin.DoAdminReport(Document: TEpiDocument);
var
  Groups: TEpiGroups;
  Users: TEpiUsers;
  U: TEpiUser;
  i: Integer;
  G: TEpiGroup;
  S: String;
  AdminRel, GrpRel: TEpiGroupRelation;
  DFs: TEpiDataFiles;
  DF: TEpiDataFile;
  Admin: TEpiAdmin;
  OGrps: TEpiGroupRelationList;
  GroupRights: TEpiGroupRight;
  EntryRights: TEpiEntryRights;
begin
  Admin := Document.Admin;
  Users := Admin.Users;

  Generator.TableHeader('Users', 8, Users.Count + 1);

  Generator.TableCell('Login',         0, 0);
  Generator.TableCell('Name',          1, 0);
  Generator.TableCell('Created',       2, 0);
  Generator.TableCell('Last Modified', 3, 0);
  Generator.TableCell('Last Login',    4, 0);
  Generator.TableCell('Expires',       5, 0);
  Generator.TableCell('Notes',         6, 0);
  Generator.TableCell('Groups',        7, 0);

  i := 1;
  for U in Users do
    begin
      Generator.TableCell(U.Login,                      0, i);
      Generator.TableCell(U.FullName,                   1, i);
      Generator.TableCell(DateTimeToStr(U.Created),     2, i);
      Generator.TableCell(DateTimeToStr(U.Modified),    3, i);

      if U.LastLogin = 0 then
        Generator.TableCell('N/A',                      4, i)
      else
        Generator.TableCell(DateTimeToStr(U.LastLogin), 4, i);

      if U.ExpireDate = 0 then
        Generator.TableCell('Never',                    5, i)
      else
        Generator.TableCell(DateTimeToStr(U.ExpireDate),5, i);

      Generator.TableCell(U.Notes,                      6, i);

      S := '';
      for G in U.Groups do
        S += G.Caption.Text + LineEnding;

      Generator.TableCell(S,                            7, i);
      Inc(i);
    end;
  Generator.TableFooter('');

  Generator.Line('');

  AdminRel := Admin.AdminRelation;
  Generator.TableHeader('Groups', 4, Admin.Groups.Count + 1);

  Generator.TableCell('Name',          0, 0);
  Generator.TableCell('Created',       1, 0);
  Generator.TableCell('Last Modified', 2, 0);
  Generator.TableCell('Rights',        3, 0);
//  Generator.TableCell('Users',         4, 0);

  i := 1;
  DoGroupRelation(AdminRel, 0, i);
  Generator.TableFooter('');

  Generator.Line('');
  Generator.Line('');

  // ENTRY RIGHTS
  Generator.Heading('Entry Rights');
  Generator.Line('');

  DFs := Document.Relations.GetOrderedDataFiles;
  OGrps := AdminRel.GroupRelations.GetOrderedItems;

  for DF in DFs do
    begin
      if DF.ProtectedItem then Continue;

      Generator.TableHeader(DF.Caption.Text, 5, OGrps.Count + 2);

      Generator.TableCell('Group',  0, 0);
      Generator.TableCell('Read',   1, 0);
      Generator.TableCell('Update', 2, 0);
      Generator.TableCell('Create', 3, 0);
      Generator.TableCell('Delete', 4, 0);

      Generator.TableCell(AdminRel.Group.Caption.Text, 0, 1, tcaLeftAdjust);
      Generator.TableCell('x', 1, 1, tcaCenter);
      Generator.TableCell('x', 2, 1, tcaCenter);
      Generator.TableCell('x', 3, 1, tcaCenter);
      Generator.TableCell('x', 4, 1, tcaCenter);

      i := 2;
      for GrpRel in OGrps do
        begin
          Generator.TableCell(GrpRel.Group.Caption.Text, 0, i, tcaLeftAdjust);

          EntryRights := DF.GroupRights.GroupRightFromGroup(GrpRel.Group).EntryRights;

          if (eerRead in EntryRights) then
            Generator.TableCell('x', 1, i, tcaCenter);

          if (eerUpdate in EntryRights) then
            Generator.TableCell('x', 2, i, tcaCenter);

          if (eerCreate in EntryRights) then
            Generator.TableCell('x', 3, i, tcaCenter);

          if (eerDelete in EntryRights) then
            Generator.TableCell('x', 4, i, tcaCenter);

          Inc(i);
        end;

      Generator.TableFooter('');
      Generator.Line('');
    end;
end;

function TReportAdmin.GetTitle: string;
begin
  result := 'Extended Access Overview';
end;

procedure TReportAdmin.DoDocumentReport(const ADocumentFile: TEpiDocumentFile;
  const Index: Integer);
begin
  inherited DoDocumentReport(ADocumentFile, Index);

  DoAdminReport(ADocumentFile.Document);
end;

end.

