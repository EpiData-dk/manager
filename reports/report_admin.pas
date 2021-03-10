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
  epireport_types, epirights, epireport_report_userlist;

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
  R: TEpiReportUserList;
begin
  Admin := Document.Admin;

  R := TEpiReportUserList.Create(Generator);
  R.Admin := Document.Admin;
  R.RunReport;
  R.Free;

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

