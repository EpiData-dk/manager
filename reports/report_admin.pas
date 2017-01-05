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
    procedure DoAdminReport(Const Admin: TEpiAdmin);
  protected
    procedure DoDocumentReport(const ADocumentFile: TEpiDocumentFile;
      const Index: Integer); override;
  end;


implementation

{ TReportAdmin }

procedure TReportAdmin.DoAdminReport(const Admin: TEpiAdmin);
begin

end;

procedure TReportAdmin.DoDocumentReport(const ADocumentFile: TEpiDocumentFile;
  const Index: Integer);
begin
  inherited DoDocumentReport(ADocumentFile, Index);

  DoAdminReport(ADocumentFile.Document.Admin);
end;

end.

