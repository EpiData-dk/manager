unit report_base;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epireport_generator_base;

type

  { TReportBase }
  TReportBase = class
  private
    FDocuments: TStringList;
    FGenerator: TEpiReportGeneratorBase;
    FSelfOpenedList: TBits;
  protected
    function    GetTitle: string; virtual; abstract;
    procedure   DoBeginReport; virtual;
    procedure   DoRunReport; virtual;
    procedure   DoEndReport; virtual;
    property    Generator: TEpiReportGeneratorBase read FGenerator;
  public
    constructor Create(const FileNames: TStringList;
      ReportGeneratorClass: TEpiReportGeneratorBaseClass); virtual;
    destructor  Destroy; override;
    function    RunReport: string;
    property    Documents: TStringList read FDocuments;
    property    ReportTitle: String read GetTitle;
  end;
  TReportBaseClass = class of TReportBase;

  { TReportFileListBase }

  TReportFileListBase = class(TReportBase)
  protected
    procedure DoRunReport; override;
  end;

implementation

uses
  epidocument, epireport_filelist, managerprocs;

{ TReportFileListBase }

procedure TReportFileListBase.DoRunReport;
var
  R: TEpiReportFileList;
begin
  inherited DoRunReport;

  R := TEpiReportFileList.Create(Generator);
  R.FileList := Documents;
  R.RunReport;
  R.Free;
end;

{ TReportBase }

procedure TReportBase.DoBeginReport;
begin
  FGenerator.StartReport(GetTitle);
end;

procedure TReportBase.DoRunReport;
begin
  FGenerator.Section('Report: ' + GetTitle + ' Created ' + FormatDateTime('YYYY/MM/DD HH:NN:SS', Now));
end;

procedure TReportBase.DoEndReport;
begin
  FGenerator.EndReport;
end;

constructor TReportBase.Create(const FileNames: TStringList;
  ReportGeneratorClass: TEpiReportGeneratorBaseClass);
var
  Doc: TEpiDocument;
  i: Integer;
begin
  FGenerator := ReportGeneratorClass.Create;
  FSelfOpenedList := TBits.Create(FileNames.Count);
  FSelfOpenedList.Clearall;

  FDocuments := TStringList.Create;
  for i := 0 to FileNames.Count - 1 do
  begin
    if Assigned(FileNames.Objects[i]) and
       (FileNames.Objects[i] is TEpiDocument) then
    begin
      FDocuments.AddObject(FileNames[i], FileNames.Objects[i]);
      Continue;
    end;

    FSelfOpenedList.Bits[i] := true;
    Doc := TOpenEpiDoc.OpenDoc(FileNames[i], '');
    FDocuments.AddObject(FileNames[i], Doc);
  end;
end;

destructor TReportBase.Destroy;
var
  i: Integer;
begin
  for i := 0 to FDocuments.Count - 1 do
    if FSelfOpenedList.Bits[i] then
      FDocuments.Objects[i].Free;

  FSelfOpenedList.Free;
  FDocuments.Free;
  FGenerator.Free;
  inherited Destroy;
end;

function TReportBase.RunReport: string;
begin
  DoBeginReport;
  DoRunReport;
  DoEndReport;

  Result := FGenerator.GetReportText;
end;

{ TReportListBase }

end.

