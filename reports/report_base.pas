unit report_base;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epireport_generator_base,
  epidocument, Forms, epiopenfile;

type

  { TReportBase }
  TReportBase = class
  private
    FDocumentFile: TEpiDocumentFile;
    FGenerator: TEpiReportGeneratorBase;
  protected
    function    GetTitle: string; virtual; abstract;
    procedure   DoBeginReport; virtual;
    procedure   DoRunReport; virtual;
    procedure   DoEndReport; virtual;
    property    Generator: TEpiReportGeneratorBase read FGenerator;
  public
    constructor Create(ReportGeneratorClass: TEpiReportGeneratorBaseClass); virtual; overload;
    destructor  Destroy; override;
    function    RunReport: string;
    class function ReportFrameClass: TCustomFrameClass; virtual; abstract;
    property    DocumentFile: TEpiDocumentFile read FDocumentFile write FDocumentFile;
    property    ReportTitle: String read GetTitle;
  end;
  TReportBaseClass = class of TReportBase;

  { TReportFileListBase }

  TReportFileListBase = class(TReportBase)
  private
    FDocumentFiles: TEpiDocumentFileList;
  protected
    procedure DoRunReport; override;
    procedure DoDocumentReport(Const ADocumentFile: TEpiDocumentFile;
      Const Index: Integer); virtual;
  public
    property DocumentFiles: TEpiDocumentFileList read FDocumentFiles write FDocumentFiles;
  end;

implementation

uses
  epireport_base, epireport_report_mainheader,
  epireport_report_projectheading,
  managerprocs;

{ TReportBase }

procedure TReportBase.DoBeginReport;
begin
  FGenerator.StartReport(GetTitle);
end;

procedure TReportBase.DoRunReport;
begin
  //
end;

procedure TReportBase.DoEndReport;
begin
  FGenerator.EndReport;
end;

constructor TReportBase.Create(
  ReportGeneratorClass: TEpiReportGeneratorBaseClass);
begin
  FGenerator := ReportGeneratorClass.Create;
end;

destructor TReportBase.Destroy;
begin
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

{ TReportFileListBase }

procedure TReportFileListBase.DoRunReport;
var
  R: TEpiReportBase;
  i: Integer;
begin
  inherited DoRunReport;

  R := TEpiReportMainHeader.Create(Generator);
  TEpiReportMainHeader(R).ProjectList := DocumentFiles;
  TEpiReportMainHeader(R).Title := GetTitle;
  R.RunReport;
  R.Free;

  for i := 0 to DocumentFiles.Count - 1 do
    DoDocumentReport(DocumentFiles[i], i);
end;

procedure TReportFileListBase.DoDocumentReport(
  const ADocumentFile: TEpiDocumentFile; const Index: Integer);
var
  R: TEpiReportProjectHeader;
begin
  Generator.Line('');
  R := TEpiReportProjectHeader.Create(Generator);
  R.Document := ADocumentFile.Document;
  R.Filename := ADocumentFile.FileName;
  R.FileNo   := Index + 1;
  R.RunReport;
  R.Free;
  Generator.Line('');
end;

end.

