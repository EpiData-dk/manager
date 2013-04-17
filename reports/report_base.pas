unit report_base;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epireport_generator_base,
  epidocument;

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
    procedure DoDocumentReport(const Doc: TEpiDocument;
      const FileName: string; Const Index: Integer); virtual;
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

{ TReportFileListBase }

procedure TReportFileListBase.DoRunReport;
var
  R: TEpiReportBase;
  i: Integer;
begin
  inherited DoRunReport;

  R := TEpiReportMainHeader.Create(Generator);
  TEpiReportMainHeader(R).ProjectList := Documents;
  TEpiReportMainHeader(R).Title := GetTitle;
  R.RunReport;
  R.Free;

  for i := 0 to Documents.Count - 1 do
    DoDocumentReport(TEpiDocument(Documents.Objects[i]), Documents[i], i);
end;

procedure TReportFileListBase.DoDocumentReport(const Doc: TEpiDocument;
  const FileName: string; const Index: Integer);
var
  R: TEpiReportProjectHeader;
begin
  Generator.Line('');
  R := TEpiReportProjectHeader.Create(Generator);
  R.Document := Doc;
  R.Filename := FileName;
  R.FileNo   := Index + 1;
  R.RunReport;
  R.Free;
  Generator.Line('');
end;

end.

