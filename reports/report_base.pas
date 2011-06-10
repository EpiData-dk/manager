unit report_base;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TReportBase }
  TReportBase = class
  private
    FStyleSheet: string;
  protected
    constructor Create; virtual;
    function GetTitle: string; virtual; abstract;
  public
    property StyleSheet: string read FStyleSheet;
    property ReportTitle: string read GetTitle;
  end;

  { TReportListBase }

  TReportListBase = class(TReportBase)
  private
    FDocuments: TStringList;
  protected
    constructor Create; override;
  public
    constructor Create(const FileNames: TStringList);
    destructor  Destroy; override;
    function    RunReport: string; virtual; abstract;
    property    Documents: TStringList read FDocuments;
  end;
  TReportListBaseClass = class of TReportListBase;

implementation

uses
  Forms, FileUtil, epireport_htmlgenerator, epidocument;

const
  CssFileName = 'reports.css';

{ TReportBase }

constructor TReportBase.Create;
var
  CssFile: String;
  Fh: THandle;
  Ss: TStringStream;
  Fs: TFileStream;
begin
  CssFile := ExtractFilePath(Application.ExeName) + CssFileName;
  try
    if not FileExistsUTF8(CssFile) then
    begin
      FStyleSheet := TEpiReportHTMLGenerator.HtmlStyleSheet;
      Ss := TStringStream.Create(TEpiReportHTMLGenerator.HtmlStyleSheet);
      Ss.Position := 0;
      Fs := TFileStream.Create(CssFile, fmCreate);
      Fs.CopyFrom(Ss, Ss.Size);
    end else begin
      Fs := TFileStream.Create(CssFile, fmOpenRead);
      Fs.Position := 0;
      Ss := TStringStream.Create('');
      Ss.CopyFrom(Fs, FS.Size);
      FStyleSheet := Ss.DataString;
    end;
  finally
    Fs.Free;
    Ss.Free;
  end;
end;

{ TReportListBase }

constructor TReportListBase.Create;
begin
  inherited Create;
end;

constructor TReportListBase.Create(const FileNames: TStringList);
var
  Doc: TEpiDocument;
  i: Integer;
begin
  inherited Create;

  FDocuments := TStringList.Create;
  for i := 0 to FileNames.Count - 1 do
  begin
    if Assigned(FileNames.Objects[i]) and
       (FileNames.Objects[i] is TEpiDocument) then
    begin
      FDocuments.AddObject(FileNames[i], FileNames.Objects[i]);
      Continue;
    end;

    Doc := TEpiDocument.Create('');
    Doc.LoadFromFile(FileNames[i]);
    FDocuments.AddObject(FileNames[i], Doc);
  end;
end;

destructor TReportListBase.Destroy;
var
  i: Integer;
begin
  for i := 0 to FDocuments.Count - 1 do
    FDocuments.Objects[i].Free;

  FDocuments.Free;
  inherited Destroy;
end;

end.

