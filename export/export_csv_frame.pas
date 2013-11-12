unit export_csv_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, MaskEdit,
  epiexportsettings, export_frame_types, epimiscutils, settings2_interface,
  settings2_var;

type

  { TExportCSVFrame }

  TExportCSVFrame = class(TFrame, IExportSettingsPresenterFrame, ISettingsFrame)
    FieldSepEdit: TEdit;
    DateSepEdit: TEdit;
    TimeSepEdit: TEdit;
    DecimalSepEdit: TEdit;
    NewLineCmbBox: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    SeparatorGrpBox: TGroupBox;
  private
    { private declarations }
    FTextExport: TFrame;
    FData:       PManagerSettings;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function GetExportName: string;
    function GetFrameCaption: string;
    function UpdateExportSetting(Setting: TEpiExportSetting): boolean;
    function GetFileDialogExtensions: TEpiDialogFilters;
    procedure SetSettings(Data: PManagerSettings);
    function ApplySettings: boolean;
    function ExportHeadings: boolean;
  end; 

implementation

{$R *.lfm}

uses
  export_form, epistringutils, export_customtext_frame;

{ TExportCSVFrame }

constructor TExportCSVFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FieldSepEdit.Text := ',';
  DateSepEdit.Text := DefaultFormatSettings.DateSeparator;
  TimeSepEdit.Text := DefaultFormatSettings.TimeSeparator;
  DecimalSepEdit.Text := DefaultFormatSettings.DecimalSeparator;

  with NewLineCmbBox.Items do
  begin
    Clear;
    AddObject('Linux' {$IFDEF LINUX}+ ' (System)'{$ENDIF}, TString.Create(#10));
    AddObject('Mac' {$IFDEF DARWIN}+ ' (System)'{$ENDIF}, TString.Create(#13));
    AddObject('Windows' {$IFDEF WINDOWS}+ ' (System)'{$ENDIF}, TString.Create(#13#10));
  end;

  // Custom Text Frame;
  FTextExport := TExportCustomTextFrame.Create(self);
  FTextExport.Parent := Self;
  FTextExport.AnchorToNeighbour(akTop, 10, NewLineCmbBox);
  FTextExport.AnchorParallel(akLeft, 20, Self);
  FTextExport.AnchorParallel(akRight, 20, Self);
  FTextExport.AnchorParallel(akBottom, 10, Self);

  // SETUP ACCORDING TO MANAGERSETTINGS.
  SetSettings(@ManagerSettings);
end;

destructor TExportCSVFrame.Destroy;
var
  i: Integer;
begin
  for i := 0 to NewLineCmbBox.Items.Count - 1 do
    NewLineCmbBox.Items.Objects[i].Free;
  inherited Destroy;
end;

function TExportCSVFrame.GetExportName: string;
begin
  result := 'CSV File';
end;

function TExportCSVFrame.GetFrameCaption: string;
begin
  result := 'CSV Export';
end;

function TExportCSVFrame.UpdateExportSetting(Setting: TEpiExportSetting): boolean;
begin
  with TEpiCSVExportSetting(Setting) do
  begin
    FieldSeparator    := FieldSepEdit.Text;
    DateSeparator     := DateSepEdit.Text;
    TimeSeparator     := TimeSepEdit.Text;
    DecimalSeparator  := DecimalSepEdit.Text;
    NewLine           := TString(NewLineCmbBox.Items.Objects[NewLineCmbBox.ItemIndex]).Str;
  end;

  result := (FTextExport as IExportSettingsFrame).UpdateExportSetting(Setting);
end;

function TExportCSVFrame.GetFileDialogExtensions: TEpiDialogFilters;
begin
  result := [dfText];
end;

procedure TExportCSVFrame.SetSettings(Data: PManagerSettings);
var
  I: Integer;
begin
  FData := Data;
  with FData^ do
  begin
    FieldSepEdit.Text                                                 := ExportCSVFieldSep;
    DateSepEdit.Text                                                  := ExportCSVDateSep;
    TimeSepEdit.Text                                                  := ExportCSVTimeSep;
    DecimalSepEdit.Text                                               := ExportCSVDecSep;
    TExportCustomTextFrame(FTextExport).ExportFieldNameChkBox.Checked := ExportCSVFieldName;
    TExportCustomTextFrame(FTextExport).QuoteCharEdit.Text            := ExportCSVQuote;
    NewLineCmbBox.ItemIndex                                           := ExportCSVNewLine;
  end;
end;

function TExportCSVFrame.ApplySettings: boolean;
begin
  Result :=
    (FieldSepEdit.Text <> '') and
    (DateSepEdit.Text <> '') and
    (TimeSepEdit.Text <> '') and
    (DecimalSepEdit.Text <> '') and
    (TExportCustomTextFrame(FTextExport).QuoteCharEdit.Text <> '');
  if not result then exit;

  with FData^ do
  begin
    ExportCSVFieldSep  := FieldSepEdit.Text;
    ExportCSVDateSep   := DateSepEdit.Text;
    ExportCSVTimeSep   := TimeSepEdit.Text;
    ExportCSVDecSep    := DecimalSepEdit.Text;
    ExportCSVFieldName := TExportCustomTextFrame(FTextExport).ExportFieldNameChkBox.Checked;
    ExportCSVQuote     := TExportCustomTextFrame(FTextExport).QuoteCharEdit.Text;
    ExportCSVNewLine   := NewLineCmbBox.ItemIndex;
  end;
end;

function TExportCSVFrame.ExportHeadings: boolean;
begin
  result := false;
end;

initialization
  RegisterExportFrame(TExportCSVFrame, TEpiCSVExportSetting);

end.

