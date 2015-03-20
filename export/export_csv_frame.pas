unit export_csv_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, MaskEdit,
  epiexportsettings, export_frame_types, epimiscutils, settings2_interface,
  settings2_var, epidocument;

type

  { TExportCSVFrame }

  TExportCSVFrame = class(TFrame, IExportSettingsPresenterFrame, ISettingsFrame)
    ByteOrderMarkCheckBox: TCheckBox;
    ExportFieldNameChkBox: TCheckBox;
    FieldSepEdit: TEdit;
    DateSepEdit: TEdit;
    QuoteCharLabel: TLabel;
    QuoteCharEdit: TEdit;
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
    function ExportRelated: boolean;
    function CheckExportAllowed(Const Setting: TEpiExportSetting;
      Const Doc: TEpiDocument;
      out ErrorText: string): boolean;
  end;

implementation

{$R *.lfm}

uses
  export_form, epistringutils, export_customtext_frame;

{ TExportCSVFrame }

constructor TExportCSVFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FieldSepEdit.Text   := ',';
  DateSepEdit.Text    := DefaultFormatSettings.DateSeparator;
  TimeSepEdit.Text    := DefaultFormatSettings.TimeSeparator;
  DecimalSepEdit.Text := DefaultFormatSettings.DecimalSeparator;

  with NewLineCmbBox.Items do
  begin
    Clear;
    AddObject('Linux' {$IFDEF LINUX}+ ' (System)'{$ENDIF}, TString.Create(#10));
    AddObject('Mac' {$IFDEF DARWIN}+ ' (System)'{$ENDIF}, TString.Create(#13));
    AddObject('Windows' {$IFDEF WINDOWS}+ ' (System)'{$ENDIF}, TString.Create(#13#10));
  end;

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
    QuoteChar         := QuoteCharEdit.Text;
    ExportFieldNames  := ExportFieldNameChkBox.Checked;
    ByteOrderMark     := ByteOrderMarkCheckBox.Checked;
  end;

  result := true;
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
    ExportFieldNameChkBox.Checked                                     := ExportCSVFieldName;
    QuoteCharEdit.Text                                                := ExportCSVQuote;
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
    (QuoteCharEdit.Text <> '');
  if not result then exit;

  with FData^ do
  begin
    ExportCSVFieldSep  := FieldSepEdit.Text;
    ExportCSVDateSep   := DateSepEdit.Text;
    ExportCSVTimeSep   := TimeSepEdit.Text;
    ExportCSVDecSep    := DecimalSepEdit.Text;
    ExportCSVFieldName := ExportFieldNameChkBox.Checked;
    ExportCSVQuote     := QuoteCharEdit.Text;
    ExportCSVNewLine   := NewLineCmbBox.ItemIndex;
  end;
end;

function TExportCSVFrame.ExportHeadings: boolean;
begin
  result := false;
end;

function TExportCSVFrame.ExportRelated: boolean;
begin
  result := false;
end;

function TExportCSVFrame.CheckExportAllowed(const Setting: TEpiExportSetting;
  const Doc: TEpiDocument; out ErrorText: string): boolean;
begin
  result := true;
end;

initialization
  RegisterExportFrame(TExportCSVFrame, TEpiCSVExportSetting);

end.

