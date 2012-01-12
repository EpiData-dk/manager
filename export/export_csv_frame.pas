unit export_csv_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, MaskEdit,
  epiexportsettings, export_frame_types, epimiscutils;

type

  { TExportCSVFrame }

  TExportCSVFrame = class(TFrame, IExportSettingsPresenterFrame)
    NewLineCmbBox: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    FieldSepEdit: TMaskEdit;
    DateSepEdit: TMaskEdit;
    TimeSepEdit: TMaskEdit;
    DecimalSepEdit: TMaskEdit;
    SeparatorGrpBox: TGroupBox;
  private
    { private declarations }
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    function GetExportName: string;
    function GetFrameCaption: string;
    function UpdateExportSetting(Setting: TEpiExportSetting): boolean;
    function GetFileDialogExtensions: TEpiDialogFilters;
  end; 

implementation

{$R *.lfm}

uses
  export_form, epistringutils;

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

{  FieldSeparator: string;
  DateSeparator: string;
  TimeSeparator: string;
  DecimalSeparator: string;
  NewLine: string;}
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
  result := true;
end;

function TExportCSVFrame.GetFileDialogExtensions: TEpiDialogFilters;
begin
  result := [dfText];
end;

initialization
  RegisterExportFrame(TExportCSVFrame, TEpiCSVExportSetting);

end.

