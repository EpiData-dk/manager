unit export_csv_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, MaskEdit,
  epiexportsettings, export_frame_types, epimiscutils;

type

  { TExportCSVFrame }

  TExportCSVFrame = class(TFrame, IExportSettingsPresenterFrame)
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
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function GetExportName: string;
    function GetFrameCaption: string;
    function UpdateExportSetting(Setting: TEpiExportSetting): boolean;
    function GetFileDialogExtensions: TEpiDialogFilters;
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

initialization
  RegisterExportFrame(TExportCSVFrame, TEpiCSVExportSetting);

end.

