unit project_settings_study_contentdesc_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls,
  settings2, settings2_var, settings2_interface,
  project_settings_interface, epicustombase, epistudy;

type

  { TProjectSetting_ContentDescFrame }

  TProjectSetting_ContentDescFrame = class(TFrame, IProjectSettingsFrame, ISettingsFrame)
    GeoCoverageEdit: TEdit;
    AbstractMemo: TMemo;
    PurposeMemo: TMemo;
    CitationsMemo: TMemo;
    TimeCoverageEdit: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
  private
    { private declarations }
    Study: TEpiStudy;
    FManagerSettings: PManagerSettings;
  public
    { public declarations }
    procedure SetProjectSettings(AValue: TEpiCustomBase);
    procedure SetSettings(Data: PManagerSettings);
    function  ApplySettings: boolean;
  end;

implementation

uses
  epidocument, project_settings_study_frame;
{$R *.lfm}

{ TProjectSetting_ContentDescFrame }

procedure TProjectSetting_ContentDescFrame.SetProjectSettings(
  AValue: TEpiCustomBase);
begin
  Study := TEpiDocument(AValue).Study;
  with Study do
  begin
    PurposeMemo.Text := Purpose.Text;
    AbstractMemo.Text := AbstractText.Text;
    CitationsMemo.Text := Citations.Text;
    GeoCoverageEdit.Text := GeographicalCoverage.Text;
    TimeCoverageEdit.Text := TimeCoverage.Text;
  end;
end;

procedure TProjectSetting_ContentDescFrame.SetSettings(Data: PManagerSettings);
begin
  FManagerSettings := Data;
  with FManagerSettings^ do
  begin
    PurposeMemo.Text := ContPurpose;
    AbstractMemo.Text := ContAbstract;
    CitationsMemo.Text := ContCitation;
    GeoCoverageEdit.Text := ContGeoCover;
    TimeCoverageEdit.Text := ContTimeCover;
  end;
end;

function TProjectSetting_ContentDescFrame.ApplySettings: boolean;
begin
  result := true;

  if Assigned(Study) then
  with Study do
  begin
    Purpose.Text := PurposeMemo.Text;
    AbstractText.Text := AbstractMemo.Text;
    Citations.Text := CitationsMemo.Text;
    GeographicalCoverage.Text := GeoCoverageEdit.Text;
    TimeCoverage.Text := TimeCoverageEdit.Text;
  end;

  if Assigned(FManagerSettings) then
  with FManagerSettings^ do
  begin
    ContPurpose := PurposeMemo.Text;
    ContAbstract := AbstractMemo.Text;
    ContCitation := CitationsMemo.Text;
    ContGeoCover := GeoCoverageEdit.Text;
    ContTimeCover := TimeCoverageEdit.Text;
  end;
end;

end.

