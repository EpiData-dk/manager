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
    KeywordsEdit: TEdit;
    GeoCoverageEdit: TEdit;
    AbstractMemo: TMemo;
    Label6: TLabel;
    Label7: TLabel;
    PurposeMemo: TMemo;
    CitationsMemo: TMemo;
    TimeCoverageEdit: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    PopulationEdit: TEdit;
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
    KeywordsEdit.Text := KeyWords;
    PurposeMemo.Text := Purpose.Text;
    AbstractMemo.Text := AbstractText.Text;
    CitationsMemo.Text := Citations.Text;
    GeoCoverageEdit.Text := GeographicalCoverage.Text;
    TimeCoverageEdit.Text := TimeCoverage.Text;
    PopulationEdit.Text := Population.Text;
  end;
end;

procedure TProjectSetting_ContentDescFrame.SetSettings(Data: PManagerSettings);
begin
  FManagerSettings := Data;
  with FManagerSettings^ do
  begin
    KeywordsEdit.Text := ContKeywords;
    PurposeMemo.Text := ContPurpose;
    AbstractMemo.Text := ContAbstract;
    CitationsMemo.Text := ContCitation;
    GeoCoverageEdit.Text := ContGeoCover;
    TimeCoverageEdit.Text := ContTimeCover;
    PopulationEdit.Text := ContPopulation;
  end;
end;

function TProjectSetting_ContentDescFrame.ApplySettings: boolean;
begin
  result := true;

  if Assigned(Study) then
  with Study do
  begin
    KeyWords                  := KeywordsEdit.Text;
    Purpose.Text              := PurposeMemo.Text;
    AbstractText.Text         := AbstractMemo.Text;
    Citations.Text            := CitationsMemo.Text;
    GeographicalCoverage.Text := GeoCoverageEdit.Text;
    TimeCoverage.Text         := TimeCoverageEdit.Text;
    Population.Text           := PopulationEdit.Text;
  end;

  if Assigned(FManagerSettings) then
  with FManagerSettings^ do
  begin
    ContKeywords := KeywordsEdit.Text;
    ContPurpose := PurposeMemo.Text;
    ContAbstract := AbstractMemo.Text;
    ContCitation := CitationsMemo.Text;
    ContGeoCover := GeoCoverageEdit.Text;
    ContTimeCover := TimeCoverageEdit.Text;
    ContPopulation := PopulationEdit.Text;
  end;
end;

end.

