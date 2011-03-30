unit project_settings_study_contentdesc_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls,
  project_settings_interface, epicustombase, epistudy;

type

  { TProjectSetting_ContentDescFrame }

  TProjectSetting_ContentDescFrame = class(TFrame, IProjectSettingsFrame)
    GeoCoverageEdit: TEdit;
    TimeCoverageEdit: TEdit;
    GroupBox1: TGroupBox;
    AbstractEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    CitationsEdit: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    PurposeEdit: TEdit;
  private
    { private declarations }
    Study: TEpiStudy;
  public
    { public declarations }
    procedure SetProjectSettings(AValue: TEpiCustomBase);
    function  ApplySettings: boolean;
  end;

implementation

uses
  epidocument;
{$R *.lfm}

{ TProjectSetting_ContentDescFrame }

procedure TProjectSetting_ContentDescFrame.SetProjectSettings(
  AValue: TEpiCustomBase);
begin
  Study := TEpiDocument(AValue).Study;
  with Study do
  begin
    PurposeEdit.Text := Purpose.Text;
    AbstractEdit.Text := AbstractText.Text;
    CitationsEdit.Text := Citations.Text;
    GeoCoverageEdit.Text := GeographicalCoverage.Text;
    TimeCoverageEdit.Text := TimeCoverage.Text;
  end;
end;

function TProjectSetting_ContentDescFrame.ApplySettings: boolean;
begin
  with Study do
  begin
    Purpose.Text := PurposeEdit.Text;
    AbstractText.Text := AbstractEdit.Text;
    Citations.Text := CitationsEdit.Text;
    GeographicalCoverage.Text := GeoCoverageEdit.Text;
    TimeCoverage.Text := TimeCoverageEdit.Text;
  end;
  result := true;
end;

end.

