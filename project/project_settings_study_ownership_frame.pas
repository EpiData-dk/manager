unit project_settings_study_ownership_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls,
  project_settings_interface, epicustombase, epistudy;

type

  { TProjectSetting_OwnershipFrame }

  TProjectSetting_OwnershipFrame = class(TFrame, IProjectSettingsFrame)
    RightsEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    PublisherEdit: TEdit;
    AuthorEdit: TEdit;
    FundingEdit: TEdit;
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

{ TProjectSetting_OwnershipFrame }

procedure TProjectSetting_OwnershipFrame.SetProjectSettings(
  AValue: TEpiCustomBase);
begin
  Study := TEpiDocument(Avalue).Study;
  with Study do
  begin
    AuthorEdit.Text := Author;
    RightsEdit.Text := Rights.Text;
    PublisherEdit.Text := Publisher.Text;
    FundingEdit.Text := Funding.Text;
  end;
end;

function TProjectSetting_OwnershipFrame.ApplySettings: boolean;
begin
  with Study do
  begin
    Author := AuthorEdit.Text;
    Rights.Text := RightsEdit.Text;
    Publisher.Text := PublisherEdit.Text;
    Funding.Text := FundingEdit.Text;
  end;
  result := true;
end;

{$R *.lfm}

end.

