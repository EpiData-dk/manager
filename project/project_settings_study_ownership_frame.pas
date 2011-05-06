unit project_settings_study_ownership_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls,
  settings2, settings2_interface, settings2_var,
  project_settings_interface, epicustombase, epistudy;

type

  { TProjectSetting_OwnershipFrame }

  TProjectSetting_OwnershipFrame = class(TFrame, IProjectSettingsFrame, ISettingsFrame)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    FundingMemo: TMemo;
    AuthorsMemo: TMemo;
    RightsMemo: TMemo;
    PublisherEdit: TEdit;
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

{ TProjectSetting_OwnershipFrame }

procedure TProjectSetting_OwnershipFrame.SetProjectSettings(
  AValue: TEpiCustomBase);
begin
  Study := TEpiDocument(Avalue).Study;
  with Study do
  begin
    AuthorsMemo.Text := Author;
    RightsMemo.Text := Rights.Text;
    PublisherEdit.Text := Publisher.Text;
    FundingMemo.Text := Funding.Text;
  end;
end;

procedure TProjectSetting_OwnershipFrame.SetSettings(Data: PManagerSettings);
begin
  FManagerSettings := Data;
  with FManagerSettings^ do
  begin
    AuthorsMemo.Text := OwnAuthers;
    RightsMemo.Text := OwnRights;
    PublisherEdit.Text := OwnPublisher;
    FundingMemo.Text := OwnFunding;
  end;
end;

function TProjectSetting_OwnershipFrame.ApplySettings: boolean;
begin
  result := true;
  if Assigned(Study) then
  with Study do
  begin
    Author := AuthorsMemo.Text;
    Rights.Text := RightsMemo.Text;
    Publisher.Text := PublisherEdit.Text;
    Funding.Text := FundingMemo.Text;
  end;

  if Assigned(FManagerSettings) then
  with FManagerSettings^ do
  begin
    OwnAuthers := AuthorsMemo.Text;
    OwnRights := RightsMemo.Text;
    OwnPublisher := PublisherEdit.Text;
    OwnFunding := FundingMemo.Text;
  end;
end;

end.

