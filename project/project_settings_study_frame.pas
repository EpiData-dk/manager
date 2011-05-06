unit project_settings_study_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls,
  settings2_interface, settings2_var, settings2,
  project_settings_interface, epistudy, epicustombase;

type

  { TProjectsettings_StudyFrame }

  TProjectsettings_StudyFrame = class(TFrame, IProjectSettingsFrame, ISettingsFrame)
    TitleEdit: TEdit;
    IdentfierEdit: TEdit;
    LanguageEdit: TEdit;
    VersionEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
  private
    { private declarations }
    FStudy: TEpiStudy;
    FManagerSettings: PManagerSettings;
  public
    { public declarations }
    procedure SetProjectSettings(AValue: TEpiCustomBase);
    procedure SetSettings(Data: PManagerSettings);
    function  ApplySettings: boolean;
  end;

implementation

uses
  epidocument, Dialogs;

{$R *.lfm}

{ TProjectsettings_StudyFrame }

procedure TProjectsettings_StudyFrame.SetProjectSettings(AValue: TEpiCustomBase
  );
begin
  FStudy := TEpiDocument(AValue).Study;
  With FStudy do
  begin
    TitleEdit.Text     := Title.Text;
    IdentfierEdit.Text := Identifier;
    LanguageEdit.Text  := Language;
    VersionEdit.Text   := Version;
  end
end;

procedure TProjectsettings_StudyFrame.SetSettings(Data: PManagerSettings);
begin
  FManagerSettings := Data;
  with FManagerSettings^ do
  begin
    TitleEdit.Text     := StudyTitle;
    IdentfierEdit.Text := StudyIndent;
    LanguageEdit.Text  := StudyLang;
    VersionEdit.Text   := StudyVersion;
  end;
end;

function TProjectsettings_StudyFrame.ApplySettings: boolean;
begin
  result := false;
  if Assigned(FStudy) then
  With FStudy do
  begin
    if LanguageEdit.Text <> Language then
      if MessageDlg('Warning!',
        'Changing language will reset ALL texts/captions (not field/section/heading names).' + LineEnding+
        'Are you sure you wish to proceed?',
        mtWarning, mbYesNo, 0, mbNo
        ) = mrNo then exit;

    Language := LanguageEdit.Text;
    TEpiDocument(RootOwner).SetLanguage(Language, true);
    TEpiDocument(RootOwner).SetLanguage(Language, false);

    Title.Text := TitleEdit.Text;
    Identifier := IdentfierEdit.Text;
    Version    := VersionEdit.Text;
  end;

  if Assigned(FManagerSettings) then
  with FManagerSettings^ do
  begin
    StudyLang    := LanguageEdit.Text;
    StudyTitle   := TitleEdit.Text;
    StudyIndent  := IdentfierEdit.Text;
    StudyVersion := VersionEdit.Text;
  end;
  result := true;
end;

end.

