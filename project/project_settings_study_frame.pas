unit project_settings_study_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls,
  project_settings_interface, epistudy, epicustombase;

type

  { TProjectsettings_StudyFrame }

  TProjectsettings_StudyFrame = class(TFrame, IProjectSettingsFrame)
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
  public
    { public declarations }
    procedure SetProjectSettings(AValue: TEpiCustomBase);
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

function TProjectsettings_StudyFrame.ApplySettings: boolean;
begin
  result := false;
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
  result := true;
end;

end.

