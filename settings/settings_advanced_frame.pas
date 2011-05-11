unit settings_advanced_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, EditBtn, FileCtrl,
  settings2_interface, settings2_var;

type

  { TSettings_PathsFrame }

  TSettings_PathsFrame = class(TFrame, ISettingsFrame)
    EntryClientDirEdit: TDirectoryEdit;
    Label1: TLabel;
    MultipleInstanceChkbox: TCheckBox;
    TutorialURLEdit: TEdit;
    Label2: TLabel;
    Label19: TLabel;
    Label17: TLabel;
    WorkingDirEdit: TDirectoryEdit;
    TutorialDirEdit: TDirectoryEdit;
  private
    { private declarations }
    FData: PManagerSettings;
  public
    { public declarations }
    procedure SetSettings(Data: PManagerSettings);
    function  ApplySettings: boolean;
  end;

implementation

{$R *.lfm}

uses
  settings2, epimiscutils, strutils, LCLProc;


{ TSettings_PathsFrame }

procedure TSettings_PathsFrame.SetSettings(Data: PManagerSettings);
begin
  FData := Data;
  with FData^ do
  begin
    WorkingDirEdit.Text := WorkingDirUTF8;
    TutorialDirEdit.Text := TutorialDirUTF8;
    TutorialURLEdit.Text := TutorialURLUTF8;
    EntryClientDirEdit.Text := EntryClientDirUTF8;
  end;
end;

function TSettings_PathsFrame.ApplySettings: boolean;
begin
  with FData^ do
  begin
    if DirectoryExistsUTF8(WorkingDirEdit.Text) then
      WorkingDirUTF8    := WorkingDirEdit.Text;
    if DirectoryExistsUTF8(TutorialDirEdit.Text) then
      TutorialDirUTF8   := TutorialDirEdit.Text;
    if (LeftStr(UTF8LowerCase(TutorialURLEdit.Text), 7) = 'http://') or
       (LeftStr(UTF8LowerCase(TutorialURLEdit.Text), 8) = 'https://') then
      TutorialURLUTF8 := TutorialURLEdit.Text;
    if DirectoryExistsUTF8(EntryClientDirEdit.Text) then
      EntryClientDirUTF8 := EntryClientDirEdit.Text;
  end;
  Result := true;
end;

end.


