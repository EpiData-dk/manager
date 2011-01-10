unit settings_advanced_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, EditBtn, FileCtrl,
  settings2_interface, settings2_var;

type

  { TSettings_AdvancedFrame }

  TSettings_AdvancedFrame = class(TFrame, ISettingsFrame)
    TutorialURLEdit: TEdit;
    Label2: TLabel;
    ShowWorkToolBarChkBox: TCheckBox;
    Label19: TLabel;
    ShowWelcomeChkBox: TCheckBox;
    SaveWindowPositionsChkBox: TCheckBox;
    DefaultSaveTypeComboBox: TComboBox;
    DefaultPasteCombo: TComboBox;
    Label1: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    WorkingDirEdit: TDirectoryEdit;
    TutorialDirEdit: TDirectoryEdit;
  private
    { private declarations }
    FData: PManagerSettings;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    procedure SetSettings(Data: PManagerSettings);
    function  ApplySettings: boolean;
  end;

implementation

{$R *.lfm}

uses
  settings2, epimiscutils, strutils, LCLProc;


{ TSettings_AdvancedFrame }

constructor TSettings_AdvancedFrame.Create(TheOwner: TComponent);
var
  S: String;
begin
  inherited Create(TheOwner);
  S := GetEpiDialogFilter(true, true, false, false, false, false, false, false, false, false, false);
  DefaultSaveTypeComboBox.AddItem(Copy2SymbDel(S, '|'), nil); Copy2SymbDel(S, '|');
  DefaultSaveTypeComboBox.AddItem(Copy2SymbDel(S, '|'), nil); Copy2SymbDel(S, '|');
  DefaultSaveTypeComboBox.ItemIndex := 0;
end;

procedure TSettings_AdvancedFrame.SetSettings(Data: PManagerSettings);
begin
  FData := Data;
  with FData^ do
  begin
    WorkingDirEdit.Text := WorkingDirUTF8;
    TutorialDirEdit.Text := TutorialDirUTF8;
    TutorialURLEdit.Text := TutorialURLUTF8;
    DefaultPasteCombo.ItemIndex := PasteSpecialType;
    DefaultSaveTypeComboBox.ItemIndex := SaveType;
    SaveWindowPositionsChkBox.Checked := SaveWindowPositions;
    ShowWelcomeChkBox.Checked         := ShowWelcome;
    ShowWorkToolBarChkBox.Checked     := ShowWorkToolBar;
  end;
end;

function TSettings_AdvancedFrame.ApplySettings: boolean;
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
    PasteSpecialType    := DefaultPasteCombo.ItemIndex;
    SaveType            := DefaultSaveTypeComboBox.ItemIndex;
    SaveWindowPositions := SaveWindowPositionsChkBox.Checked;
    ShowWelcome         := ShowWelcomeChkBox.Checked;
    ShowWorkToolBar     := ShowWorkToolBarChkBox.Checked;
  end;
  Result := true;
end;

initialization

begin
  RegisterSettingFrame(2, TSettings_AdvancedFrame, 'Advanced');
end;

end.


