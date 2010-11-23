unit settings_advanced_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, EditBtn,
  settings2_interface, settings2_var;

type

  { TSettings_AdvancedFrame }

  TSettings_AdvancedFrame = class(TFrame, ISettingsFrame)
    SaveWindowPositionsChkBox: TCheckBox;
    DefaultSaveTypeComboBox: TComboBox;
    DefaultPasteCombo: TComboBox;
    Label1: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    WorkingDirEdit: TDirectoryEdit;
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
  settings2, epimiscutils, strutils;


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
    DefaultPasteCombo.ItemIndex := PasteSpecialType;
    DefaultSaveTypeComboBox.ItemIndex := SaveType;
    SaveWindowPositionsChkBox.Checked := SaveWindowPositions;
  end;
end;

function TSettings_AdvancedFrame.ApplySettings: boolean;
begin
  with FData^ do
  begin
    if DirectoryExistsUTF8(WorkingDirEdit.Text) then
      WorkingDirUTF8 := WorkingDirEdit.Text;
    PasteSpecialType := DefaultPasteCombo.ItemIndex;
    SaveType         := DefaultSaveTypeComboBox.ItemIndex;
    SaveWindowPositions := SaveWindowPositionsChkBox.Checked;
  end;
  Result := true;
end;

initialization

begin
  RegisterSettingFrame(2, TSettings_AdvancedFrame, 'Advanced');
end;

end.


