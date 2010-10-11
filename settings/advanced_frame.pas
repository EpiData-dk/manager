unit advanced_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, EditBtn,
  settings2_interface, settings2_var;

type

  { TAdvancedFrame }

  TAdvancedFrame = class(TFrame, ISettingsFrame)
    DefaultPasteCombo: TComboBox;
    Label17: TLabel;
    Label18: TLabel;
    WorkingDirEdit: TDirectoryEdit;
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
  settings2;


{ TAdvancedFrame }

procedure TAdvancedFrame.SetSettings(Data: PManagerSettings);
begin
  with Data^ do
  begin
    WorkingDirEdit.Text := WorkingDirUTF8;
    DefaultPasteCombo.ItemIndex := PasteSpecialType;
  end;
end;

function TAdvancedFrame.ApplySettings: boolean;
begin
  with FData^ do
  begin
    if DirectoryExistsUTF8(WorkingDirEdit.Text) then
      WorkingDirUTF8 := WorkingDirEdit.Text;
    PasteSpecialType := DefaultPasteCombo.ItemIndex;
  end;
  Result := true;
end;

initialization

begin
  RegisterSettingFrame(2, TAdvancedFrame, 'Advanced');
end;

end.


