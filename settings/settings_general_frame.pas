unit settings_general_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, Dialogs, EditBtn,
  settings2_interface, settings2_var;

type

  { TSettings_GeneralFrame }

  TSettings_GeneralFrame = class(TFrame, ISettingsFrame)
    DefaultPasteCombo: TComboBox;
    DefaultSaveTypeComboBox: TComboBox;
    EditButton1: TEditButton;
    FontDialog1: TFontDialog;
    Label1: TLabel;
    Label18: TLabel;
    Label2: TLabel;
    MultipleInstanceChkbox: TCheckBox;
    SaveWindowPositionsChkBox: TCheckBox;
    ShowWelcomeChkBox: TCheckBox;
    ShowWorkToolBarChkBox: TCheckBox;
    procedure EditButton1ButtonClick(Sender: TObject);
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
  settings2, epimiscutils,strutils, main, graphics;

{ TSettings_GeneralFrame }

procedure TSettings_GeneralFrame.EditButton1ButtonClick(Sender: TObject);
begin
  FontDialog1.Font := MainForm.Font;
  if not FontDialog1.Execute then exit;
  EditButton1.Text := FontDialog1.Font.Name + ' (' + IntToStr(FontDialog1.Font.Size) + ')';
  EditButton1.Font.Assign(FontDialog1.Font);
end;

constructor TSettings_GeneralFrame.Create(TheOwner: TComponent);
var
  S: String;
begin
  inherited Create(TheOwner);
  S := GetEpiDialogFilter(true, true, false, false, false, false, false, false, false, false, false);
  DefaultSaveTypeComboBox.AddItem(Copy2SymbDel(S, '|'), nil); Copy2SymbDel(S, '|');
  DefaultSaveTypeComboBox.AddItem(Copy2SymbDel(S, '|'), nil); Copy2SymbDel(S, '|');
  DefaultSaveTypeComboBox.ItemIndex := 0;
end;

procedure TSettings_GeneralFrame.SetSettings(Data: PManagerSettings);
begin
  FData := Data;
  with FData^ do
  begin
    DefaultPasteCombo.ItemIndex := PasteSpecialType;
    DefaultSaveTypeComboBox.ItemIndex := SaveType;
    SaveWindowPositionsChkBox.Checked := SaveWindowPositions;
    ShowWelcomeChkBox.Checked         := ShowWelcome;
    ShowWorkToolBarChkBox.Checked     := ShowWorkToolBar;
    MultipleInstanceChkbox.Checked    := MultipleInstances;
    if Assigned(DesignerFont) then
      EditButton1.Text                  := DesignerFont.Name + ' (' + IntToStr(DesignerFont.Size) + ')';
  end;
end;

function TSettings_GeneralFrame.ApplySettings: boolean;
begin
  with FData^ do
  begin
    PasteSpecialType    := DefaultPasteCombo.ItemIndex;
    SaveType            := DefaultSaveTypeComboBox.ItemIndex;
    SaveWindowPositions := SaveWindowPositionsChkBox.Checked;
    ShowWelcome         := ShowWelcomeChkBox.Checked;
    ShowWorkToolBar     := ShowWorkToolBarChkBox.Checked;
    MultipleInstances   := MultipleInstanceChkbox.Checked;
    if not Assigned(DesignerFont) then
      DesignerFont := TFont.Create;
    DesignerFont.Assign(EditButton1.Font)
  end;
  Result := true;
end;

end.

