unit settings_general_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, Dialogs, EditBtn,
  ExtCtrls, MaskEdit, settings2_interface, settings2_var;

type

  { TSettings_GeneralFrame }

  TSettings_GeneralFrame = class(TFrame, ISettingsFrame)
    AutomaticUpdatesChkBox: TCheckBox;
    CheckUpdateGrpBox: TGroupBox;
    Label2: TLabel;
    DaysBetweenUpdatedEdit: TMaskEdit;
    OutputFormatRadioGrp: TRadioGroup;
    ShowA4LinesChkBox: TCheckBox;
    UnAssociateBtn: TButton;
    AssociateBtn: TButton;
    DefaultSaveTypeComboBox: TComboBox;
    Label1: TLabel;
    AssociateLabel: TLabel;
    MultipleInstanceChkbox: TCheckBox;
    SaveWindowPositionsChkBox: TCheckBox;
    ShowWorkToolBarChkBox: TCheckBox;
    procedure AssociateBtnClick(Sender: TObject);
    procedure AutomaticUpdatesChkBoxChange(Sender: TObject);
    procedure UnAssociateBtnClick(Sender: TObject);
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
  settings2, epimiscutils, strutils
  {$IFDEF WINDOWS}
  ,registry, ufileassociation
  {$ENDIF};

{ TSettings_GeneralFrame }

procedure TSettings_GeneralFrame.AssociateBtnClick(Sender: TObject);
begin
{$IFDEF WINDOWS}
  AssociateFiles('EpiData Manager', 'Manager Tool', Application.ExeName);
{$ENDIF}
end;

procedure TSettings_GeneralFrame.AutomaticUpdatesChkBoxChange(Sender: TObject);
begin
  DaysBetweenUpdatedEdit.Enabled := TCheckBox(Sender).Checked;
end;

procedure TSettings_GeneralFrame.UnAssociateBtnClick(Sender: TObject);
begin
{$IFDEF WINDOWS}
  UnAssociateFiles('EpiData Manager', 'Manager Tool', Application.ExeName);
{$ENDIF}
end;

constructor TSettings_GeneralFrame.Create(TheOwner: TComponent);
var
  S: String;
begin
  inherited Create(TheOwner);
  S := GetEpiDialogFilter([dfEPX, dfEPZ]);
  DefaultSaveTypeComboBox.AddItem(Copy2SymbDel(S, '|'), nil); Copy2SymbDel(S, '|');
  DefaultSaveTypeComboBox.AddItem(Copy2SymbDel(S, '|'), nil); Copy2SymbDel(S, '|');
  DefaultSaveTypeComboBox.ItemIndex := 0;
  OutputFormatRadioGrp.ItemIndex := 0;
  {$IFNDEF WINDOWS}
  AssociateBtn.Visible := false;
  UnAssociateBtn.Visible := false;
  AssociateLabel.Visible := false;
  {$ENDIF}
end;

procedure TSettings_GeneralFrame.SetSettings(Data: PManagerSettings);
begin
  FData := Data;
  with FData^ do
  begin
    DefaultSaveTypeComboBox.ItemIndex := SaveType;
    SaveWindowPositionsChkBox.Checked := SaveWindowPositions;
    ShowWorkToolBarChkBox.Checked     := ShowWorkToolBar;
    ShowA4LinesChkBox.Checked         := ShowA4GuideLines;
    MultipleInstanceChkbox.Checked    := MultipleInstances;
    AutomaticUpdatesChkBox.Checked    := CheckForUpdates;
    DaysBetweenUpdatedEdit.EditText   := IntToStr(DaysBetweenChecks);

    OutputFormatRadioGrp.ItemIndex := ReportOutputFormat;
  end;

  AutomaticUpdatesChkBoxChange(AutomaticUpdatesChkBox);
end;

function TSettings_GeneralFrame.ApplySettings: boolean;
begin
  with FData^ do
  begin
    if AutomaticUpdatesChkBox.Checked and
       (Trim(DaysBetweenUpdatedEdit.EditText) = '')
    then
      Exit(false);

    ReportOutputFormat := OutputFormatRadioGrp.ItemIndex;

    SaveType            := DefaultSaveTypeComboBox.ItemIndex;
    SaveWindowPositions := SaveWindowPositionsChkBox.Checked;
    ShowWorkToolBar     := ShowWorkToolBarChkBox.Checked;
    MultipleInstances   := MultipleInstanceChkbox.Checked;
    ShowA4GuideLines    := ShowA4LinesChkBox.Checked;
    CheckForUpdates     := AutomaticUpdatesChkBox.Checked;
    if CheckForUpdates then
      DaysBetweenChecks   := StrToInt(Trim(DaysBetweenUpdatedEdit.EditText));;
  end;
  Result := true;
end;

end.

