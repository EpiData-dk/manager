unit design_properties_dataformframe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, Buttons, Spin,
  MaskEdit, design_properties_baseframe, design_types, epicustombase,
  epidatafiles;

type

  { TDataformPropertiesFrame }

  TDataformPropertiesFrame = class(TDesignPropertiesFrame, IDesignPropertiesFrame)
    CaptionEdit: TEdit;
    GroupAssignedListBox: TListBox;
    GroupAvailableListBox: TListBox;
    GroupBox1: TGroupBox;
    GrpRightsMoveLeft: TSpeedButton;
    GrpRightsMoveRight: TSpeedButton;
    Label5: TLabel;
    Label6: TLabel;
    Label9: TLabel;
    MaskEdit1: TMaskEdit;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    SectionGroupAccessGroupBox: TGroupBox;
    procedure AllowedRecordsEditKeyPress(Sender: TObject; var Key: char);
    procedure MaskEdit1EditingDone(Sender: TObject);
    procedure RadioButton1Click(Sender: TObject);
  private
    procedure DoUpdateCaption;
    procedure UpdateVisibility;
    procedure UpdateContent;
  public
    procedure FocusOnNewControl;
    procedure SetEpiControls(EpiControls: TEpiCustomControlItemArray);
    procedure ResetControls;
    function ApplyChanges: boolean;
  end;

implementation

{$R *.lfm}

uses
  epirelations;

{ TDataformPropertiesFrame }

procedure TDataformPropertiesFrame.AllowedRecordsEditKeyPress(Sender: TObject; var Key: char
  );
begin
  if not (Key in ['0'..'9', #8]) then Key := #0;
end;

procedure TDataformPropertiesFrame.MaskEdit1EditingDone(Sender: TObject);
begin
  //
end;

procedure TDataformPropertiesFrame.RadioButton1Click(Sender: TObject);
begin
  MaskEdit1.Enabled := (Sender = RadioButton2);
end;

procedure TDataformPropertiesFrame.DoUpdateCaption;
begin
  UpdateCaption('DataForm Properties: ' + DataFile.Caption.Text);
end;

procedure TDataformPropertiesFrame.UpdateVisibility;
var
  MasterDF: TEpiDataFile;
begin
  SectionGroupAccessGroupBox.Visible := false;
  GroupBox1.Visible := Relation.InheritsFrom(TEpiDetailRelation);

  if GroupBox1.Visible then
  begin
    MasterDF := TEpiDetailRelation(Relation).MasterRelation.Datafile;
    GroupBox1.Enabled := (MasterDF.KeyFields.Count < DataFile.KeyFields.Count);
  end;
end;

procedure TDataformPropertiesFrame.UpdateContent;
begin
  CaptionEdit.Text := DataFile.Caption.Text;

  if GroupBox1.Visible then
    if GroupBox1.Enabled then
      begin
        if (TEpiDetailRelation(Relation).MaxRecordCount = 0) then
          RadioButton1.Checked := true
        else
          RadioButton2.Checked := true;
        MaskEdit1.Text := IntToStr(TEpiDetailRelation(Relation).MaxRecordCount);
      end
    else
      begin
        RadioButton2.Checked := true;
        MaskEdit1.Text := '1';
      end;
end;

procedure TDataformPropertiesFrame.FocusOnNewControl;
begin
  CaptionEdit.SetFocus;
end;

procedure TDataformPropertiesFrame.SetEpiControls(EpiControls: TEpiCustomControlItemArray);
begin
  if not Assigned(DataFile) then exit;
  if not Assigned(Relation) then exit;

  UpdateVisibility;
  UpdateContent;
  DoUpdateCaption;
end;

procedure TDataformPropertiesFrame.ResetControls;
begin
  UpdateVisibility;
  UpdateContent;
end;

function TDataformPropertiesFrame.ApplyChanges: boolean;
var
  S: String;
begin
  result := true;

  if not Assigned(DataFile) then exit;
  if not Assigned(Relation) then exit;

  if CaptionEdit.Modified then
  begin
    if CaptionEdit.Text = '' then
    begin
      ShowHintMsg('A dataform name cannot be empty!', CaptionEdit);
      Exit(false);
    end;
  end;

  S := Trim(MaskEdit1.Text);
  if (GroupBox1.Visible) and
     (RadioButton2.Checked) and
     (
      (S = '') or
      (StrToInt(S) = 0)
     )
  then
  begin
    ShowHintMsg('Number of child records cannot be empty or 0!', MaskEdit1);
    Exit(false);
  end;

  DataFile.Caption.Text := CaptionEdit.Text;

  if GroupBox1.Visible then
  with TEpiDetailRelation(Relation) do
  begin
    if RadioButton1.Checked then
      MaxRecordCount := 0
    else
      MaxRecordCount := StrToInt(S);
  end;

  ShowHintMsg('', nil);
  DoUpdateCaption;
  Result := true;
end;

end.

