unit design_properties_dataformframe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, Buttons, Spin,
  design_properties_baseframe, design_types, epicustombase,
  epidatafiles;

type

  { TDataformPropertiesFrame }

  TDataformPropertiesFrame = class(TDesignPropertiesFrame, IDesignPropertiesFrame)
    CaptionEdit: TEdit;
    AllowedRecordsEdit: TEdit;
    GroupAssignedListBox: TListBox;
    GroupAvailableListBox: TListBox;
    GrpRightsMoveLeft: TSpeedButton;
    GrpRightsMoveRight: TSpeedButton;
    Label1: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label9: TLabel;
    SectionGroupAccessGroupBox: TGroupBox;
    procedure AllowedRecordsEditKeyPress(Sender: TObject; var Key: char);
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

procedure TDataformPropertiesFrame.DoUpdateCaption;
begin
  UpdateCaption('DataForm Properties: ' + DataFile.Caption.Text);
end;

procedure TDataformPropertiesFrame.UpdateVisibility;
begin
  SectionGroupAccessGroupBox.Visible := false;

  AllowedRecordsEdit.Enabled := Relation.InheritsFrom(TEpiDetailRelation);
end;

procedure TDataformPropertiesFrame.UpdateContent;
begin
  CaptionEdit.Text := DataFile.Caption.Text;

  if AllowedRecordsEdit.Enabled then;
    AllowedRecordsEdit.Text := IntToStr(TEpiDetailRelation(Relation).MaxRecordCount);
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
  UpdateContent;
end;

function TDataformPropertiesFrame.ApplyChanges: boolean;
begin
  result := true;

  if not Assigned(DataFile) then exit;
  if not Assigned(Relation) then exit;


  if (AllowedRecordsEdit.Text = '')
  then
  begin
    ShowHintMsg('Allowed recordcount must have a number specified!', AllowedRecordsEdit);
    Exit(false);
  end;

  if (StrToInt(AllowedRecordsEdit.Text) < 0)
  then
  begin
    ShowHintMsg('A dataset cannot have a negative recordcount!', AllowedRecordsEdit);
    Exit(false);
  end;

  if CaptionEdit.Modified then
  begin
    if CaptionEdit.Text = '' then
    begin
      ShowHintMsg('A dataform name cannot be empty!', CaptionEdit);
      Exit(false);
    end;
  end;

  DataFile.Caption.Text := CaptionEdit.Text;
  if AllowedRecordsEdit.Enabled then
    TEpiDetailRelation(Relation).MaxRecordCount := StrToInt(AllowedRecordsEdit.Text);

  ShowHintMsg('', nil);
  DoUpdateCaption;
  Result := true;
end;

end.

