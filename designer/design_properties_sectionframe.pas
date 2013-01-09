unit design_properties_sectionframe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, StdCtrls, Buttons,
  epicustombase, design_types, design_properties_baseframe;

type

  { TSectionPropertiesFrame }

  TSectionPropertiesFrame = class(TDesignPropertiesFrame, IDesignPropertiesFrame)
    CaptionEdit: TEdit;
    GroupAssignedListBox: TListBox;
    GroupAvailableListBox: TListBox;
    GrpRightsMoveLeft: TSpeedButton;
    GrpRightsMoveRight: TSpeedButton;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label9: TLabel;
    NameEdit: TEdit;
    SectionBasicSheet: TTabSheet;
    SectionGroupAccessGroupBox: TGroupBox;
    SectionPageControl: TPageControl;
  private
    { private declarations }
    FSections: TEpiCustomControlItemArray;
    procedure UpdateVisibility;
    procedure UpdateContent;
    procedure DoUpdateCaption;
  public
    { public declarations }
    procedure FocusOnNewControl;
    procedure SetEpiControls(EpiControls: TEpiCustomControlItemArray);
    procedure ResetControls;
    function ApplyChanges: boolean;
  end;

implementation

{$R *.lfm}

uses
  epidatafiles, LazUTF8, epistringutils;

{ TSectionPropertiesFrame }

procedure TSectionPropertiesFrame.UpdateVisibility;
begin
  NameEdit.Enabled :=
    (Length(FSections) = 1) and
    (FSections[0].Name <> 'MAIN');
  CaptionEdit.Enabled := NameEdit.Enabled;

  {$IFNDEF EPI_DEBUG}
  SectionGroupAccessGroupBox.Visible := false;
  SectionGroupAccessGroupBox.Enabled := false;
  {$ELSE}
  SectionGroupAccessGroupBox.Visible := true;
  {$ENDIF}
end;

procedure TSectionPropertiesFrame.UpdateContent;
var
  i: Integer;
begin
  NameEdit.Text := FSections[0].Name;
  CaptionEdit.Text := TEpiSection(FSections[0]).Caption.Text;

  for i := Low(FSections)+1 to High(FSections) do
    if TEpiSection(FSections[i]).Caption.Text <> CaptionEdit.Text then
      begin
        CaptionEdit.Text := '';
        break;
      end;
end;

procedure TSectionPropertiesFrame.DoUpdateCaption;
var
  S: String;
  i: Integer;
begin
  S := FSections[0].Name;
  for i := 1 to High(FSections) do
    S := S + ', ' + FSections[i].Name;

  S := EpiCutString(S, 20);
  UpdateCaption('Sections Properties: ' + S);
end;

procedure TSectionPropertiesFrame.FocusOnNewControl;
begin
  CaptionEdit.SetFocus;
end;

procedure TSectionPropertiesFrame.SetEpiControls(
  EpiControls: TEpiCustomControlItemArray);
begin
  FSections := EpiControls;

  if not Assigned(FSections[0]) then exit;

  UpdateVisibility;
  UpdateContent;
  DoUpdateCaption;
end;

procedure TSectionPropertiesFrame.ResetControls;
begin
  UpdateContent;
end;

function TSectionPropertiesFrame.ApplyChanges: boolean;
var
  i: Integer;
begin
  result := false;

  if NameEdit.Modified then
    if not FSections[0].ValidateRename(NameEdit.Text, false)
    then
      begin
        ShowHintMsg('Name already exists or invalid identifier', NameEdit);
        Exit;
      end;

  if CaptionEdit.Modified then
    for i := Low(FSections) to High(FSections) do
      TEpiSection(FSections[i]).Caption.Text := CaptionEdit.Text;

  ShowHintMsg('', nil);
  Result := true;

  DoUpdateCaption;
end;

end.

