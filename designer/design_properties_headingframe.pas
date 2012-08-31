unit design_properties_headingframe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls,
  epicustombase, design_types, design_properties_baseframe;

type

  { THeadingPropertiesFrame }

  THeadingPropertiesFrame = class(TDesignPropertiesFrame, IDesignPropertiesFrame)
    NameEdit: TEdit;
    CaptionEdit: TEdit;
    Label10: TLabel;
    Label9: TLabel;
  private
    { private declarations }
    FHeadings: TEpiCustomControlItemArray;
    procedure UpdateVisibility;
    procedure UpdateContent;
  public
    { public declarations }
    procedure SetEpiControls(EpiControls: TEpiCustomControlItemArray);
    procedure ResetControls;
    function ApplyChanges: boolean;
  end;

implementation

{$R *.lfm}

uses
  epidatafiles, LazUTF8, LCLIntf, LMessages;

{ THeadingPropertiesFrame }

procedure THeadingPropertiesFrame.UpdateVisibility;
begin
  if Length(FHeadings) > 1 then
    NameEdit.Enabled := false;
end;

procedure THeadingPropertiesFrame.UpdateContent;
var
  i: Integer;
begin
  NameEdit.Text := TEpiHeading(FHeadings[0]).Name;

  CaptionEdit.Text := TEpiHeading(FHeadings[0]).Caption.Text;
  for i := Low(FHeadings)+1 to High(FHeadings) do
    if CaptionEdit.Text <> TEpiHeading(FHeadings[i]).Caption.Text then
      begin
        CaptionEdit.Text := '';
        break;
      end;
end;

procedure THeadingPropertiesFrame.SetEpiControls(
  EpiControls: TEpiCustomControlItemArray);
begin
  FHeadings := EpiControls;

  if not Assigned(FHeadings[0]) then exit;

  UpdateVisibility;
  UpdateContent;
end;

procedure THeadingPropertiesFrame.ResetControls;
begin
  UpdateContent;
end;

function THeadingPropertiesFrame.ApplyChanges: boolean;
var
  i: Integer;
begin
  result := false;

  if NameEdit.Modified then
    if not TEpiHeading(FHeadings[0]).ValidateRename(NameEdit.Text, false)
    then
      begin
        ShowHintMsg('Name already exists or invalid identifier', NameEdit);
        Exit;
      end;

  if (CaptionEdit.Modified) and
     (UTF8Length(CaptionEdit.Text) = 0)
  then
    begin
      ShowHintMsg('Empty heading not allowed...', CaptionEdit);
      Exit;
    end;

  if NameEdit.Modified
  then
    TEpiHeading(FHeadings[0]).Name := NameEdit.Text;

  if (CaptionEdit.Modified)
  then
    for i := Low(FHeadings) to High(FHeadings) do
      TEpiHeading(FHeadings[i]).Caption.Text := CaptionEdit.Text;

  ShowHintMsg('', nil);
  result := true;
end;

end.

