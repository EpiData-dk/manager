unit design_headingproperties_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls,
  epicustombase, design_types;

type

  { THeadingPropertiesFrame }

  THeadingPropertiesFrame = class(TFrame, IDesignPropertiesFrame)
    NameEdit: TEdit;
    CaptionEdit: TEdit;
    Label10: TLabel;
    Label9: TLabel;
  private
    { private declarations }
  protected
    procedure SetEpiControl(const AValue: TEpiCustomControlItem); override;
    procedure ShiftToTabSheet(const SheetNo: Byte); override;
  public
    { public declarations }
    function ValidateControl: boolean; override;
    procedure UpdateFormContent; override;
    procedure ForceShow; override;
  end;

implementation

{$R *.lfm}

uses
  epidatafiles, LCLProc;
{ THeadingPropertiesFrame }


procedure THeadingPropertiesFrame.SetEpiControl(
  const AValue: TEpiCustomControlItem);
begin
  inherited SetEpiControl(AValue);
  UpdateFormContent;
end;

procedure THeadingPropertiesFrame.ShiftToTabSheet(const SheetNo: Byte);
begin
  // Do nothing - has no tab-sheets.
end;

procedure THeadingPropertiesFrame.UpdateFormContent;
begin
  NameEdit.Text := TEpiHeading(EpiControl).Name;
  CaptionEdit.Text := TEpiHeading(EpiControl).Caption.Text;
  UpdateCaption('Heading Properties: ' + TEpiHeading(EpiControl).Name);
end;

procedure THeadingPropertiesFrame.ForceShow;
begin
  inherited ForceShow;
  CaptionEdit.SetFocus;
end;

function THeadingPropertiesFrame.ValidateControl: boolean;
var
  FHeading: TEpiHeading;
begin
  result := false;
  FHeading := TEpiHeading(EpiControl);

  if not FHeading.ValidateRename(NameEdit.Text, false) then
  begin
    ShowHintMsg('Name already exists or invalid identifier', NameEdit);
    Exit;
  end;

  if UTF8Length(CaptionEdit.Text) = 0 then
  begin
    ShowHintMsg('Empty heading not allowed...', CaptionEdit);
    Exit;
  end;

  FHeading.BeginUpdate;
  FHeading.Caption.Text := CaptionEdit.Text;
  FHeading.Name := NameEdit.Text;
  FHeading.EndUpdate;

  Result := true;
end;

end.

