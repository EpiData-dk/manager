unit design_headingproperties_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, design_controls,
  epicustombase, design_propertiesbase_frame;

type

  { THeadingPropertiesFrame }

  THeadingPropertiesFrame = class(TDesignPropertiesFrame)
    CaptionEdit: TEdit;
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
  CaptionEdit.Text := TEpiHeading(EpiControl).Caption.Text;
  UpdateCaption('Heading Properties');
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

  if UTF8Length(CaptionEdit.Text) = 0 then
  begin
    ShowHintMsg('Empty heading not allowed...', CaptionEdit);
    Exit;
  end;

  FHeading.BeginUpdate;
  FHeading.Caption.Text := CaptionEdit.Text;
  FHeading.EndUpdate;

  Result := true;
end;

end.

