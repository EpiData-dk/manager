unit settings_visualdesign_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, MaskEdit, EditBtn,
  Dialogs, settings2_interface, settings2_var, Graphics, ExtCtrls;

type

  { TSettings_VisualDesign }

  TSettings_VisualDesign = class(TFrame, ISettingsFrame)
    ColorDialog1: TColorDialog;
    DefaultRightPosEdit: TMaskEdit;
    DistGrpBox: TGroupBox;
    FieldColourBtn: TColorButton;
    FieldFieldEdit: TMaskEdit;
    FieldFontEditBtn: TEditButton;
    FieldLabelEdit: TMaskEdit;
    FontDialog1: TFontDialog;
    FontGrpBox: TGroupBox;
    HeadingColourBtn: TColorButton;
    HeadingFontEditBtn: TEditButton;
    Label10: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LabelLabelEdit: TMaskEdit;
    LabelLeftPosition: TMaskEdit;
    PositionsGrpBox: TGroupBox;
    OutputFormatRadioGrp: TRadioGroup;
    ScrollBox1: TScrollBox;
    SectionColourBtn: TColorButton;
    SectionFontEditBtn: TEditButton;
    procedure FieldColourBtnColorChanged(Sender: TObject);
    procedure FieldFontEditBtnButtonClick(Sender: TObject);
  private
    { private declarations }
    FData: PManagerSettings;
    procedure SetFont(AFont: TFont; Btn: TEditButton);
  public
    { public declarations }
    procedure SetSettings(Data: PManagerSettings);
    function  ApplySettings: boolean;
  end;

implementation

{$R *.lfm}

uses
  settings2;


{ TSettings_VisualDesign }

procedure TSettings_VisualDesign.FieldFontEditBtnButtonClick(Sender: TObject);
var
  Btn: TEditButton absolute Sender;
begin
  FontDialog1.Font.Assign(Btn.Font);
  if not FontDialog1.Execute then exit;
  SetFont(FontDialog1.Font, Btn);
end;

procedure TSettings_VisualDesign.FieldColourBtnColorChanged(Sender: TObject);
var
  Btn: TEditButton;
begin
  if Sender = FieldColourBtn then
    Btn := FieldFontEditBtn;
  if Sender = HeadingColourBtn then
    Btn := HeadingFontEditBtn;
  if SEnder = SectionColourBtn then
    Btn := SectionFontEditBtn;
  Btn.Font.Color := TColorButton(Sender).ButtonColor;
end;

procedure TSettings_VisualDesign.SetFont(AFont: TFont; Btn: TEditButton);
begin
  Btn.Text := AFont.Name + ' (' + IntToStr(AFont.Size) + ')';
  Btn.Font.Assign(AFont);
end;

procedure TSettings_VisualDesign.SetSettings(Data: PManagerSettings);
begin
  FData := Data;

  with FData^ do
  begin
    DefaultRightPosEdit.Text          := IntToStr(DefaultRightPosition);
    LabelLeftPosition.Text               := IntToStr(DefaultLabelPosition);
    FieldFieldEdit.Text               := IntToStr(SpaceBtwFieldField);
    FieldLabelEdit.Text               := IntToStr(SpaceBtwFieldLabel);
    LabelLabelEdit.Text               := IntToStr(SpaceBtwLabelLabel);
    SetFont(FieldFont, FieldFontEditBtn);
    FieldColourBtn.ButtonColor := FieldFont.Color;
    SetFont(HeadingFont, HeadingFontEditBtn);
    HeadingColourBtn.ButtonColor := HeadingFont.Color;
    SetFont(SectionFont, SectionFontEditBtn);
    SectionColourBtn.ButtonColor := SectionFont.Color;

    OutputFormatRadioGrp.ItemIndex := ReportOutputFormat;
  end;
end;

function TSettings_VisualDesign.ApplySettings: boolean;
var
  S: String;
begin
  // Visual desing:
  with FData^ do
  begin
    S := Trim(DefaultRightPosEdit.Text);
    if not((S = '') or (StrToInt(S) <= 0)) then
      DefaultRightPosition := StrToInt(S);
    S := Trim(LabelLeftPosition.Text);
    if not((S = '') or (StrToInt(S) <= 0)) then
      DefaultLabelPosition := StrToInt(S);
    S := Trim(FieldFieldEdit.Text);
    if not((S = '') or (StrToInt(S) <= 0)) then
      SpaceBtwFieldField := StrToInt(S);
    S := Trim(FieldLabelEdit.Text);
    if not((S = '') or (StrToInt(S) <= 0)) then
      SpaceBtwFieldLabel := StrToInt(S);
    S := Trim(LabelLabelEdit.Text);
    if not((S = '') or (StrToInt(S) <= 0)) then
      SpaceBtwLabelLabel := StrToInt(S);
    FieldFont.Assign(FieldFontEditBtn.Font);
    HeadingFont.Assign(HeadingFontEditBtn.Font);
    SectionFont.Assign(SectionFontEditBtn.Font);
    ReportOutputFormat := OutputFormatRadioGrp.ItemIndex;
  end;
  Result := true;
end;

end.

