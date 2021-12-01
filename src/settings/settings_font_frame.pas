unit settings_font_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, EditBtn, Dialogs,
  settings2_interface, settings2_var, Graphics;

type

  { TSettingsFontFrame }

  TSettingsFontFrame = class(TFrame, ISettingsFrame)
    ColorDialog1: TColorDialog;
    FieldColourBtn: TColorButton;
    FontDialog1: TFontDialog;
    HeadingColourBtn1: TColorButton;
    FieldFontEditBtn: TEditButton;
    HeadingColourBtn2: TColorButton;
    HeadingColourBtn3: TColorButton;
    HeadingColourBtn4: TColorButton;
    HeadingColourBtn5: TColorButton;
    HeadingFontEditBtn1: TEditButton;
    FontGrpBox: TGroupBox;
    HeadingFontEditBtn2: TEditButton;
    HeadingFontEditBtn3: TEditButton;
    HeadingFontEditBtn4: TEditButton;
    HeadingFontEditBtn5: TEditButton;
    HeadingsGrpBox: TGroupBox;
    Label10: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    SectionColourBtn: TColorButton;
    SectionFontEditBtn: TEditButton;
    procedure FontChangeClick(Sender: TObject);
    procedure FontColorChange(Sender: TObject);
  private
    { private declarations }
    FData: PManagerSettings;
    procedure SetFont(AFont: TFont; Btn: TEditButton);
  public
    { public declarations }
    procedure SetSettings(Data: PManagerSettings);
    function ApplySettings: boolean;
  end;

implementation

{$R *.lfm}

{ TSettingsFontFrame }

procedure TSettingsFontFrame.FontColorChange(Sender: TObject);
var
  Btn: TEditButton;
begin
  if Sender = FieldColourBtn then
    Btn := FieldFontEditBtn;
  if SEnder = SectionColourBtn then
    Btn := SectionFontEditBtn;
  if Sender = HeadingColourBtn1 then
    Btn := HeadingFontEditBtn1;
  if Sender = HeadingColourBtn2 then
    Btn := HeadingFontEditBtn2;
  if Sender = HeadingColourBtn3 then
    Btn := HeadingFontEditBtn3;
  if Sender = HeadingColourBtn4 then
    Btn := HeadingFontEditBtn4;
  if Sender = HeadingColourBtn5 then
    Btn := HeadingFontEditBtn5;

  Btn.Font.Color := TColorButton(Sender).ButtonColor;
end;

procedure TSettingsFontFrame.SetFont(AFont: TFont; Btn: TEditButton);
begin
  Btn.Text := AFont.Name + ' (' + IntToStr(AFont.Size) + ')';
  Btn.Font.Assign(AFont);
end;

procedure TSettingsFontFrame.SetSettings(Data: PManagerSettings);
begin
  FData := Data;

  with FData^ do
  begin
    SetFont(FieldFont, FieldFontEditBtn);
    FieldColourBtn.ButtonColor := FieldFont.Color;
    SetFont(SectionFont, SectionFontEditBtn);
    SectionColourBtn.ButtonColor := SectionFont.Color;

    SetFont(HeadingFont1, HeadingFontEditBtn1);
    HeadingColourBtn1.ButtonColor := HeadingFont1.Color;
    SetFont(HeadingFont2, HeadingFontEditBtn2);
    HeadingColourBtn2.ButtonColor := HeadingFont2.Color;
    SetFont(HeadingFont3, HeadingFontEditBtn3);
    HeadingColourBtn3.ButtonColor := HeadingFont3.Color;
    SetFont(HeadingFont4, HeadingFontEditBtn4);
    HeadingColourBtn4.ButtonColor := HeadingFont4.Color;
    SetFont(HeadingFont5, HeadingFontEditBtn5);
    HeadingColourBtn5.ButtonColor := HeadingFont5.Color;
  end;
end;

function TSettingsFontFrame.ApplySettings: boolean;
begin
  with FData^ do
  begin
    FieldFont.Assign(FieldFontEditBtn.Font);
    SectionFont.Assign(SectionFontEditBtn.Font);
    HeadingFont1.Assign(HeadingFontEditBtn1.Font);
    HeadingFont2.Assign(HeadingFontEditBtn2.Font);
    HeadingFont3.Assign(HeadingFontEditBtn3.Font);
    HeadingFont4.Assign(HeadingFontEditBtn4.Font);
    HeadingFont5.Assign(HeadingFontEditBtn5.Font);
  end;

  Result := true;
end;

procedure TSettingsFontFrame.FontChangeClick(Sender: TObject);
var
  Btn: TEditButton absolute Sender;
begin
  FontDialog1.Font.Assign(Btn.Font);
  if not FontDialog1.Execute then exit;
  SetFont(FontDialog1.Font, Btn);
end;

end.

