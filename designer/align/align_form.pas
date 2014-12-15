unit align_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  Spin, StdCtrls, ExtCtrls, design_runtimedesigner, design_types;

type

  { TAlignmentForm }

  TAlignmentForm = class(TForm)
    SidesgrpBox: TGroupBox;
    CenterGrpBox: TGroupBox;
    EvenDistGrpBox: TGroupBox;
    FixedDistGrpBox: TGroupBox;
    TopAlignBtn: TBitBtn;
    FixedDistHorzBtn: TBitBtn;
    LeftAlignBtn: TBitBtn;
    RightAlignBtn: TBitBtn;
    BottomAlignBtn: TBitBtn;
    EvenDistVertBtn: TBitBtn;
    EvenDistHorzBtn: TBitBtn;
    CenterVertBtn: TBitBtn;
    CenterHorzBtn: TBitBtn;
    FixedDistVertBtn: TBitBtn;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    procedure BottomAlignBtnClick(Sender: TObject);
    procedure CenterVertBtnClick(Sender: TObject);
    procedure CenterHorzBtnClick(Sender: TObject);
    procedure EvenDistVertBtnClick(Sender: TObject);
    procedure EvenDistHorzBtnClick(Sender: TObject);
    procedure FixedDistVertBtnClick(Sender: TObject);
    procedure FixedDistHorzBtnClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure LeftAlignBtnClick(Sender: TObject);
    procedure RightAlignBtnClick(Sender: TObject);
    procedure TopAlignBtnClick(Sender: TObject);
  private
    FDesignFrame: TRuntimeDesignFrame;
    { private declarations }
    procedure DoAlignControls(AAlignment: TDesignControlsAlignment;
      Const FixedDist: Integer = -1);
    procedure LoadGlyphs;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    property DesignFrame: TRuntimeDesignFrame read FDesignFrame write FDesignFrame;
    class procedure RestoreDefaultPos;
  end;

var
  AlignForm: TAlignmentForm;


implementation

{$R *.lfm}

uses
  settings2, settings2_var, LCLType, epiv_datamodule;

{ TAlignmentForm }

procedure TAlignmentForm.EvenDistVertBtnClick(Sender: TObject);
begin
  DoAlignControls(dcaEvenVert);
end;

procedure TAlignmentForm.EvenDistHorzBtnClick(Sender: TObject);
begin
  DoAlignControls(dcaEvenHorz);
end;

procedure TAlignmentForm.FixedDistVertBtnClick(Sender: TObject);
begin
  DoAlignControls(dcaFixedVert, SpinEdit1.Value);
end;

procedure TAlignmentForm.FixedDistHorzBtnClick(Sender: TObject);
begin
  DoAlignControls(dcaFixedHorz, SpinEdit2.Value);
end;

procedure TAlignmentForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := true;
  if ManagerSettings.SaveWindowPositions
  then
    SaveFormPosition(Self, 'AlignmentForm');
end;

procedure TAlignmentForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) and (Shift = []) then
  begin
    Key := VK_UNKNOWN;
    Close;
  end;
end;

procedure TAlignmentForm.FormShow(Sender: TObject);
begin
  if ManagerSettings.SaveWindowPositions then
    LoadFormPosition(Self, 'AlignmentForm');
end;

procedure TAlignmentForm.BottomAlignBtnClick(Sender: TObject);
begin
  DoAlignControls(dcaBottomMost);
end;

procedure TAlignmentForm.CenterVertBtnClick(Sender: TObject);
begin
  DoAlignControls(dcaCenterVert);
end;

procedure TAlignmentForm.CenterHorzBtnClick(Sender: TObject);
begin
  DoAlignControls(dcaCenterHorz);
end;

procedure TAlignmentForm.LeftAlignBtnClick(Sender: TObject);
begin
  DoAlignControls(dcaLeftMost);
end;

procedure TAlignmentForm.RightAlignBtnClick(Sender: TObject);
begin
  DoAlignControls(dcaRightMost);
end;

procedure TAlignmentForm.TopAlignBtnClick(Sender: TObject);
begin
  DoAlignControls(dcaTopMost);
end;

procedure TAlignmentForm.DoAlignControls(AAlignment: TDesignControlsAlignment;
  const FixedDist: Integer);
begin
  if Assigned(DesignFrame) then
    DesignFrame.AlignControls(AAlignment, FixedDist);
end;

procedure TAlignmentForm.LoadGlyphs;
var
  LConstraints: TSizeConstraints;
begin
  DM.Icons16.GetBitmap(23, CenterHorzBtn.Glyph);
  DM.Icons16.GetBitmap(24, CenterVertBtn.Glyph);
  DM.Icons16.GetBitmap(25, LeftAlignBtn.Glyph);
  DM.Icons16.GetBitmap(26, RightAlignBtn.Glyph);
  DM.Icons16.GetBitmap(27, EvenDistHorzBtn.Glyph);
  DM.Icons16.GetBitmap(27, FixedDistHorzBtn.Glyph);
  DM.Icons16.GetBitmap(28, EvenDistVertBtn.Glyph);
  DM.Icons16.GetBitmap(28, FixedDistVertBtn.Glyph);
  DM.Icons16.GetBitmap(29, TopAlignBtn.Glyph);
  DM.Icons16.GetBitmap(30, BottomAlignBtn.Glyph);

  {$IFDEF DARWIN}
  LConstraints := LeftAlignBtn.Constraints;
  LConstraints.MinHeight := 28;
  LConstraints.MinWidth := 28;

  TopAlignBtn.Constraints.Assign(LConstraints);
  BottomAlignBtn.Constraints.Assign(LConstraints);
  RightAlignBtn.Constraints.Assign(LConstraints);
  CenterVertBtn.Constraints.Assign(LConstraints);
  CenterHorzBtn.Constraints.Assign(LConstraints);
  EvenDistVertBtn.Constraints.Assign(LConstraints);
  EvenDistHorzBtn.Constraints.Assign(LConstraints);
  FixedDistVertBtn.Constraints.Assign(LConstraints);
  FixedDistHorzBtn.Constraints.Assign(LConstraints);
  {$ENDIF}
end;

constructor TAlignmentForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  LoadGlyphs;
end;

class procedure TAlignmentForm.RestoreDefaultPos;
var
  Aform: TForm;
begin
  if Assigned(AlignForm) then
    AForm := AlignForm
  else
    Aform := TForm.Create(nil);
  Aform.LockRealizeBounds;
  Aform.Width := 500;
  Aform.Height := 500;
  Aform.top := (Screen.Monitors[0].Height - Aform.Height) div 2;
  Aform.Left := (Screen.Monitors[0].Width - Aform.Width) div 2;
  Aform.UnlockRealizeBounds;
  SaveFormPosition(Aform, 'AlignmentForm');

  if Aform <> AlignForm then
    AForm.free;
end;


end.

