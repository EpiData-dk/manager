unit align_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  Spin, StdCtrls, ExtCtrls, design_runtimedesigner;

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
    procedure FormShow(Sender: TObject);
    procedure LeftAlignBtnClick(Sender: TObject);
    procedure RightAlignBtnClick(Sender: TObject);
    procedure TopAlignBtnClick(Sender: TObject);
  private
    FDesignFrame: TRuntimeDesignFrame;
    { private declarations }
  public
    { public declarations }
    property DesignFrame: TRuntimeDesignFrame read FDesignFrame write FDesignFrame;
  end;

procedure ShowAlignmentForm(DesignFrame: TRuntimeDesignFrame);
procedure CloseAlignmentForm;
function AlignmentFormIsVisible: boolean;
procedure AlignmentFormRestoreDefaultPos;

implementation

{$R *.lfm}

uses
  design_types, settings2, settings2_var;

var
  AlignForm: TAlignmentForm;

procedure ShowAlignmentForm(DesignFrame: TRuntimeDesignFrame);
begin
  if not Assigned(AlignForm) then
    AlignForm  := TAlignmentForm.Create(nil);

  AlignForm.DesignFrame := DesignFrame;
  AlignForm.Show;
end;

procedure CloseAlignmentForm;
begin
  if Assigned(AlignForm) then
    FreeAndNil(AlignForm);
end;

function AlignmentFormIsVisible: boolean;
begin
  if not Assigned(AlignForm) then exit(false);

  result := AlignForm.Visible;
end;

procedure AlignmentFormRestoreDefaultPos;
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

{ TAlignmentForm }

procedure TAlignmentForm.EvenDistVertBtnClick(Sender: TObject);
begin
  DesignFrame.AlignControls(dcaEvenVert);
end;

procedure TAlignmentForm.EvenDistHorzBtnClick(Sender: TObject);
begin
  DesignFrame.AlignControls(dcaEvenHorz);
end;

procedure TAlignmentForm.FixedDistVertBtnClick(Sender: TObject);
begin
  DesignFrame.AlignControls(dcaFixedVert, SpinEdit1.Value);
end;

procedure TAlignmentForm.FixedDistHorzBtnClick(Sender: TObject);
begin
  DesignFrame.AlignControls(dcaFixedHorz, SpinEdit2.Value);
end;

procedure TAlignmentForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := true;
  if ManagerSettings.SaveWindowPositions
  then
    SaveFormPosition(Self, 'AlignmentForm');
end;

procedure TAlignmentForm.FormShow(Sender: TObject);
begin
  if ManagerSettings.SaveWindowPositions then
    LoadFormPosition(Self, 'AlignmentForm');
end;

procedure TAlignmentForm.BottomAlignBtnClick(Sender: TObject);
begin
  DesignFrame.AlignControls(dcaBottomMost);
end;

procedure TAlignmentForm.CenterVertBtnClick(Sender: TObject);
begin
  DesignFrame.AlignControls(dcaCenterVert);
end;

procedure TAlignmentForm.CenterHorzBtnClick(Sender: TObject);
begin
  DesignFrame.AlignControls(dcaCenterHorz);
end;

procedure TAlignmentForm.LeftAlignBtnClick(Sender: TObject);
begin
  DesignFrame.AlignControls(dcaLeftMost);
end;

procedure TAlignmentForm.RightAlignBtnClick(Sender: TObject);
begin
  DesignFrame.AlignControls(dcaRightMost);
end;

procedure TAlignmentForm.TopAlignBtnClick(Sender: TObject);
begin
  DesignFrame.AlignControls(dcaTopMost);
end;


end.

