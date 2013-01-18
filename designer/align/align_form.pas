unit align_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  Spin, StdCtrls, ExtCtrls, design_runtimedesigner;

type

  TControlsAlignment = (
    caLeftMost,
    caRightMost,
    caTopMost,
    caBottomMost,
    caCenterVert,
    caCenterHorz,
    caEvenVert,
    caEvenHorz,
    caFixedVert,
    caFixedHorz
  );

  { TAlignmentForm }

  TAlignmentForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn10: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    BitBtn7: TBitBtn;
    BitBtn8: TBitBtn;
    BitBtn9: TBitBtn;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
  private
    FDesignFrame: TRuntimeDesignFrame;
    { private declarations }
  public
    { public declarations }
    property DesignFrame: TRuntimeDesignFrame read FDesignFrame write FDesignFrame;
  end;

procedure ShowAlignmentForm(DesignFrame: TRuntimeDesignFrame);
function AlignmentFormIsVisible: boolean;

implementation

{$R *.lfm}

var
  AlignForm: TAlignmentForm;

procedure ShowAlignmentForm(DesignFrame: TRuntimeDesignFrame);
begin
  if not Assigned(AlignForm) then
    AlignForm  := TAlignmentForm.Create(nil);

  AlignForm.DesignFrame := DesignFrame;
  AlignForm.Show;
end;

function AlignmentFormIsVisible: boolean;
begin
  if not Assigned(AlignForm) then exit(false);

  result := AlignForm.Visible;
end;


end.

