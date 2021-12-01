unit design_properties_emptyframe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls,
  design_properties_baseframe, design_types;

type

  { TEmptyPropertiesFrame }

  TEmptyPropertiesFrame = class(TDesignPropertiesFrame, IDesignPropertiesFrame)
    Label1: TLabel;
    procedure FrameResize(Sender: TObject);
  private
    { private declarations }
  protected
    procedure SetParent(AParent: TWinControl); override;
  public
    { public declarations }
    procedure FocusOnNewControl;
    procedure SetEpiControls(EpiControls: TEpiCustomControlItemArray);
    procedure ResetControls;
    function  ApplyChanges: boolean;
  end;

implementation

{$R *.lfm}

{ TEmptyPropertiesFrame }

procedure TEmptyPropertiesFrame.FrameResize(Sender: TObject);
var
  L: Integer;
  T: Integer;
begin
  L := (Width - Label1.Width) div 2;
  T := (Height - Label1.Height) div 2;

  with Label1 do
    SetBounds(L, T, Width, Height);
end;

procedure TEmptyPropertiesFrame.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  UpdateCaption('Common Properties');
end;

procedure TEmptyPropertiesFrame.FocusOnNewControl;
begin
  // Do nothing.
end;

procedure TEmptyPropertiesFrame.SetEpiControls(EpiControls: TEpiCustomControlItemArray);
begin
  // Do nothing.
end;

procedure TEmptyPropertiesFrame.ResetControls;
begin
  // Do nothing
end;

function TEmptyPropertiesFrame.ApplyChanges: boolean;
begin
  result := true;
end;

end.

