unit design_propertiesbase_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, epicustombase, design_types;

type
  { DesignPropertiesFrame }

  TDesignPropertiesFrame = class(TFrame)
  private
    FEpiControl: TEpiCustomControlItem;
    FOnShowHintMsg: TDesignFrameShowHintEvent;
  protected
    procedure SetEpiControl(const AValue: TEpiCustomControlItem); virtual;
    procedure ShowHintMsg(const Msg: string; Ctrl: TControl);
  public
    function  ValidateControl: boolean; virtual; abstract;
    procedure UpdateFormContent; virtual; abstract;
    procedure ShiftToTabSheet(Const SheetNo: Byte); virtual; abstract;
    procedure ForceShow; virtual;
    property  EpiControl: TEpiCustomControlItem read FEpiControl write SetEpiControl;
    property  OnShowHintMsg: TDesignFrameShowHintEvent read FOnShowHintMsg write FOnShowHintMsg;
  end;

implementation

{$R *.lfm}

{ DesignPropertiesFrame }

procedure TDesignPropertiesFrame.SetEpiControl(
  const AValue: TEpiCustomControlItem);
begin
  FEpiControl := AValue;
end;

procedure TDesignPropertiesFrame.ShowHintMsg(const Msg: string; Ctrl: TControl);
begin
  if Assigned(OnShowHintMsg) then
    OnShowHintMsg(Self, Ctrl, Msg);
end;

procedure TDesignPropertiesFrame.ForceShow;
begin
  ShiftToTabSheet(1);
end;

end.

