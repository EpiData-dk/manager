unit design_properties_baseframe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, epicustombase, design_types;

type

  { TDesignPropertiesFrame }

  TDesignPropertiesFrame = class(TFrame)
  private
    FOnShowHintMsg: TDesignFrameShowHintEvent;
    FOnUpdateCaption: TGetStrProc;
  protected
    procedure ShowHintMsg(const Msg: string; Ctrl: TControl);
    procedure UpdateCaption(Const S: String); virtual;
  public
    procedure ShiftToTabSheet(Const SheetNo: Byte); virtual; abstract;
    property  OnShowHintMsg: TDesignFrameShowHintEvent read FOnShowHintMsg write FOnShowHintMsg;
    property  OnUpdateCaption: TGetStrProc read FOnUpdateCaption write FOnUpdateCaption;
  end;

implementation

{ DesignPropertiesFrame }

procedure TDesignPropertiesFrame.ShowHintMsg(const Msg: string; Ctrl: TControl);
begin
  if Assigned(OnShowHintMsg) then
    OnShowHintMsg(Self, Ctrl, Msg);
end;

procedure TDesignPropertiesFrame.UpdateCaption(const S: String);
begin
  if Assigned(OnUpdateCaption) then
    OnUpdateCaption(S);
end;

end.

