unit design_properties_baseframe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, epicustombase, design_types,
  epidatafiles, epirelations;

type

  { TDesignPropertiesFrame }

  TDesignPropertiesFrame = class(TFrame)
  private
    FDataFile: TEpiDataFile;
    FRelation: TEpiMasterRelation;
    FOnShowHintMsg: TDesignFrameShowHintEvent;
    FOnUpdateCaption: TGetStrProc;
  protected
    procedure ShowHintMsg(const Msg: string; Ctrl: TControl);
    procedure UpdateCaption(Const S: String); virtual;
  public
//    procedure ShiftToTabSheet(Const SheetNo: Byte); virtual; abstract;
    procedure SetDataFile(const ADataFile: TEpiDataFile); virtual;
    procedure SetRelation(const Relation: TEpiMasterRelation);
    property  OnShowHintMsg: TDesignFrameShowHintEvent read FOnShowHintMsg write FOnShowHintMsg;
    property  OnUpdateCaption: TGetStrProc read FOnUpdateCaption write FOnUpdateCaption;
    property  DataFile: TEpiDataFile read FDataFile;
    property  Relation: TEpiMasterRelation read FRelation;
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

procedure TDesignPropertiesFrame.SetDataFile(const ADataFile: TEpiDataFile);
begin
  FDataFile := ADataFile;
end;

procedure TDesignPropertiesFrame.SetRelation(const Relation: TEpiMasterRelation
  );
begin
  FRelation := Relation;
end;

end.

