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
    FOnShowHintMsg: TDesignFrameShowHintEvent;
    FOnUpdateCaption: TGetStrProc;
  protected
    FDataFile: TEpiDataFile;
    FRelation: TEpiMasterRelation;
    procedure ShowHintMsg(const Msg: string; Ctrl: TControl);
    procedure UpdateCaption(Const S: String); virtual;
  public
    procedure ShiftToTabSheet(Const SheetNo: Byte); virtual; abstract;
    procedure SetDataFile(const DataFile: TEpiDataFile);
    procedure SetRelation(const Relation: TEpiMasterRelation);
    property  OnShowHintMsg: TDesignFrameShowHintEvent read FOnShowHintMsg write FOnShowHintMsg;
    property  OnUpdateCaption: TGetStrProc read FOnUpdateCaption write FOnUpdateCaption;
    property  DataFile: TEpiDataFile read FDataFile;
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

procedure TDesignPropertiesFrame.SetDataFile(const DataFile: TEpiDataFile);
begin
  FDataFile := DataFile;
end;

procedure TDesignPropertiesFrame.SetRelation(const Relation: TEpiMasterRelation
  );
begin
  FRelation := Relation;
end;

end.

