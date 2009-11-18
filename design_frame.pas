unit design_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, ComCtrls, ActnList, StdCtrls,
  Controls, MaskEdit, Buttons, ExtCtrls, UEpiDataFile, FieldEdit;

type

  { TDesignFrame }

  TDesignFrame = class(TFrame)
    ActionList1: TActionList;
    FieldToolBar: TToolBar;
    FieldToolBarImageList: TImageList;
    IntFieldBtn: TToolButton;
    DesignPanel: TPanel;
    SelectorButton: TToolButton;
    FloatFieldBtn: TToolButton;
    SepToolBtn1: TToolButton;
    ToolButton1: TToolButton;
    ClearToolBtn: TToolButton;
    procedure ClearToolBtnClick(Sender: TObject);
    procedure FrameDockDrop(Sender: TObject; Source: TDragDockObject; X,
      Y: Integer);
    procedure FrameDockOver(Sender: TObject; Source: TDragDockObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure FrameMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ToolBtnClick(Sender: TObject);
  private
    { private declarations }
    ActiveButton: TToolButton;
    function NewFieldEdit(AField: TEpiField; ATop, ALeft: Integer): TFieldEdit;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
  end;

implementation

uses
  main, graphics, UDataFileTypes, Design_Field_Frame, designutils;

{ TDesignFrame }

function TDesignFrame.NewFieldEdit(AField: TEpiField; ATop, ALeft: Integer
  ): TFieldEdit;
var
  FieldForm: TFieldCreateForm;
begin
  Result := nil;

  FieldForm := TFieldCreateForm.Create(Self, AField.FieldType = ftFloat);
  FieldForm.Top := Self.Parent.Top + ATop;
  FieldForm.Left := Self.Parent.Left + ALeft;
  if FieldForm.ShowModal = mrCancel then exit;

  Result := TFieldEdit.Create(AField, DesignPanel);
  Result.Parent := DesignPanel;
  Result.Left := ALeft;
  Result.Top := ATop;
  Result.DragMode := dmAutomatic;
  Result.DragKind := dkDock;
end;

constructor TDesignFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ActiveButton := SelectorButton;
end;

procedure TDesignFrame.ToolBtnClick(Sender: TObject);
begin
  if not (Sender is TToolButton) then exit;
  ActiveButton.Down := false;
  TToolButton(Sender).Down := true;
  ActiveButton := TToolButton(Sender);
end;

procedure TDesignFrame.FrameDockDrop(Sender: TObject; Source: TDragDockObject;
  X, Y: Integer);
begin
  MainForm.StatusBar1.Panels[0].Text := Format(
    'Sender: %s,  Source: %s, X: %d, Y: %d',
    [Sender.ClassName, Source.Control.ClassName, X, Y]);
  Source.Control.Left := X - Source.DockOffset.X;
  Source.Control.Top := Y - Source.DockOffset.Y;
end;

procedure TDesignFrame.ClearToolBtnClick(Sender: TObject);
var
  Comp: TControl;
begin
  while DesignPanel.ControlCount > 0 do
  begin
    Comp := DesignPanel.Controls[DesignPanel.ControlCount-1];
    if Comp is TFieldEdit then
      // Currently destroy field, later remove through DF!
      TFieldEdit(Comp).Field.Free;
    DesignPanel.RemoveControl(Comp);
    FreeAndNil(Comp);
  end;
end;

procedure TDesignFrame.FrameDockOver(Sender: TObject; Source: TDragDockObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  s, t: string;
begin
  WriteStr(S, State);
  WriteStr(t, Accept);
  MainForm.StatusBar1.Panels[0].Text := Format(
    'Sender: %s,  Source: %s, X: %d, Y: %d, State: %s, Accept: %s',
    [Sender.ClassName, Source.Control.ClassName, X, Y, S, t]);
end;

procedure TDesignFrame.FrameMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  TmpField: TEpiField;
begin
  if Button <> mbLeft then exit;
  if ActiveButton.Tag = 0 then Exit;
  case ActiveButton.Tag of
    1: TmpField := TEpiField.CreateField(ftInteger);
    2: TmpField := TEpiField.CreateField(ftFloat);
  end;
  NewFieldEdit(TmpField, X, Y);
  ToolBtnClick(SelectorButton);
end;

initialization
  {$I design_frame.lrs}

end.

