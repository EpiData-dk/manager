unit design_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, ComCtrls, ActnList, StdCtrls,
  Controls, MaskEdit;

type

  { TDesignFrame }

  TDesignFrame = class(TFrame)
    NewIntField: TAction;
    ActionList1: TActionList;
    ToolBar1: TToolBar;
    IntFieldBtn: TToolButton;
    procedure Edit1EndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure FrameDockDrop(Sender: TObject; Source: TDragDockObject; X,
      Y: Integer);
    procedure FrameDockOver(Sender: TObject; Source: TDragDockObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure NewIntFieldExecute(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

implementation

uses
  main, graphics;

{ TDesignFrame }

procedure TDesignFrame.NewIntFieldExecute(Sender: TObject);
var
  NewField:  TEdit;
begin
  NewField := TEdit.Create(Self);
  NewField.Parent := self;
  NewField.Left := 5;
  NewField.Top := 15;
  NewField.Width := 100;
  NewField.Height := 12;
  NewField.DragMode := dmAutomatic;
  NewField.DragKind := dkDock;
  NewField.OnEndDrag := @Edit1EndDrag;
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

procedure TDesignFrame.Edit1EndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  MainForm.StatusBar1.Panels[0].Text := Format(
    'Sender: %s, Target: %s, X: %d, Y: %d',
    [Sender.ClassName, Target.ClassName, X, Y]);
end;

initialization
  {$I design_frame.lrs}

end.

