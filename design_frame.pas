unit design_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, ComCtrls, ActnList, StdCtrls,
  Controls, MaskEdit, Buttons, ExtCtrls, Dialogs, UEpiDataFile, FieldEdit;

type

  { TDesignFrame }

  TDesignFrame = class(TFrame)
    ActionList1: TActionList;
    Edit1: TEdit;
    FieldToolBar: TToolBar;
    FieldToolBarImageList: TImageList;
    IntFieldBtn: TToolButton;
    DesignPanel: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Panel1: TPanel;
    SaveDialog1: TSaveDialog;
    SelectorButton: TToolButton;
    FloatFieldBtn: TToolButton;
    SepToolBtn1: TToolButton;
    ToolButton1: TToolButton;
    ClearToolBtn: TToolButton;
    SaveToolBtn: TToolButton;
    procedure ClearToolBtnClick(Sender: TObject);
    procedure DesignPanelGetSiteInfo(Sender: TObject; DockClient: TControl;
      var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
    procedure DesignPanelMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure DesignPanelUnDock(Sender: TObject; Client: TControl;
      NewTarget: TWinControl; var Allow: Boolean);
    procedure EditChange(Sender: TObject);
    procedure FrameDockDrop(Sender: TObject; Source: TDragDockObject; X,
      Y: Integer);
    procedure FrameDockOver(Sender: TObject; Source: TDragDockObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure FrameMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ToolBtnClick(Sender: TObject);
    procedure SaveToolBtnClick(Sender: TObject);
  private
    { private declarations }
    ActiveButton: TToolButton;
    ActiveDatafile: TEpiDataFile;
    function NewFieldEdit(AField: TEpiField; ATop, ALeft: Integer): TFieldEdit;
    procedure StartDock(Sender: TObject; var DragObject: TDragDockObject);
    procedure EndDock(Sender, Target: TObject; X,Y: Integer);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
  end;

implementation

uses
  main, graphics, UDataFileTypes, Design_Field_Frame, designutils,
  types;

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
  Result.Align := alNone;
  Result.DragMode := dmAutomatic;
  Result.DragKind := dkDock;
  Result.OnStartDock := @StartDock;
  Result.OnEndDock := @EndDock;
  Result.Text := FieldForm.FieldNameEdit.Text;
  Result.OnChange := @EditChange;
  Result.AutoSelect := false;
  Result.AutoSize := false;

  With Result do
  begin
    VariableLabel.Left := Left - VariableLabel.Width + 10;
    VariableLabel.Top := Top;
    VariableLabel.Parent := DesignPanel;
    VariableLabel.Caption := FieldForm.LabelEdit.Text;
    VariableLabel.DragMode := dmAutomatic;
    VariableLabel.DragKind := dkDock;
  end;

  AField.FieldName := Result.Text;
  AField.FieldLength := StrToInt(FieldForm.FieldSizeEdit.Text);
  if AField.FieldType = ftFloat then
    AField.FieldDecimals := StrToInt(FieldForm.FieldDecimalSizeEdit.Text);
  AField.FieldX := ALeft;
  AField.FieldY := ATop;
  AField.VariableLabel := FieldForm.LabelEdit.Text;

  FreeAndNil(FieldForm);
end;

procedure TDesignFrame.StartDock(Sender: TObject;
  var DragObject: TDragDockObject);
var
  S: string;
begin
  DragObject := TDragDockObject.Create(TControl(Sender));
//  if Sender is TFieldEdit then
//    TFieldEdit(Sender).VariableLabel.BeginDrag(false);

  {$IFDEF VER2.4}
  WriteStr(S, DesignPanel.DockSite);
  {$ELSE}
  S := '';
  {$ENDIF}
  Label1.Caption := Format(
    'DockSite: %s, DockClientCount: %d',
    [s, DesignPanel.DockClientCount ]);
end;

procedure TDesignFrame.EndDock(Sender, Target: TObject; X, Y: Integer);
begin
  if not (Sender is TFieldEdit) then exit;

  with TFieldEdit(Sender) do
  begin
    Align := alNone;
    // Using to positional controls of the Edit since its position is updated correctly in the
    // FrameDockDrop event.
    Field.FieldX := Left;
    Field.FieldY := Top;
  end;
end;

constructor TDesignFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ActiveButton := SelectorButton;
  ActiveDatafile := TEpiDataFile.Create();
end;


procedure TDesignFrame.ToolBtnClick(Sender: TObject);
begin
  if not (Sender is TToolButton) then exit;
  ActiveButton.Down := false;
  TToolButton(Sender).Down := true;
  ActiveButton := TToolButton(Sender);
end;

procedure TDesignFrame.SaveToolBtnClick(Sender: TObject);
var
  Fn: String;
begin
  if SaveDialog1.Execute then
  begin
    Fn := SaveDialog1.FileName;
    ActiveDatafile.Save(Fn, []);
  end;
  MainForm.StatusBar2.Panels[0].Text := 'Saving complete: ' + Fn;
end;

procedure TDesignFrame.FrameDockDrop(Sender: TObject; Source: TDragDockObject;
  X, Y: Integer);
var
 s: string;
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
  Field: TEpiField;
begin
  Field := nil;
  while DesignPanel.ControlCount > 0 do
  begin
    Comp := DesignPanel.Controls[DesignPanel.ControlCount-1];
//    Field := (Sender as TFieldEdit).Field;
//    ActiveDatafile.RemoveField(Field, true);
    DesignPanel.RemoveControl(Comp);
    FreeAndNil(Comp);
  end;
end;

procedure TDesignFrame.DesignPanelGetSiteInfo(Sender: TObject;
  DockClient: TControl; var InfluenceRect: TRect; MousePos: TPoint;
  var CanDock: Boolean);
var
  s: string;
begin
  {$IFDEF VER2.4}
  writestr(s, candock);
  {$ELSE}
  s := '';
  {$ENDIF}
  Label3.Caption := Format(
    'Sender: %s, DockClient: %s, CanDock: %s',
    [Sender.ClassName, DockClient.ClassName, s]);
end;

procedure TDesignFrame.DesignPanelMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  MainForm.StatusBar2.Panels[1].Text := Format(
    'Mouse - X: %d Y: %d',
    [X, Y]);
end;

procedure TDesignFrame.DesignPanelUnDock(Sender: TObject; Client: TControl;
  NewTarget: TWinControl; var Allow: Boolean);
var
 s: string;
 t: String;
begin
  {$IFDEF VER2.4}
  writestr(s, allow);
  {$ELSE}
  s := '';
  {$ENDIF}
  t := 'Sender: %s, ';
  if Assigned(client) then
    t := t + 'Client: ' + client.ClassName + ',';
  if Assigned(NewTarget) then
    t := t + 'NewTarget: ' + NewTarget.ClassName + ', Allow: %s';

  Label2.Caption := Format(T,
    [sender.ClassName, S]);
end;

procedure TDesignFrame.EditChange(Sender: TObject);
begin
  TFieldEdit(Sender).Text := TFieldEdit(Sender).Field.FieldName;
end;

procedure TDesignFrame.FrameDockOver(Sender: TObject; Source: TDragDockObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  s, t: string;
begin
  {$IFDEF VER2.4}
  WriteStr(S, State);
  WriteStr(t, Accept);
  {$ELSE}
  S := 'na';
  T := S;
  {$ENDIF}
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

{  MainForm.StatusBar2.SimpleText := Format(
    'Mouse - X: %d Y:%d',
    [X, Y]);              }

  if ActiveButton.Tag = 0 then Exit;
  case ActiveButton.Tag of
    1: TmpField := TEpiField.CreateField(ftInteger);
    2: TmpField := TEpiField.CreateField(ftFloat);
  end;
  NewFieldEdit(TmpField, Y, X);
  ActiveDatafile.AddField(TmpField);
  ToolBtnClick(SelectorButton);
end;

initialization
  {$I design_frame.lrs}

end.

