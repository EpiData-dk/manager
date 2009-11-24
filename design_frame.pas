unit design_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, ComCtrls, ActnList, StdCtrls,
  Controls, MaskEdit, Buttons, ExtCtrls, Dialogs, Menus, UEpiDataFile,
  FieldEdit, Design_Field_Frame;

type

  { TDesignFrame }

  TDesignFrame = class(TFrame)
    FontDialog1: TFontDialog;
    Label1: TLabel;
    NewFloatFieldAction: TAction;
    NewIntFieldAction: TAction;
    ActionList1: TActionList;
    FieldToolBar: TToolBar;
    FieldToolBarImageList: TImageList;
    IntFieldBtn: TToolButton;
    DesignPanel: TPanel;
    EditFieldMenuItem: TMenuItem;
    FieldPopUp: TPopupMenu;
    DeleteFieldMenuItem: TMenuItem;
    SaveDialog1: TSaveDialog;
    SelectorButton: TToolButton;
    FloatFieldBtn: TToolButton;
    ClearToolBtn: TToolButton;
    SaveToolBtn: TToolButton;
    FontSelectButton: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    LabelFieldBtn: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    procedure ClearToolBtnClick(Sender: TObject);
    procedure DeleteFieldMenuItemClick(Sender: TObject);
    procedure DesignPanelMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure EditChange(Sender: TObject);
    procedure EditFieldMenuItemClick(Sender: TObject);
    procedure FontSelectButtonClick(Sender: TObject);
    procedure FrameDockDrop(Sender: TObject; Source: TDragDockObject; X,
      Y: Integer);
    procedure FrameMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure NewFloatFieldActionExecute(Sender: TObject);
    procedure NewIntFieldActionExecute(Sender: TObject);
    procedure ToolBtnClick(Sender: TObject);
    procedure SaveToolBtnClick(Sender: TObject);
  private
    { private declarations }
    ActiveButton: TToolButton;
    ActiveDatafile: TEpiDataFile;
    ClickedField: TFieldEdit;
    function NewFieldEdit(AField: TEpiField; ATop, ALeft: Integer;
      CreateForm: boolean = true): TFieldEdit;
    procedure UpdateFieldEditFromForm(FieldEdit: TFieldEdit;
      FieldForm: TFieldCreateForm; ATop, ALeft: Integer);
    procedure StartFieldDock(Sender: TObject; var DragObject: TDragDockObject);
    procedure EndFieldDock(Sender, Target: TObject; X,Y: Integer);
    procedure FieldMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    function  GetLowestControlPosition(var XCtrl, YCtrl: TControl; IgnoreCtrl: TControl): TPoint;
    function  FindNewAutoControlPostion: TPoint;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateAllFields;
  end;

implementation

uses
  main, graphics, UDataFileTypes, designutils,
  types, math, settings;

{ TDesignFrame }

function TDesignFrame.NewFieldEdit(AField: TEpiField; ATop, ALeft: Integer;
  CreateForm: boolean): TFieldEdit;
var
  FieldForm: TFieldCreateForm;
  Pt: TPoint;
begin
  Result := nil;

//  if CreateForm then begin;
  FieldForm := TFieldCreateForm.Create(Self, ActiveDatafile, AField.FieldType = ftFloat);
  Pt := ClientToScreen(Point(ATop, ALeft));
  FieldForm.Top := Pt.Y;
  FieldForm.Left := Pt.X;
  if FieldForm.ShowModal = mrCancel then exit;

  Result := TFieldEdit.Create(AField, DesignPanel);
  Result.Parent := DesignPanel;
  Result.Align := alNone;
  Result.DragMode := dmAutomatic;
  Result.DragKind := dkDock;
  Result.OnStartDock := @StartFieldDock;
  Result.OnEndDock := @EndFieldDock;
  Result.OnChange := @EditChange;
  Result.AutoSelect := false;
  Result.AutoSize := false;
  Result.PopupMenu := FieldPopUp;
  Result.OnMouseDown := @FieldMouseDown;

  UpdateFieldEditFromForm(Result, FieldForm, ATop, ALeft);

  FreeAndNil(FieldForm);
end;

procedure TDesignFrame.UpdateFieldEditFromForm(FieldEdit: TFieldEdit;
  FieldForm: TFieldCreateForm; ATop, ALeft: Integer);
begin
  FieldEdit.Text := FieldForm.FieldNameEdit.Text;
  FieldEdit.Left := ALeft;
  FieldEdit.Top  := ATop;

  With FieldEdit do
  begin
    VariableLabel.Caption := FieldForm.LabelEdit.Text;
    VariableLabel.Left    := Left - (VariableLabel.Width + 5);
    VariableLabel.Top     := Top + (Height - VariableLabel.Height);

    Field.FieldName       := FieldForm.FieldNameEdit.Text;
    Field.VariableLabel   := FieldForm.LabelEdit.Text;
    Field.FieldLength     := StrToInt(FieldForm.FieldSizeEdit.Text);
    if Field.FieldType = ftFloat then
      Field.FieldDecimals := StrToInt(FieldForm.FieldDecimalSizeEdit.Text);
    Field.FieldX          := ALeft;
    Field.FieldY          := ATop;

    FieldNameLabel.Caption := Field.FieldName;
    FieldNameLabel.Left    := VariableLabel.Left - (FieldNameLabel.Width + 5);
    FieldNameLabel.Top     := VariableLabel.Top;
    if BuilderSettings.ShowFieldNamesInLabel then
      FieldNameLabel.Parent := DesignPanel
    else
      FieldNameLabel.Parent := nil;
  end;
end;

procedure TDesignFrame.StartFieldDock(Sender: TObject;
  var DragObject: TDragDockObject);
var
  S: string;
begin
  DragObject := TFieldDockObject.Create(TControl(Sender));
end;

procedure TDesignFrame.EndFieldDock(Sender, Target: TObject; X, Y: Integer);
begin
  if not (Sender is TFieldEdit) then exit;

  with TFieldEdit(Sender) do
  begin
    Align := alNone;

    // Using to positional controls of the Edit since its position is updated correctly in the
    // FrameDockDrop event.
    Field.FieldX := Left;
    Field.FieldY := Top;
    Field.LabelX := VariableLabel.Left;
    Field.LabelY := VariableLabel.Top;
  end;
end;

procedure TDesignFrame.FieldMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button <> mbRight then exit;
  if not (Sender is TFieldEdit) then exit;
  ClickedField := TFieldEdit(Sender);
end;

function TDesignFrame.GetLowestControlPosition(var XCtrl, YCtrl: TControl;
  IgnoreCtrl: TControl): TPoint;
var
  i: Integer;
begin
  result := Point(0,0);
  YCtrl := nil;
  XCtrl := nil;
  for i := 0 to DesignPanel.ControlCount - 1 do
  with DesignPanel do
  begin
    if not (Controls[i] is TFieldEdit) then continue;
    if Controls[i] = IgnoreCtrl then continue;


    if Controls[i].Left > Result.X then
    begin
      XCtrl := Controls[i];
      Result.X := Controls[i].Left;
    end;

    if Controls[i].Top > Result.Y then
    begin
      YCtrl := Controls[i];
      Result.Y := Controls[i].Top;
    end;
  end;

  // Case where no edit field have been placed yet.
  if not Assigned(YCtrl) then
  begin
    Result.X := 25;
    Result.Y := 25;
    Exit;
  end;

  if XCtrl <> YCtrl then
    Result.X := YCtrl.Left;
end;

function TDesignFrame.FindNewAutoControlPostion: TPoint;
var
  XCtrl, YCtrl: TControl;
begin
  Result := GetLowestControlPosition(XCtrl, YCtrl, nil);
  Result.Y += (YCtrl.Height + BuilderSettings.SpaceBetweenFields);
end;

constructor TDesignFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ActiveButton := SelectorButton;
  ActiveDatafile := TEpiDataFile.Create();
  ClickedField := nil;
end;

destructor TDesignFrame.Destroy;
begin
  if Assigned(ActiveDatafile) then
   FreeAndNil(ActiveDatafile);
  inherited Destroy;
end;

procedure TDesignFrame.UpdateAllFields;
var
  i: Integer;
begin
  for i := DesignPanel.ControlCount -1 downto  0 do
  begin
    if not (DesignPanel.Controls[i] is TFieldEdit) then continue;

    if BuilderSettings.ShowFieldNamesInLabel then
      TFieldEdit(DesignPanel.Controls[i]).FieldNameLabel.Parent := DesignPanel
    else
      TFieldEdit(DesignPanel.Controls[i]).FieldNameLabel.Parent := nil;
  end;
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
 Pt: TPoint;
 Dx, Nx: Integer;
 Dy, Ny: Integer;
 Ctrl: TControl;
begin
  Pt := GetLowestControlPosition(Ctrl, Ctrl, Source.Control);

  Nx := X - Source.DockOffset.X;
  Ny := Y - Source.DockOffset.Y;
  Dx := Nx - Pt.X;
  Dy := Ny - Pt.Y;

  if Abs(Dx) <= BuilderSettings.SnappingThresHold then
    Nx := Pt.X;
  if Abs(Dy) <= BuilderSettings.SnappingThresHold then
    Ny := Pt.Y;

  // If the component was placed (within threshold) on top of
  // the lowest component - place it where it was marked.
  if (Dx = Pt.X) and (Dy = Pt.Y) then
  begin
    Nx := X - Source.DockOffset.X;
    Ny := Y - Source.DockOffset.Y;
  end;

  Source.Control.Left := Nx;
  Source.Control.Top := Ny;
end;

procedure TDesignFrame.ClearToolBtnClick(Sender: TObject);
var
  Comp: TControl;
  Field: TEpiField;
  i: Integer;
begin
  Field := nil;
  for i := DesignPanel.ControlCount - 1 downto  0 do
  begin
    Comp := DesignPanel.Controls[i];
    if not (Comp is TFieldEdit) then continue;
    Field :=TFieldEdit(Comp).Field;
    ActiveDatafile.RemoveField(Field, true);
    DesignPanel.RemoveControl(Comp);
    FreeAndNil(Comp);
  end;
end;

procedure TDesignFrame.DeleteFieldMenuItemClick(Sender: TObject);
var
  TmpField: TEpiField;
begin
  if not Assigned(ClickedField) then exit;

  TmpField := ClickedField.Field;
  ActiveDatafile.RemoveField(TmpField, true);

  DesignPanel.RemoveControl(ClickedField);
  FreeAndNil(ClickedField);
end;

procedure TDesignFrame.DesignPanelMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  MainForm.StatusBar2.Panels[1].Text := Format(
    'Mouse - X: %d Y: %d',
    [X, Y]);
end;

procedure TDesignFrame.EditChange(Sender: TObject);
begin
  TFieldEdit(Sender).Text := TFieldEdit(Sender).Field.FieldName;
end;

procedure TDesignFrame.EditFieldMenuItemClick(Sender: TObject);
var
  FieldForm: TFieldCreateForm;
begin
  if not Assigned(ClickedField) then exit;
  With ClickedField do
  begin
    FieldForm := TFieldCreateForm.Create(Self, ActiveDatafile, Field.FieldType = ftFloat, false);
    FieldForm.FieldNameEdit.Text := Field.FieldName;
    FieldForm.LabelEdit.Text     := Field.VariableLabel;
    FieldForm.FieldSizeEdit.Text := IntToSTr(Field.FieldLength);
    FieldForm.OldFieldName := Field.FieldName;
    if Field.FieldType = ftFloat then
      FieldForm.FieldDecimalSizeEdit.Text := IntToStr(Field.FieldDecimals);
    if FieldForm.ShowModal = mrCancel then exit;

    UpdateFieldEditFromForm(ClickedField, FieldForm, ClickedField.Top, ClickedField.Left);
  end;
end;

procedure TDesignFrame.FontSelectButtonClick(Sender: TObject);
begin
  if FontDialog1.Execute then
    begin
      DesignPanel.Font := FontDialog1.Font;
      DesignPanel.Repaint;
    end;
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
  NewFieldEdit(TmpField, Y, X);
  ActiveDatafile.AddField(TmpField);
  ToolBtnClick(SelectorButton);
end;

procedure TDesignFrame.NewFloatFieldActionExecute(Sender: TObject);
var
  Pt: TPoint;
begin
  ActiveButton := FloatFieldBtn;
  Pt := FindNewAutoControlPostion;
  FrameMouseDown(nil, mbLeft, [], Pt.X, Pt.Y);
end;

procedure TDesignFrame.NewIntFieldActionExecute(Sender: TObject);
var
  Pt: TPoint;
begin
  ActiveButton := IntFieldBtn;
  Pt := FindNewAutoControlPostion;
  FrameMouseDown(nil, mbLeft, [], Pt.X, Pt.Y);
end;

initialization
  {$I design_frame.lrs}

end.

