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
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
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
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  main, graphics, UDataFileTypes, designutils,
  types, math;

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
    VariableLabel.Left    := Left - (VariableLabel.Width + 20);
    VariableLabel.Top     := Top + (Height - VariableLabel.Height);

    Field.FieldName       := FieldForm.FieldNameEdit.Text;
    Field.VariableLabel   := FieldForm.LabelEdit.Text;
    Field.FieldLength     := StrToInt(FieldForm.FieldSizeEdit.Text);
    if Field.FieldType = ftFloat then
      Field.FieldDecimals := StrToInt(FieldForm.FieldDecimalSizeEdit.Text);
    Field.FieldX          := ALeft;
    Field.FieldY          := ATop;

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
  end;
end;

procedure TDesignFrame.FieldMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button <> mbRight then exit;
  if not (Sender is TFieldEdit) then exit;
  ClickedField := TFieldEdit(Sender);
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
  Source.Control.Left := X - Source.DockOffset.X;
  Source.Control.Top := Y - Source.DockOffset.Y;
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
begin
  ActiveButton := FloatFieldBtn;
  FrameMouseDown(nil, mbLeft, [], 150, 40+(ActiveDatafile.DataFields.Count*30));
end;

procedure TDesignFrame.NewIntFieldActionExecute(Sender: TObject);
begin
  ActiveButton := IntFieldBtn;
  FrameMouseDown(nil, mbLeft, [], 150,40+(ActiveDatafile.DataFields.Count*30));
end;

initialization
  {$I design_frame.lrs}

end.

