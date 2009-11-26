unit design_frame;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, ComCtrls, ActnList, StdCtrls,
  Controls, MaskEdit, Buttons, ExtCtrls, Dialogs, Menus, UEpiDataFile,
  FieldEdit, Design_Field_Frame;

type

  { TDesignFrame }

  TDesignFrame = class(TFrame)
    NewLabelFieldAction: TAction;
    NewStringFieldAction: TAction;
    FontDialog1: TFontDialog;
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
    FontSelectBtn: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    StringFieldBtn: TToolButton;
    LabelFieldBtn: TToolButton;
    OpenToolBtn: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    procedure ClearToolBtnClick(Sender: TObject);
    procedure DeleteFieldMenuItemClick(Sender: TObject);
    procedure DesignPanelMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure EditChange(Sender: TObject);
    procedure EditFieldMenuItemClick(Sender: TObject);
    procedure FontSelectBtnClick(Sender: TObject);
    procedure FrameDockDrop(Sender: TObject; Source: TDragDockObject; X,
      Y: Integer);
    procedure FrameMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure NewFloatFieldActionExecute(Sender: TObject);
    procedure NewIntFieldActionExecute(Sender: TObject);
    procedure NewLabelFieldActionExecute(Sender: TObject);
    procedure NewStringFieldActionExecute(Sender: TObject);
    procedure OpenToolBtnClick(Sender: TObject);
    procedure ToolBtnClick(Sender: TObject);
    procedure SaveToolBtnClick(Sender: TObject);
  private
    { private declarations }
    ActiveButton: TToolButton;
    ActiveDatafile: TEpiDataFile;
    ClickedField: TFieldEdit;
    ClickedLabel: TFieldLabel;
    function NewFieldEdit(AField: TEpiField; ATop, ALeft: Integer; ShowForm: Boolean = true): TFieldEdit;
    function NewQuestionLabel(AField: TEpiField; ATop, ALeft: Integer; ShowForm: Boolean = true): TFieldLabel;
    procedure StartFieldDock(Sender: TObject; var DragObject: TDragDockObject);
    procedure EndFieldDock(Sender, Target: TObject; X,Y: Integer);
    procedure FieldMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure EndLabelDock(Sender, Target: TObject; X,Y: Integer);
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
  types, math, settings, design_label_form,
  UEpiDataGlobals, UImportExport;

{ TDesignFrame }

function TDesignFrame.NewFieldEdit(AField: TEpiField; ATop, ALeft: Integer;
  ShowForm: Boolean): TFieldEdit;
var
  FieldForm: TFieldCreateForm;
  Pt: TPoint;
begin
  Result := TFieldEdit.Create(AField, DesignPanel);
  Result.Align := alNone;
  Result.DragMode := dmAutomatic;
  Result.DragKind := dkDock;
  Result.OnStartDock := @StartFieldDock;
  Result.OnEndDock := @EndFieldDock;
  Result.AutoSelect := false;
  Result.AutoSize := false;
  Result.PopupMenu := FieldPopUp;
  Result.OnMouseDown := @FieldMouseDown;
  Result.Left := ALeft;
  Result.Top := ATop;
  Result.Parent := DesignPanel;

  FieldForm := nil;
  if ShowForm then
  begin
    FieldForm := TFieldCreateForm.Create(Self, ActiveDatafile, AField.FieldType = ftFloat);
    Pt := DesignPanel.ClientToScreen(Point(ALeft, ATop));
    FieldForm.Top := Pt.Y;
    FieldForm.Left := Pt.X;

    if FieldForm.ShowModal = mrCancel then
    begin
      FreeAndNil(Result);
      FreeAndNil(FieldForm);
      Exit;
    end;

    with Result do
    begin
      Field.FieldName       := FieldForm.FieldNameEdit.Text;
      Field.VariableLabel   := FieldForm.LabelEdit.Text;
      Field.FieldLength     := StrToInt(FieldForm.FieldLengthEdit.Text);
      if Field.FieldType = ftFloat then
        Field.FieldDecimals := StrToInt(FieldForm.FieldDecimalSizeEdit.Text);
      Field.FieldX          := ALeft;
      Field.FieldY          := ATop;

      Field.LabelX          := ALeft - (VariableLabel.Width + 5);
      Field.LabelY          := ATop + (Height - VariableLabel.Height);
    end;
  end;

  if Assigned(FieldForm) then FreeAndNil(FieldForm);
end;

function TDesignFrame.NewQuestionLabel(AField: TEpiField; ATop, ALeft: Integer;
  ShowForm: Boolean): TFieldLabel;
var
  LabelForm: TCreateLabelForm;
  Pt: TPoint;
begin
  Result := TFieldLabel.Create(AField, DesignPanel);
  Result.Align := alNone;
  Result.DragMode := dmAutomatic;
  Result.DragKind := dkDock;
  Result.OnEndDock := @EndLabelDock;
  Result.PopupMenu := FieldPopUp;
  Result.OnMouseDown := @FieldMouseDown;
  Result.Left := ALeft;
  Result.Top := ATop;
  Result.Parent := DesignPanel;

  LabelForm := nil;
  if ShowForm then
  begin
    LabelForm := TCreateLabelForm.Create(Self, ActiveDatafile);
    Pt := DesignPanel.ClientToScreen(Point(ALeft, ATop));
    LabelForm.Top := Pt.Y;
    LabelForm.Left := Pt.X;

    if LabelForm.ShowModal = mrCancel then
    begin
      FreeAndNil(Result);
      exit;
    end;

    Result.Field.FieldName := LabelForm.GetFieldName;
    Result.Field.VariableLabel := LabelForm.LabelEdit.Text;
    Result.Field.FieldX := ALeft;
    Result.Field.FieldY := ATop;
    Result.Field.LabelX := ALeft;
    Result.Field.LabelY := ATop;
  end;

  if Assigned(Labelform) then FreeAndNil(LabelForm);
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

  if (Sender is TFieldEdit) then
  begin
    ClickedField := TFieldEdit(Sender);
    ClickedLabel := nil;
    EditFieldMenuItem.Caption := 'Edit Field';
    DeleteFieldMenuItem.Caption := 'Delete Field';
  end;
  if (Sender is TFieldLabel) then
  begin
    ClickedLabel := TFieldLabel(Sender);
    ClickedField := nil;
    EditFieldMenuItem.Caption := 'Edit Label';
    DeleteFieldMenuItem.Caption := 'Delete Label';
  end;
end;

procedure TDesignFrame.EndLabelDock(Sender, Target: TObject; X, Y: Integer);
begin
  if not (Sender is TFieldLabel) then exit;

  with TFieldLabel(Sender) do
  begin
    Align := alNone;

    // Using to positional controls of the Edit since its position is updated correctly in the
    // FrameDockDrop event
    Field.FieldX := Left;
    Field.FieldY := Top;
    Field.LabelX := Left;
    Field.LabelY := Top;
  end;
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
    Result.X := BuilderSettings.DefaultRightPostion;
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
  if Assigned(YCtrl) then
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
  MainForm.PageControl1.ActivePage.Caption := ExtractFileName(Fn);
  MainForm.PageControl1.Hint := ExpandFileNameUTF8(Fn);
  MainForm.PageControl1.ShowHint := true;
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

  Nx := X - Source.DockOffset.X;
  Ny := Y - Source.DockOffset.Y;

  if (Source.Control is TFieldEdit) then
  begin
    Pt := GetLowestControlPosition(Ctrl, Ctrl, Source.Control);
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

    if not ((Comp is TFieldEdit) or
      (Comp is TFieldLabel)) then continue;

    if (Comp is TFieldEdit) then
      Field := TFieldEdit(Comp).Field
    else
      Field := TFieldLabel(Comp).Field;
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
//  TFieldEdit(Sender).Text := TFieldEdit(Sender).Field.FieldName;
end;

procedure TDesignFrame.EditFieldMenuItemClick(Sender: TObject);
var
  FieldForm: TFieldCreateForm;
  LabelForm: TCreateLabelForm;
  Pt: TPoint;
begin
  if Assigned(ClickedField) then
  With ClickedField do
  begin
    Pt := DesignPanel.ClientToScreen(Point(ClickedField.Left + ClickedField.Width,
      ClickedField.Top + ClickedField.Height));
    FieldForm := TFieldCreateForm.Create(Self, ActiveDatafile, Field.FieldType = ftFloat, false);
    FieldForm.Left := Pt.X;
    FieldForm.Top  := Pt.Y;
    FieldForm.ReadField(Field);
    if FieldForm.ShowModal = mrCancel then exit;
    FieldForm.WriteField(Field);
    FreeAndNil(FieldForm);
  end;
  if Assigned(ClickedLabel) then
  With ClickedLabel do
  begin
    Pt := DesignPanel.ClientToScreen(Point(ClickedLabel.Left + ClickedLabel.Width,
      ClickedLabel.Top + ClickedLabel.Height));
    LabelForm := TCreateLabelForm.Create(Self, ActiveDatafile);
    LabelForm.Left := Pt.X;
    LabelForm.Top  := Pt.Y;
    LabelForm.LabelEdit.Text := Field.VariableLabel;
    if LabelForm.ShowModal = mrCancel then exit;

    Caption :=  LabelForm.LabelEdit.Text;
    Field.VariableLabel := LabelForm.LabelEdit.Text;

    FreeAndNil(LabelForm);
  end;
end;

procedure TDesignFrame.FontSelectBtnClick(Sender: TObject);
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
  TmpFieldType: TFieldType;
begin
  if Button <> mbLeft then exit;
  if ActiveButton.Tag = 0 then Exit;
  case ActiveButton.Tag of
    1: TmpFieldType := ftInteger;
    2: TmpFieldType := ftFloat;
    3: TmpFieldType := ftString;
    4: TmpFieldType := ftQuestion;
  end;
  TmpField := TEpiField.CreateField(TmpFieldType, ActiveDatafile.Size);
  if TmpFieldType = ftQuestion then
    NewQuestionLabel(TmpField, Y, X)
  else
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

procedure TDesignFrame.NewLabelFieldActionExecute(Sender: TObject);
var
  Pt: TPoint;
begin
  ActiveButton := LabelFieldBtn;
  Pt := FindNewAutoControlPostion;
  FrameMouseDown(nil, mbLeft, [], Pt.X, Pt.Y);
end;

procedure TDesignFrame.NewStringFieldActionExecute(Sender: TObject);
var
  Pt: TPoint;
begin
  ActiveButton := StringFieldBtn;
  Pt := FindNewAutoControlPostion;
  FrameMouseDown(nil, mbLeft, [], Pt.X, Pt.Y);
end;

procedure TDesignFrame.OpenToolBtnClick(Sender: TObject);
var
  Dlg: TOpenDialog;
  Import: TEpiImportExport;
  Fn: String;
  Dummy: boolean;
  i: Integer;
  TmpField: TEpiField;
begin
  Dlg := TOpenDialog.Create(nil);
  Dlg.Filter := EpiOpenFileFilter;
  if Dlg.Execute then
  begin

    Fn := Dlg.FileName;
    if (CompareFileExt(Fn, '.REC') <> 0) and
       (CompareFileExt(Fn, '.RECXML') <> 0) then
    begin

      Import := TEpiImportExport.Create;
      Import.OnProgress := @MainForm.ShowProgress;
//      Importer.OnClipBoardRead := @MainForm.ReadClipboard;
      if CompareFileExt(Fn, '.DTA') = 0 then
        Dummy := Import.ImportStata(Fn, ActiveDatafile)
      else if CompareFileExt(Fn, '.DBF') = 0 then
        Dummy := Import.ImportDBase(Fn, ActiveDatafile)
      else if CompareFileExt(Fn, '.ODS') = 0 then
        Dummy := Import.ImportSpreadSheet(Fn, ActiveDatafile)
      else if (CompareFileExt(Fn, '.TXT') = 0) or
              (CompareFileExt(Fn, '.CSV') = 0) then
        Dummy := Import.ImportTXT(Fn, ActiveDatafile, @ImportTxtGuess);

      FreeAndNil(Import);
    end else begin
      ActiveDatafile := TEpiDataFile.Create();
      ActiveDatafile.OnProgress := @MainForm.ShowProgress;
  //    ActiveDatafile.OnPassword := GetPassword;
      Dummy := ActiveDatafile.Open(Fn, []);
    end;

    for i := 0 to ActiveDatafile.NumFields - 1 do
    begin
      TmpField := ActiveDatafile[i];
      if TmpField.FieldType = ftQuestion then
        NewQuestionLabel(TmpField, TmpField.FieldY, TmpField.FieldX, false)
      else
        NewFieldEdit(TmpField, TmpField.FieldY, TmpField.FieldX, false);
    end;

    MainForm.PageControl1.ActivePage.Caption := ExtractFileName(Fn);
    MainForm.PageControl1.Hint := ExpandFileNameUTF8(Fn);
    MainForm.PageControl1.ShowHint := true;
  end;
  FreeAndNil(Dlg);
end;

initialization
  {$I design_frame.lrs}

end.

