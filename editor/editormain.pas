unit editormain;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Menus, ActnList, ComCtrls, SynEdit, SynHighlighterXML, SynHighlighterAny, SynEditKeyCmds, LCLType;

type

  { TEditorForm }

  TEditorForm = class(TForm)
    MenuItem4: TMenuItem;
    PasteAsLabelMenuItem: TMenuItem;
    SendToDesignerMenuItem: TMenuItem;
    PasteAsLabelAction: TAction;
    CutMenuItem: TMenuItem;
    CopyMenuItem: TMenuItem;
    CutPopMenuItem: TMenuItem;
    CopyPopMenuItem: TMenuItem;
    PasteAsLabelPopMenuItem: TMenuItem;
    SendToDesignerPopMenuItem: TMenuItem;
    PopupDivider1: TMenuItem;
    PastePopMenuItem: TMenuItem;
    PasteMenuItem: TMenuItem;
    PasteAction: TAction;
    CopyAction: TAction;
    CutAction: TAction;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    EditorPopupMenu: TPopupMenu;
    RedoMenuItem: TMenuItem;
    RedoAction: TAction;
    XMLHighlighter: TSynXMLSyn;
    UndoAction: TAction;
    EditMenu: TMenuItem;
    UndoMenuItem: TMenuItem;
    SaveAsMenuItem: TMenuItem;
    SaveMenuItem: TMenuItem;
    SaveAsAction: TAction;
    SaveAction: TAction;
    OpenFileMenuItem: TMenuItem;
    OpenFileAction: TAction;
    CloseAction: TAction;
    EditorActionList: TActionList;
    EditorMenu: TMainMenu;
    FileMenu: TMenuItem;
    MenuItem1: TMenuItem;
    StatusBar1: TStatusBar;
    SynEditor: TSynEdit;
    procedure CloseActionExecute(Sender: TObject);
    procedure CopyActionExecute(Sender: TObject);
    procedure CutActionExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
    procedure OpenFileActionExecute(Sender: TObject);
    procedure PasteActionExecute(Sender: TObject);
    procedure PasteAsLabelActionExecute(Sender: TObject);
    procedure RedoActionExecute(Sender: TObject);
    procedure SaveActionExecute(Sender: TObject);
    procedure SaveAsActionExecute(Sender: TObject);
    procedure SynEditorStatusChange(Sender: TObject; Changes: TSynStatusChanges
      );
    procedure UndoActionExecute(Sender: TObject);
  private
    { private declarations }
    procedure SetTextFileState;
    procedure SetXMLFileState;
  public
    { public declarations }
  end;

var
  EditorForm: TEditorForm;

implementation

uses
  UEpiUtils, settings, UStringUtils, main, design_frame;

{ TEditorForm }

procedure TEditorForm.CloseActionExecute(Sender: TObject);
begin
  Close;
end;

procedure TEditorForm.CopyActionExecute(Sender: TObject);
begin
  SynEditor.CopyToClipboard;
end;

procedure TEditorForm.CutActionExecute(Sender: TObject);
begin
  SynEditor.CutToClipboard;
end;

procedure TEditorForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  {$IFNDEF EPI_DEBUG}
  CanClose := false;
  if (SynEditor.Modified) and
     (MessageDlg('Editor was modified since last save.' +
                 LineEnding + 'Close Editor?', mtWarning, mbYesNo, 0) = mrNo) then
    Exit;
  {$ENDIf}
  CanClose := true;
end;

procedure TEditorForm.FormShow(Sender: TObject);
var
  i: Integer;
begin
  SynEditor.Lines.Clear;

  SetTextFileState;

  StatusBar1.Panels[0].Text := Format('%d: %d', [SynEditor.CaretX, SynEditor.CaretY]);
  StatusBar1.Panels[1].Text := '';
  StatusBar1.Panels[2].Text := 'INS';
  StatusBar1.Panels[3].Text := '';
end;

procedure TEditorForm.OpenFileActionExecute(Sender: TObject);
var
  Dlg: TOpenDialog;
  Lst: TStringList;
begin
  try
    Dlg := TOpenDialog.Create(Self);
    Dlg.Filter := GetEpiDialogFilter(True, true, true, false, false, false, false, true, true, true);
    Dlg.InitialDir := ManagerSettings.WorkingDirUTF8;
    if not Dlg.Execute then exit;

    SetTextFileState;
    if UpperCase(ExtractFileExt(Dlg.FileName)) = '.RECXML' then
      SetXMLFileState;

    Lst := TStringList.Create;
    Lst.LoadFromFile(Dlg.FileName);
    EpiUnknownStringsToUTF8(Lst);
    SynEditor.Lines.AddStrings(Lst);

    StatusBar1.Panels[3].Text := Dlg.FileName;
  finally
    Dlg.Free
  end;
end;

procedure TEditorForm.PasteActionExecute(Sender: TObject);
begin
  SynEditor.PasteFromClipboard;
end;

procedure TEditorForm.PasteAsLabelActionExecute(Sender: TObject);
begin
  if MainForm.ActiveFrame is TDesignFrame then
  with TDesignFrame(MainForm.ActiveFrame) do
  begin
    CopyAction.Execute;
    PasteAsLabel.Execute;
  end;
end;

procedure TEditorForm.RedoActionExecute(Sender: TObject);
begin
  SynEditor.Redo;
end;

procedure TEditorForm.SaveActionExecute(Sender: TObject);
begin
  if StatusBar1.Panels[3].Text = '' then
  begin
    SaveAsAction.Execute;
    Exit;
  end;

  SynEditor.Lines.SaveToFile(StatusBar1.Panels[3].Text);
  SynEditor.Modified := false;
end;

procedure TEditorForm.SaveAsActionExecute(Sender: TObject);
var
  Dlg: TSaveDialog;
begin
  Dlg := TSaveDialog.Create(self);
  try
    Dlg.Filter := GetEpiDialogFilter(True, true, true, false, false, false, false, true, true, true);
    Dlg.Options := Dlg.Options + [ofOverwritePrompt];
    if not Dlg.Execute then exit;

    SynEditor.Lines.SaveToFile(Dlg.FileName);
    SynEditor.Modified := false;
    StatusBar1.Panels[3].Text := Dlg.FileName;
  finally
    Dlg.Free;
  end;
end;

procedure TEditorForm.SynEditorStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  StatusBar1.Panels[0].Text := Format('%d: %d', [SynEditor.CaretY, SynEditor.CaretX]);

  if SynEditor.Modified then
    StatusBar1.Panels[1].Text := 'Modified'
  else
    StatusBar1.Panels[1].Text := '';

  if SynEditor.InsertMode then
    StatusBar1.Panels[2].Text := 'INS'
  else
    StatusBar1.Panels[2].Text := 'OVR'
end;

procedure TEditorForm.UndoActionExecute(Sender: TObject);
begin
  SynEditor.Undo;
end;

procedure TEditorForm.SetTextFileState;
begin
  SynEditor.Highlighter := nil;
  SynEditor.Gutter.CodeFoldPart().Visible := false;
end;

procedure TEditorForm.SetXMLFileState;
begin
  SynEditor.Highlighter := XMLHighlighter;
  SynEditor.Gutter.CodeFoldPart().Visible := true;
end;

initialization
  {$I editormain.lrs}

end.

