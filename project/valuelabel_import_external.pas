unit valuelabel_import_external;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  ExtCtrls, ComCtrls, StdCtrls, Buttons,
  epivaluelabels, epiopenfile, epicustombase, epidocument;

type

  { TExtVLSetForm }

  TExtVLSetForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    ErrorListBox: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    ProgressBar1: TProgressBar;
    StringGrid1: TStringGrid;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure FileProgress(const Sender: TEpiCustomBase;
      ProgressType: TEpiProgressType; CurrentPos, MaxPos: Cardinal;
      var Canceled: Boolean);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
    procedure StringGrid1GetCellHint(Sender: TObject; ACol, ARow: Integer;
      var HintText: String);
    procedure StringGrid1GetCheckboxState(Sender: TObject; ACol, ARow: Integer;
      var Value: TCheckboxState);
    procedure StringGrid1PrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
  private
    { private declarations }
    FValueLabelSets: TEpiValueLabelSets;
    FDocFiles: TList;
    procedure  ReportError(Const Msg: string);
    procedure AddToGrid(Const DocFile: TEpiDocumentFile);
    procedure AddFile(Const FileName: String);
    procedure AddFiles(Const Files: TStrings);
    function  ShowDialog(): boolean;
    procedure LoadGlyphs;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; Const ValueLabelSets: TEpiValueLabelSets);
    destructor Destroy; override;
    class procedure RestoreDefaultPos;
  end;

implementation

{$R *.lfm}

uses
  epiv_documentfile, LazFileUtils, epimiscutils, epiv_datamodule,
  settings2_var, settings2;

const
  FormName = 'ExternalValueLabelSetForm';

{ TExtVLSetForm }

procedure TExtVLSetForm.FormShow(Sender: TObject);
begin
  if ManagerSettings.SaveWindowPositions then
    LoadFormPosition(Self, FormName);

  if not ShowDialog() then close;
end;

procedure TExtVLSetForm.StringGrid1GetCellHint(Sender: TObject; ACol, ARow: Integer;
  var HintText: String);
begin
  if not (aCol = 2) then exit;

  if StringGrid1.Objects[aCol, aRow] = TObject($1) then
    HintText :=
      'The name of this value label set is already used!' + LineEnding +
      LineEnding +
      'Use the Value Label Editor to rename the existing value label set.';

  if StringGrid1.Objects[aCol, aRow] = TObject($2) then
    HintText :=
      'The name of this value label set is already present in the import!' + LineEnding +
      LineEnding +
      'Only unique names may be added as external value label sets.';
end;

procedure TExtVLSetForm.StringGrid1GetCheckboxState(Sender: TObject; ACol,
  ARow: Integer; var Value: TCheckboxState);
begin
  if Assigned(StringGrid1.Objects[2, aRow]) then
    Value := cbUnchecked
  else
    if StringGrid1.Cells[ACol, ARow] = '1' then
      Value := cbChecked
    else
      Value := cbUnchecked;
end;

procedure TExtVLSetForm.StringGrid1PrepareCanvas(sender: TObject; aCol, aRow: Integer;
  aState: TGridDrawState);
var
  OldColor: TColor;
begin
  if not (aCol = 2) then exit;

  if Assigned(StringGrid1.Objects[aCol, aRow]) then
    StringGrid1.Canvas.Brush.Color := clYellow;
end;

procedure TExtVLSetForm.ReportError(const Msg: string);
begin
  if not ErrorListBox.Visible then
    ErrorListBox.Visible := true;

  ErrorListBox.Items.Add(Msg);
end;

procedure TExtVLSetForm.AddToGrid(const DocFile: TEpiDocumentFile);
var
  VLS: TEpiValueLabelSets;
  Idx: Integer;
  VL: TEpiValueLabelSet;
  i: Integer;
begin
  VLS := DocFile.Document.ValueLabelSets;

  Idx := StringGrid1.RowCount;
  StringGrid1.RowCount := StringGrid1.RowCount + VLS.Count;

  for i := 0 to VLS.Count - 1 do
  begin
    VL := VLS[i];
    StringGrid1.Objects[0, Idx + i] := VL;
    StringGrid1.Objects[1, Idx + i] := DocFile;

    if FValueLabelSets.ItemExistsByName(VL.Name) then
      StringGrid1.Objects[2, Idx + i] := TObject($1);
    if StringGrid1.Cols[2].IndexOf(VL.Name) > -1 then
      StringGrid1.Objects[2, Idx + i] := TObject($2);


    if Assigned(StringGrid1.Objects[2, Idx + i]) then
      StringGrid1.Cells[0, Idx + i] := '0'                               // Add
    else
      StringGrid1.Cells[0, Idx + i] := '1';
    StringGrid1.Cells[1, Idx + i] := ExtractFileName(DocFile.FileName);  // Filename
    StringGrid1.Cells[2, Idx + i] := VL.Name;                            // Set Name
    StringGrid1.Cells[3, Idx + i] := EpiTypeNamesShort[VL.LabelType];    // Type
    StringGrid1.Cells[4, Idx + i] := IntToStr(VL.Count);                 // Value Labels
    StringGrid1.Cells[5, Idx + i] := IntToStr(VL.MissingCount);          // Missings
  end;
end;

procedure TExtVLSetForm.FileProgress(const Sender: TEpiCustomBase;
  ProgressType: TEpiProgressType; CurrentPos, MaxPos: Cardinal;
  var Canceled: Boolean);
Const
  LastUpdate: Cardinal = 0;
  ProgressUpdate: Cardinal = 0;
begin
  case ProgressType of
    eptInit:
      begin
        ProgressUpdate := MaxPos div 50;
        ProgressBar1.Position := CurrentPos;
        ProgressBar1.Visible := true;
        ProgressBar1.Max := MaxPos;
        Application.ProcessMessages;
      end;
    eptFinish:
      begin
        ProgressBar1.Visible := false;
        Application.ProcessMessages;
        LastUpdate := 0;
      end;
    eptRecords:
      begin
        if CurrentPos > (LastUpdate + ProgressUpdate) then
        begin
          ProgressBar1.Position := CurrentPos;
          {$IFNDEF MSWINDOWS}
          Application.ProcessMessages;
          {$ENDIF}
          LastUpdate := CurrentPos;
        end;
      end;
  end;
end;

procedure TExtVLSetForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if ManagerSettings.SaveWindowPositions then
    SaveFormPosition(Self, FormName);
end;

procedure TExtVLSetForm.BitBtn1Click(Sender: TObject);
var
  VL: TEpiValueLabelSet;
  i: Integer;
begin
  Screen.Cursor := crHourGlass;
  Application.ProcessMessages;

  for i := 1 to StringGrid1.RowCount - 1 do
  begin
    if StringGrid1.Cells[0, i] = '0' then continue;

    VL := TEpiValueLabelSet(StringGrid1.Objects[0, i]);
    VL := TEpiValueLabelSet(VL.Clone(FValueLabelSets));
    VL.ExtFileName := StringGrid1.Cells[1, i];
    VL.LabelScope  := vlsExternal;
    FValueLabelSets.AddItem(VL);
  end;

  Screen.Cursor := crDefault;
  Application.ProcessMessages;
end;

procedure TExtVLSetForm.BitBtn3Click(Sender: TObject);
begin
  ShowDialog();
end;

procedure TExtVLSetForm.AddFile(const FileName: String);
var
  Docfile: TDocumentFile;
begin
  Docfile := TDocumentFile.Create;
  try
//    Docfile.OnProgress := @FileProgress;
    Docfile.OnError := @ReportError;
    if Docfile.OpenFile(FileName, true) then
      AddToGrid(DocFile);
    FDocFiles.Add(DocFile);
  except
    on E: Exception do
    begin
      ReportError('Failed to read file "' + ExtractFileName(FileName) + '": ' + E.Message);
      if FDocFiles.IndexOf(Docfile) > -1 then
        FDocFiles.Remove(Docfile);
      Docfile.Free;
    end;
  end;
end;

procedure TExtVLSetForm.AddFiles(const Files: TStrings);
var
  i: Integer;
begin
  Screen.Cursor := crHourGlass;
  Application.ProcessMessages;
  for i := 0 to Files.Count - 1 do
    AddFile(Files[i]);
  Screen.Cursor := crDefault;
  Application.ProcessMessages;
end;

function TExtVLSetForm.ShowDialog: boolean;
var
  Dlg: TOpenDialog;
begin
  Dlg := TOpenDialog.Create(self);
  Dlg.Filter := GetEpiDialogFilter([dfEPX, dfEPZ, dfCollection]);
  Dlg.Options := Dlg.Options + [ofAllowMultiSelect];
  Result := Dlg.Execute;
  if result then
    AddFiles(Dlg.Files);
  Dlg.Free;
end;

procedure TExtVLSetForm.LoadGlyphs;
begin
  DM.Icons16.GetBitmap(19, BitBtn3.Glyph);
end;

constructor TExtVLSetForm.Create(TheOwner: TComponent;
  const ValueLabelSets: TEpiValueLabelSets);
begin
  inherited Create(TheOwner);
  FValueLabelSets := ValueLabelSets;
  FDocFiles := TList.Create;

  LoadGlyphs;
end;

destructor TExtVLSetForm.Destroy;
var
  Item: Pointer;
begin
  for Item in FDocFiles do
    TObject(Item).Free;
  FDocFiles.Clear;
  FDocFiles.Free;
  inherited Destroy;
end;

class procedure TExtVLSetForm.RestoreDefaultPos;
var
  F: TForm;
begin
  F := TForm.Create(nil);
  with F do
  begin
    Width := 800;
    Height := 350;
    Left := 300;
    Top := 300;
  end;
  SaveFormPosition(F, FormName);
  F.Free;
end;

end.

