unit validate_double_entry_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  CheckLst, ExtCtrls, Buttons, epidocument;

type

  { TValidateDoubleEntryForm }

  TValidateDoubleEntryForm = class(TForm)
    Bevel1: TBevel;
    OkBtn: TBitBtn;
    CancelBtn: TBitBtn;
    OptionsChkGrp: TCheckGroup;
    CmpFAllBtn: TButton;
    CmpFAutoIncBtn: TButton;
    CmpFTextBtn: TButton;
    KFNoneBtn: TButton;
    KFIndexBtn: TButton;
    KFAutoIncBtn: TButton;
    CmpFAutoDateTimeBtn: TButton;
    KFCheckList: TCheckListBox;
    CmpFCheckList: TCheckListBox;
    FileNameLabel1: TLabel;
    FileNameLabel2: TLabel;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    ProjectTitle1: TLabel;
    ProjectTitle2: TLabel;
    RecordCount1: TLabel;
    RecordCount2: TLabel;
    OpenDblEntryFileBtn: TSpeedButton;
    procedure OpenDblEntryFileBtnClick(Sender: TObject);
  private
    { private declarations }
    FMainDoc: TEpiDocument;
    FDupDoc: TEpiDocument;
    procedure UpdateKeyFields;
    procedure UpdateCompareFields;
    procedure UpdateMainDocInfo(Const Fn: string);
    procedure UpdateDupDocInfo(Const Fn: string);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; MainDoc: TEpiDocument; Const FileName: string);
  end;

procedure ValidateDoubleEntry(Doc: TEpiDocument; Const Filename: string);

implementation

{$R *.lfm}

uses
  epidatafiles, epimiscutils, settings2_var;

procedure ValidateDoubleEntry(Doc: TEpiDocument; const Filename: string);
var
  F: TValidateDoubleEntryForm;
begin
  F := TValidateDoubleEntryForm.Create(nil, Doc, Filename);
  F.ShowModal;
  F.Free;
end;

{ TValidateDoubleEntryForm }

procedure TValidateDoubleEntryForm.OpenDblEntryFileBtnClick(Sender: TObject);
var
  Dlg: TOpenDialog;
begin
  Dlg := TOpenDialog.Create(Self);
  Dlg.Filter := GetEpiDialogFilter([dfEPX,dfEPZ]);
  Dlg.Options := [ofPathMustExist,   // shows an error message if selected path does not exist
                  ofFileMustExist,   // shows an error message if selected file does not exist
                  ofEnableSizing,    // dialog can be resized, e.g. via the mouse
                  ofViewDetail      // details are OS and interface dependent
                 ];
  Dlg.InitialDir := ManagerSettings.WorkingDirUTF8;

  try
    if not Dlg.Execute then exit;

    FDupDoc := TEpiDocument.Create(ManagerSettings.StudyLang);
    FDupDoc.LoadFromFile(Dlg.FileName);

    UpdateDupDocInfo(Dlg.FileName);
    UpdateKeyFields;
    UpdateCompareFields;
  finally
    Dlg.Free;
  end;
end;

procedure TValidateDoubleEntryForm.UpdateKeyFields;
var
  MainDF: TEpiDataFile;
  DupDF: TEpiDataFile;
  i: Integer;
begin
  KFCheckList.Clear;
  if not Assigned(FDupDoc) then exit;

  MainDF := FMainDoc.DataFiles[0];
  DupDF  := FDupDoc.DataFiles[0];

  KFCheckList.Items.BeginUpdate;
  for i := 0 to MainDF.Fields.Count - 1 do
  begin
    if DupDF.Fields.ItemExistsByName(MainDF.Fields[i].Name) then
      KFCheckList.AddItem(MainDF.Fields[i].Name, MainDF.Fields[i]);
  end;
  KFCheckList.Items.EndUpdate;
end;

procedure TValidateDoubleEntryForm.UpdateCompareFields;
var
  MainDF: TEpiDataFile;
  DupDF: TEpiDataFile;
  i: Integer;
begin
  CmpFCheckList.Clear;
  if not Assigned(FDupDoc) then exit;

  MainDF := FMainDoc.DataFiles[0];
  DupDF  := FDupDoc.DataFiles[0];

  CmpFCheckList.Items.BeginUpdate;
  for i := 0 to MainDF.Fields.Count - 1 do
  begin
    if DupDF.Fields.ItemExistsByName(MainDF.Fields[i].Name) then
      CmpFCheckList.AddItem(MainDF.Fields[i].Name, MainDF.Fields[i]);
  end;
  CmpFCheckList.Items.EndUpdate;
end;

procedure TValidateDoubleEntryForm.UpdateMainDocInfo(const Fn: string);
begin
  FileNameLabel1.Caption := Fn;
  if Assigned(FMainDoc) then
  begin
    ProjectTitle1.Caption := FMainDoc.Study.Title.Text;
    RecordCount1.Caption := IntToSTr(FMainDoc.DataFiles[0].Size);
  end;
end;

procedure TValidateDoubleEntryForm.UpdateDupDocInfo(const Fn: string);
begin
  if FN <> '' then
    FileNameLabel2.Caption := Fn
  else
    FileNameLabel2.Caption := '[none]';
  if Assigned(FDupDoc) then
  begin
    ProjectTitle2.Caption := FDupDoc.Study.Title.Text;
    RecordCount2.Caption := IntToSTr(FDupDoc.DataFiles[0].Size);
  end else begin
    ProjectTitle2.Caption := '';
    RecordCount2.Caption := '';
  end;
end;

constructor TValidateDoubleEntryForm.Create(TheOwner: TComponent;
  MainDoc: TEpiDocument; const FileName: string);
begin
  inherited Create(TheOwner);
  FMainDoc := MainDoc;
  UpdateMainDocInfo(FileName);
  UpdateDupDocInfo('');
end;

end.

