unit archive_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  EditBtn, Buttons, FileCtrl, ComboEx, CheckLst;

type

  { TArchiveForm }

  TArchiveForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    CheckListBox1: TCheckListBox;
    SubDirChkBox: TCheckBox;
    EncryptCheckBox: TCheckBox;
    FolderEdit: TDirectoryEdit;
    PasswordEdit: TEdit;
    RepeatPasswordEdit: TEdit;
    SaveAsEdit: TFileNameEdit;
    SingleFileEdit: TFileNameEdit;
    ArchiveBox: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Panel1: TPanel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    procedure RadioButton1Change(Sender: TObject);
  private
    procedure UpdateFilterList(clear: boolean);
  public
    constructor Create(TheOwner: TComponent); override;
  end;

var
  ArchiveForm: TArchiveForm;

implementation

{$R *.lfm}

uses
  epimiscutils;

{ TArchiveForm }

procedure TArchiveForm.RadioButton1Change(Sender: TObject);
begin
  case (TComponent(Sender).Tag) of
    // All files
    1: begin
         UpdateFilterList(true);
         SingleFileEdit.Enabled  := false;
         SubDirChkBox.Enabled    := true;
       end;

    // Filter files
    2: begin
         UpdateFilterList(false);
         SingleFileEdit.Enabled  := false;
         SubDirChkBox.Enabled    := true;
       end;

    // Single file
    3: begin
         UpdateFilterList(true);
         SingleFileEdit.Enabled  := true;
         SubDirChkBox.Enabled    := false;
       end;
  end;
end;

procedure TArchiveForm.UpdateFilterList(clear: boolean);
begin
  CheckListBox1.Enabled := not clear;
end;

constructor TArchiveForm.Create(TheOwner: TComponent);
var
  Item: TEpiDialogFilter;
  Items: TEpiDialogFilters;
begin
  inherited Create(TheOwner);

  Items := dfAllFilters - [dfCollection, dfAll];
  for Item in Items do
    CheckListBox1.AddItem(GetEpiDialogFilterName([Item]), nil);

  CheckListBox1.AutoSize := true;
end;

end.

