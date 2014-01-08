unit rename_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, epidatafiles;

type

  { TRenameForm }

  TRenameForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Panel1: TPanel;
    procedure CheckBox1Change(Sender: TObject);
  private
    FDataFile: TEpiDataFile;
  public
    constructor Create(TheOwner: TComponent; Const DataFile: TEpiDataFile);
  end;

implementation

{$R *.lfm}

{ TRenameForm }

procedure TRenameForm.CheckBox1Change(Sender: TObject);
begin
  case TCheckBox(Sender).Tag of
    1: Edit1.Enabled := TCheckBox(Sender).Checked;
    2: Edit2.Enabled := TCheckBox(Sender).Checked;
    3: Edit3.Enabled := TCheckBox(Sender).Checked;
  end;
end;

constructor TRenameForm.Create(TheOwner: TComponent;
  const DataFile: TEpiDataFile);
begin
  inherited Create(TheOwner);
  FDataFile := DataFile;
end;

end.

