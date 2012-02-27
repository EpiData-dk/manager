unit prepare_double_entry_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, EditBtn, epidocument;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    TitleEdit: TEdit;
    FileNameEdit: TFileNameEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    FileNameLabel: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Panel1: TPanel;
    TitleLabel: TLabel;
  private
    { private declarations }
    FDoc: TEpiDocument;
    FFileName: string;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; Const Doc: TEpiDocument;
      Const FileName: String);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

constructor TForm1.Create(TheOwner: TComponent; const Doc: TEpiDocument;
  const FileName: String);
begin
  inherited Create(TheOwner);
  FDoc := Doc;
  FFileName := FileName;

  FileNameLabel.Caption := FFileName;
  TitleLabel.Caption := FDoc.Study.Title.Text;
end;

end.

