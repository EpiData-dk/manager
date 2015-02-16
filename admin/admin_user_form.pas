unit admin_user_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, CheckBoxThemed, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, StdCtrls, ComboEx, EditBtn, Buttons;

type

  { TUserAdminForm }

  TUserAdminForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    CheckBoxThemed1: TCheckBoxThemed;
    DateEdit1: TDateEdit;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure CheckBoxThemed1Change(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

implementation

{$R *.lfm}

{ TUserAdminForm }

procedure TUserAdminForm.CheckBoxThemed1Change(Sender: TObject);
begin
  DateEdit1.Enabled := not CheckBoxThemed1.Checked;
end;

end.

