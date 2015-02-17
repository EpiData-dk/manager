unit admin_user_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, CheckBoxThemed, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, StdCtrls, ComboEx, EditBtn, Buttons, ComCtrls;

type

  { TAdminUserForm }

  TAdminUserForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    CheckBoxThemed2: TCheckBoxThemed;
    DateEdit2: TDateEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    PageControl1: TPageControl;
    Panel2: TPanel;
    BasicSheet: TTabSheet;
    GroupsSheet: TTabSheet;
    procedure CheckBoxThemed1Change(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

implementation

{$R *.lfm}

{ TAdminUserForm }

procedure TAdminUserForm.CheckBoxThemed1Change(Sender: TObject);
begin
  DateEdit1.Enabled := not CheckBoxThemed1.Checked;
end;

end.

