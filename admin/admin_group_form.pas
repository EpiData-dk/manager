unit admin_group_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, CheckBoxThemed, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, StdCtrls, EditBtn, Buttons, epiadmin;

type

  { TGroupAdminForm }

  TGroupAdminForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    CheckGroup1: TCheckGroup;
    Edit2: TEdit;
    Label2: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  GroupAdminForm: TGroupAdminForm;

implementation

{$R *.lfm}

end.

