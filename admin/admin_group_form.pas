unit admin_group_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, CheckBoxThemed, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, StdCtrls, EditBtn, Buttons, ComCtrls, epiadmin;

type

  { TAdminGroupForm }

  TAdminGroupForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    CheckGroup2: TCheckGroup;
    Edit3: TEdit;
    Label3: TLabel;
    PageControl1: TPageControl;
    Panel2: TPanel;
    BasicSheet: TTabSheet;
    UsersSheet: TTabSheet;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  AdminGroupForm: TAdminGroupForm;

implementation

{$R *.lfm}

end.

