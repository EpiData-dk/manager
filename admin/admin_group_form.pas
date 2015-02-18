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
    ManageRightsChkGrp: TCheckGroup;
    CaptionEdit: TEdit;
    Label3: TLabel;
    PageControl1: TPageControl;
    Panel2: TPanel;
    BasicSheet: TTabSheet;
    UsersSheet: TTabSheet;
    procedure BitBtn1Click(Sender: TObject);
  private
    FGroup: TEpiGroup;
    procedure FillRights;
    procedure FormShow(Sender: TObject);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    property Group: TEpiGroup read FGroup write FGroup;
  end;

var
  AdminGroupForm: TAdminGroupForm;

implementation

{$R *.lfm}

{ TAdminGroupForm }

procedure TAdminGroupForm.BitBtn1Click(Sender: TObject);
begin
  Group.Caption.Text := CaptionEdit.Text;
end;

procedure TAdminGroupForm.FillRights;
begin
  // TODO
end;

procedure TAdminGroupForm.FormShow(Sender: TObject);
begin
  CaptionEdit.Text := Group.Caption.Text;
end;

constructor TAdminGroupForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  OnShow := @FormShow;
end;

end.

