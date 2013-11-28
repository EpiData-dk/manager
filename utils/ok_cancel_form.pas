unit ok_cancel_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, manager_types;

type

  { TOkCancelForm }

  TOkCancelForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Panel1: TPanel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    CanCloseInt: ICanCloseQuery;
    class procedure RestoreDefaultPos;
  end;

implementation

uses
  settings2_var, settings2;

{ TOkCancelForm }

const
  OkCancelForm = 'OkCancelForm';

procedure TOkCancelForm.FormShow(Sender: TObject);
begin
  if ManagerSettings.SaveWindowPositions then
    LoadFormPosition(Self, OkCancelForm);
end;

class procedure TOkCancelForm.RestoreDefaultPos;
var
  F: TForm;
begin
  F := TForm.Create(nil);
  F.Width := 400;
  F.Height := 400;
  F.Top := 200;
  F.Left := 200;
  SaveFormPosition(F, OkCancelForm);
  F.Free;
end;

procedure TOkCancelForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if (ModalResult = mrOk) and
     (Assigned(CanCloseInt))
  then
    CanClose := CanCloseInt.CanClose;

  if ManagerSettings.SaveWindowPositions then
    SaveFormPosition(Self, OkCancelForm);
end;

{$R *.lfm}

end.

