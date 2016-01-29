unit empty_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, forms;


type

  { TPositionForm }

  TPositionForm = class(TForm)
  private
    FStorageName: String;
  protected
    procedure DoShow; override;
    procedure DoClose(var CloseAction: TCloseAction); override;
  public
    constructor Create(TheOwner: TComponent); override;
    class procedure RestoreDefaultPos(AStorageName: String);
    property StorageName: String read FStorageName write FStorageName;
  end;

implementation

uses
  settings2_var, settings2;

{ TPositionForm }

procedure TPositionForm.DoShow;
begin
  inherited DoShow;

  if ManagerSettings.SaveWindowPositions then
    LoadFormPosition(Self, FStorageName);
end;

procedure TPositionForm.DoClose(var CloseAction: TCloseAction);
begin
  inherited DoClose(CloseAction);

  if ManagerSettings.SaveWindowPositions then
    SaveFormPosition(Self, FStorageName);
end;

constructor TPositionForm.Create(TheOwner: TComponent);
begin
  inherited CreateNew(TheOwner);
end;

class procedure TPositionForm.RestoreDefaultPos(AStorageName: String);
var
  Aform: TForm;
begin
  Aform := TForm.Create(nil);
  Aform.Width := 600;
  Aform.Height := 480;
  Aform.top := (Screen.Monitors[0].Height - Aform.Height) div 2;
  Aform.Left := (Screen.Monitors[0].Width - Aform.Width) div 2;
  SaveFormPosition(Aform, AStorageName);
  AForm.free;
end;

end.

