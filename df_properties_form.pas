unit df_properties_form; 

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls;

type

  { TDataFilePropertiesForm }

  TDataFilePropertiesForm = class(TForm)
    CancelBtn: TButton;
    Label1: TLabel;
    OkBtn: TButton;
    Edit1: TEdit;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  DataFilePropertiesForm: TDataFilePropertiesForm;

implementation

initialization
  {$I df_properties_form.lrs}

end.

