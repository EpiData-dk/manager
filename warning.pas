unit Warning;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  LResources, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type

  { TWarningForm }

  TWarningForm = class(TForm)
    Label1: TLabel;
    Label3: TLabel;
    Button1: TButton;
    Button2: TButton;
    Label4: TLabel;
    Memo1: TMemo;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  WarningForm: TWarningForm;

implementation

initialization
  {$i warning.lrs}

end.
