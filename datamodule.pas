unit datamodule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Controls;

type

  { TDM }

  TDM = class(TDataModule)
    Icons16: TImageList;
    ProgramIcons16: TImageList;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  DM: TDM;

implementation

{$R *.lfm}

end.

