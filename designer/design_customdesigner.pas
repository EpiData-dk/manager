unit design_customdesigner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, epidatafiles;

type

  { TCustomDesignFrame }

  TCustomDesignFrame = class(TFrame)
  protected
    FImportedFileName: string;
    function GetDataFile: TEpiDataFile; virtual; abstract;
    procedure SetDataFile(AValue: TEpiDataFile); virtual; abstract;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure   UpdateFrame; virtual; abstract;
    procedure   RestoreDefaultPos; virtual; abstract;
    property    DataFile: TEpiDataFile read GetDataFile write SetDataFile;
    property    ImportedFileName: string read FImportedFileName;
  end;

implementation

{ TCustomDesignFrame }

constructor TCustomDesignFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

end.

