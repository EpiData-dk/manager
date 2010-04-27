unit design_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, ComCtrls, epidatafiles;

type

  { TDesignFrame }

  TDesignFrame = class(TFrame)
    ToolBar1: TToolBar;
  private
    { private declarations }
    FDataFile: TEpiDataFile;
    FDesignerBox: TScrollBox;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; ADataFile: TEpiDataFile);
    property    DataFile: TEpiDataFile read FDataFile;
  end; 

implementation

{$R *.lfm}

uses
  Controls, Graphics;

{ TDesignFrame }

constructor TDesignFrame.Create(TheOwner: TComponent; ADataFile: TEpiDataFile);
begin
  inherited Create(TheOwner);
  FDataFile := ADataFile;

  // Designer box creation and setup.
  // - (This is subject to change if we find a better component than
  //    the form or a scrollbox).
  FDesignerBox             := TScrollBox.Create(Self);
  FDesignerBox.Name        := 'DesingerBox';
  FDesignerBox.Parent      := Self;
  FDesignerBox.Align       := alClient;
  FDesignerBox.DockSite    := true;
{  FDesignerBox.OnDockDrop  := @DesignerDockDrop;
  FDesignerBox.OnUnDock    := @DesignerUnDock;
  FDesignerBox.OnMouseDown := @DesignerMouseDown;
  FDesignerBox.OnMouseMove := @DesignerMouseMove;  }
  FDesignerBox.Color       := clWhite;
  FDesignerBox.AutoScroll  := true;
//  FDesignerBox.PopupMenu   := DesignerPopup;
end;

end.

