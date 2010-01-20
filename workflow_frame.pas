unit workflow_frame;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, ExtCtrls, Controls, StdCtrls,
  managertypes;

type

  { TWorkFlowFrame }

  TWorkFlowFrame = class(TFrame, IManagerFrame)
    BackgroundImage: TImage;
    DesignBtnImage: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    WorkFlowImageList: TImageList;
    ExtendBtnImage: TImage;
    DocumentBtnImage: TImage;
    AdvBtnImage: TImage;
    BgPanel: TPanel;
    procedure ImageBtnImageMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImageBtnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImageBtnMouseEnter(Sender: TObject);
    procedure ImageBtnMouseLeave(Sender: TObject);
    procedure FrameResize(Sender: TObject);
  private
    { private declarations }
    procedure LoadImage(Img: TImage; Idx: Integer);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure ActivateFrame;
    procedure DeActivateFrame;
  end; 

implementation

uses
  main, Graphics;

{ TWorkFlowFrame }

procedure TWorkFlowFrame.FrameResize(Sender: TObject);
begin
  BgPanel.Left := (Width div 2) - (BgPanel.Width div 2);
  BgPanel.Top := (Height div 2) - (BgPanel.Height div 2);
end;

procedure TWorkFlowFrame.LoadImage(Img: TImage; Idx: Integer);
var
  Bmp: TBitmap;
begin
  // Imagelist:
  //  0 = Down
  //  1 = Idle
  //  2 = Over
  Bmp := TBitmap.Create;
  WorkFlowImageList.GetBitmap(Idx, Bmp);
  Img.Picture.Bitmap := bmp;
  Bmp.Free;
end;

procedure TWorkFlowFrame.ImageBtnMouseEnter(Sender: TObject);
begin
  LoadImage(TImage(Sender), 2);
end;

procedure TWorkFlowFrame.ImageBtnMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  LoadImage(TImage(Sender), 0);
end;

procedure TWorkFlowFrame.ImageBtnImageMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  LoadImage(TImage(Sender), 2);
end;

procedure TWorkFlowFrame.ImageBtnMouseLeave(Sender: TObject);
begin
  LoadImage(TImage(Sender), 1);
end;

constructor TWorkFlowFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  LoadImage(DesignBtnImage, 1);
  LoadImage(ExtendBtnImage, 1);
  LoadImage(DocumentBtnImage, 1);
  LoadImage(AdvBtnImage, 1);

  DesignBtnImage.OnClick := MainForm.NewDataFormBtn.OnClick;
  AdvBtnImage.OnClick := MainForm.EditorBtn.OnClick;
end;

destructor TWorkFlowFrame.Destroy;
begin
  inherited Destroy;
end;

procedure TWorkFlowFrame.ActivateFrame;
begin
  // do nothing for now...
end;

procedure TWorkFlowFrame.DeActivateFrame;
begin
  // do nothing for now...
end;

initialization
  {$I workflow_frame.lrs}

end.

