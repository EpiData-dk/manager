unit workflow_frame;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, ExtCtrls, Controls;

type

  { TWorkFlowFrame }

  TWorkFlowFrame = class(TFrame)
    BackgroundImage: TImage;
    DesignBtnImage: TImage;
    WorkFlowImageList: TImageList;
    ModifyBtnImage: TImage;
    ExtendBtnImage: TImage;
    DocumentBtnImage: TImage;
    AdvBtnImage: TImage;
    BgPanel: TPanel;
    procedure DesignBtnImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DesignBtnImageMouseEnter(Sender: TObject);
    procedure DesignBtnImageMouseLeave(Sender: TObject);
    procedure FrameResize(Sender: TObject);
  private
    { private declarations }
    CircleIdleStream: TMemoryStream;
    CircleOverStrean: TMemoryStream;
    CircleDownStream: TMemoryStream;
    procedure LoadImage(Img: TImage; Idx: Integer);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
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

procedure TWorkFlowFrame.DesignBtnImageMouseEnter(Sender: TObject);
begin
  LoadImage(TImage(Sender), 2);
end;

procedure TWorkFlowFrame.DesignBtnImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  LoadImage(TImage(Sender), 0);
end;

procedure TWorkFlowFrame.DesignBtnImageMouseLeave(Sender: TObject);
begin
  LoadImage(TImage(Sender), 1);
end;

constructor TWorkFlowFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner) ;

  LoadImage(DesignBtnImage, 1);
  LoadImage(ModifyBtnImage, 1);
  LoadImage(ExtendBtnImage, 1);
  LoadImage(DocumentBtnImage, 1);
  LoadImage(AdvBtnImage, 1);

  DesignBtnImage.OnClick := MainForm.NewDataFormBtn.OnClick;
  AdvBtnImage.OnClick := MainForm.EditorBtn.OnClick;
end;

destructor TWorkFlowFrame.Destroy;
begin
  CircleDownStream.Free;
  CircleIdleStream.Free;
  CircleOverStrean.Free;
  inherited Destroy;
end;

initialization
  {$I workflow_frame.lrs}

end.

