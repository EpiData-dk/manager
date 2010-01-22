unit FieldEdit;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UEpiDataFile, StdCtrls, Controls,
  UDataFileTypes, LMessages, Graphics, AVL_Tree;

type

  { TSelectCorner }

  TSelectCorner = class(TCustomControl)
  private
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { IFieldControl }

  IFieldControl = interface ['{8F9E1A89-AA09-473E-A1DD-9BD7356F96A0}']
    function GetXTreeNode: TAVLTreeNode;
    function GetYTreeNode: TAVLTreeNode;
    procedure SetXTreeNode(const AValue: TAVLTreeNode);
    procedure SetYTreeNode(const AValue: TAVLTreeNode);
    procedure SetField(const AValue: TEpiField);
    function  GetField: TEpiField;
    property  Field: TEpiField read GetField write SetField;
    property  YTreeNode: TAVLTreeNode read GetYTreeNode write SetYTreeNode;
    property  XTreeNode: TAVLTreeNode read GetXTreeNode write SetXTreeNode;
  end;

  { TFieldEdit }

  TFieldEdit = class(TEdit, IFieldControl)
  private
    FField: TEpiField;
    // Optional field name label.
    FFieldNameLabel: TLabel;
    FVariableLabel: TLabel;
    FVariableLabelOffset: TPoint;

    // Experimental grapper.
    FSelectCorner: TSelectCorner;
    FXTreeNode: TAVLTreeNode;
    FYTreeNode: TAVLTreeNode;
    function GetField: TEpiField;
    procedure SetField(const AValue: TEpiField);
    procedure UpdateFieldNameLabel;
    procedure UpdateHint(aShow: boolean = true);
    procedure OnFieldChange(Sender: TObject; EventType: TEpiFieldChangeEventType; OldValue: EpiVariant);
    function GetXTreeNode: TAVLTreeNode;
    function GetYTreeNode: TAVLTreeNode;
    procedure SetXTreeNode(const AValue: TAVLTreeNode);
    procedure SetYTreeNode(const AValue: TAVLTreeNode);
  protected
    procedure SetParent(NewParent: TWinControl); override;
    procedure SetVisible(Value: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure  ForceVisualUpdate;
    procedure DoStartDock(var DragObject: TDragObject); override;
    procedure DoEndDock(Target: TObject; X, Y: Integer); override;
    procedure DoEnter; override;
    procedure DoExit; override;
    property Field: TEpiField read GetField write SetField;
    property VariableLabel: TLabel read FVariableLabel;
    property FieldNameLabel: TLabel read FFieldNameLabel;
    property YTreeNode: TAVLTreeNode read GetYTreeNode write SetYTreeNode;
    property XTreeNode: TAVLTreeNode read GetXTreeNode write SetXTreeNode;
  published
    property OnStartDock;
    property OnEndDock;
  end;

  { TFieldAVLTree }

  TFieldAVLTreeType = (attY, attX);
  TFieldAVLTree = class(TAVLTree)
  private
    FAVLTreeType: TFieldAVLTreeType;
  public
    function Add(Data: Pointer): TAVLTreeNode;
    property AVLTreeType: TFieldAVLTreeType read FAVLTreeType write FAVLTreeType;
  end;


  { TFieldDockObject }

{  TFieldDockObject = class(TDragDockObject)
  private
     OldFieldRec: TRect;
     FieldRec: TRect;
  protected
     procedure AdjustDockRect(ARect: TRect); override;
     procedure InitDock(APosition: TPoint); override;
     procedure ShowDockImage; override;
     procedure MoveDockImage; override;
     procedure HideDockImage; override;
  public
    constructor Create(AControl: TControl); override;
    destructor Destroy; override;
  end;                     }

  { TFieldLabel }

  TFieldLabel = class(TLabel, IFieldControl)
  private
    FField: TEpiField;
    FXTreeNode: TAVLTreeNode;
    FYTreeNode: TAVLTreeNode;
    function GetField: TEpiField;
    function GetXTreeNode: TAVLTreeNode;
    function GetYTreeNode: TAVLTreeNode;
    procedure OnFieldChange(Sender: TObject; EventType: TEpiFieldChangeEventType; OldValue: EpiVariant);
    procedure SetField(const AValue: TEpiField);
    procedure SetXTreeNode(const AValue: TAVLTreeNode);
    procedure SetYTreeNode(const AValue: TAVLTreeNode);
    procedure UpdateHint(aShow: boolean = true);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Field: TEpiField read GetField write SetField;
    property YTreeNode: TAVLTreeNode read GetYTreeNode write SetYTreeNode;
    property XTreeNode: TAVLTreeNode read GetXTreeNode write SetXTreeNode;
  published
    property OnStartDock;
    property OnEndDock;
  end;

implementation

uses
  InterfaceBase, LCLType, Math, LCLProc, main, settings, ExtCtrls,
  design_frame, UEpiUtils;

{ TFieldEdit }

procedure TFieldEdit.OnFieldChange(Sender: TObject;
  EventType: TEpiFieldChangeEventType; OldValue: EpiVariant);
var
  OldWidth: LongInt;
  Dx: Integer;
begin
  with TEpiField(Sender) do
  begin
    case EventType of
      fceUpdate:
        begin
          // Edit:
          FFieldNameLabel.Caption := FieldName;
          Top  := FieldY;
          Left := FieldX;
          Width := TDesignFrame(Parent).Canvas.TextWidth('W') * FieldLength;
          // Label:
          Self.FVariableLabel.Left := LabelX;
          Self.FVariableLabel.Top  := LabelY;
          Self.FVariableLabel.Caption := VariableLabel;
        end;
      fceName:   FFieldNameLabel.Caption := FieldName;
      fceLength: Width := TDesignFrame(Parent).Canvas.TextWidth('W') * FieldLength;
      fceDecimals: ;
      fceFX: Left := FieldX;
      fceFY: Top  := FieldY;
      fceFColTxt:;
      fceFColHl:;
      fceFColBg:;
      fceVarLabel:
        begin
          OldWidth := Self.VariableLabel.Width;
          Self.FVariableLabel.Caption := VariableLabel;
          if VariableLabel = '' then
            Self.FVariableLabel.Caption := ' ';
          if (Self.FVariableLabel.Width + Self.FVariableLabel.Left) > Left then
          begin
            Dx := Self.FVariableLabel.Width - OldWidth;
            LabelX := LabelX - Max(0, Dx);
          end;
        end;
      fceVX: Self.FVariableLabel.Left := LabelX;
      fceVY: Self.FVariableLabel.Top  := LabelY;
      fceVColTxt:;
      fceVColBg:;
    end;
  end;
  if EventType in [fceUpdate, fceName, fceVarLabel, fceVX, fceVY] then
    UpdateFieldNameLabel;
  UpdateHint;
end;

procedure TFieldEdit.UpdateFieldNameLabel;
begin
  if Assigned(Field) then
    FieldNameLabel.Caption  := Field.FieldName;
  FieldNameLabel.Left     := VariableLabel.Left - (FieldNameLabel.Width + 5);
  FieldNameLabel.Top      := VariableLabel.Top;
end;

procedure TFieldEdit.UpdateHint(aShow: boolean);
begin
  ShowHint := aShow;

  with Field do
  begin
    if FieldType = ftFloat then
      Hint := WideFormat(
        'Name: %s' + LineEnding +
        'Type: %s' + LineEnding +
        'Length: %d.%d' + LineEnding +
        'Label: %s' + LineEnding +
        'X: %d, Y: %d',
        [UTF8Decode(FieldName), FieldTypeToFieldTypeName(FieldType, nil),
         FieldLength-FieldDecimals-1, FieldDecimals, UTF8Decode(VariableLabel), FieldX, FieldY]
      )
    else
      Hint := WideFormat(
        'Name: %s' + LineEnding +
        'Type: %s' + LineEnding +
        'Length: %d' + LineEnding +
        'Label: %s' + LineEnding +
        'X: %d, Y: %d',
        [UTF8Decode(FieldName), FieldTypeToFieldTypeName(FieldType, nil),
         FieldLength, UTF8Decode(VariableLabel), FieldX, FieldY]
      );
  end;
end;

procedure TFieldEdit.SetField(const AValue: TEpiField);
begin
  if not Assigned(Parent) then
    Exit;

  FField := AValue;
  Field.RegisterOnChangeHook(@OnFieldChange);

  Top    := Field.FieldY;
  Left   := Field.FieldX;
  Width  := TDesignFrame(Parent).Canvas.TextWidth('W') * Field.FieldLength;

  VariableLabel.Caption := Field.VariableLabel;
  // To avoid big empty space.
  if Field.VariableLabel = '' then
    VariableLabel.Caption := ' ';
  if Field.LabelX = 0 then
    Field.LabelX := Left - (VariableLabel.Width + 5)
  else
    VariableLabel.Left := Field.LabelX;
  if Field.LabelY = 0 then
    Field.LabelY := Top + (Height - VariableLabel.Height)
  else
    VariableLabel.Top := Field.LabelY;

  UpdateFieldNameLabel;
  UpdateHint;
end;

procedure TFieldEdit.SetXTreeNode(const AValue: TAVLTreeNode);
begin
  FXTreeNode := AValue;
end;

procedure TFieldEdit.SetYTreeNode(const AValue: TAVLTreeNode);
begin
  FYTreeNode := AValue;
end;

function TFieldEdit.GetField: TEpiField;
begin
  Result := FField;
end;

function TFieldEdit.GetXTreeNode: TAVLTreeNode;
begin
  Result := FXTreeNode;
end;

function TFieldEdit.GetYTreeNode: TAVLTreeNode;
begin
  Result := FYTreeNode;
end;

procedure TFieldEdit.SetParent(NewParent: TWinControl);
begin
  inherited SetParent(NewParent);
  if csDestroying in ComponentState then exit;

  FVariableLabel.Parent := NewParent;
  FFieldNameLabel.Parent := NewParent;

  UpdateFieldNameLabel;
end;

procedure TFieldEdit.SetVisible(Value: Boolean);
begin
  inherited SetVisible(Value);
  UpdateFieldNameLabel;
end;

constructor TFieldEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FVariableLabel := TLabel.Create(Self);
  FFieldNameLabel := TLabel.Create(Self);
  if ManagerSettings.ShowFieldBorder then
    BorderStyle := bsSingle
  else
    BorderStyle := bsNone;

  // Experimental:
  FSelectCorner := nil;
end;

destructor TFieldEdit.Destroy;
begin
  if Assigned(FField) then
    FField.UnRegisterOnChangeHook(@OnFieldChange);
  FField := nil;
  // Do not destroy - it's handled byt the visual destruction of the frame.
  FVariableLabel := nil;
  FFieldNameLabel := nil;
  inherited Destroy;
end;

procedure TFieldEdit.ForceVisualUpdate;
begin
  Left := Field.FieldX;
  Top  := Field.FieldY;
  if Assigned(Parent) then
    Width := TDesignFrame(Parent).Canvas.TextWidth('W') * Field.FieldLength
  else
    Width := 8 * Field.FieldLength;
  VariableLabel.Caption := Field.VariableLabel;
  VariableLabel.Left := Field.LabelX;
  VariableLabel.Top  := Field.LabelY;
  if ManagerSettings.ShowFieldBorder then
    BorderStyle := bsSingle
  else
    BorderStyle := bsNone;
  UpdateFieldNameLabel;
end;

procedure TFieldEdit.DoStartDock(var DragObject: TDragObject);
var
  s: string;
begin
  inherited DoStartDock(DragObject);
  FVariableLabelOffset := Point(Left - FVariableLabel.Left, Top - FVariableLabel.Top);
end;

procedure TFieldEdit.DoEndDock(Target: TObject; X, Y: Integer);
var
  s: string;
begin
  inherited DoEndDock(Target, X, Y);
  Field.FieldX := Left;
  Field.FieldY := Top;

  Field.LabelX := Left - FVariableLabelOffset.X;
  Field.LabelY := Top - FVariableLabelOffset.Y;

  UpdateFieldNameLabel;
end;

procedure TFieldEdit.DoEnter;
begin
  inherited DoEnter;

{  FSelectCorner := TSelectCorner.Create(Self);
  FSelectCorner.Top := Top - (FSelectCorner.Height div 2);
  FSelectCorner.Left := Left - (FSelectCorner.Width div 2);
  FSelectCorner.Parent := Parent;
  FSelectCorner.DoubleBuffered := true;    }
end;

procedure TFieldEdit.DoExit;
begin
  inherited DoExit;
{  FSelectCorner.Free;}
end;

{ TFieldLabel }

procedure TFieldLabel.OnFieldChange(Sender: TObject;
  EventType: TEpiFieldChangeEventType; OldValue: EpiVariant);
begin
  with TEpiField(Sender) do
  begin
    case EventType of
      fceName: ;
      fceLength: ;
      fceDecimals: ;
      fceFX: Left := FieldX;
      fceFY: Top  := FieldY;
      fceFColTxt:;
      fceFColHl:;
      fceFColBg:;
      fceVarLabel: Caption := VariableLabel;
      fceVX: Left := LabelX;
      fceVY: Top  := LabelY;
      fceVColTxt:;
      fceVColBg:;
    end;
  end;
  UpdateHint;
end;

function TFieldLabel.GetField: TEpiField;
begin
  Result := FField;
end;

function TFieldLabel.GetXTreeNode: TAVLTreeNode;
begin
  Result := FXTreeNode;
end;

function TFieldLabel.GetYTreeNode: TAVLTreeNode;
begin
  Result := FYTreeNode;
end;

procedure TFieldLabel.SetField(const AValue: TEpiField);
begin
  FField := AValue;
  Field.RegisterOnChangeHook(@OnFieldChange);

  Top := Field.FieldY;
  Left := Field.FieldX;

  Caption := Field.VariableLabel;
  UpdateHint;
end;

procedure TFieldLabel.SetXTreeNode(const AValue: TAVLTreeNode);
begin
  FXTreeNode := AValue;
end;

procedure TFieldLabel.SetYTreeNode(const AValue: TAVLTreeNode);
begin
  FYTreeNode := AValue;
end;

procedure TFieldLabel.UpdateHint(aShow: boolean);
begin
  ShowHint := aShow;

  Hint := Format(
    'Name: %s' + LineEnding +
    'Type: %s' + LineEnding +
    'Length: %d' + LineEnding +
    'Label: %s' + LineEnding +
    'X: %d, Y: %d',
    [Field.FieldName, FieldTypeToFieldTypeName(Field.FieldType, nil),
     Field.FieldLength, Field.VariableLabel, Field.FieldX, Field.FieldY]
  );
end;

constructor TFieldLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TFieldLabel.Destroy;
begin
  inherited Destroy;
  FField := nil;
end;

(*
{ TFieldDockObject }

procedure TFieldDockObject.AdjustDockRect(ARect: TRect);
begin
  inherited AdjustDockRect(ARect);
{  with ARect do
    MainForm.Label1.Caption := Format(
     'AdjustDockRect - Top: %d, Left: %d, Bottom: %d, Right: %d',
     [Top, Left, Bottom, Right]);       }
{  with DockOffset do
    OffsetRect(FieldRec, -X, -Y);      }
end;

procedure TFieldDockObject.InitDock(APosition: TPoint);
var
  TmpPoint: TPoint;
begin
  inherited InitDock(APosition);

  // Determine hotspot scale for adjusting the undocked rectangle.
  // Since the undocked extent of the control doesn't change, we fix the hotspot offset.
  // Usage: OffsetRect(DockRect, FDockOffset);

  {MainForm.Label2.Caption := Format(
    'InitDock. Position - X: %d , Y: %d',
    [APosition.X, APosition.Y]);   }

  // mouse click offset from control TopLeft in screen coordinates
  with DockRect do
  begin
    TopLeft := Control.ClientToScreen(Point(0, 0));
    TmpPoint := TFieldEdit(Control).VariableLabel.ClientToScreen(Point(0,0));

    Left := Min(Left, TmpPoint.X);
    Top := Min(Top, TmpPoint.Y);

    BottomRight := Control.ClientToScreen(Point(Control.Width, Control.Height));
    TmpPoint := TFieldEdit(Control).VariableLabel.ClientToScreen(
      Point(TFieldEdit(Control).VariableLabel.Width,
            TFieldEdit(Control).VariableLabel.Height));

    Bottom := Max(Bottom, TmpPoint.Y);
    Right  := Max(Right, TmpPoint.X);
  end;
//  OldFieldRec := Rect(MaxInt, 0, MaxInt, 0);
end;

procedure TFieldDockObject.ShowDockImage;
begin
  Inherited ShowDockImage;
{  With FieldRec do
    MainForm.Label3.Caption := Format(
      'ShowDockImage: FieldRect - Top: %d, Left: %d, Bottom: %d, Right: %d',
      [Top, Left, Bottom, Right]);      }

{  WidgetSet.DrawDefaultDockImage(OldFieldRec, FieldRec, disShow);
  OldFieldRec := FieldRec;    }
end;

procedure TFieldDockObject.MoveDockImage;
begin
  inherited MoveDockImage;
{  With FieldRec do
    MainForm.Label4.Caption := Format(
      'MoveDockImage: FieldRect - Top: %d, Left: %d, Bottom: %d, Right: %d',
      [Top, Left, Bottom, Right]);     }

{  //Draw the form outlines when the position has changed
  if not CompareMem(@FieldRec, @OldFieldRec, SizeOf(TRect)) then
  begin
    WidgetSet.DrawDefaultDockImage(OldFieldRec, FieldRec, disMove);
    OldFieldRec := FieldRec;
  end;   }
end;

procedure TFieldDockObject.HideDockImage;
begin
  inherited HideDockImage;
 { With FieldRec do
    MainForm.Label5.Caption := Format(
      'HideDockImage: FieldRect - Top: %d, Left: %d, Bottom: %d, Right: %d',
      [Top, Left, Bottom, Right]);  }
{
  WidgetSet.DrawDefaultDockImage(OldFieldRec, FieldRec, disHide);
  OldFieldRec := Rect(MaxInt, 0, MaxInt, 0);  }
end;

constructor TFieldDockObject.Create(AControl: TControl);
begin
  inherited Create(AControl);
end;

destructor TFieldDockObject.Destroy;
begin
  inherited Destroy;
end;

*)

{ TSelectCorner }

procedure TSelectCorner.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TSelectCorner.Paint;
var
  Bmp: TBitmap;
begin
  Bmp := TBitmap.Create;
  try
    Bmp.Height := Height;
    Bmp.Width := Width;

    Bmp.Canvas.Pen.Color := clBlack;
    Bmp.Canvas.Brush.Color := clBlack;
    Bmp.Canvas.Rectangle(0, 0, Width, Height);

    Canvas.Draw(0, 0, Bmp);
  finally
    FreeAndNil(Bmp);
  end;

  inherited Paint;
end;

constructor TSelectCorner.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 6;
  Height := 6;
end;

destructor TSelectCorner.Destroy;
begin
  inherited Destroy;
end;

{ TFieldAVLTree }

function TFieldAVLTree.Add(Data: Pointer): TAVLTreeNode;
begin
  Result := inherited Add(TControl(Data));
  case AVLTreeType of
    attX: (TControl(Data) as IFieldControl).XTreeNode := Result;
    attY: (TControl(Data) as IFieldControl).YTreeNode := result;
  end;
end;

end.

