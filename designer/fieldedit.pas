unit FieldEdit;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidatafile, StdCtrls, Controls,
  epidatatypes, LMessages, Graphics, AVL_Tree;

type

  { TFieldAVLTree }

  TFieldAVLTreeType = (attY, attX);
  TFieldAVLTree = class(TAVLTree)
  private
    FAVLTreeType: TFieldAVLTreeType;
  public
    function Add(Data: Pointer): TAVLTreeNode;
    procedure WriteReportToStream(s: TStream; var StreamSize: int64);
    property AVLTreeType: TFieldAVLTreeType read FAVLTreeType write FAVLTreeType;
  end;

  { IEpiControl }

  IEpiControl = interface ['{8F9E1A89-AA09-473E-A1DD-9BD7356F96A0}']
    function GetEpiLeft: Integer;
    function GetEpiTop: Integer;
    function GetId: string;
    function GetXTreeNode: TAVLTreeNode;
    function GetYTreeNode: TAVLTreeNode;
    procedure SetEpiLeft(const AValue: Integer);
    procedure SetEpiTop(const AValue: Integer);
    procedure SetXTreeNode(const AValue: TAVLTreeNode);
    procedure SetYTreeNode(const AValue: TAVLTreeNode);
    property  EpiTop:    Integer read GetEpiTop write SetEpiTop;
    property  EpiLeft:   Integer read GetEpiLeft write SetEpiLeft;
    property  Id:        String  read GetId;
    property  YTreeNode: TAVLTreeNode read GetYTreeNode write SetYTreeNode;
    property  XTreeNode: TAVLTreeNode read GetXTreeNode write SetXTreeNode;
  end;

  { TFieldEdit }

  TFieldEdit = class(TEdit, IEpiControl)
  private
    FField: TEpiField;
    // Optional field name label.
    FFieldNameLabel: TLabel;
    FVariableLabel: TLabel;
    FVariableLabelOffset: TPoint;
    FTtop: Integer;
    FXTreeNode: TAVLTreeNode;
    FYTreeNode: TAVLTreeNode;
    function GetEpiLeft: Integer;
    function GetEpiTop: Integer;
    function GetField: TEpiField;
    function GetId: String;
    procedure SetEpiLeft(const AValue: Integer);
    procedure SetEpiTop(const AValue: Integer);
    procedure SetField(const AValue: TEpiField);
    procedure UpdateFieldNameLabel;
    procedure UpdateHint(aShow: boolean = true);
    procedure OnFieldChange(Sender: TObject; EventType: TEpiFieldChangeEventType; OldValue: Pointer);
    function  GetXTreeNode: TAVLTreeNode;
    function  GetYTreeNode: TAVLTreeNode;
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
    property Field: TEpiField read GetField write SetField;
    property VariableLabel: TLabel read FVariableLabel;
    property FieldNameLabel: TLabel read FFieldNameLabel;
    property EpiTop:    Integer read GetEpiTop write SetEpiTop;
    property EpiLeft:   Integer read GetEpiLeft write SetEpiLeft;
    property  Id:        String  read GetId;
    property YTreeNode: TAVLTreeNode read GetYTreeNode write SetYTreeNode;
    property XTreeNode: TAVLTreeNode read GetXTreeNode write SetXTreeNode;
  published
    property OnStartDock;
    property OnEndDock;
  end;

  { TFieldLabel }

  TFieldLabel = class(TLabel, IEpiControl)
  private
    FTextLabel: TEpiTextLabel;
    FXTreeNode: TAVLTreeNode;
    FYTreeNode: TAVLTreeNode;
    function GetEpiLeft: Integer;
    function GetEpiTop: Integer;
    function GetId: String;
    function GetTextLabel: TEpiTextLabel;
    function GetXTreeNode: TAVLTreeNode;
    function GetYTreeNode: TAVLTreeNode;
    procedure OnTextLabelChange(Sender: TObject; EventType: TEpiTextLabelChangeEventType; OldValue: Pointer);
    procedure SetEpiLeft(const AValue: Integer);
    procedure SetEpiTop(const AValue: Integer);
    procedure SetTextLabel(const AValue: TEpiTextLabel);
    procedure SetXTreeNode(const AValue: TAVLTreeNode);
    procedure SetYTreeNode(const AValue: TAVLTreeNode);
    procedure UpdateHint(aShow: boolean = true);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property TextLabel: TEpiTextLabel read GetTextLabel write SetTextLabel;
    property EpiTop:    Integer read GetEpiTop write SetEpiTop;
    property EpiLeft:   Integer read GetEpiLeft write SetEpiLeft;
    property  Id:        String  read GetId;
    property YTreeNode: TAVLTreeNode read GetYTreeNode write SetYTreeNode;
    property XTreeNode: TAVLTreeNode read GetXTreeNode write SetXTreeNode;
  published
    property OnStartDock;
    property OnEndDock;
  end;

implementation

uses
  InterfaceBase, LCLType, Math, LCLProc, main, settings, ExtCtrls,
  design_frame, epiutils;

{ TFieldEdit }

procedure TFieldEdit.OnFieldChange(Sender: TObject;
  EventType: TEpiFieldChangeEventType; OldValue: Pointer);
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
          Top  := FieldTop;
          Left := FieldLeft;
          Width := TDesignFrame(Parent).Canvas.TextWidth('W') * FieldLength;
          // Label:
          Self.FVariableLabel.Left := VarLabelLeft;
          Self.FVariableLabel.Top  := VarLabelTop;
          Self.FVariableLabel.Caption := VariableLabel;
        end;
      fceName:   FFieldNameLabel.Caption := FieldName;
      fceLength: Width := TDesignFrame(Parent).Canvas.TextWidth('W') * FieldLength;
      fceDecimals: ;
      fceFLeft: Left := FieldLeft;
      fceFTop: Top  := FieldTop;
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
            VarLabelLeft := VarLabelLeft - Max(0, Dx);
          end;
        end;
      fceVLeft: Self.FVariableLabel.Left := VarLabelLeft;
      fceVTop: Self.FVariableLabel.Top  := VarLabelTop;
      fceVColTxt:;
      fceVColBg:;
    end;
  end;
  if EventType in [fceUpdate, fceName, fceVarLabel, fceVLeft, fceVTop] then
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
         FieldLength-FieldDecimals-1, FieldDecimals, UTF8Decode(VariableLabel), FieldLeft, FieldTop]
      )
    else
      Hint := WideFormat(
        'Name: %s' + LineEnding +
        'Type: %s' + LineEnding +
        'Length: %d' + LineEnding +
        'Label: %s' + LineEnding +
        'X: %d, Y: %d',
        [UTF8Decode(FieldName), FieldTypeToFieldTypeName(FieldType, nil),
         FieldLength, UTF8Decode(VariableLabel), FieldLeft, FieldTop]
      );
  end;
end;

procedure TFieldEdit.SetField(const AValue: TEpiField);
begin
  if not Assigned(Parent) then
    Exit;

  FField := AValue;
  Field.RegisterOnChangeHook(@OnFieldChange);

  Top    := Field.FieldTop;
  Left   := Field.FieldLeft;
  Width  := TDesignFrame(Parent).Canvas.TextWidth('W') * Field.FieldLength;

  VariableLabel.Caption := Field.VariableLabel;
  // To avoid big empty space.
  if Field.VariableLabel = '' then
    VariableLabel.Caption := ' ';
  if Field.VarLabelLeft = 0 then
    Field.VarLabelLeft := Left - (VariableLabel.Width + 5)
  else
    VariableLabel.Left := Field.VarLabelLeft;
  if Field.VarLabelTop = 0 then
    Field.VarLabelTop := Top + (Height - VariableLabel.Height)
  else
    VariableLabel.Top := Field.VarLabelTop;

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

function TFieldEdit.GetId: String;
begin
  result := Field.Id;
end;

function TFieldEdit.GetEpiLeft: Integer;
begin
  result := Field.FieldLeft;
end;

function TFieldEdit.GetEpiTop: Integer;
begin
  result := Field.FieldTop;
end;

procedure TFieldEdit.SetEpiLeft(const AValue: Integer);
begin
  Field.FieldLeft := AValue;
end;

procedure TFieldEdit.SetEpiTop(const AValue: Integer);
begin
  Field.FieldTop := AValue;
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
  Left := Field.FieldLeft;
  Top  := Field.FieldTop;
  if Assigned(Parent) then
    Width := TDesignFrame(Parent).Canvas.TextWidth('W') * Field.FieldLength
  else
    Width := 8 * Field.FieldLength;
  VariableLabel.Caption := Field.VariableLabel;
  VariableLabel.Left := Field.VarLabelLeft;
  VariableLabel.Top  := Field.VarLabelTop;
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
  Field.BeginUpdate;
  Field.FieldLeft := Left;
  Field.FieldTop := Top;

  Field.VarLabelLeft := Left - FVariableLabelOffset.X;
  Field.VarLabelTop := Top - FVariableLabelOffset.Y;
  Field.EndUpdate;
  UpdateFieldNameLabel;
end;

{ TFieldLabel }

procedure TFieldLabel.SetTextLabel(const AValue: TEpiTextLabel);
begin
  FTextLabel := AValue;
  FTextLabel.RegisterOnChangeHook(@OnTextLabelChange);

  Top := TextLabel.TextTop;
  Left := TextLabel.TextLeft;

  Caption := TextLabel.Text;
  UpdateHint;
end;

function TFieldLabel.GetXTreeNode: TAVLTreeNode;
begin
  Result := FXTreeNode;
end;

function TFieldLabel.GetTextLabel: TEpiTextLabel;
begin
  result := FTextLabel;
end;

function TFieldLabel.GetEpiLeft: Integer;
begin
  result := TextLabel.TextLeft;
end;

function TFieldLabel.GetEpiTop: Integer;
begin
  result := TextLabel.TextTop;
end;

function TFieldLabel.GetId: String;
begin
  Result := TextLabel.Id;
end;

function TFieldLabel.GetYTreeNode: TAVLTreeNode;
begin
  Result := FYTreeNode;
end;

procedure TFieldLabel.OnTextLabelChange(Sender: TObject;
  EventType: TEpiTextLabelChangeEventType; OldValue: Pointer);
begin
  with TEpiTextLabel(Sender) do
  begin
    case EventType of
      tceUpdate:;
      tceId:;
      tceLeft: Left := TextLeft;
      tceTop:  Top  := TextTop;
      tceColTxt:;
      tceColHl:;
      tceColBg:;
    end;
  end;
  UpdateHint;
end;

procedure TFieldLabel.SetEpiLeft(const AValue: Integer);
begin
  TextLabel.TextLeft := AValue;
end;

procedure TFieldLabel.SetEpiTop(const AValue: Integer);
begin
  TextLabel.TextTop := AValue;
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
    'Label: %s' + LineEnding +
    'X: %d, Y: %d',
    [TextLabel.Id, TextLabel.Text, TextLabel.TextLeft, TextLabel.TextTop]
  );
end;

constructor TFieldLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TFieldLabel.Destroy;
begin
  inherited Destroy;
  FTextLabel := nil;
end;

{ TFieldAVLTree }

function TFieldAVLTree.Add(Data: Pointer): TAVLTreeNode;
begin
  Result := inherited Add(TControl(Data));
  case AVLTreeType of
    attX: (TControl(Data) as IEpiControl).XTreeNode := Result;
    attY: (TControl(Data) as IEpiControl).YTreeNode := Result;
  end;
end;

procedure TFieldAVLTree.WriteReportToStream(s: TStream; var StreamSize: int64);
var h: string;

  procedure WriteStr(const Txt: string);
  begin
    if s<>nil then
      s.Write(Txt[1],length(Txt));
    inc(StreamSize,length(Txt));
  end;

  procedure WriteTreeNode(ANode: TAVLTreeNode; const Prefix: string);
  var b: string;
  begin
    if ANode=nil then exit;
    WriteTreeNode(ANode.Right,Prefix+'  ');
    With TControl(ANode.Data) do
      b:=Prefix +
         Format(
           'Id: %-10.10s ' +
           'CName: %-11.11s ' +
           'Left:%-4d ' +
           'Top:%-4d',
           [(TControl(ANode.Data) as IEpiControl).Id, ClassName,
            Left, Top]) +
         LineEnding;

    WriteStr(b);
    WriteTreeNode(ANode.Left,Prefix+'  ');
  end;

// TAVLTree.WriteReportToStream
begin
  h:='Consistency: '+IntToStr(ConsistencyCheck)+' ---------------------'+#13#10;
  WriteStr(h);
  WriteTreeNode(Root,'  ');
  h:='-End-Of-AVL-Tree---------------------'+#13#10;
  WriteStr(h);
end;

end.

