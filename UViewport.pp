unit UViewport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Math, LazLogger, BGRATransform, UGeometryUtils;

const
  VISIBLE_RATIO = 0.98;
  MIN_SCALE_LIMIT = 0.001;

type
  TMatrixList = array [0 .. 99] of TAffineMatrix;

  TViewport = class
    constructor Create;

  private
    FPortWidth: longint;
    FPortHeight: longint;
    FPortCenter: TPoint;

    FWorldCenter: TPointF;
    FWorldScale: single;

    FTransformStack: TMatrixList;
    FTransformCount: longint;
    FBasicTransform: TAffineMatrix;
    FTransform: TAffineMatrix;
    FBasicTransformPtr: ^TAffineMatrix;
    FTransformPtr: ^TAffineMatrix;

    function LookupBasicTransform: TAffineMatrix;
    function BasicTransform: TAffineMatrix;
    function LookupFinalTransform: TAffineMatrix;
    procedure DamageTransform; inline;
    function FittingRatio(X1, Y1, X2, Y2: single): single;

  public
    {Device Operations}
    procedure SetPortWidth(Width: integer);
    procedure SetPortHeight(Height: integer);
    procedure SetPortSize(Width, Height: integer);
    procedure SetPortCenter(H, V: integer);
    function GetPortCenter: TPoint;
    procedure ResetPortCenter;

    {World Operations}
    procedure SetWorldCenter(X, Y: single);
    procedure OffSetWorldCenter(DeltaX, DeltaY: single);
    procedure SetWorldScale(scale: single);
    procedure ResetWorld;

    procedure PushTransform(t: TAffineMatrix);
    function PopTransform: TAffineMatrix;

    function FittingTransform(X1, Y1, X2, Y2: single): TAffineMatrix;
    function Transform: TAffineMatrix;
    procedure SetWorldBounds(X1, Y1, X2, Y2: single);
    procedure SetWorldBounds(AWorldBounds: TRectangleF);

    function WorldToDevice(X, Y: single): TPointF;
    function DeviceToWorld(H, V: single): TPointF;
    procedure WheelZoom(H, V: integer; X, Y: single; Direction: single);
    procedure WheelZoom(H, V: integer; Direction: single);
    property PortHeight: longint read FPortHeight;
    property PortWidth: longint read FPortWidth;
    property WorldScale: single read FWorldScale;
  end;



implementation


constructor TViewport.Create;
begin
  FTransformPtr := nil;
  FBasicTransformPtr := nil;
  FWorldScale := 1.0;
  ResetPortCenter;
end;


procedure TViewport.SetPortWidth(Width: integer);
begin
  DebugLn(IntToStr(Width));
  if Width <> FPortWidth then
  begin
    FPortWidth := Width;
    DamageTransform;
  end;
end;


procedure TViewport.SetPortHeight(Height: integer);
begin
  DebugLn(IntToStr(Height));
  if Height <> FPortHeight then
  begin
    FPortHeight := Height;
    DamageTransform;
  end;
end;


procedure TViewport.SetPortSize(Width, Height: integer);
begin
  SetPortWidth(Width);
  SetPortHeight(Height);
end;


procedure TViewport.SetWorldCenter(X, Y: single);
begin
  if SameValue(FWorldCenter.x, X) and SameValue(FWorldCenter.y, Y) then
    exit;
  FWorldCenter.x := X;
  FWorldCenter.y := Y;
  DamageTransform;
end;


procedure TViewport.OffSetWorldCenter(DeltaX, DeltaY: single);
begin
  SetWorldCenter(FWorldCenter.x + DeltaX, FWorldCenter.y + DeltaY);
end;


procedure TViewport.SetWorldScale(scale: single);
begin
  if FWorldScale = scale then
    exit;
  FWorldScale := scale;
  DamageTransform;
end;


procedure TViewport.PushTransform(t: TAffineMatrix);
begin
  FTransformStack[FTransformCount] := t;
  Inc(FTransformCount);
  DamageTransform;
end;


function TViewport.PopTransform: TAffineMatrix;
begin
  Result := FTransformStack[FTransformCount - 1];
  Dec(FTransformCount);
  DamageTransform;
end;


function TViewport.LookupBasicTransform: TAffineMatrix;
var
  tx: TAffineMatrix;
begin
  tx := AffineMatrixTranslation(FPortCenter.x, (FPortHeight - FPortCenter.y));
  tx *= AffineMatrixScale(FWorldScale, -FWorldScale);
  tx *= AffineMatrixTranslation(-FWorldCenter.x, -FWorldCenter.y);
  Result := tx;
end;


function TViewport.FittingTransform(X1, Y1, X2, Y2: single): TAffineMatrix;
var
  tx: TAffineMatrix;
  ratio: double;
begin
  ratio := FittingRatio(X1, Y1, X2, Y2);
  tx := AffineMatrixIdentity;
  tx *= AffineMatrixTranslation(FPortCenter.x, FPortCenter.y);
  tx *= AffineMatrixScale(ratio, ratio);
  tx *= AffineMatrixScale(VISIBLE_RATIO, VISIBLE_RATIO);
  tx *= AffineMatrixTranslation(-MidValue(X2, X1), -MidValue(Y2, Y1));
  Result := tx;
end;


function TViewport.BasicTransform: TAffineMatrix;
begin
  if FBasicTransformPtr = nil then
    FBasicTransform := LookupBasicTransform;
  FBasicTransformPtr := @FBasicTransform;
  Result := FBasicTransform;
end;


function TViewport.LookupFinalTransform: TAffineMatrix;
var
  tx: TAffineMatrix;
  i: integer;
begin
  tx := AffineMatrixIdentity;
  tx *= BasicTransform;
  for i := 0 to FTransformCount - 1 do
  begin
    tx *= FTransformStack[i];
  end;
  Result := tx;
end;


function TViewport.Transform: TAffineMatrix;
begin
  if FTransformPtr = nil then
  begin
    FTransform := LookupFinalTransform;
    FTransformPtr := @FTransform;
  end;
  Result := FTransform;
end;


procedure TViewport.DamageTransform; inline;
begin
  FTransformPtr := nil;
  FBasicTransformPtr := nil;
end;


function TViewport.FittingRatio(X1, Y1, X2, Y2: single): single;
var
  hRatio, vRatio: double;
begin
  hRatio := FPortWidth / (Max(X2, X1) - Min(X2, X1));
  vRatio := FPortHeight / (Max(Y2, Y1) - Min(Y2, Y1));
  Result := Min(hRatio, vRatio);
end;


procedure TViewport.SetWorldBounds(X1, Y1, X2, Y2: single);
var
  ratio: double;
begin
  ratio := FittingRatio(X1, Y1, X2, Y2);
  ResetPortCenter;
  SetWorldScale(ratio);
  SetWorldCenter(MidValue(X2, X1), MidValue(Y2, Y1));
end;


procedure TViewport.SetWorldBounds(AWorldBounds: TRectangleF);
begin
  if not AWorldBounds.IsValid then
    exit;
  SetWorldBounds(AWorldBounds.Origin.x, AWorldBounds.Origin.y,
    AWorldBounds.Corner.x, AWorldBounds.Corner.y);
end;


procedure TViewport.ResetWorld;
begin
  SetWorldScale(1.0);
  SetWorldCenter(0, 0);
end;


procedure TViewport.ResetPortCenter;
begin
  SetPortCenter(trunc(FPortWidth / 2), trunc(FPortHeight / 2));
end;


function TViewport.WorldToDevice(X, Y: single): TPointF;
var
  pt: TPointF;
begin
  pt.x := X;
  pt.y := Y;
  Result := Transform * pt;
end;


function TViewport.DeviceToWorld(H, V: single): TPointF;
var
  pt, pt2: TPointF;
  t: TAffineMatrix;

begin
  pt.x := H;
  pt.y := V;
  t := Transform;
  if IsAffineMatrixInversible(t) then
    pt2 := AffineMatrixInverse(t) * pt
  else
    pt2 := pt;
  Result := pt2;
end;


procedure TViewport.WheelZoom(H, V: integer; X, Y: single; Direction: single);
begin
  SetPortCenter(H, V);
  SetWorldCenter(X, Y);
  SetWorldScale(FWorldScale * (1.0 + (0.125 * Direction)));
end;


procedure TViewport.WheelZoom(H, V: integer; Direction: single);
var
  xyCenter: TPointF;
begin
  xyCenter := DeviceToWorld(H, V);
  WheelZoom(H, V, xyCenter.x, xyCenter.y, Direction);
end;


procedure TViewport.SetPortCenter(H, V: integer);
begin
  FPortCenter.x := H;
  FPortCenter.y := FPortHeight - V;
  DamageTransform;
end;


function TViewport.GetPortCenter: TPoint;
begin
  Result := FPortCenter;
end;


end.
