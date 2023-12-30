unit UGeometryUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Math, fgl;

type

  TPointF4 = array [0..3] of TPointF;
  { Smalltalk style }
  TRectangleF = object
    Origin: TPointF;
    Corner: TPointF;
  private
    function GetWidth: single;
    function GetHeight: single;
    function GetExtent: TPointF;
    function GetCenter: TPointF;
  public
    function Merge(ABounds: TRectangleF): TRectangleF;
    function IsValid: boolean;
    property Width: single read GetWidth;
    property Height: single read GetHeight;
    property Extent: TPointF read GetExtent;
    function CornerPoints: TPointF4;
  end;

  TXYPointList = specialize TFPGList<TPointF>;
  THVPointList = specialize TFPGList<TPoint>;

function MidValue(V1, V2: single): single; inline;

function PointF(X, Y: single): TPointF;
function RectangleF(X1, Y1, X2, Y2: single): TRectangleF;
function RectangleF(P1, P2: TPointF): TRectangleF;
function EmptyRectangleF: TRectangleF;
function CalcExtent(Points: TXYPointList): TRectangleF;
function StringFromPointF(PointF: TPointF): string;
function StringFromRectangleF(RectangleF: TRectangleF): string;


implementation


function TRectangleF.GetWidth: single;
begin
  Result := Corner.x - Origin.x;
end;


function TRectangleF.GetHeight: single;
begin
  Result := Corner.y - Origin.y;
end;


function TRectangleF.GetExtent: TPointF;
begin
  Result := Corner - Origin;
end;


function TRectangleF.GetCenter: TPointF;
begin
  Result := Corner - Origin;
end;


function PointF(X, Y: single): TPointF;
begin
  Result.x := X;
  Result.y := Y;
end;


function RectangleF(X1, Y1, X2, Y2: single): TRectangleF;
begin
  Result.Origin.x := Min(X1, X2);
  Result.Origin.y := Min(Y1, Y2);
  Result.Corner.x := Max(X1, X2);
  Result.Corner.y := Max(Y1, Y2);
end;


function RectangleF(P1, P2: TPointF): TRectangleF;
begin
  Result := RectangleF(P1.x, P1.y, P2.x, P2.y);
end;


function TRectangleF.Merge(ABounds: TRectangleF): TRectangleF;
var
  minX, minY, maxX, maxY: single;
begin
  minX := Min(Origin.x, ABounds.Origin.x);
  minY := Min(Origin.y, ABounds.Origin.y);
  maxX := Max(Corner.x, ABounds.Corner.x);
  maxY := Max(Corner.y, ABounds.Corner.y);
  Result := RectangleF(minX, minY, maxX, maxY);
end;


function TRectangleF.IsValid: boolean;
begin
  Result := False;
  if Origin.x >= Corner.x then
    exit;
  if Origin.y >= Corner.y then
    exit;
  Result := True;
end;


function MidValue(V1, V2: single): single;
begin
  Result := (Max(V2, V1) + Min(V2, V1)) * 0.5;
end;


function EmptyRectangleF: TRectangleF;
begin
  Result.Origin.x := single.MaxValue;
  Result.Origin.y := single.MaxValue;
  Result.Corner.x := single.MinValue;
  Result.Corner.y := single.MinValue;
end;


function TRectangleF.CornerPoints: TPointF4;
begin
  Result[0] := Origin;
  Result[1] := PointF(Origin.x, Corner.y);
  Result[2] := Corner;
  Result[3] := PointF(Corner.x, Origin.y);
end;


function CalcExtent(Points: TXYPointList): TRectangleF;
var
  P: TPointF;
begin
  Result := EmptyRectangleF;
  for P in Points do
  begin
    Result.Origin.x := Min(P.x, Result.Origin.x);
    Result.Origin.y := Min(P.y, Result.Origin.y);
    Result.Corner.x := Max(P.x, Result.Corner.x);
    Result.Corner.y := Max(P.x, Result.Corner.y);
  end;
end;


function StringFromPointF(PointF: TPointF): string;
begin
  Result := Format('(%6.4f, %6.4f)', [PointF.x, PointF.y]);
end;


function StringFromRectangleF(RectangleF: TRectangleF): string;
begin
  Result := Format('%s - %s', [StringFromPointF(RectangleF.Origin),
    StringFromPointF(RectangleF.Corner)]);
end;

end.
