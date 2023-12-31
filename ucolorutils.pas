unit UColorUtils;

{$mode ObjFPC}{$H+}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Graphics, Math;

type
  TColors = array of TColor;

function ColorWheel(AThisMany: integer; sat: double; bri: double; hue: double): TColors;

implementation

{ Assumes H, S, V in the range 0..1 and calculates the R, G, B values which are
  returned to be in the range 0..255.
  From: http://axonflux.com/handy-rgb-to-hsl-and-rgb-to-hsv-color-model-c
}
procedure HSVtoRGB(H, S, V: Double; out R, G, B: Integer);
var
  i: Integer;
  f: Double;
  p, q, t: Double;

  procedure MakeRgb(rr, gg, bb: Double);
  begin
    R := Round(rr * 255);
    G := Round(gg * 255);
    B := Round(bb * 255);
  end;

begin
  i := floor(H * 6);
  f := H * 6 - i;
  p := V * (1 - S);
  q := V * (1 - f*S);
  t := V * (1 - (1 - f) * S);
  case i mod 6 of
    0: MakeRGB(V, t, p);
    1: MakeRGB(q, V, p);
    2: MakeRGB(p, V, t);
    3: MakeRGB(p, q, V);
    4: MakeRGB(t, p, V);
    5: MakeRGB(V, p, q);
    else MakeRGB(0, 0, 0);
  end;
end;

function HSVToColor(H, S, V: Double): TColor;
var
  r, g, b: Integer;
begin
  HSVtoRGB(H, S, V, r, g, b);
  Result := RgbToColor(r, g, b);
end;


function ColorWheel(AThisMany: integer; sat: double; bri: double; hue: double): TColors;
var
  colors: TColors;
  i: integer;
  step: double;
begin
  colors := [];
  SetLength(colors, AThisMany);
  step := 1.0 / Max(AThisMany, 1);
  for i := 0 to AThisMany - 1 do
  begin
    colors[i] := HSVtoColor(hue + (i * step), sat, bri);
  end;
  Result := colors;
end;

end.

