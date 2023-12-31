unit UGdsView;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, fgl, UWorldView, UGds;

type
  TGdsDrawer = class;
  TElementDrawer = class;
  TDrawerMap =  specialize TFPGMapObject<string, TElementDrawer>;

  TGdsView = class(TWorldView)
    constructor Create(AOwner: TComponent); override;
  public
    procedure HandlePaint(Sender: TObject); override;
    function GdsDrawer: TGdsDrawer;
  private
    FDrawerMap: TDrawerMap;
  end;

  TGdsDrawer = class(TWorldDrawer)
  public
    procedure StrokeCoords(ACanvas: TCanvas; ACoords: TCoords);
  end;

  TElementDrawer = class(TGdsDrawer)
    Element: TGdsElement;
    procedure DrawOn(ACanvas: TCanvas); virtual;
  end;

  TBoundaryDrawer = class(TElementDrawer)
    procedure DrawOn(ACanvas: TCanvas); override;
  end;

  TPathDrawer = class(TElementDrawer)
    procedure DrawOn(ACanvas: TCanvas); override;
  end;

  TTextDrawer = class(TElementDrawer)

  end;

  TBoxDrawer = class(TElementDrawer)

  end;

  TSrefDrawer = class(TElementDrawer)
    procedure DrawOn(ACanvas: TCanvas); override;
  end;

  TArefDrawer = class(TSrefDrawer)
    procedure DrawOn(ACanvas: TCanvas); override;
  end;


implementation

uses
  Types, UGdsStation, UGeometryUtils, LazLogger;


procedure TElementDrawer.DrawOn(ACanvas: TCanvas);
begin
  DebugLn('TElementDrawer.DrawOn');
  StrokeCoords(ACanvas, Element.Coords);
end;


procedure TBoundaryDrawer.DrawOn(ACanvas: TCanvas);
begin
  DebugLn('TBoundaryDrawer.DrawOn');
  inherited;
end;


procedure TPathDrawer.DrawOn(ACanvas: TCanvas);
begin
  DebugLn('TPathDrawer.DrawOn');
  inherited;
end;


procedure TSrefDrawer.DrawOn(ACanvas: TCanvas);
var
  Origin: TPointF;
begin
  DebugLn('TSrefDrawer.DrawOn');
  Origin := TPointF.Create(Element.Coords[0][0], Element.Coords[0][1]);
  DebugLn(StringFromPointF(Origin));
  FramePointOn(ACanvas, Origin, 4);
end;


procedure TArefDrawer.DrawOn(ACanvas: TCanvas);
var
  Origin: TPointF;
begin
  DebugLn('TArefDrawer.DrawOn');
  Origin := TPointF.Create(Element.Coords[0][0], Element.Coords[0][1]);
  DebugLn(StringFromPointF(Origin));
  FillPointOn(ACanvas, Origin, 4);
end;


constructor TGdsView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WorldDrawer := TGdsDrawer.Create(FViewport);
  FDrawerMap := TDrawerMap.Create;
  FDrawerMap['TGdsBoundary'] := TBoundaryDrawer.Create(FViewport);
  FDrawerMap['TGdsPath'] := TPathDrawer.Create(FViewport);
  FDrawerMap['TGdsText'] := TTextDrawer.Create(FViewport);
  FDrawerMap['TGdsBox'] := TBoxDrawer.Create(FViewport);
  FDrawerMap['TGdsSref'] := TSrefDrawer.Create(FViewport);
  FDrawerMap['TGdsAref'] := TArefDrawer.Create(FViewport);
end;


function TGdsView.GdsDrawer: TGdsDrawer; inline;
begin
  Result := (WorldDrawer as TGdsDrawer);
end;


procedure TGdsView.HandlePaint(Sender: TObject);
var
  E: TGdsElement;
  DP: TPointF;
  i: integer;
  GD: TElementDrawer;
begin
  Canvas.Brush.Color := clNavy;
  Canvas.Clear;

  if GdsStation.GdsStructure = nil then
    Exit;

  Canvas.Font.Color := clYellow;
  Canvas.TextOut(10, 10, GdsStation.GdsStructure.Name);
  Canvas.TextOut(10, 30, StringFromRectangleF(
    GdsStation.GdsStructure.GetExtentBounds));
//  GdsDrawer.Viewport.SetWorldBounds(GdsStation.GdsStructure.GetExtentBounds);
  Canvas.Pen.Color := clWhite;
  for E in GdsStation.GdsStructure.Elements do
  begin
    GD := FDrawerMap[E.ClassName];
    GD.Element := E;
    GD.DrawOn(Canvas);
//    GdsDrawer.StrokeCoords(Canvas, E.Coords);
  end;
end;


procedure TGdsDrawer.StrokeCoords(ACanvas: TCanvas; ACoords: TCoords);
var
  DP: TPointF;
  i: integer;
begin
  for i := 0 to High(ACoords) do
  begin
    DP := Viewport.WorldToDevice(ACoords[i][0], ACoords[i][1]);
    if i = 0 then
      ACanvas.MoveTo(trunc(DP.x), trunc(DP.y))
    else
      ACanvas.LineTo(trunc(DP.x), trunc(DP.y));
  end;

end;


end.
