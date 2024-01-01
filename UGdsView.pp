unit UGdsView;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, fgl, UWorldView, UGds;

type
  TGdsDrawer = class;
  TElementDrawer = class;
  TDrawerMap = specialize TFPGMapObject<string, TElementDrawer>;

  { TGdsView }

  TGdsView = class(TWorldView)
    constructor Create(AOwner: TComponent); override;
  public
    destructor Destroy; override;
    procedure HandlePaint(Sender: TObject); override;
    procedure DrawElements(AElements: TGdsElements);
    procedure DrawStructure(AStructure: TGdsStructure);
    function GdsDrawer: TGdsDrawer;
  private
    FDrawerMap: TDrawerMap;
  end;

  TGdsDrawer = class(TWorldDrawer)
  public
    procedure StrokeCoords(ACanvas: TCanvas; ACoords: TCoords);
  end;

  { TElementDrawer }

  TElementDrawer = class(TGdsDrawer)
    GdsView: TGdsView;
    Element: TGdsElement;
    procedure DrawOn(ACanvas: TCanvas); virtual;

    procedure DoStrokeOn(ACanvas: TCanvas); virtual;
    procedure StrokeOn(ACanvas: TCanvas); virtual;

    procedure DoFillOn(ACanvas: TCanvas); virtual;
    procedure FillOn(ACanvas: TCanvas); virtual;
  end;

  TBoundaryDrawer = class(TElementDrawer)
  end;

  TPathDrawer = class(TElementDrawer)
    procedure StrokeOn(ACanvas: TCanvas); override;
  end;

  TTextDrawer = class(TElementDrawer)

  end;

  TBoxDrawer = class(TElementDrawer)

  end;

  TSrefDrawer = class(TElementDrawer)
    procedure DrawOn(ACanvas: TCanvas); override;
  end;

  { TArefDrawer }

  TArefDrawer = class(TSrefDrawer)
    procedure DrawOn(ACanvas: TCanvas); override;
  end;


implementation

uses
  Types, UGdsStation, UGeometryUtils, UColorUtils, LazLogger;


procedure TElementDrawer.DrawOn(ACanvas: TCanvas);
begin
  DebugLn('TElementDrawer.DrawOn');
  DoFillOn(ACanvas);
  DoStrokeOn(ACanvas);
end;


procedure TElementDrawer.DoStrokeOn(ACanvas: TCanvas);
var
  savedColor: TColor;
begin
  savedColor := ACanvas.Pen.Color;
  ACanvas.Pen.Color := GdsStation.LayerToColor(Element.Layer);
  StrokeOn(ACanvas);
  ACanvas.Pen.Color := savedColor;
end;


procedure TElementDrawer.StrokeOn(ACanvas: TCanvas);
begin
  StrokeCoords(ACanvas, Element.Coords);
end;


procedure TElementDrawer.DoFillOn(ACanvas: TCanvas);
var
  savedColor: TColor;
begin
  savedColor := ACanvas.Brush.Color;
  StrokeOn(ACanvas);
  ACanvas.Brush.Color := savedColor;
end;


procedure TElementDrawer.FillOn(ACanvas: TCanvas);
begin

end;


procedure TPathDrawer.StrokeOn(ACanvas: TCanvas);
begin
  DebugLn('TPathDrawer.StrokeOn');
  // Path Center
  // inherited StrokeOn(ACanvas);
  // Path OutLine
  StrokeCoords(ACanvas, (Element as TGdsPath).OutlineCoords);
end;


procedure TSrefDrawer.DrawOn(ACanvas: TCanvas);
var
  Origin: TPointF;
  eSref: TGdsSref;
begin
  DebugLn('TSrefDrawer.DrawOn');
  //Origin := TPointF.Create(Element.Coords[0][0], Element.Coords[0][1]);
  //DebugLn(StringFromPointF(Origin));
  //FramePointOn(ACanvas, Origin, 4);
  eSref := (Element as TGdsSref);
  Viewport.PushTransform(eSref.GetTransform);
  GdsView.DrawStructure(eSref.RefStructure);
  Viewport.PopTransform;
end;


procedure TArefDrawer.DrawOn(ACanvas: TCanvas);
var
  Origin: TPointF;
begin
  DebugLn('TArefDrawer.DrawOn');
  //Origin := TPointF.Create(Element.Coords[0][0], Element.Coords[0][1]);
  //DebugLn(StringFromPointF(Origin));
  //FillPointOn(ACanvas, Origin, 4);
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


destructor TGdsView.Destroy;
begin
  FreeAndNil(FDrawerMap);
  inherited Destroy;
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
  textY: integer;
  textHeight: integer;


  procedure DrawColors;
  var
    ThisMany: integer;
    colors: TColors;
    i: integer;
    step: integer;
    x1, x2, y1, y2: integer;
    savedColor: TColor;
  begin
    ThisMany := 100;
    colors := ColorWheel(ThisMany, 0.7, 1.0, 0.0);
    step := Round(double(ClientWidth) / ThisMany);
    savedColor := Canvas.Brush.Color;
    y1 := ClientHeight - 50;
    y2 := ClientHeight;
    for i := 0 to ThisMany - 1 do
    begin
      Canvas.Brush.Color := colors[i];
      x1 := i * step;
      x2 := i * step + step;
      Canvas.FillRect(x1, y1, x2, y2);
    end;
    Canvas.Brush.Color := savedColor;
    colors := nil;
  end;


  procedure DrawDebugInfo;
  var
    stringList: TStringList;
    each: string;
  begin
    Canvas.Font.Color := clYellow;
    Canvas.Font.Name := 'Courier New';
    Canvas.Font.Size := 18;
    textHeight := trunc(Canvas.Font.Size * 1.8);
    textY := 10;
    Canvas.TextOut(10, textY, GdsStation.GdsStructure.Name);
    Inc(textY, textHeight);
    Canvas.TextOut(10, textY, StringFromRectangleF(
      GdsStation.GdsStructure.GetExtentBounds));
    if GdsStation.GdsElement <> nil then
    begin
      stringList := TStringList.Create;
      GdsStation.GdsElement.DebugStringsOn(stringList);
      for each in stringList do
      begin
        Inc(textY, textHeight);
        Canvas.Textout(10, textY, each);
      end;
      stringList.Free;
    end;
  end;

begin

  Canvas.Brush.Color := clBlack;
  Canvas.Clear;

  if GdsStation.GdsStructure = nil then
  begin
    DrawColors;
    Exit;
  end;

  DrawStructure(GdsStation.GdsStructure);
  if Assigned(GdsStation.GdsElement) then
  begin
    E := GdsStation.GdsElement;
    GD := FDrawerMap[E.ClassName];
    GD.Element := E;
    Canvas.Pen.Color := clWhite;
    Canvas.Pen.Width := 2;
    GD.StrokeOn(Canvas);
    Canvas.Pen.Width := 1;
  end;
  DrawDebugInfo;
end;


procedure TGdsView.DrawElements(AElements: TGdsElements);
var
  GD: TElementDrawer;
  E: TGdsElement;
begin
  for E in AElements do
  begin
    GD := FDrawerMap[E.ClassName];
    GD.Element := E;
    GD.GdsView := self;
    GD.DrawOn(Canvas);
  end;
end;


procedure TGdsView.DrawStructure(AStructure: TGdsStructure);
begin
  DrawElements(AStructure.Elements);
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
