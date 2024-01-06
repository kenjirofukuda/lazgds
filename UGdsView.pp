unit UGdsView;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, fgl, BGRATransform,
  UGeometryUtils, UWorldView, UGds;

type
  TGdsDrawer = class;
  TElementDrawer = class;
  TDrawerMap = specialize TFPGMapObject<string, TElementDrawer>;

  { TGdsView }

  TGdsView = class(TWorldView)
  private
    FDrawerMap: TDrawerMap;
    FDrawMilliSeconds: int64;
    FSelectedDrawing: integer;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure HandlePaint(Sender: TObject); override;
    function GetFitBounds: TRectangleF; override;

    { event handlers }
    procedure HandlePanelEnter(Sender: TObject);
    procedure HandlePanelExit(Sender: TObject);

    procedure DrawElements(ACanvas: TCanvas; AElements: TGdsElements);
    procedure DrawStructure(ACanvas: TCanvas; AStructure: TGdsStructure);
    function GdsDrawer: TGdsDrawer;
    property SelectedDrawing: integer read FSelectedDrawing;
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

    procedure DoStrokeOn(ACanvas: TCanvas);
    procedure StrokeOn(ACanvas: TCanvas); virtual;

    procedure DoFillOn(ACanvas: TCanvas);
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
  Types, LCLType, DateUtils, LazLogger,
  UGdsStation, UColorUtils, BGRABitmap, BGRABitmapTypes;


procedure TElementDrawer.DrawOn(ACanvas: TCanvas);
begin
  //DebugLn('TElementDrawer.DrawOn');
  // DoFillOn(ACanvas);
  DoStrokeOn(ACanvas);
end;


procedure TElementDrawer.DoStrokeOn(ACanvas: TCanvas);
begin
  //  savedColor := ACanvas.Pen.Color;
  if GdsView.SelectedDrawing > 0 then
  begin
    ACanvas.Pen.Color := clWhite;
    ACanvas.Pen.Width := 2;
  end
  else
  begin
    ACanvas.Pen.Color := GdsStation.LayerToColor(Element.Layer);
    ACanvas.Pen.Width := 1;
  end;
  StrokeOn(ACanvas);
  //  ACanvas.Pen.Color := savedColor;
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
  //DebugLn('TPathDrawer.StrokeOn');
  // Path Center
  // inherited StrokeOn(ACanvas);
  // Path OutLine
  StrokeCoords(ACanvas, (Element as TGdsPath).OutlineCoords);
end;


procedure TSrefDrawer.DrawOn(ACanvas: TCanvas);
var
  eSref: TGdsSref;
begin
  eSref := (Element as TGdsSref);
  Viewport.PushTransform(eSref.GetTransform);
  GdsView.DrawStructure(ACanvas, eSref.RefStructure);
  Viewport.PopTransform;
end;


procedure TArefDrawer.DrawOn(ACanvas: TCanvas);
var
  eAref: TGdsAref;
  otx: TAffineMatrix;
begin
  eAref := (Element as TGdsAref);
  for otx in eAref.RepeatedTransforms do
  begin
    Viewport.PushTransform(otx);
    GdsView.DrawStructure(ACanvas, eAref.RefStructure);
    Viewport.PopTransform;
  end;
end;


constructor TGdsView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TabStop := True;
  TabOrder := 2;
  OnEnter := @HandlePanelEnter;
  OnExit := @HandlePanelExit;
  WorldDrawer := TGdsDrawer.Create(FViewport);
  FDrawerMap := TDrawerMap.Create;
  FDrawerMap['TGdsBoundary'] := TBoundaryDrawer.Create(FViewport);
  FDrawerMap['TGdsPath'] := TPathDrawer.Create(FViewport);
  FDrawerMap['TGdsText'] := TTextDrawer.Create(FViewport);
  FDrawerMap['TGdsBox'] := TBoxDrawer.Create(FViewport);
  FDrawerMap['TGdsSref'] := TSrefDrawer.Create(FViewport);
  FDrawerMap['TGdsAref'] := TArefDrawer.Create(FViewport);
  FDrawMilliSeconds := -1;
  FViewMoveRatio := 0.25;
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


function TGdsView.GetFitBounds: TRectangleF;
begin
  Result := inherited GetFitBounds;
  if Assigned(GdsStation.GdsStructure) then
  begin
    Result := GdsStation.GdsStructure.GetExtentBounds;
  end;
end;


function BGRAColor(AColor: TColor): TBGRAPixel; inline;
begin
  Result := ColorToBGRA(ColorToRGB(AColor))
end;

procedure TGdsView.HandlePaint(Sender: TObject);
var
  textY: integer;
  textHeight: integer;
  startTime, endTime: TDateTime;

  procedure DrawExample(ACanvas: TCanvas);
  var
    BGRABitmap: TBGRABitmap;
  begin
    BGRABitmap := TBGRABitmap.Create(ClientWidth, ClientHeight); //, BGRAColor(clBlue));
    try
       BGRABitmap.DrawLine(0, 0, ClientWidth, ClientHeight, BGRAColor(clYellow), False);
       BGRABitmap.DrawLineAntialias(0, ClientHeight, ClientWidth, 0, BGRAColor(clYellow), False);
       BGRABitmap.Draw(ACanvas, 0, 0, False);
    finally
      FreeAndNil(BGRABitmap);
    end;
  end;


  procedure DrawColors(ACanvas: TCanvas);
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
      ACanvas.Brush.Color := colors[i];
      x1 := i * step;
      x2 := i * step + step;
      ACanvas.FillRect(x1, y1, x2, y2);
    end;
    ACanvas.Brush.Color := savedColor;
    colors := nil;
  end;


  procedure DrawDebugInfo(ACanvas: TCanvas);
  var
    stringList: TStringList;
    each: string;
  begin
    Canvas.Font.Color := clYellow;
    Canvas.Font.Name := 'Courier New';
    Canvas.Font.Size := 18;
    textHeight := trunc(Canvas.Font.Size * 1.8);
    textY := 10;
    ACanvas.TextOut(10, textY, GdsStation.GdsStructure.Name);
    Inc(textY, textHeight);
    ACanvas.TextOut(10, textY, StringFromRectangleF(
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
    ACanvas.TextOut(10, ClientHeight - textHeight,
      Format('DRAWTIME: %d msecs %6.3f secs', [FDrawMilliSeconds, FDrawMilliSeconds / 1000]));
  end;

begin
  Canvas.Brush.Color := clBlack;
  Canvas.Clear;
  if BorderWidth > 0 then
  begin
    Canvas.DrawFocusRect(ClientRect);
  end;
  if GdsStation.GdsStructure = nil then
  begin
    DrawExample(Canvas);
    DrawColors(Canvas);
    Exit;
  end;
  FSelectedDrawing := 0;
  startTime := Time;
  DrawStructure(Canvas, GdsStation.GdsStructure);
  endTime := Time;
  FDrawMilliSeconds := MilliSecondsBetween(endTime, startTime);
  DrawExample(Canvas);
  DrawDebugInfo(Canvas);
end;


procedure TGdsView.HandlePanelEnter(Sender: TObject);
begin
  if Sender is TGdsView then
  begin
    with (Sender as TGdsView) do
    begin
      BorderWidth := 3;
    end;
  end;

end;


procedure TGdsView.HandlePanelExit(Sender: TObject);
begin
  if Sender is TGdsView then
  begin
    with (Sender as TGdsView) do
    begin
      BorderWidth := 0;
    end;
  end;

end;


procedure TGdsView.DrawElements(ACanvas: TCanvas; AElements: TGdsElements);
var
  GD: TElementDrawer;
  E: TGdsElement;
begin
  for E in AElements do
  begin
    GD := FDrawerMap[E.ClassName];
    GD.Element := E;
    GD.GdsView := self;
    GD.DrawOn(ACanvas);
  end;
  if (Assigned(GdsStation.GdsElement)) and (ViewPort.PushedTransformCount = 0) then
  begin
    E := GdsStation.GdsElement;
    Inc(FSelectedDrawing);
    GD := FDrawerMap[E.ClassName];
    GD.Element := E;
    GD.GdsView := self;
    GD.DrawOn(ACanvas);
    Dec(FSelectedDrawing);
  end;
end;


procedure TGdsView.DrawStructure(ACanvas: TCanvas; AStructure: TGdsStructure);
begin
  DrawElements(ACanvas, AStructure.Elements);
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
