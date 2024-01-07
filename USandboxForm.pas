unit USandboxForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fgl,
  Forms, Controls, Graphics, Dialogs, PairSplitter, ExtCtrls, StdCtrls,
  OpenGLContext, GL, GLU, UViewport, UMultiEvent, UGds, FPImage;

type

  TLayerToFPColorMap = specialize TFPGMap<int32, TFPColor>;
  TGLTransparency = 0..255;

  { TSandboxForm }

  TSandboxForm = class(TForm)
    TimerOnce: TTimer;
    procedure TimerOnceTimer(Sender: TObject);
  private
    FStructure: TGdsStructure;
    FViewport: TViewport;
    FEvents: TMultiEventReceive;
    FLayerToFPColorMap :TLayerToFPColorMap;
    FTransparency: TGLTransparency;
  published
    OpenGView: TOpenGLControl;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Timer: TTimer;
    procedure Button1KeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure Button1KeyPress(Sender: TObject; var Key: char);
    procedure Button1KeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure OpenGViewPaint(Sender: TObject);
    procedure OpenGViewResize(Sender: TObject);
    procedure PanelAnyExit(Sender: TObject);
    procedure PanelAnyEnter(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  public
    procedure DrawElement(const E: TGdsElement);
    procedure DrawStructure(const S: TGdsStructure);
    procedure HandleUpdate(Sender, Arg: TObject);
    procedure StrokeCoords(const ACoords: TCoords);
    property Viewport: TViewport read FViewport;
  private
    procedure StrokeBoundary(const E: TGdsBoundary);
    procedure StrokePath(const E: TGdsPath);
    procedure StrokeSrefAt(const E: TGdsSref; X, Y: double);
    procedure StrokeSref(const E: TGdsSref);
    procedure StrokeAref(const E: TGdsAref);
    procedure SetTransparency(ATransparency: TGLTransparency);
    procedure SetGLColor(AColor: TFPColor);
    function LayerToFPColor(ALayer: int32): TFPColor;
  end;


var
  SandboxForm: TSandboxForm;

implementation

{$R *.lfm}

uses
  LazLogger, UGdsStation, UGeometryUtils, UColorUtils;

  { TSandboxForm }

procedure TSandboxForm.FormCreate(Sender: TObject);
begin
  FViewport := TViewPort.Create;
  FEvents := TMultiEventReceive.Create(nil);
  SetTransparency(0);
  FEvents.OnUpdate := @HandleUpdate;
  GdsStation.Events.Add(FEvents);
end;


procedure TSandboxForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FViewport);
  FreeAndNil(FEvents);
  FreeAndNil(FLayerToFPColorMap);
end;


procedure TSandboxForm.FormResize(Sender: TObject);
begin

end;


procedure TSandboxForm.OpenGViewPaint(Sender: TObject);
var
  bounds: TRectangleF;
begin
  glClearColor(0.0, 0.0, 0.0, 1.0);
  glClear(GL_COLOR_BUFFER_BIT);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  if Assigned(FStructure) then
  begin
    bounds := FStructure.GetExtentBounds;
    FViewport.SetWorldBounds(bounds);
    glTranslatef(FViewPort.GetPortCenter.x, FViewPort.GetPortCenter.y, 0.0);
    glScalef(FViewport.WorldScale * 0.9, FViewport.WorldScale * 0.9, 0.0);
    glTranslatef(-FViewPort.WorldCenter.x, -FViewPort.WorldCenter.y, 0.0);

    DrawStructure(FStructure);
  end;
  OpenGView.SwapBuffers;
  Timer.Enabled := False;
end;


procedure TSandboxForm.OpenGViewResize(Sender: TObject);
begin
  Timer.Enabled := True;
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  with OpenGView do
  begin
    gluOrtho2D(0, Width, 0, Height);
    FViewport.SetPortSize(Width, Height);
  end;
end;

{ Fix: Can't draw open first in Windows }
procedure TSandboxForm.TimerOnceTimer(Sender: TObject);
begin
  HandleUpdate(nil, GdsStation);
  OpenGViewResize(nil);
  TimerOnce.Enabled := False;
end;

procedure TSandboxForm.Button1KeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  DebugLn('TSandboxForm.Button1KeyDown(%d)', [Key]);
end;


procedure TSandboxForm.Button1KeyPress(Sender: TObject; var Key: char);
begin
  DebugLn('TSandboxForm.Button1KeyPress(%d)', [integer(Key)]);

end;


procedure TSandboxForm.Button1KeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  DebugLn('TSandboxForm.Button1KeyUp(%d)', [Key]);
  DebugLn('');
end;



procedure TSandboxForm.PanelAnyExit(Sender: TObject);
begin
  if Sender is TPanel then
  begin
    with (Sender as TPanel) do
    begin
      BevelColor := clDefault;
      BevelWidth := 1;
    end;
  end;

end;


procedure TSandboxForm.PanelAnyEnter(Sender: TObject);
begin
  if Sender is TPanel then
  begin
    with (Sender as TPanel) do
    begin
      BevelColor := clYellow;
      BevelWidth := 3;
    end;
  end;

end;


procedure TSandboxForm.TimerTimer(Sender: TObject);
begin
  OpenGView.Invalidate;
end;


procedure TSandboxForm.DrawElement(const E: TGdsElement);
begin
  if E.ClassType = TGdsBoundary then
  begin
    SetGLColor(LayerToFPColor((E as TGdsBoundary).Layer));
    StrokeBoundary((E as TGdsBoundary));
    Exit;
  end;
  if E.ClassType = TGdsPath then
  begin
    SetGLColor(LayerToFPColor((E as TGdsPath).Layer));
    StrokePath((E as TGdsPath));
    Exit;
  end;
  if E.ClassType = TGdsSref then
  begin
    StrokeSref((E as TGdsSref));
    Exit;
  end;
  if E.ClassType = TGdsAref then
  begin
    StrokeAref((E as TGdsAref));
    Exit;
  end;
end;


procedure TSandboxForm.DrawStructure(const S: TGdsStructure);
var
  E: TGdsElement;
begin
  for E in S.Elements do
  begin
    DrawElement(E);
  end;
end;


procedure TSandboxForm.HandleUpdate(Sender, Arg: TObject);
begin
  if Arg.ClassType = TGdsStation then
  begin
    FStructure := (Arg as TGdsStation).GdsStructure;
    {$IFDEF Windows}
    OpenGViewResize(nil);
    {$ENDIF}
    Timer.Enabled := True;
  end;
end;


procedure TSandboxForm.StrokeCoords(const ACoords: TCoords);
var
  I: integer;
  CE: TXY;
begin
  glBegin(GL_LINE_STRIP);
  for I := Low(ACoords) to High(ACoords) do
  begin
    CE := ACoords[I];
    glVertex2d(CE[0], CE[1]);
  end;
  glEnd();
end;


procedure TSandboxForm.StrokeBoundary(const E: TGdsBoundary);
begin
  StrokeCoords(E.Coords);
end;


procedure TSandboxForm.StrokePath(const E: TGdsPath);
begin
  StrokeCoords(E.OutlineCoords);
end;


procedure TSandboxForm.StrokeSrefAt(const E: TGdsSref; X, Y: double);
begin
  glPushMatrix();
  glTranslated(X, Y, 0.0);
  glScaled(E.Mag, E.Mag, 1.0);
  glRotated(E.AngleDeg, 0.0, 0.0, 0.1);
  if E.IsRefrected then
    glScaled(1.0, -1.0, 0.0);
  DrawStructure(E.RefStructure);
  glPopMatrix();
end;


procedure TSandboxForm.StrokeSref(const E: TGdsSref);
begin
  StrokeSrefAt(E, E.Coords[0][0], E.Coords[0][1]);
end;


procedure TSandboxForm.StrokeAref(const E: TGdsAref);
var
  M: TAffineMatrix;
begin
  for M in E.RepeatedTransforms do
    StrokeSrefAt(E, M[1, 3], M[2, 3]);
end;

procedure TSandboxForm.SetTransparency(ATransparency: TGLTransparency);
begin
  FTransparency := ATransparency;
end;


function TSandboxForm.LayerToFPColor(ALayer: int32): TFPColor;
var
  layers: TInt32s;
  colors: TColors;
  i: integer;
  colorFP: TFPColor;
begin
  if FLayerToFPColorMap = nil then
  begin
    FLayerToFPColorMap := TLayerToFPColorMap.Create;
    layers := GdsStation.GdsLibrary.UsedLayerNumbers;
    colors := ColorWheel(Length(layers), 0.7, 1.0, 0.0);
    for i := Low(layers) to High(layers) do
    begin
      FLayerToFPColorMap[layers[i]] := TColorToFPColor(colors[i]);
    end;
  end;
  try
    colorFP := FLayerToFPColorMap[ALayer];
  except
    on Exception do
      colorFP := TColorToFPColor(clGray);
  end;
  Result := colorFP;
end;


procedure TSandboxForm.SetGLColor(AColor: TFPColor);
begin
  with AColor do
    glColor4us(red, green, blue, (255 - FTransparency) shl 8);
end;


end.
