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
    OnceTimer: TTimer;
    procedure OnceTimerTimer(Sender: TObject);
  private
    FStructure: TGdsStructure;
    FViewport: TViewport;
    FEvents: TMultiEventReceive;
    FLayerToFPColorMap: TLayerToFPColorMap;
    FTransparency: TGLTransparency;
    FFontColor: TFPColor;
    FDrawMilliSeconds: int64;
  published
    OpenGLView: TOpenGLControl;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    RedrawTimer: TTimer;
    procedure Button1KeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure Button1KeyPress(Sender: TObject; var Key: char);
    procedure Button1KeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OpenGLViewPaint(Sender: TObject);
    procedure OpenGLViewResize(Sender: TObject);
    procedure PanelAnyExit(Sender: TObject);
    procedure PanelAnyEnter(Sender: TObject);
    procedure RedrawTimerTimer(Sender: TObject);
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
    //   procedure SimpleTextOut(AX, AY: Integer; const AText: String);
    function LayerToFPColor(ALayer: int32): TFPColor;
  end;


var
  SandboxForm: TSandboxForm;

implementation

{$R *.lfm}

uses
  LazLogger, DateUtils, UGdsStation, UGeometryUtils, UColorUtils {, GLUT};

  { TSandboxForm }

procedure TSandboxForm.FormCreate(Sender: TObject);
var
  CmdCount: integer;
  Cmd: array of PChar;
  I: integer;
begin
  //CmdCount := Paramcount+1;
  //SetLength(Cmd,CmdCount);
  //for I := 0 to CmdCount - 1 do
  //   Cmd[I] := PChar(ParamStr(I));
  //glutInit (@CmdCount,@Cmd);
  FViewport := TViewPort.Create;
  FEvents := TMultiEventReceive.Create(nil);
  SetTransparency(0);
  FFontColor := TColorToFPColor(clYellow);
  FEvents.OnUpdate := @HandleUpdate;
  GdsStation.Events.Add(FEvents);
end;


procedure TSandboxForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FViewport);
  FreeAndNil(FEvents);
  FreeAndNil(FLayerToFPColorMap);
end;


procedure TSandboxForm.OpenGLViewPaint(Sender: TObject);
var
  bounds: TRectangleF;
  startTime, endTime: TDateTime;
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
    startTime := Time;
    DrawStructure(FStructure);
    endTime := Time;
    FDrawMilliSeconds := MilliSecondsBetween(endTime, startTime);
    DebugLn(Format('DRAWTIME: %d msecs %6.3f secs',
      [FDrawMilliSeconds, FDrawMilliSeconds / 1000]));
  end;
  //  glMatrixMode(GL_MODELVIEW);
  //  glLoadIdentity();
  //  SimpleTextOut(0, 0, 'DRAWTIME:');
  OpenGLView.SwapBuffers;
  RedrawTimer.Enabled := False;
end;


procedure TSandboxForm.OpenGLViewResize(Sender: TObject);
begin
  RedrawTimer.Enabled := True;
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  with OpenGLView do
  begin
    gluOrtho2D(0, Width, 0, Height);
    FViewport.SetPortSize(Width, Height);
  end;
end;

{ Fix: Can't draw open first in Windows }
procedure TSandboxForm.OnceTimerTimer(Sender: TObject);
begin
  OpenGLViewResize(nil);
  HandleUpdate(nil, GdsStation);
  OnceTimer.Enabled := False;
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


procedure TSandboxForm.RedrawTimerTimer(Sender: TObject);
begin
  OpenGLView.Invalidate;
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
    OpenGLViewResize(nil);
    {$ENDIF}
    RedrawTimer.Enabled := True;
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


//procedure TSandboxForm.SimpleTextOut(AX, AY: Integer; const AText: String);
//const
//  X_OFFSET = 0;
//  Y_OFFSET = 10;
//var
//  i: Integer;
//begin
//  SetGLColor(FFontColor);
//  glRasterPos2i(AX + X_OFFSET, AY + Y_OFFSET);
//  for i := 1 to Length(AText) do
//    glutBitmapCharacter(GLUT_BITMAP_8_BY_13, Ord(AText[i]));

//end;

end.
