unit UWorldView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Graphics, fgl,
  Controls, Menus, ExtCtrls, Dialogs, LCLIntf,
  UGeometryUtils, UViewport;

type
  TViewTracking = class;
  TToolMap = specialize TFPGMap<string, TViewTracking>;

  { TWorldDrawer }

  TWorldDrawer = class
    constructor Create(AViewport: TViewport); virtual;

  protected
    FViewport: TViewport;

  public
    destructor Destroy; override;
    procedure VLine(Canvas: TCanvas; AXValue: single);
    procedure HLine(Canvas: TCanvas; AYValue: single);
    procedure DrawAxisLineOn(Canvas: TCanvas); virtual;
    procedure FramePointOn(Canvas: TCanvas; AWorldPoint: TPointF; AUnitSize: integer);
      virtual;
    procedure FillPointOn(Canvas: TCanvas; AWorldPoint: TPointF; AUnitSize: integer);
      virtual;
    procedure FrameBoundsOn(Canvas: TCanvas; AWorldBounds: TRectangleF); virtual;
    procedure FillHandle(Canvas: TCanvas; At: TPointF); virtual;

    property Viewport: TViewport read FViewport write FViewport;
  end;


  { TWorldView }

  TWorldView = class(TPanel)
    constructor Create(AOwner: TComponent); override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: integer;
      MousePos: TPoint): boolean; override;

  protected
    FViewport: TViewport;
    FWorldDrawer: TWorldDrawer;
    FViewTracking: TViewTracking;
    FFirstResizeHandled: boolean;
    FShowExtentBounds: boolean;
    FShowAxisLine: boolean;
    FToolMap: TToolMap;
    FViewMoveRatio: single;

  public
    procedure HandleKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure HandleMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure HandleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure HandleMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure HandleMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
    procedure HandlePaint(Sender: TObject); virtual;
    procedure HandleResize(Sender: TObject);
    procedure SetWorldDrawer(ADrawer: TWorldDrawer);
  public
    destructor Destroy; override;

    procedure ChooseTool(toolName: string);

    function GetFitBounds: TRectangleF; virtual;

    procedure ViewFit;
    procedure ViewZoomDouble;
    procedure ViewZoomHalf;
    procedure ViewMoveUp;
    procedure ViewMoveDown;
    procedure ViewMoveRight;
    procedure ViewMoveLeft;

    property Viewport: TViewport read FViewport;
    property WorldDrawer: TWorldDrawer read FWorldDrawer write SetWorldDrawer;
    property ShowExtentBounds: boolean read FShowExtentBounds write FShowExtentBounds;
    property ShowAxisLine: boolean read FShowAxisLine write FShowAxisLine;
    property ViewMoveRatio: single read FViewMoveRatio write FViewMoveRatio;
  end;

  { TViewTracking }

  TViewTracking = class
  public
    constructor Create(AWorldView: TWorldView);
    destructor Destroy; override;
    procedure Reset; virtual;

    procedure TrackBegin(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
      virtual;
    procedure TrackMove(Shift: TShiftState; X, Y: integer); virtual;
    procedure TrackEnd(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
      virtual;
    procedure TrackWheel(Shift: TShiftState; WheelDelta: integer;
      MousePos: TPoint; var Handled: boolean); virtual;
  private
    procedure ViewMove(Shift: TShiftState; X, Y: integer);
  protected
    FMiddleDown: boolean;
    FCurrPoint: TPoint;
    FDownPoint: TPoint;
    FMovePoints: THVPointList;
    FWorldView: TWorldView;
  end;


implementation

uses
  LCLType;


constructor TWorldDrawer.Create(AViewport: TViewport);
begin
  inherited Create;
  FViewport := AViewport;
end;


destructor TWorldDrawer.Destroy;
begin
  inherited;
end;


procedure TWorldDrawer.VLine(Canvas: TCanvas; AXValue: single);
var
  xyPoint: TPointF;
  hvPoint: TPointF;
begin
  xyPoint.x := AXValue;
  xyPoint.y := 0;
  hvPoint := Viewport.WorldToDevice(xyPoint.x, xyPoint.y);
  Canvas.Line(round(hvPoint.x), 0, round(hvPoint.x), Canvas.Height);
end;

procedure TWorldDrawer.HLine(Canvas: TCanvas; AYValue: single);
var
  xyPoint: TPointF;
  hvPoint: TPointF;
begin
  xyPoint.x := 0;
  xyPoint.y := AYValue;
  hvPoint := Viewport.WorldToDevice(xyPoint.x, xyPoint.y);
  Canvas.Line(0, round(hvPoint.y), Canvas.Width, round(hvPoint.y));
end;

procedure TWorldDrawer.DrawAxisLineOn(Canvas: TCanvas);
begin
  VLine(Canvas, 0);
  HLine(Canvas, 0);
end;

procedure TWorldDrawer.FramePointOn(Canvas: TCanvas; AWorldPoint: TPointF;
  AUnitSize: integer);
var
  hvPoint: TPointF;
begin
  // TODO: FIXME
  hvPoint := Viewport.WorldToDevice(AWorldPoint.x, AWorldPoint.y);
  Canvas.Ellipse(round(hvPoint.x - AUnitSize), round(hvPoint.y - AUnitSize),
    round(hvPoint.x + AUnitSize), round(hvPoint.y + AUnitSize));
end;

procedure TWorldDrawer.FillPointOn(Canvas: TCanvas; AWorldPoint: TPointF;
  AUnitSize: integer);
var
  hvPoint: TPointF;
  savedColor: TColor;
begin
  // TODO: FIXME
  savedColor := Canvas.Brush.Color;
  Canvas.Brush.Color := Canvas.Pen.Color;
  hvPoint := Viewport.WorldToDevice(AWorldPoint.x, AWorldPoint.y);
  Canvas.Ellipse(round(hvPoint.x - AUnitSize), round(hvPoint.y - AUnitSize),
    round(hvPoint.x + AUnitSize), round(hvPoint.y + AUnitSize));
  Canvas.Brush.Color := savedColor;
end;

procedure TWorldDrawer.FrameBoundsOn(Canvas: TCanvas; AWorldBounds: TRectangleF);
var
  r: TRectangleF;
begin
  r := RectangleF(Viewport.WorldToDevice(AWorldBounds.Origin.x, AWorldBounds.Origin.y),
    Viewport.WorldToDevice(AWorldBounds.Corner.x, AWorldBounds.Corner.y));
  Canvas.Frame(round(r.Origin.x), round(r.Origin.y), round(r.Corner.x),
    round(r.Corner.y));
end;

procedure TWorldDrawer.FillHandle(Canvas: TCanvas; At: TPointF);
var
  handleSize: longint;
  hvPoint: TPointF;
  r: TRect;
begin
  handleSize := 4;
  hvPoint := Viewport.WorldToDevice(At.x, At.y);
  r := Rect(round(hvPoint.x) - handleSize, round(hvPoint.y) - handleSize,
    round(hvPoint.x) + handleSize, round(hvPoint.y) + handleSize);
  Canvas.FillRect(r);
end;


constructor TWorldView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FViewport := TViewport.Create;
  FViewport.ResetWorld;
  FViewport.ResetPortCenter;
  FShowExtentBounds := False;
  FShowAxisLine := True;
  FViewTracking := TViewTracking.Create(self);
  FToolMap := TToolMap.Create;
  OnPaint := @HandlePaint;
  OnResize := @HandleResize;
  OnKeyUp := @HandleKeyUp;
end;


destructor TWorldView.Destroy;
begin
  FViewTracking := nil;
  FreeAndNil(FViewport);
  FreeAndNil(FToolMap);
  inherited Destroy;
end;


procedure TWorldView.ChooseTool(toolName: string);
begin
  if Assigned(FViewTracking) then
    FViewTracking.Reset;
  FViewTracking := FToolMap.KeyData[toolName];
  FViewTracking.Reset;
end;

function TWorldView.GetFitBounds: TRectangleF;
begin
  Result := RectangleF(-100, -100, 100, 100);
end;


procedure TWorldView.HandleKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  case Key of
    VK_LEFT, VK_NUMPAD4:
    begin
      ViewMoveLeft;
    end;
    VK_RIGHT, VK_NUMPAD6:
    begin
      ViewMoveRight;
    end;
    VK_UP, VK_NUMPAD8:
    begin
      ViewMoveUp;
    end;
    VK_DOWN, VK_NUMPAD2:
    begin
      ViewMoveDown;
    end;
    VK_PRIOR, VK_ADD:
    begin
      ViewZoomDouble;
    end;
    VK_NEXT, VK_SUBTRACT:
    begin
      ViewZoomHalf;
    end;
    VK_HOME, VK_RETURN:
    begin
      ViewFit;
    end;
  end;
end;


procedure TWorldView.HandleMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if not Assigned(FWorldDrawer) then
    exit;
  if Assigned(FViewTracking) then
    FViewTracking.TrackBegin(Button, Shift, X, Y);
end;


procedure TWorldView.HandleMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
begin
  if not Assigned(FWorldDrawer) then
    exit;
  if Assigned(FViewTracking) then
    FViewTracking.TrackMove(Shift, X, Y);
end;


procedure TWorldView.HandleMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if not Assigned(FWorldDrawer) then
    exit;
  if Assigned(FViewTracking) then
    FViewTracking.TrackEnd(Button, Shift, X, Y);
end;


procedure TWorldView.HandleMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
begin
  if not Assigned(FWorldDrawer) then
    exit;
  if Assigned(FViewTracking) then
    FViewTracking.TrackWheel(Shift, WheelDelta, MousePos, Handled);
end;


procedure TWorldView.HandlePaint(Sender: TObject);
begin
  Canvas.Brush.Color := clBlack;
  Canvas.Clear;
  if not Assigned(FWorldDrawer) then
    exit;
  if ShowAxisLine then
  begin
    FWorldDrawer.DrawAxisLineOn(Canvas);
  end;
end;


procedure TWorldView.HandleResize(Sender: TObject);
begin
  Viewport.SetPortSize(ClientWidth, ClientHeight);
  if not FFirstResizeHandled then
  begin
    Viewport.ResetPortCenter;
    FFirstResizeHandled := True;
  end;
end;

procedure TWorldView.SetWorldDrawer(ADrawer: TWorldDrawer);
begin
  if Assigned(FWorldDrawer) then
    exit;
  ADrawer.Viewport := FViewport;
  FWorldDrawer := ADrawer;
end;

procedure TWorldView.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  HandleMouseDown(self, Button, Shift, X, Y);
end;


procedure TWorldView.MouseMove(Shift: TShiftState; X, Y: integer);
begin
  inherited MouseMove(Shift, X, Y);
  HandleMouseMove(self, Shift, X, Y);
end;


procedure TWorldView.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  HandleMouseUp(self, Button, Shift, X, Y);
end;


function TWorldView.DoMouseWheel(Shift: TShiftState; WheelDelta: integer;
  MousePos: TPoint): boolean;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  HandleMouseWheel(self, Shift, WheelDelta, MousePos, Result);
end;


procedure TWorldView.ViewFit;
begin
  ViewPort.SetWorldBounds(GetFitBounds);
  Invalidate;
end;


procedure TWorldView.ViewZoomDouble;
begin
  Viewport.SetWorldScale(ViewPort.WorldScale * 2.0);
  Invalidate;
end;


procedure TWorldView.ViewZoomHalf;
begin
  Viewport.SetWorldScale(ViewPort.WorldScale * 0.5);
  Invalidate;
end;


procedure TWorldView.ViewMoveUp;
begin
  ViewPort.ViewMoveFraction(0.0, ViewMoveRatio);
  Invalidate;
end;


procedure TWorldView.ViewMoveDown;
begin
  ViewPort.ViewMoveFraction(0.0, -ViewMoveRatio);
  Invalidate;
end;


procedure TWorldView.ViewMoveRight;
begin
  ViewPort.ViewMoveFraction(ViewMoveRatio, 0.0);
  Invalidate;
end;


procedure TWorldView.ViewMoveLeft;
begin
  ViewPort.ViewMoveFraction(-ViewMoveRatio, 0.0);
  Invalidate;
end;


constructor TViewTracking.Create(AWorldView: TWorldView);
begin
  FWorldView := AWorldView;
  if not Assigned(FMovePoints) then
    FMovePoints := THVPointList.Create;
  FMovePoints.Clear;
end;


destructor TViewTracking.Destroy;
begin
  FreeAndNil(FMovePoints);
  inherited Destroy;
end;

procedure TViewTracking.Reset;
begin
  FMovePoints.Clear;
  FMiddleDown := False;
end;

procedure TViewTracking.TrackBegin(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  if Button = mbMiddle then
  begin
    FMiddleDown := True;
    FDownPoint := Point(X, Y);
    FMovePoints.Add(FDownPoint);
  end;
end;

procedure TViewTracking.TrackMove(Shift: TShiftState; X, Y: integer);
begin
  if not FMiddleDown then
    exit;
  if FDownPoint = Point(X, Y) then
    exit;
  ViewMove(Shift, X, Y);
end;

procedure TViewTracking.TrackEnd(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  FMiddleDown := False;
end;

procedure TViewTracking.TrackWheel(Shift: TShiftState; WheelDelta: integer;
  MousePos: TPoint; var Handled: boolean);
var
  direction: single;
begin
  if WheelDelta < 0 then
    direction := -1.0
  else
    direction := 1.0;
  FWorldView.Viewport.WheelZoom(MousePos.x, MousePos.y, direction);
  FWorldView.Invalidate;
end;

procedure TViewTracking.ViewMove(Shift: TShiftState; X, Y: integer);
var
  p1, p2: TPoint;
  wp1, wp2, moved: TPointF;
begin
  FCurrPoint := Point(X, Y);
  FMovePoints.Add(FCurrPoint);
  if FMovePoints.Count > 2 then
  begin
    p1 := FMovePoints.Items[FMovePoints.Count - 2];
    p2 := FMovePoints.Items[FMovePoints.Count - 1];
    with FWorldView.WorldDrawer.Viewport do
    begin
      wp1 := DeviceToWorld(p1.x, p1.y);
      wp2 := DeviceToWorld(p2.x, p2.y);
      moved := wp2 - wp1;
      OffSetWorldCenter(-moved.x, -moved.y);
    end;
    FWorldView.Invalidate;
  end;
end;


end.
