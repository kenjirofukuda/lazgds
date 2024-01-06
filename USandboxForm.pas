unit USandboxForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, PairSplitter, ExtCtrls,
  StdCtrls, OpenGLContext, GL, GLU, UViewport, UMultiEvent, UGds;

type


  { TSandboxForm }

  TSandboxForm = class(TForm)
    OpenGLControl: TOpenGLControl;
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
    procedure OpenGLControlPaint(Sender: TObject);
    procedure OpenGLControlResize(Sender: TObject);
    procedure PairSplitterSide1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PanelAnyExit(Sender: TObject);
    procedure PanelAnyEnter(Sender: TObject);
    procedure TimerTimer(Sender: TObject);

    procedure OnUpdate(Sender, Arg: TObject);

  private
    FStructure: TGdsStructure;
    FViewport: TViewport;
    FEvents: TMultiEventReceive;
  end;

var
  SandboxForm: TSandboxForm;

implementation

{$R *.lfm}

uses
  LazLogger, UGdsStation, UGeometryUtils;

  { TSandboxForm }

procedure TSandboxForm.FormCreate(Sender: TObject);
begin
  FViewport := TViewPort.Create;
  FEvents := TMultiEventReceive.Create(nil);
  FEvents.OnUpdate := @OnUpdate;
  GdsStation.Events.Add(FEvents);
end;


procedure TSandboxForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FViewport);
  FreeAndNil(FEvents);
end;


procedure TSandboxForm.FormResize(Sender: TObject);
begin

end;


procedure TSandboxForm.OpenGLControlPaint(Sender: TObject);
var
  E: TGdsElement;
  I: integer;
  CE: TXY;
  bounds: TRectangleF;
begin
  glClearColor(0.0, 0.0, 0.0, 1.0);
  glClear(GL_COLOR_BUFFER_BIT);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  with OpenGLControl do
  begin
    gluOrtho2D(0, Width, 0, Height);
    FViewport.SetPortSize(Width, Height);
  end;
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  if Assigned(FStructure) then
  begin
    bounds := FStructure.GetExtentBounds;
    FViewport.SetWorldBounds(bounds);
    glTranslatef(FViewPort.GetPortCenter.x, FViewPort.GetPortCenter.y, 0.0);
    glScalef(FViewport.WorldScale * 0.9, FViewport.WorldScale * 0.9, 0.0);
    glTranslatef(-FViewPort.WorldCenter.x, -FViewPort.WorldCenter.y, 0.0);

    for E in FStructure.Elements do
    begin
      if Length(E.Coords) >= 2 then
      begin
        glBegin(GL_LINE_STRIP);
        for I := Low(E.Coords) to High(E.Coords) do
        begin
          CE := E.Coords[I];
          glVertex2d(CE[0], CE[1]);
        end;
        glEnd();
      end;
    end;
  end;
  OpenGLControl.SwapBuffers;
  Timer.Enabled := False;
end;


procedure TSandboxForm.OpenGLControlResize(Sender: TObject);
begin
  Timer.Enabled := True;
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


procedure TSandboxForm.PairSplitterSide1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin

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
  OpenGLControl.Invalidate;
end;


procedure TSandboxForm.OnUpdate(Sender, Arg: TObject);
begin
  if Arg.ClassType = TGdsStation then
  begin
    FStructure := (Arg as TGdsStation).GdsStructure;
    Timer.Enabled := True;
  end;
end;

end.
