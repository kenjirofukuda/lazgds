unit USandboxForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, PairSplitter, ExtCtrls,
  StdCtrls, OpenGLContext, GL, GLU, UViewport;

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
    procedure OpenGLControlPaint(Sender: TObject);
    procedure OpenGLControlResize(Sender: TObject);
    procedure PairSplitterSide1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PanelAnyExit(Sender: TObject);
    procedure PanelAnyEnter(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FViewport: TViewport;
  public

  end;

var
  SandboxForm: TSandboxForm;

implementation

{$R *.lfm}

uses
  LazLogger, UGds, UGdsStation, UGeometryUtils, UGdsBrowserForm;

  { TSandboxForm }

procedure TSandboxForm.FormCreate(Sender: TObject);
begin
  FViewport := TViewPort.Create;
end;


procedure TSandboxForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FViewport);
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
  E := GdsStation.GdsElement;
  if Assigned(GdsStation.GdsStructure) then
  begin
    bounds := GdsStation.GdsStructure.GetExtentBounds;
    FViewport.SetWorldBounds(bounds);
    glTranslatef(FViewPort.GetPortCenter.x, FViewPort.GetPortCenter.y, 0.0);
    glScalef(FViewport.WorldScale * 0.9, FViewport.WorldScale * 0.9, 0.0);
    glTranslatef(-FViewPort.WorldCenter.x, -FViewPort.WorldCenter.y, 0.0);

    for E in GdsStation.GdsStructure.Elements do
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
end;


procedure TSandboxForm.OpenGLControlResize(Sender: TObject);
begin

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

end.
