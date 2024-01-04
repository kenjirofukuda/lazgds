unit USandboxForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, PairSplitter, ExtCtrls,
  StdCtrls;

type


  { TSandboxForm }

  TSandboxForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    GroupBox1: TGroupBox;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    procedure Button1KeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure Button1KeyPress(Sender: TObject; var Key: char);
    procedure Button1KeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure PairSplitterSide1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PanelAnyExit(Sender: TObject);
    procedure PanelAnyEnter(Sender: TObject);
  private

  public

  end;

var
  SandboxForm: TSandboxForm;

implementation

{$R *.lfm}

uses
  LazLogger;

  { TSandboxForm }

procedure TSandboxForm.FormCreate(Sender: TObject);
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

end.
