program lazgds;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, UGdsBrowserForm, UGdsView, UGdsStation, USandboxForm;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Title := 'LazGds';
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TGdsBrowserForm, GdsBrowserForm);
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

