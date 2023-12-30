unit UGdsView;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, UWorldView;

type
  TGdsView = class(TWorldView)
    procedure HandlePaint(Sender: TObject); override;
  end;

implementation


procedure TGdsView.HandlePaint(Sender: TObject);
begin
  // inherited;
  Canvas.Brush.Color := clBlack;
  Canvas.Clear;
  Canvas.Pen.Color := clWhite;
  Canvas.Line(0, 0, ClientWidth, ClientHeight);
end;

end.
