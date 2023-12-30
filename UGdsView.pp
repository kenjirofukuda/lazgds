unit UGdsView;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, UWorldView, UGdsStation;

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
  if GdsStation.GdsStructure = nil then
     Exit;
  // Canvas.Brush.Color := clWhite;
  Canvas.TextOut(10, 10, GdsStation.GdsStructure.Name);
end;

end.
