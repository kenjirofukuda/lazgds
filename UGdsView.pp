unit UGdsView;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, UWorldView;

type
  TGdsView = class(TWorldView)
    procedure HandlePaint(Sender: TObject); override;
  end;

implementation

uses
  Graphics, UGdsStation, UGeometryUtils;


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
  Canvas.TextOut(10, 30, StringFromRectangleF(
    GdsStation.GdsStructure.GetExtentBounds));
end;

end.
