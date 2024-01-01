unit UGdsStation;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, fgl, UGds;

type
  TLayerToColorMap = specialize TFPGMap<Int32, TColor>;

  { TGdsStation }

  TGdsStation = class
  private
    FGdsLibrary: TGdsLibrary;
    FGdsStructure: TGdsStructure;
    FGdsElement: TGdsElement;
    FLayerToColorMap: TLayerToColorMap;
  public
    destructor Destroy; override;
    function LayerToColor(ALayer: Int32): TColor;
    property GdsLibrary: TGdsLibrary read FGdsLibrary write FGdsLibrary;
    property GdsStructure: TGdsStructure read FGdsStructure write FGdsStructure;
    property GdsElement: TGdselement read FGdsElement write FGdsElement;
  end;

var
  GdsStation: TGdsStation;

implementation

uses
  LazLogger, UColorUtils;

destructor TGdsStation.Destroy;
begin
  DebugLn('TGdsStation.Destroy');
  FreeAndNil(FLayerToColorMap);
  FreeAndNil(FGdsLibrary);
end;

function TGdsStation.LayerToColor(ALayer: Int32): TColor;
var
  layers: TInt32s;
  colors: TColors;
  i: integer;
  color: TColor;
begin
  if FLayerToColorMap = nil then
  begin
    FLayerToColorMap := TLayerToColorMap.Create;
    layers := GdsLibrary.UsedLayerNumbers;
    colors := ColorWheel(Length(layers), 0.7, 1.0, 0.0);
    for i := Low(layers) to High(layers) do
    begin
      FLayerToColorMap[layers[i]] := colors[i];
    end;
  end;
  try
    color := FLayerToColorMap[ALayer];
  except
    on Exception do
       color := clGray;
  end;
  Result := color;
end;

initialization
  GdsStation := TGdsStation.Create;

finalization
  FreeAndNil(GdsStation);
end.
