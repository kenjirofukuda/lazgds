unit UGdsStation;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, fgl, UGds, UMultiEvent;

type
  TLayerToColorMap = specialize TFPGMap<int32, TColor>;

  { TGdsStation }

  TGdsStation = class
  private
    FGdsLibrary: TGdsLibrary;
    FGdsStructure: TGdsStructure;
    FGdsElement: TGdsElement;
    FLayerToColorMap: TLayerToColorMap;
    FEvents: TMultiEventSend;
  public
    constructor Create;
    destructor Destroy; override;
    function LayerToColor(ALayer: int32): TColor;
    procedure SetGdsStructure(AStructure: TGdsStructure);
    property GdsLibrary: TGdsLibrary read FGdsLibrary write FGdsLibrary;
    property GdsStructure: TGdsStructure read FGdsStructure write SetGdsStructure;
    property GdsElement: TGdselement read FGdsElement write FGdsElement;
    property Events: TMultiEventSend read FEvents;
  end;

var
  GdsStation: TGdsStation;

implementation

uses
  LazLogger, UColorUtils;


constructor TGdsStation.Create;
begin
  FEvents := TMultiEventSend.Create(nil);
end;


destructor TGdsStation.Destroy;
begin
  DebugLn('TGdsStation.Destroy');
  FreeAndNil(FLayerToColorMap);
  FreeAndNil(FGdsLibrary);
  FreeAndNil(FEvents);
end;


function TGdsStation.LayerToColor(ALayer: int32): TColor;
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


procedure TGdsStation.SetGdsStructure(AStructure: TGdsStructure);
begin
  if AStructure <> FGdsStructure then
  begin
    FGdsStructure := AStructure;
    Events.Notify(Self);
  end;
end;

initialization
  GdsStation := TGdsStation.Create;

finalization
  FreeAndNil(GdsStation);
end.
